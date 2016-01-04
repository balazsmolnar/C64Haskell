module Screen where

import Data.List
import Data.Bits
import Data.Array
import Base
import Instructions
import C64

import Foreign.Ptr
import Foreign.Storable
import Control.Parallel
import Control.Monad

pokeBytes :: Storable a => Ptr b -> Int -> [a] -> IO ()
pokeBytes p _ [] = return ()
pokeBytes p off (x:xs) = do
    pokeByteOff p off x
    pokeBytes p (off+1) xs

updateScreen :: Ptr Byte -> CPUState -> IO ()
updateScreen buffer cpu = do
        updateHorizontalBorder buffer cpu [0..borderWidth-1]
        updateHorizontalBorder buffer cpu [borderWidth+200..screenHeight-1]
        updateVerticalBorder buffer cpu [borderWidth..screenHeight-borderWidth-1]
        let isHiRes = ((memory cpu) ! 0xD011) .&. 32 > 0
        if isHiRes then error "Hi res mode turned on"
        else do
            updateGraphics buffer cpu [(x,y) | x <- [0..39], y <- [0..24]]
            updateSprites buffer cpu [0..7]            
            return ()

updateCharLine buffer cpu ch line bkColor fgColor x y = do
        let addr = word8ToInt ch * 8 + line            
        let b = charByteAtAddress cpu addr
        let l = [testBit b a | a <- [7,6..0]]
        let colors = map (\a -> if a==True then fgColor else bkColor) l
        let memAddr = xyToIndex x (y+line)
        pokeBytes buffer memAddr colors

updateCharLineMultiColor buffer cpu ch line bkColor bkColor2 bkColor3 fgColor x y = do
        let addr = word8ToInt ch * 8 + line            
        let b = charByteAtAddress cpu addr
        let l = [(b `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]]
        let colors = map (\a -> case a of
                                    0 -> bkColor
                                    1 -> bkColor2
                                    2 -> bkColor3
                                    3 -> (fgColor .&. 7)
                          ) l
        let memAddr = xyToIndex x (y+line)
        pokeBytes buffer memAddr colors

updateCharOnScreen :: Ptr Byte -> CPUState -> Int -> Int -> IO ()
updateCharOnScreen buffer cpu x y = do
        let screenX = x*8 + borderWidth
        let screenY = y*8 + borderWidth
        let chAddress = charScreenStart cpu + y*40 + x
        let ch = (memory cpu) ! chAddress
        let bkColor = (memory cpu) ! 0xD021
        let bkColor2 = (memory cpu) ! 0xD022
        let bkColor3 = (memory cpu) ! 0xD023
        let fgColor = (memory cpu) ! (0xD800 + y*40 + x)
     
        let isMultiColor = (((memory cpu) ! 0xD016) .&. 16 > 0) && (fgColor .&. 8 > 0)
        
        if (isMultiColor) then do
            foldM (\acc x -> updateCharLineMultiColor buffer cpu ch x bkColor bkColor2 bkColor3 fgColor screenX screenY) () [0..7]
        else do
            foldM (\acc x -> updateCharLine buffer cpu ch x bkColor fgColor screenX screenY) () [0..7]
            
        return ()

updateGraphics :: Ptr Byte -> CPUState -> [(Int, Int)] -> IO ()
updateGraphics buffer cpu [] = return ()
updateGraphics buffer cpu (x:xs) = do
        updateCharOnScreen buffer cpu (fst x) (snd x)
        updateGraphics buffer cpu xs
        return ()

updateSprites :: Ptr Byte -> CPUState -> [Int] -> IO ()
updateSprites buffer cpu [] = return ()
updateSprites buffer cpu (x:xs) = do
        updateSprite buffer cpu x
        updateSprites buffer cpu xs
        return ()

updateSprite :: Ptr Byte -> CPUState -> Int -> IO ()
updateSprite buffer cpu index = do

    let enabled = ((memory cpu) ! 0xD015) `testBit` index
    if enabled then do
        let screenX = word8ToInt ((memory cpu) ! (0xD000+index*2)) + borderWidth - 24 + if ((memory cpu) ! 0xD010) `testBit` index  then 256 else 0
        let screenY = word8ToInt ((memory cpu) ! (0xD001+index*2)) + borderWidth - 50
        let spriteMemory = word8ToInt ((memory cpu) ! (index + 1016 + charScreenStart cpu)) * 64 + vicBank cpu
        let fgColor = ((memory cpu) ! (0xD027+index)) .&. 0x0F
        let isMultiColor = ((memory cpu) ! 0xD01C) `testBit` index
        let bkColor1 = ((memory cpu) ! 0xD025)
        let bkColor2 = ((memory cpu) ! 0xD026)

        if (isMultiColor) then do
            foldM (\acc x -> updateSpriteLineMultiColor buffer cpu x fgColor bkColor1 bkColor2 screenX (screenY+x) (spriteMemory+x*3)) () [0..20]
        else do
            foldM (\acc x -> updateSpriteLine buffer cpu x fgColor screenX (screenY+x) (spriteMemory+x*3)) () [0..20]
        return ()
    else do
        return ()

updateSpriteLine :: Ptr Byte -> CPUState -> Int -> Byte -> Int -> Int -> Int -> IO ()
updateSpriteLine buffer cpu spriteIndex fgColor screenX screenY spriteMemory = do
    let spriteByte1 = (memory cpu) ! spriteMemory
    let spriteByte2 = (memory cpu) ! (spriteMemory+1)
    let spriteByte3 = (memory cpu) ! (spriteMemory+2)
    let l = [spriteByte1 `testBit` a | a <- [7,6..0]] ++ [spriteByte2 `testBit` a | a <- [7,6..0]] ++ [spriteByte3 `testBit` a | a <- [7,6..0]]
    let memAddr = xyToIndex screenX screenY

    let addrValue = [(fst addr, fgColor) | addr <- zip [memAddr..memAddr+23] l, snd addr]
    foldM (\acc x -> pokeByteOff buffer (fst x) (snd x)) () addrValue
    return ()

updateSpriteLineMultiColor :: Ptr Byte -> CPUState -> Int -> Byte -> Byte -> Byte -> Int -> Int -> Int -> IO ()
updateSpriteLineMultiColor buffer cpu spriteIndex fgColor bkColor1 bkColor2 screenX screenY spriteMemory = do
    let spriteByte1 = (memory cpu) ! spriteMemory
    let spriteByte2 = (memory cpu) ! (spriteMemory+1)
    let spriteByte3 = (memory cpu) ! (spriteMemory+2)
    
    let l = [(spriteByte1 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]] ++ [(spriteByte2 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]] ++ [(spriteByte3 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]]
    let colors = map (\a -> case a of
                                    0 -> 0xFF
                                    1 -> bkColor1
                                    2 -> bkColor2
                                    3 -> (fgColor .&. 7)
                          ) l

    let memAddr = xyToIndex screenX screenY

    let addrValue = [(fst addr, snd addr) | addr <- zip [memAddr..memAddr+23] colors, snd addr /= 0xFF]
    foldM (\acc x -> pokeByteOff buffer (fst x) (snd x)) () addrValue
    return ()

updateHorizontalBorder :: Ptr Byte -> CPUState -> [Int] -> IO ()
updateHorizontalBorder buffer cpu [] = return ()
updateHorizontalBorder buffer cpu (x:xs) = do
        let color = (memory cpu) ! 0xD020
        pokeBytes buffer (xyToIndex 0 x) (take screenWidth $ repeat color)
        updateHorizontalBorder buffer cpu xs

updateVerticalBorder :: Ptr Byte -> CPUState -> [Int] -> IO ()
updateVerticalBorder buffer cpu [] = return ()
updateVerticalBorder buffer cpu (x:xs) = do
        let color = (memory cpu) ! 0xD020
        pokeBytes buffer (xyToIndex 0 x) (take borderWidth $ repeat color)
        pokeBytes buffer (xyToIndex (screenWidth-borderWidth-1) x) (take borderWidth $ repeat color)
        updateVerticalBorder buffer cpu xs

vicBank cpu = 
    case (((memory cpu) ! 0xDD00) .&. 0x03) of
            0 -> 0xC000
            1 -> 0x8000
            2 -> 0x4000
            3 -> 0x0000
        
charScreenStart cpu = 
    let startOffset = word8ToInt(((memory cpu) ! 0xD018) `shiftR` 4) * 1024 in
    vicBank cpu + startOffset
    
charByteAtAddress cpu addr 
    | (vicBank cpu == 0) && (startOffset == 4096) = (characterROM cpu) ! addr
    | otherwise =  (memory cpu) ! (vicBank cpu + startOffset + addr)
    where startOffset = word8ToInt((((memory cpu) ! 0xD018) `shiftR` 1) .&. 7) * 2048
-- error $ (show (vicBank cpu)) ++ "  " ++ show startOffset--