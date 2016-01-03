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
            return ()

updateCharLine buffer cpu ch line bkColor fgColor x y = do
        let addr = (word8ToInt (ch Data.Bits..&. 0x7F)) * 8 + line            
        let b = charByteAtAddress cpu addr
        let l = if ch < 128 then [testBit b a | a <- [7,6..0]] else [not (testBit b a) | a <- [7,6..0]]
        let colors = map (\a -> if a==True then fgColor else bkColor) l
        let memAddr = xyToIndex x (y+line)
        pokeBytes buffer memAddr colors

updateCharOnScreen :: Ptr Byte -> CPUState -> Int -> Int -> IO ()
updateCharOnScreen buffer cpu x y = do
        let screenX = x*8 + borderWidth
        let screenY = y*8 + borderWidth
        let chAddress = charScreenStart cpu + y*40 + x
        let ch = (memory cpu) ! chAddress
        let bkColor = (memory cpu) ! 0xD021
        let fgColor = (memory cpu) ! (0xD800 + y*40 + x)
        updateCharLine buffer cpu ch 0 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 1 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 2 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 3 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 4 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 5 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 6 bkColor fgColor screenX screenY
        updateCharLine buffer cpu ch 7 bkColor fgColor screenX screenY
        return ()

updateGraphics :: Ptr Byte -> CPUState -> [(Int, Int)] -> IO ()
updateGraphics buffer cpu [] = return ()
updateGraphics buffer cpu (x:xs) = do
        updateCharOnScreen buffer cpu (fst x) (snd x)
        updateGraphics buffer cpu xs
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