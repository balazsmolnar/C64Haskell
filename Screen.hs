--
-- Creates screen content from memory
--
module Screen (createScreenBitmap) where

import Data.List
import Data.Bits
import Data.Array
import Base
import Instructions
import C64

import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.Marshal.Alloc
import Graphics.Win32
import Control.Monad

--
-- Public
--

createScreenBitmap :: CPUState -> Graphics.Win32.HDC -> IO Graphics.Win32.HBITMAP
createScreenBitmap cpu dc = do 
    let size = fromIntegral screenWidth*screenHeight
    let headerBytes = [
            40, 0, 0, 0,                                                                      -- DWORD biSize; 
            intToWord8 (screenWidth `mod` 256), intToWord8 (screenWidth `div` 256), 0, 0,     -- LONG  biWidth;
            intToWord8 (screenHeight `mod` 256), intToWord8 (screenHeight `div` 256), 0, 0,   -- LONG  biHeight;
                  1, 0,                                                                       -- WORD  biPlanes;
                  8, 0,                                                                       -- WORD  biBitCount
            0, 0, 0, 0,                                                                       -- DWORD biCompression;
            0, 0, 0, 0,                                                                       -- DWORD biSizeImage;
            0, 0, 0, 0,                                                                       -- DWORD biXPelsPerMeter;
            0, 0, 0, 0,                                                                       -- DWORD biYPelsPerMeter;
            16, 0, 0, 0,                                                                      -- DWORD biClrUsed;
            16, 0, 0, 0 ]                                                                     -- DWORD biClrImportant;
       
    pBitmapInfo <- mallocBytesFromList (headerBytes ++ colorCodes)
    
    let bytes = Foreign.Marshal.Alloc.mallocBytes size
    pb <- bytes
    let pb2 = castPtr pb::Ptr Byte
    drawScreen pb2 cpu

    hBitmap <- Graphics.Win32.createDIBitmap dc 
                (castPtr pBitmapInfo ::Graphics.Win32.LPBITMAPINFOHEADER)
                Graphics.Win32.cBM_INIT pb
                (castPtr pBitmapInfo ::Graphics.Win32.LPBITMAPINFO) 
                Graphics.Win32.dIB_RGB_COLORS
                
    Foreign.Marshal.Alloc.free pb   
    Foreign.Marshal.Alloc.free pBitmapInfo   
    return hBitmap


drawScreen :: Ptr Byte -> CPUState -> IO ()
drawScreen buffer cpu = do
    drawHorizontalBorder buffer cpu [0..borderWidth-1]
    drawHorizontalBorder buffer cpu [borderWidth+200..screenHeight-1]
    drawVerticalBorder buffer cpu [borderWidth..screenHeight-borderWidth-1]
    let isHiRes = ((memory cpu) ! 0xD011) .&. 32 > 0
    if isHiRes then 
        error "Hi res mode turned on. Not implemented"
    else do
        drawGraphicsInCharMode buffer cpu [(x,y) | x <- [0..39], y <- [0..24]]
        drawSprites buffer cpu [0..7]            
        return ()

--
-- Draw characters
--            

drawGraphicsInCharMode :: Ptr Byte -> CPUState -> [(Int, Int)] -> IO ()
drawGraphicsInCharMode buffer cpu [] = return ()
drawGraphicsInCharMode buffer cpu (x:xs) = do
    drawCharOnScreen buffer cpu (fst x) (snd x)
    drawGraphicsInCharMode buffer cpu xs
    return ()

drawCharOnScreen :: Ptr Byte -> CPUState -> Int -> Int -> IO ()
drawCharOnScreen buffer cpu x y = do
    let screenX = x*8 + borderWidth
    let screenY = y*8 + borderWidth
    let chAddress = charScreenStartAddress cpu + y*40 + x
    let ch = (memory cpu) ! chAddress
    let bkColor = (memory cpu) ! 0xD021
    let bkColor2 = (memory cpu) ! 0xD022
    let bkColor3 = (memory cpu) ! 0xD023
    let fgColor = (memory cpu) ! (0xD800 + y*40 + x)
 
    let isMultiColor = (((memory cpu) ! 0xD016) .&. 16 > 0) && (fgColor .&. 8 > 0)
    let vBank = (vicBank cpu)
    if (isMultiColor) then do
        forM_  [0..7] (\x -> drawCharLineMultiColor buffer cpu ch x bkColor bkColor2 bkColor3 fgColor screenX screenY vBank) 
    else do
        forM_  [0..7] (\x -> drawCharLine buffer cpu ch x bkColor fgColor screenX screenY vBank) 
        
    return ()

drawCharLine buffer cpu ch line bkColor fgColor x y vBank = do
    let addr = word8ToInt ch * 8 + line            
    let b = charByteAtAddress cpu addr vBank
    let l = [testBit b a | a <- [7,6..0]]
    let colors = map (\a -> if a==True then fgColor else bkColor) l
    let memAddr = xyToIndex x (y+line)
    pokeBytes buffer memAddr colors

drawCharLineMultiColor buffer cpu ch line bkColor bkColor2 bkColor3 fgColor x y vBank = do
    let addr = word8ToInt ch * 8 + line            
    let b = charByteAtAddress cpu addr vBank
    let l = [(b `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]]
    let colors = map (\a -> case a of
                                0 -> bkColor
                                1 -> bkColor2
                                2 -> bkColor3
                                3 -> (fgColor .&. 7)
                      ) l
    let memAddr = xyToIndex x (y+line)
    pokeBytes buffer memAddr colors

--
-- Draw sprites
--            
        
drawSprites :: Ptr Byte -> CPUState -> [Int] -> IO ()
drawSprites buffer cpu [] = return ()
drawSprites buffer cpu (x:xs) = do
    drawSprite buffer cpu x
    drawSprites buffer cpu xs
    return ()

drawSprite :: Ptr Byte -> CPUState -> Int -> IO ()
drawSprite buffer cpu index = do

    let enabled = ((memory cpu) ! 0xD015) `testBit` index
    if enabled then do
        let screenX = word8ToInt ((memory cpu) ! (0xD000+index*2)) + borderWidth - 24 + if ((memory cpu) ! 0xD010) `testBit` index  then 256 else 0
        let screenY = word8ToInt ((memory cpu) ! (0xD001+index*2)) + borderWidth - 50
        let spriteMemory = word8ToInt ((memory cpu) ! (index + 1016 + charScreenStartAddress cpu)) * 64 + vicBank cpu
        let fgColor = ((memory cpu) ! (0xD027+index)) .&. 0x0F
        let isMultiColor = ((memory cpu) ! 0xD01C) `testBit` index
        let extraColor1 = ((memory cpu) ! 0xD025)
        let extraColor2 = ((memory cpu) ! 0xD026)

        if (isMultiColor) then do
            forM_  [0..20] (\x -> drawSpriteLineMultiColor buffer cpu x fgColor extraColor1 extraColor2 screenX (screenY+x) (spriteMemory+x*3)) 
        else do
            forM_  [0..20] (\x -> drawSpriteLine buffer cpu x fgColor screenX (screenY+x) (spriteMemory+x*3))
        return ()
    else do
        return ()

drawSpriteLine :: Ptr Byte -> CPUState -> Int -> Byte -> Int -> Int -> Int -> IO ()
drawSpriteLine buffer cpu spriteIndex fgColor screenX screenY spriteMemory = do
    let spriteByte1 = (memory cpu) ! spriteMemory
    let spriteByte2 = (memory cpu) ! (spriteMemory+1)
    let spriteByte3 = (memory cpu) ! (spriteMemory+2)
    let l = [spriteByte1 `testBit` a | a <- [7,6..0]] ++ [spriteByte2 `testBit` a | a <- [7,6..0]] ++ [spriteByte3 `testBit` a | a <- [7,6..0]]
    let memAddr = xyToIndex screenX screenY

    let addrValue = [(fst addr, fgColor) | addr <- zip [memAddr..memAddr+23] l, snd addr]
    forM_ addrValue (\x -> pokeByteOff buffer (fst x) (snd x))
    return ()

drawSpriteLineMultiColor :: Ptr Byte -> CPUState -> Int -> Byte -> Byte -> Byte -> Int -> Int -> Int -> IO ()
drawSpriteLineMultiColor buffer cpu spriteIndex fgColor extraColor1 extraColor2 screenX screenY spriteMemory = do

    let spriteByte1 = (memory cpu) ! spriteMemory
    let spriteByte2 = (memory cpu) ! (spriteMemory+1)
    let spriteByte3 = (memory cpu) ! (spriteMemory+2)
    
    let l = [(spriteByte1 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]] ++ 
            [(spriteByte2 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]] ++ 
            [(spriteByte3 `shiftR` a) .&. 3 | a <- [6, 6, 4, 4, 2, 2, 0, 0]]
            
    let colors = map (\a -> case a of
                                    0 -> 0xFF
                                    1 -> extraColor1
                                    2 -> fgColor
                                    3 -> extraColor2
                          ) l

    let memAddr = xyToIndex screenX screenY

    let addrValue = [(fst addr, snd addr) | addr <- zip [memAddr..memAddr+23] colors, snd addr /= 0xFF]
    forM_ addrValue(\x -> pokeByteOff buffer (fst x) (snd x))
    return ()

--
-- Border
--
    
drawHorizontalBorder :: Ptr Byte -> CPUState -> [Int] -> IO ()
drawHorizontalBorder buffer cpu [] = return ()
drawHorizontalBorder buffer cpu (x:xs) = do
    let color = (memory cpu) ! 0xD020
    pokeBytes buffer (xyToIndex 0 x) (take screenWidth $ repeat color)
    drawHorizontalBorder buffer cpu xs

drawVerticalBorder :: Ptr Byte -> CPUState -> [Int] -> IO ()
drawVerticalBorder buffer cpu [] = return ()
drawVerticalBorder buffer cpu (x:xs) = do
    let color = (memory cpu) ! 0xD020
    pokeBytes buffer (xyToIndex 0 x) (take borderWidth $ repeat color)
    pokeBytes buffer (xyToIndex (screenWidth-borderWidth-1) x) (take borderWidth $ repeat color)
    drawVerticalBorder buffer cpu xs

--
-- Helper
--
        
vicBank cpu = 
    case ((memory cpu) ! 0xDD00) .&. 0x03 of
        0 -> 0xC000
        1 -> 0x8000
        2 -> 0x4000
        3 -> 0x0000
        
charScreenStartAddress cpu = 
    let startOffset = word8ToInt(((memory cpu) ! 0xD018) `shiftR` 4) * 1024 in
    vicBank cpu + startOffset
    
charByteAtAddress cpu relativeAddress vBank
    | (vBank == 0) && (startOffset == 4096) = (characterROM cpu) ! relativeAddress
    | otherwise =  (memory cpu) ! (vBank + startOffset + relativeAddress)
    where startOffset = word8ToInt((((memory cpu) ! 0xD018) `shiftR` 1) .&. 7) * 2048

pokeBytes :: Storable a => Ptr b -> Int -> [a] -> IO ()
pokeBytes p _ [] = return ()
pokeBytes p off (x:xs) = do
    pokeByteOff p off x
    pokeBytes p (off+1) xs

mallocBytesFromList :: [Byte] -> IO (Ptr ())
mallocBytesFromList xs = do
    pBuffer <- Foreign.Marshal.Alloc.mallocBytes $ length xs
    let pByteBuffer = castPtr pBuffer::Ptr Byte
    pokeBytes pByteBuffer 0 xs
    return pBuffer
    
colorCodes = [
    0x00, 0x00, 0x00, 0x00,
    0xFF, 0xFF, 0xFF, 0x00,
    0x2B, 0x37, 0x68, 0x00,
    0xB2, 0xA4, 0x70, 0x00,
    0x86, 0x3D, 0x6F, 0x00,
    0x43, 0x8D, 0x58, 0x00,
    0x79, 0x28, 0x35, 0x00,
    0x6F, 0xC7, 0xB8, 0x00,
    0x25, 0x4F, 0x6F, 0x00,
    0x00, 0x39, 0x43, 0x00,
    0x59, 0x67, 0x9A, 0x00,
    0x44, 0x44, 0x44, 0x00,
    0x6C, 0x6C, 0x6C, 0x00,
    0x84, 0xD2, 0x9A, 0x00,
    0xB5, 0x5E, 0x6C, 0x00,
    0x95, 0x95, 0x95, 0x00 ]               

xyToIndex x y = x+y*screenWidth
