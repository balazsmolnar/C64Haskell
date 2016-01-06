module C64 where

import Data.List as L
import Data.Array
import Data.Array.Unsafe 
import Data.Array.Base (unsafeThawIOArray, unsafeFreezeIOArray, unsafeWrite, thawIOArray)
import Data.Binary
import Data.Bits
import Data.Char    
import Data.Maybe
import Data.ByteString as BS
--import System.Console.ANSI
import qualified Data.Vector.Unboxed as V
--import Test.QuickCheck
import Data.Array.ST
import Control.Monad.ST
import Numeric (showHex)
import Base
import Instructions
import Keyboard
import GHC.IOArray

step :: CPUState ->  CPUState
step cpu = 
    let byte0 = (memory cpu) ! pPointer cpu 
        byte1 = (memory cpu) ! (pPointer cpu+1)
        byte2 = (memory cpu) ! (pPointer cpu+2)
        instruction = instructionCodes ! byte0 
        m = mode instruction
        inst = iType instruction        
        cpu2 = inst cpu m byte1 byte2 in
        cpuNewPointer (pPointer cpu2 + instructionLength m) cpu2 
            
--clcTest :: Byte -> Bool
--clcTest s = 
--        let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0xFF 0 False []  (array (0, 0x0400) [(i, 0::Byte) | i <- [0..0x0400]]) in
--        not $ getFlag flagCBit (fCLC cpu)
        
fileToByteList :: String -> IO [Byte]
fileToByteList path = do
    contents <- BS.readFile path
    return $ unpack contents
    
loadMemory :: String  -> String  -> IO CPUState
loadMemory romPath characterRomPath = do
    content <- fileToByteList romPath
    charContent <- fileToByteList characterRomPath
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    let memoryL = (L.take address $ repeat 0) ++ (L.tail $ L.tail content) ++ (repeat 0)
    
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF address False [] (array (0, 0x0400) $ L.zip [0..0x0400] charContent)

loadRom = do
    let path = "d:\\Hackaton\\Phase3\\64c.251913-01.bin"
    let charPath = "d:\\temp\\Haskell\\C64\\characters.901225-01.bin"
    content <- fileToByteList path
    charContent <- fileToByteList charPath
    let charROM = charContent ++ (L.map (255-) charContent)
    let basic_address = 0xA000   
    let kernal_address = 0xE000
    let memoryL = (L.take basic_address $ repeat 0) ++ (L.take 0x2000 content) ++ (L.take 0x2000 $ repeat 0) ++ (L.take 0x2000 $ L.drop 0x2000 content)
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF 0xFCE2 False [] (array (0, 0x0800) $ L.zip [0..0x0800] charROM)

loadGame cpu = do
    print "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    content <- fileToByteList "c:\\temp\\MI\\hunchback.prg"
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    mutableArr <- unsafeThawIOArray (memory cpu)
    unsafeWriteList mutableArr address (L.tail $ L.tail content)
    newArr <- unsafeFreezeIOArray mutableArr
    let newCpu = CPUState (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
    return newCpu


unsafeWriteList arr index [] = return ()
unsafeWriteList mutableArr index (x:xs) = do
    Data.Array.Base.unsafeWrite mutableArr index x
    unsafeWriteList mutableArr (index+1) xs
    return ()

unsafeWriteValues :: GHC.IOArray.IOArray Int Byte -> [(Int, Byte)] -> IO ()
unsafeWriteValues mutableArr [] = return ()
unsafeWriteValues mutableArr ((addr, value):xs) = do

    if addr /= 0xDC00 then do
        Data.Array.Base.unsafeWrite mutableArr addr value
    else do
        val <- getKeyMatrixState value
        Data.Array.Base.unsafeWrite mutableArr 0xDC01 val

    unsafeWriteValues mutableArr  xs
    return ()
    
--unsafeWrite arr index newValue = do
    --print $ show index ++ "  " ++ show newValue
--    mutableArr <- unsafeThawIOArray arr
--    if index /= 0xDC00 then
--        Data.Array.Base.unsafeWrite mutableArr index newValue
--    else do
--        val <- getKeyMatrixState newValue
--        Data.Array.Base.unsafeWrite mutableArr 0xDC01 val
--    if (index == 0xDD00 || index == 0xD018 || index == 0xD011 || index == 0xD016) then do
--        print $ (showHex index "" ) ++ ": " ++ show newValue
--        newArr <- unsafeFreezeIOArray mutableArr
--        return newArr
--    else do
--        newArr <- unsafeFreezeIOArray mutableArr
--        return newArr

unsafeWrite arr values = do
    mutableArr <- unsafeThawIOArray arr
    unsafeWriteValues mutableArr values        
    newArr <- unsafeFreezeIOArray mutableArr
    return newArr
    
run cpu = do
    --Prelude.putStrLn $ show (pPointer cpu) ++ " A:"++ show  (regA cpu) ++ " P:"++ show  (regS cpu)        
   --print cpu\
    --let a = pPointer cpu
    newCpu <- updateMemory cpu    
    
    --print newCpu
  --  print (changedMemory newCpu)
    --print (memory cpu ! 56333)
    if (stopped newCpu) then return ()--print cpu
    else run $ step newCpu

stepN cpu 0 = return cpu
stepN cpu n = do
    --print cpu
    newCpu <- updateMemory cpu
    if  (regS cpu) == -1 then return newCpu            
    else stepN (step newCpu) (n-1)

keyPressed :: CPUState -> Byte -> IO CPUState
keyPressed cpu ch = do
    if (ch == 93) then loadGame cpu -- ']'
    else do
         updateMemory cpu
--        let cpu2 =  setMemory 0x00c6 1 cpu0
--        let cpu3 =  setMemory 0x0277 ch cpu2
--        newCpu <- updateMemory cpu3
--        return newCpu

interrupt cpu
    | getFlag flagIBit cpu == True = cpu
    | otherwise = cpuNewPointer (0xFF48) $ push (regS cpu) $ push (intToWord8(pPointer cpu `mod` 256)) $ push (intToWord8((pPointer cpu) `div` 256)) cpu 

    
start = do
--    content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\1_first.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\2_loop.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\3_subroutine.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\5_addressingmodes.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\7_snake.prg"
    content <- loadRom    
    cpu <- run content
    return cpu

--createScreenMemory cpu = 

    
updateMemory :: CPUState -> IO (CPUState)
updateMemory cpu = do
     let cm = [x | x <- changedMemory cpu, fst x > -1, snd x >= 0]
     newMemory <- C64.unsafeWrite (memory cpu) cm
     let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
     return newCpu
    
         
--updateMemory cpu = do
--     print (changedMemory cpu)
--     if (changedMemory cpu) == Nothing then return cpu
--     else do
--        newMemory <- Main.unsafeWrite (memory cpu) (fromJust (changedMemory cpu)) (changedMemoryValue cpu)
        --   $ newMemory ! (fromJust (changedMemory cpu))
--        let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (changedMemoryValue cpu)
--        return newCpu

borderWidth = 20
screenWidth :: Int
screenWidth = borderWidth*2 + 320
screenHeight :: Int
screenHeight = borderWidth*2 + 200


