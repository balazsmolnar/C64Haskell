module C64 where

import Data.List as L
import Data.Array
import qualified Data.Array.IO

import Data.Binary
import Data.Bits
import Data.Char    
import Data.Maybe
import Data.ByteString as BS
import Control.Monad
import Control.Monad.ST
import Numeric (showHex)
import Base
import Instructions
import MemoryModule
import Keyboard

step :: CPUState -> Memory -> CPUState
step cpu memory = 
        cpuIncreaseCounter $ cpuNewPointer (pPointer cpu2 + instructionLength m) cpu2
        where
            byte0 = getByteFromMemory memory (pPointer cpu)
            byte1 = getByteFromMemory memory (pPointer cpu+1)
            byte2 = getByteFromMemory memory (pPointer cpu+2)
            instruction = instructionCodes ! byte0 
            m = mode instruction
            inst = iType instruction             
            cpu2 = seq (cpu,memory) (inst cpu memory m byte1 byte2)
            
--clcTest :: Byte -> Bool
--clcTest s = 
--        let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0xFF 0 False []  (array (0, 0x0400) [(i, 0::Byte) | i <- [0..0x0400]]) in
--        not $ getFlag flagCBit (fCLC cpu)
        
fileToByteList :: String -> IO [Byte]
fileToByteList path = do
    contents <- BS.readFile path
    return $ unpack contents
    
loadRom memory characterROM = do
    let path = "d:\\Hackaton\\Phase3\\64c.251913-01.bin"
    let charPath = "d:\\temp\\Haskell\\C64\\characters.901225-01.bin"
    content <- fileToByteList path
    charContent <- fileToByteList charPath
    let charROM = charContent ++ (L.map (255-) charContent)
    let basic_address = 0xA000   
    let kernal_address = 0xE000

    writeMemoryFromAddress memory basic_address (L.take 0x2000 content)
    writeMemoryFromAddress memory kernal_address (L.take 0x2000 $ L.drop 0x2000 content) 

    writeMemoryFromAddress characterROM 0 charROM
    print $ "hhhhhhhh  " ++ show (getByteFromMemory memory kernal_address)
    return $ CPUState 0 0 0 0 0xFF 0xFCE2 False [] 0

loadGame :: CPUState -> Memory -> IO (CPUState)
loadGame cpu memory = do
    print "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    content <- fileToByteList "c:\\temp\\MI\\hunchback.prg"
    --content <- fileToByteList "c:\\temp\\MI\\pitstop ii.prg"
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    writeMemoryFromAddress memory address (L.drop 2 content)
    let newCpu = CPUState (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] 0
    return newCpu
    
stepN cpu memory 0 = return cpu
stepN cpu memory n = do
    if  (regS cpu) == -1 then return cpu
    else do
        let cpu2 = seq (cpu,memory) (step cpu memory)
        newCpu <- updateMemory cpu2 memory
        seq (newCpu,memory) (stepN newCpu memory (n-1))

--stepN cpu memory n = do
--   if  (regS cpu) == -1 then return cpu
--    else do
--        foldM (\acc _ -> do 
--                            newCpu <- seq (acc, memory) updateMemory acc memory
--                            let cpu2 = seq (newCpu, memory) step newCpu memory
--                            newCpu2 <- seq (acc, memory) updateMemory cpu2 memory
--                            return newCpu2
--                            )
--                            cpu [1..n]

keyPressed :: CPUState -> Memory -> Byte -> IO CPUState
keyPressed cpu memory ch = do
    if (ch == 93) then loadGame cpu memory -- ']'
    else return cpu
             
    
updateMemory :: CPUState -> Memory -> IO (CPUState)
updateMemory cpu memory = do
    let cm = [x | x <- changedMemory cpu, fst x > -1, snd x >= 0]
    if L.length cm == 0 then return cpu
    else do
        let newCpu = CPUState (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (counter cpu)
        --print $ show cm
        if fst (cm !! 0) /= 0xDC00 then do   
            writeMemory memory cm
        else do
            val <- getKeyMatrixState $ snd (cm !! 0)
            writeMemory memory [(0xDC01, val)]

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


