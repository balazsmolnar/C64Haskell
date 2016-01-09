module C64 where

import Data.List as L
import Data.Array
import qualified Data.Array.IO

import Data.Binary
import Data.Bits
import Data.Char    
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Numeric (showHex)


import Base
import Instructions
import MemoryModule
import Keyboard
import File

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
                
    
stepN cpu memory 0 = return cpu
stepN cpu memory n = do
    if  (regS cpu) == -1 then return cpu
    else do
        let cpu2 = seq (cpu,memory) (step cpu memory)
        newCpu <- updateMemory cpu2 memory
        seq (newCpu,memory) (stepN newCpu memory (n-1))

keyPressed :: CPUState -> Memory -> Byte -> IO CPUState
keyPressed cpu memory ch = do
    if (ch == 93) then loadHunchback cpu memory -- ']'
    else return cpu
             
    
updateMemory :: CPUState -> Memory -> IO (CPUState)
updateMemory cpu memory = do
    let cm = [x | x <- changedMemory cpu, fst x > -1, snd x >= 0]
    if L.length cm == 0 then return cpu
    else do
        let newCpu = CPUState (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (counter cpu)
        if fst (cm !! 0) /= 0xDC00 then do   
            writeMemory memory cm
        else do
            val <- getKeyMatrixState $ snd (cm !! 0)
            writeMemory memory [(0xDC01, val)]

        return newCpu
    

borderWidth = 20
screenWidth :: Int
screenWidth = borderWidth*2 + 320
screenHeight :: Int
screenHeight = borderWidth*2 + 200


