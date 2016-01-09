module File where

import Base
import MemoryModule
import Instructions

import Data.List as L
import Data.ByteString as BS

loadRom memory characterROM = do

    let path = "64c.251913-01.bin"
    let charPath = "characters.901225-01.bin"
    content <- fileToByteList path
    charContent <- fileToByteList charPath
    let charROM = charContent ++ (L.map (255-) charContent)
    let basic_address = 0xA000   
    let kernal_address = 0xE000

    writeMemoryFromAddress memory basic_address (L.take 0x2000 content)
    writeMemoryFromAddress memory kernal_address (L.take 0x2000 $ L.drop 0x2000 content) 
    writeMemoryFromAddress characterROM 0 charROM
    
    return $ CPUState 0 0 0 0 0xFF 0xFCE2 False [] 0

loadHunchback :: CPUState -> Memory -> IO (CPUState)
loadHunchback cpu memory = do
    print "Load hunchback"
    content <- fileToByteList "hunchback.prg"
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    writeMemoryFromAddress memory address (L.drop 2 content)
    let newCpu = CPUState (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] 0
    return newCpu

fileToByteList :: String -> IO [Byte]
fileToByteList path = do
    contents <- BS.readFile path
    return $ unpack contents
