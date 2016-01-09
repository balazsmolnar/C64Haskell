module MemoryModule where

--import Data.Array.Unboxed as MemArr
import Data.Array as MemArr
import qualified Data.Array.IO
import Data.Array.IO.Internals
import Data.Array.Unsafe 
import Data.Array.Base (unsafeThawIOArray, unsafeFreezeIOArray, unsafeWrite)

import Base
type Memory = MemArr.Array Int Byte
type MutableMemory = Data.Array.IO.IOArray Int Byte

thaw :: Memory -> IO (MutableMemory)
thaw = unsafeThawIOArray

freeze :: MutableMemory -> IO (Memory)
freeze = unsafeFreezeIOArray

getByteFromMemory memory address = 
    memory MemArr.! address

initMemory  :: Int -> Memory    
initMemory size = 
    MemArr.array (0, size) [(i, 0::Byte) | i <- [0..size]]

 
unsafeWriteList :: MutableMemory -> Int -> [Byte] -> IO ()
unsafeWriteList memory index [] = return ()
unsafeWriteList mutableMemory index (x:xs) = do
    Data.Array.Base.unsafeWrite mutableMemory index x
    unsafeWriteList mutableMemory (index+1) xs
    return ()

unsafeWriteValues :: MutableMemory -> [(Int, Byte)] -> IO ()
unsafeWriteValues mutableMemory [] = return ()
unsafeWriteValues mutableMemory ((addr, value):xs) = do
    Data.Array.Base.unsafeWrite mutableMemory addr value
    unsafeWriteValues mutableMemory  xs
    return ()

unsafeWrite :: Memory -> [(Int,Byte)] -> IO (Memory)
unsafeWrite memory [] = return memory
unsafeWrite memory values = do
    mutableMemory <- thaw memory
    unsafeWriteValues mutableMemory values        
    newMemory <- freeze mutableMemory
    return newMemory

unsafeWriteFromAddress :: Memory -> Int -> [Byte] -> IO (Memory)
unsafeWriteFromAddress memory  address values = do
    mutableMemory <- thaw memory
    unsafeWriteList mutableMemory address values        
    newMemory <- freeze mutableMemory
    return newMemory

