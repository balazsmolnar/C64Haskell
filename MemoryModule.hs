module MemoryModule (Memory,
                     initMemory, 
                     getByteFromMemory, 
                     writeMemory, 
                     writeMemoryFromAddress) where

import Data.Array.Unboxed as MemArr
import Data.Array --as MemArr
import qualified Data.Array.IO
import Data.Array.IO.Internals
import Data.Array.Unsafe 
import Control.Monad.ST
import Data.Array.Base (unsafeThawIOArray, unsafeFreezeIOArray, unsafeFreezeSTUArray, unsafeWrite)

import Base
type Memory = MemArr.UArray Int Byte
type MutableMemory = Data.Array.IO.IOUArray Int Byte

--
-- Public
--

getByteFromMemory memory address = 
    memory MemArr.! address

initMemory  :: Int -> Memory    
initMemory size = 
    MemArr.array (0, size) [(i, 0::Byte) | i <- [0..size]]

writeMemory :: Memory -> [(Int,Byte)] -> IO (Memory)
writeMemory memory [] = return memory
writeMemory memory values = do
    mutableMemory <- thaw memory
    unsafeWriteValues mutableMemory values        
    newMemory <- freeze mutableMemory
    return newMemory
    
writeMemoryFromAddress :: Memory -> Int -> [Byte] -> IO (Memory)
writeMemoryFromAddress memory  address values = do
    mutableMemory <- thaw memory
    unsafeWriteList mutableMemory address values        
    newMemory <- freeze mutableMemory
    return newMemory

--
-- Internal
--

thaw :: Memory -> IO (MutableMemory)
thaw = unsafeThawIOUArray

unsafeFreezeIOUArray (IOUArray marr) = stToIO (unsafeFreezeSTUArray marr)

freeze :: MutableMemory -> IO (Memory)
freeze = unsafeFreezeIOUArray
 
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

