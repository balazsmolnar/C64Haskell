
module Base where

import Data.Binary (Word8)

type Byte = Word8

word8ToInt :: Word8 -> Int
word8ToInt x = fromInteger (toInteger x) :: Int

intToWord8 :: Int -> Word8
intToWord8 x = fromInteger (toInteger x) :: Word8
