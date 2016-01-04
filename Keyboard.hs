module Keyboard where

import Data.List as L
import Data.Map as M
import Data.Bits
import Graphics.Win32
import Graphics.Win32.Key
import Control.Monad
import Data.Binary

toInt :: Integral a => a -> Int
toInt = fromInteger . toInteger

notSupportedKey = 0

keyboardMatrix_old = [[toInt Graphics.Win32.vK_DELETE, fromEnum '3', fromEnum '5', fromEnum '7', fromEnum '9', fromEnum '+', notSupportedKey, fromEnum '1'],
                  [toInt Graphics.Win32.vK_RETURN, fromEnum 'w', fromEnum 'r', fromEnum 'y', fromEnum 'i', fromEnum 'p', fromEnum '*', toInt Graphics.Win32.vK_BACK],
                  [toInt Graphics.Win32.vK_RIGHT, fromEnum 'a', fromEnum 'd', fromEnum 'g', fromEnum 'j', fromEnum 'l', fromEnum ';', toInt Graphics.Win32.vK_CONTROL],
                  [toInt Graphics.Win32.vK_F7, fromEnum '4', fromEnum '6', fromEnum '8', fromEnum '0', fromEnum '-', toInt Graphics.Win32.vK_HOME, fromEnum '2'],
                  [toInt Graphics.Win32.vK_F1, fromEnum 'z', fromEnum 'c', fromEnum 'b', fromEnum 'm', fromEnum '.', toInt Graphics.Win32.vK_SHIFT, fromEnum ' '],
                  [toInt Graphics.Win32.vK_F3, fromEnum 's', fromEnum 'f', fromEnum 'h', fromEnum 'k', fromEnum ':', fromEnum '=', notSupportedKey],
                  [toInt Graphics.Win32.vK_F5, fromEnum 'e', fromEnum 't', fromEnum 'u', fromEnum 'o', fromEnum '@', fromEnum '^', fromEnum 'q'],
                  [toInt Graphics.Win32.vK_DOWN, toInt Graphics.Win32.vK_SHIFT, fromEnum 'x', fromEnum 'v', fromEnum 'n', fromEnum '.', fromEnum '/', notSupportedKey]
                  ]
                  
keyboardMatrix = M.fromList [(0xFE::Word8, [notSupportedKey, toInt Graphics.Win32.vK_RETURN, toInt Graphics.Win32.vK_RIGHT, toInt Graphics.Win32.vK_F7, toInt Graphics.Win32.vK_F1, toInt Graphics.Win32.vK_F3, toInt Graphics.Win32.vK_F5,toInt Graphics.Win32.vK_DOWN]),
                  (0xFD::Word8, [fromEnum '3', fromEnum 'W', fromEnum 'A', fromEnum '4', fromEnum 'Z', fromEnum 'S', fromEnum 'E', toInt Graphics.Win32.vK_SHIFT]),
                  (0xFB::Word8, [fromEnum '5', fromEnum 'R', fromEnum 'D', fromEnum '6', fromEnum 'C', fromEnum 'F', fromEnum 'T', fromEnum 'X']),
                  (0xF7::Word8, [fromEnum '7', fromEnum 'Y', fromEnum 'G', fromEnum '8', fromEnum 'B', fromEnum 'H', fromEnum 'U', fromEnum 'V']),
                  (0xEF::Word8, [fromEnum '9', fromEnum 'I', fromEnum 'J', fromEnum '0', fromEnum 'M', fromEnum 'K', fromEnum 'O', fromEnum 'N']),
                  (0xDF::Word8, [fromEnum '+', fromEnum 'P', fromEnum 'L', fromEnum '-', 0xBE, fromEnum ':', fromEnum '@', 0xBC]),
                  (0xBF::Word8, [notSupportedKey, fromEnum '*', fromEnum ';' ,toInt Graphics.Win32.vK_HOME, toInt Graphics.Win32.vK_SHIFT, fromEnum '=', fromEnum '^', fromEnum '/']),
                  (0x7F::Word8, [fromEnum '1', toInt Graphics.Win32.vK_DELETE, toInt Graphics.Win32.vK_CONTROL, fromEnum '2', fromEnum ' ', notSupportedKey, fromEnum 'q', notSupportedKey])
                  ]

getKeyMatrixByRow :: Word8 -> IO Word8                  
getKeyMatrixByRow 255 = return 255
getKeyMatrixByRow 0 = do
                        b0 <- getKeyMatrixByRow 0xFE
                        b1 <- getKeyMatrixByRow 0xFD 
                        b2 <- getKeyMatrixByRow 0xFB 
                        b3 <- getKeyMatrixByRow 0xF7 
                        b4 <- getKeyMatrixByRow 0xEF
                        b5 <-  getKeyMatrixByRow 0xDF 
                        b6 <- getKeyMatrixByRow 0xBF 
                        b7 <- getKeyMatrixByRow 0x7F
                        return $ b0 .&. b1 .&. b2 .&. b3 .&. b4 .&. b5 .&. b6 .&. b7
getKeyMatrixByRow row = do
                        result <- foldM (\acc x -> do 
                                            y <- getKeyState x
                                            return $ acc*2 + y) 
                                0 (L.reverse $ keyboardMatrix!row)
                        --print result
                        return result
                      

getKeyState :: Int -> IO Word8
getKeyState x = do
    state <- getAsyncKeyState x
    if state == 0 then return 1
    else return 0

