module Keyboard (getKeyMatrixState) where

import Data.List as L
import Data.Map as M
import Data.Bits
import Graphics.Win32
import Graphics.Win32.Key
import Control.Monad
import Data.Binary

import Base

--
-- Public
--

getKeyMatrixState :: Byte -> IO Byte                  
getKeyMatrixState value = do
                        result <- foldM (\acc x -> do 
                                            if (value `testBit` x) then return acc
                                            else do                                                
                                                y <- getKeyMatrixByRow x
                                                return $ acc .&. y
                                        ) 0xFF [0..7]
                        return result


toInt :: Integral a => a -> Int
toInt = fromInteger . toInteger

--
-- Keyboard Matrix
--

notSupportedKey = 0
                  
keyboardMatrix = M.fromList [(0, [toInt Graphics.Win32.vK_BACK, toInt Graphics.Win32.vK_RETURN, toInt Graphics.Win32.vK_RIGHT, toInt Graphics.Win32.vK_F7, toInt Graphics.Win32.vK_F1, toInt Graphics.Win32.vK_F3, toInt Graphics.Win32.vK_F5,toInt Graphics.Win32.vK_DOWN]),
                             (1, [fromEnum '3', fromEnum 'W', fromEnum 'A', fromEnum '4', fromEnum 'Z', fromEnum 'S', fromEnum 'E', toInt Graphics.Win32.vK_SHIFT]),
                             (2, [fromEnum '5', fromEnum 'R', fromEnum 'D', fromEnum '6', fromEnum 'C', fromEnum 'F', fromEnum 'T', fromEnum 'X']),
                             (3, [fromEnum '7', fromEnum 'Y', fromEnum 'G', fromEnum '8', fromEnum 'B', fromEnum 'H', fromEnum 'U', fromEnum 'V']),
                             (4, [fromEnum '9', fromEnum 'I', fromEnum 'J', fromEnum '0', fromEnum 'M', fromEnum 'K', fromEnum 'O', fromEnum 'N']),
                             (5, [fromEnum '+', fromEnum 'P', fromEnum 'L', fromEnum '-', 0xBE, fromEnum ':', fromEnum '@', 0xBC]),
                             (6, [notSupportedKey, fromEnum '*', fromEnum ';' ,toInt Graphics.Win32.vK_HOME, toInt Graphics.Win32.vK_SHIFT, {-fromEnum '='-}0xBB, fromEnum '^', fromEnum '/']),
                             (7, [fromEnum '1', toInt Graphics.Win32.vK_DELETE, toInt Graphics.Win32.vK_CONTROL, fromEnum '2', fromEnum ' ', notSupportedKey, fromEnum 'Q', notSupportedKey])
                  ]

getKeyMatrixByRow :: Int -> IO Byte                  
getKeyMatrixByRow row = do
                        result <- foldM (\acc x -> do 
                                            y <- getKeyState x
                                            return $ acc*2 + y) 
                                0 (L.reverse $ keyboardMatrix!row)
                        return result

                        

getKeyState :: Int -> IO Byte
getKeyState x = do
    state <- getAsyncKeyState x
    if state == 0 then return 1
    else return 0

