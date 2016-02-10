module Main(main) where

import Data.List as L
import Data.Array
import qualified Data.Array.IO
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.Win32.DLL (getModuleHandle)
import Control.Exception (SomeException, bracket, catch)
import qualified Graphics.Win32
import System.IO.Unsafe

import Data.Binary
import Data.Bits
import Data.Char    
import Data.Maybe
import Data.IORef

import Control.Monad
import Control.Monad.ST
import Numeric (showHex)

import Base
import Instructions
import MemoryModule
import Keyboard
import File
import Screen

scale = 3

--
-- main
-- 
main :: IO ()
main =
  Graphics.Win32.allocaPAINTSTRUCT $ \ lpps -> do
  hwnd <- createWindow (screenWidth*3+10) (screenHeight*3+30) (wndProc lpps onPaint)
  messagePump hwnd
  
--
-- Windows
--

onPaint :: Graphics.Win32.RECT -> Graphics.Win32.HDC -> IO ()
onPaint (_,_,w,h) hdc = do
   
   memDC <- Graphics.Win32.createCompatibleDC $ Just hdc
   bitmap <- createBitmap hdc
   Graphics.Win32.selectBitmap memDC bitmap

   Graphics.Win32.stretchBlt hdc 0 (fromIntegral (screenHeight)*scale) (fromIntegral screenWidth*scale) (fromIntegral (-screenHeight)*scale) memDC 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight) Graphics.Win32.sRCCOPY
   
   Graphics.Win32.deleteDC memDC
   Graphics.Win32.deleteBitmap bitmap

   return ()

wndProc :: Graphics.Win32.LPPAINTSTRUCT
    -> (Graphics.Win32.RECT -> Graphics.Win32.HDC -> IO ()) -- on paint action
        -> Graphics.Win32.HWND
        -> Graphics.Win32.WindowMessage
    -> Graphics.Win32.WPARAM
    -> Graphics.Win32.LPARAM
    -> IO Graphics.Win32.LRESULT
wndProc lpps onPaint hwnd wmsg wParam lParam
 | wmsg == Graphics.Win32.wM_DESTROY = do
     Graphics.Win32.sendMessage hwnd Graphics.Win32.wM_QUIT 1 0
     return 0
 | wmsg == Graphics.Win32.wM_PAINT && hwnd /= Graphics.Win32.nullPtr = do
     r <- Graphics.Win32.getClientRect hwnd
     paintWith lpps hwnd (onPaint r)
     Graphics.Win32.invalidateRect (Just hwnd) Nothing False
     return 0
 | wmsg == Graphics.Win32.wM_CHAR = do
     cpu <- readIORef globalCPUState
     memory <- readIORef globalMemory
     cpu2 <- keyPressed cpu memory (fromInteger (toInteger wParam)::Word8) 
     writeIORef globalCPUState cpu2
     return 0
 | otherwise =
     Graphics.Win32.defWindowProc (Just hwnd) wmsg wParam lParam

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

globalCPUState :: IORef CPUState
globalCPUState =
        unsafePerformIO $ newIORef (initialCPUState 0)

globalMemory :: IORef Memory
globalMemory =
        unsafePerformIO $ newIORef (initMemory 0xFFFF)

globalCharacterROM :: IORef Memory
globalCharacterROM =
        unsafePerformIO $ newIORef (initMemory 0x2000)
                
createBitmap :: Graphics.Win32.HDC -> IO Graphics.Win32.HBITMAP
createBitmap dc = do 
    
    cpu  <- readIORef globalCPUState 
    memory  <- readIORef globalMemory
    characterROM <- readIORef globalCharacterROM

    cpu2 <- stepN cpu memory 2000
    
    hb <- createScreenBitmap cpu2 memory characterROM dc
    writeIORef globalCPUState cpu2
    if hb == Graphics.Win32.nullPtr then error "hb is null"                    
    else return hb
     
createWindow :: Int -> Int -> Graphics.Win32.WindowClosure -> IO Graphics.Win32.HWND
createWindow screenWidth screenHeight wndProc = do
  let winClass = Graphics.Win32.mkClassName "Hello"
  icon         <- Graphics.Win32.loadIcon   Nothing Graphics.Win32.iDI_APPLICATION
  cursor       <- Graphics.Win32.loadCursor Nothing Graphics.Win32.iDC_ARROW
  bgBrush      <- Graphics.Win32.createSolidBrush (Graphics.Win32.rgb 0 0 255)
  mainInstance <- getModuleHandle Nothing
  Graphics.Win32.registerClass
        ( Graphics.Win32.cS_VREDRAW + Graphics.Win32.cS_HREDRAW
        , mainInstance  
        , Just icon
        , Just cursor
        , Nothing --Just bgBrush
        , Nothing
        , winClass  
        )
  w <- Graphics.Win32.createWindow
        winClass
        "C64 Emulator (Haskell version)"
        Graphics.Win32.wS_OVERLAPPEDWINDOW
        Nothing Nothing -- leave it to the shell to decide the position
        (Just screenWidth)
        (Just screenHeight)
        Nothing      -- no parent, i.e, root window is the parent.
        Nothing      -- no menu handle
        mainInstance
        wndProc
  memory <- readIORef globalMemory
  characterROM <- readIORef globalCharacterROM
  cpu <- loadRom memory characterROM

  writeIORef globalCPUState cpu
  Graphics.Win32.showWindow w Graphics.Win32.sW_SHOWNORMAL
    
  Graphics.Win32.updateWindow w
  return w

messagePump :: Graphics.Win32.HWND -> IO ()
messagePump hwnd = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        Graphics.Win32.getMessage msg (Just hwnd)
        Graphics.Win32.translateMessage msg
        Graphics.Win32.dispatchMessage msg
        pump
  in pump

paintWith :: Graphics.Win32.LPPAINTSTRUCT -> Graphics.Win32.HWND -> (Graphics.Win32.HDC -> IO a) -> IO a
paintWith lpps hwnd p =
  bracket
    (Graphics.Win32.beginPaint hwnd lpps)
    (const $ Graphics.Win32.endPaint hwnd lpps)
    p
