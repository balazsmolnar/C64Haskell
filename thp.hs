


module Main(main) where

import Control.Exception (SomeException, bracket, catch)
import qualified Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign (newStablePtr, castStablePtrToPtr ) 
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.Win32.DLL (getModuleHandle)
import qualified Graphics.Win32
--import GHC.Int (Byte)
import Data.Binary
import Data.List
import Data.Array
import Data.IORef
import C64
import Screen
import System.IO.Unsafe

type Byte = Word8

scale = 2

pokeBytes :: Storable a => Ptr b -> Int -> [a] -> IO ()
pokeBytes p _ [] = return ()
pokeBytes p off (x:xs) = do
    pokeByteOff p off x
    pokeBytes p (off+1) xs

globalCPUState :: IORef CPUState
globalCPUState =
        unsafePerformIO $ newIORef (initialCPUState 0)

main :: IO ()
main =
  Graphics.Win32.allocaPAINTSTRUCT $ \ lpps -> do
  hwnd <- createWindow (screenWidth*2+10) (screenHeight*2+30) (wndProc lpps onPaint)
  messagePump hwnd

onPaint :: Graphics.Win32.RECT -> Graphics.Win32.HDC -> IO ()
onPaint (_,_,w,h) hdc = do
--   Graphics.Win32.setBkMode hdc Graphics.Win32.tRANSPARENT
--   Graphics.Win32.setTextColor hdc (Graphics.Win32.rgb 255 255 0)
--   let y | h==10     = 0
--         | otherwise = ((h-10) `div` 2)
--       x | w==50     = 0
--         | otherwise = (w-50) `div` 2
--   Graphics.Win32.textOut hdc x y "Hello, world"
--   Graphics.Win32.lineTo hdc 100 199
   
   memDC <- Graphics.Win32.createCompatibleDC $ Just hdc
   bitmap <- createBitmap hdc
   Graphics.Win32.selectBitmap memDC bitmap

   Graphics.Win32.stretchBlt hdc 0 (fromIntegral (screenHeight)*scale) (fromIntegral screenWidth*scale) (fromIntegral (-screenHeight)*scale) memDC 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight) Graphics.Win32.sRCCOPY

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
 | wmsg == Graphics.Win32.wM_PAINT && hwnd /= nullPtr = do
     r <- Graphics.Win32.getClientRect hwnd
     paintWith lpps hwnd (onPaint r)
     Graphics.Win32.invalidateRect (Just hwnd) Nothing False
     return 0
 | wmsg == Graphics.Win32.wM_CHAR = do
     cpu <- readIORef globalCPUState
     cpu2 <- keyPressed cpu (fromInteger (toInteger wParam)::Word8) 
     writeIORef globalCPUState cpu2
     return 0
 | otherwise =
     Graphics.Win32.defWindowProc (Just hwnd) wmsg wParam lParam

        
intToByte :: Int -> Byte
intToByte x = fromInteger (toInteger x) :: Byte


createBitmap :: Graphics.Win32.HDC -> IO Graphics.Win32.HBITMAP
createBitmap dc = do 
    

    cpu <- readIORef globalCPUState
    --print cpu
    cpu2 <- stepN cpu 5000
    cpu3 <- updateMemory cpu2
    let cpu4 = interrupt cpu3
    
    cpu5 <- stepN cpu4 5000
    cpu6 <- updateMemory cpu5
    let cpu7 = interrupt cpu6

    --let screen = sort $ getScreenBytes cpu4

    writeIORef globalCPUState cpu7
    hb <- createScreenBitmap cpu dc
    if hb == nullPtr then error "hb is null"                    
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
		 "Hello, World example"
		 Graphics.Win32.wS_OVERLAPPEDWINDOW
		 Nothing Nothing -- leave it to the shell to decide the position
		 		 -- at where to put the window initially
                 (Just screenWidth)
		 (Just screenHeight)
		 Nothing      -- no parent, i.e, root window is the parent.
		 Nothing      -- no menu handle
		 mainInstance
		 wndProc
  
  cpu <- loadRom
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
