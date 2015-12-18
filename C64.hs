module C64 where

import Data.List as L
import Data.Array
import Data.Array.Unsafe 
import Data.Array.Base (unsafeThawIOArray, unsafeFreezeIOArray, unsafeWrite)
import Data.Binary
import Data.Bits
import Data.Char    
import Data.Maybe
import Data.ByteString as BS
import System.Console.ANSI
import qualified Data.Vector.Unboxed as V
import Test.QuickCheck
import Data.Array.ST
import Control.Monad.ST

import Base
import Instructions

type Memory = Array Int Byte

colorList = [
                (Dull,Black),
                (Vivid,White),
                (Dull,Red),
                (Vivid,Cyan),
                (Vivid,Magenta),
                (Dull,Green),
                (Dull,Blue),
                (Vivid,Yellow),
                (Dull,Yellow),
                (Dull,Magenta),
                (Vivid,Red),
                (Dull,White),
                (Dull,White),
                (Vivid,Green),
                (Vivid,Blue),
                (Dull,Cyan)
            ]
colors = array (0::Byte,0x0F::Byte) (L.zip [0::Byte ..0x0F ::Byte] colorList)

                        -- +-+-+-+-+-+-+-+-+
                        -- |N|V| |B|D|I|Z|C|  PROCESSOR STATUS REG "P"
                        -- +-+-+-+-+-+-+-+-+
                        --  | |   | | | | |
                        --  | |   | | | | +>  CARRY         1=TRUE
                        --  | |   | | | +-->  ZERO          1=RESULT ZERO
                        --  | |   | | +---->  IRQ DISABLE   1=DISABLE
                        --  | |   | +------>  DECIMAL MODE  1=TRUE
                        --  | |   +-------->  BRK COMMAND
                        --  | |
                        --  | +------------>  OVERFLOW      1=TRUE
                        --  +-------------->  NEGATIVE      1=NEG
            
flagCBit = 0
flagZBit = 1
flagIBit = 2
flagVBit = 6
flagNBit = 7

data CPUState = CPUState { memory :: Memory, regA :: Byte, regX :: Byte, regY :: Byte, regS :: Byte, stackPointer ::Byte, pPointer :: Int, stopped :: Bool, changedMemory :: [(Int, Byte)] }  

instance Show CPUState where
  show cpu = "A: " ++ (show $ regA cpu) ++ "  X: " ++ (show $ regX cpu) ++ "  Y: " ++ (show $ regY cpu) ++ "  P: " ++ (show $ pPointer cpu) ++ "  SP: " ++ (show $ stackPointer cpu) ++ 
             " C: " ++ (show $ regS cpu `testBit` flagCBit)  ++ " Z: " ++ (show $ regS cpu `testBit` flagZBit)  ++ " N: " ++ (show $ regS cpu `testBit` flagNBit)  ++
               " V: " ++ (show $ regS cpu `testBit` flagVBit)  ++ " Stooped: " ++ (show $ stopped cpu)  ++ 
             " Memory: " ++ (show $ L.take 10 $ L.drop 0x0400  $ Data.Array.elems $ memory cpu) ++ 
             " Stack: " ++ (show $ L.take 5 $ L.drop 0x1FB  $ Data.Array.elems $ memory cpu) ++
             (show $ (iType (instructionCodes ! ((memory cpu) ! pPointer cpu))))
  
initialCPUState pointer = CPUState 
                        (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 0 0xFF pointer False []

cpuNewPointer pointer cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) pointer (stopped cpu) (changedMemory cpu)

cpuNewMemory changedMemoryAddress value cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) ((changedMemoryAddress, value):(changedMemory cpu))

cpuNewRegA :: Byte -> CPUState -> CPUState                        
cpuNewRegA regA cpu = CPUState 
                        (memory cpu) regA (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu)

cpuNewRegX :: Byte -> CPUState -> CPUState                        
cpuNewRegX regX cpu = CPUState 
                        (memory cpu) (regA cpu) regX (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu)

cpuNewRegY :: Byte -> CPUState -> CPUState                        
cpuNewRegY regY cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) regY (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu)

cpuNewRegS regS cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) regS (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu)

cpuNewSP :: Byte -> CPUState -> CPUState                        
cpuNewSP sp cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) sp (pPointer cpu) (stopped cpu) (changedMemory cpu)

                        
cpuStop cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) True []
                        
setBitValue :: Int -> Bool -> Byte -> Byte
setBitValue flagBit True v = v `setBit` flagBit
setBitValue flagBit False v = v `clearBit` flagBit
    
setZN :: Byte -> Byte -> Byte
setZN value s = 
        setBitValue flagZBit (value==0) (setBitValue flagNBit (value `testBit` 7) s)

setFlag :: Int -> CPUState -> CPUState
setFlag flagBit cpu = 
    cpuNewRegS (regS cpu `setBit` flagBit) cpu

clearFlag :: Int -> CPUState -> CPUState
clearFlag flagBit cpu = 
    cpuNewRegS (regS cpu `clearBit` flagBit) cpu

setFlagExp :: Int -> Bool -> CPUState -> CPUState
setFlagExp flagBit value cpu = 
    cpuNewRegS (setBitValue flagBit value (regS cpu)) cpu

getFlag :: Int -> CPUState -> Bool
getFlag flagBit cpu = 
     regS cpu `testBit` flagBit

setMemory :: Int -> Byte -> CPUState -> CPUState
setMemory address value cpu 
    | address >= 0xa000 && address < 0xc000 = cpu
    | address > 0xe000 = cpu
    | otherwise = cpuNewMemory address value cpu

push value cpu = 
     cpuNewSP (stackPointer cpu -1) $ setMemory (word8ToInt (stackPointer cpu)+0x100) value cpu

pull cpu = 
     cpuNewSP (stackPointer cpu +1) cpu
     
peek cpu = (memory cpu) ! (word8ToInt (stackPointer cpu)+0x101)
     
compareHelper :: Byte -> AddressingMode -> Byte -> Byte -> CPUState -> CPUState
compareHelper value mode byte1 byte2 cpu =
    let result = word8ToInt value - word8ToInt (getValue cpu mode byte1 byte2)
        s = regS cpu in
        cpuNewRegS (setBitValue flagZBit (result==0) (setBitValue flagNBit (result<0) (setBitValue flagCBit (result>=0) s))) cpu

branchHelper :: Bool -> Byte -> CPUState -> CPUState
branchHelper value offset cpu
    | value==False = cpuNewPointer (pPointer cpu + 2) cpu
    | offset `testBit` 7 = cpuNewPointer (pPointer cpu -256 + word8ToInt offset +2) cpu
    | otherwise = cpuNewPointer (pPointer cpu + word8ToInt offset+2) cpu

fBRK cpu = cpuStop cpu
 
fCLC :: CPUState -> CPUState
fCLC =
     clearFlag flagCBit

fCLI :: CPUState -> CPUState
fCLI =
     clearFlag flagIBit

fCLV :: CPUState -> CPUState
fCLV =
     clearFlag flagVBit

fSEC :: CPUState -> CPUState
fSEC =
     setFlag flagCBit

fSEI :: CPUState -> CPUState
fSEI =
     setFlag flagIBit

fLDA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fLDA cpu mode b1 b2 =
    let value = getValue cpu mode b1 b2
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s cpu 

fLDX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fLDX cpu mode b1 b2 =
    let value = getValue cpu mode b1 b2
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fLDY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fLDY cpu mode b1 b2 =
    let value = getValue cpu mode b1 b2
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 
    
fSTA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSTA cpu mode b1 b2 =
     let addr = getAddress cpu mode b1 b2 in
     setMemory addr (regA cpu) cpu

fSTX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSTX cpu mode b1 b2 =
     let addr = getAddress cpu mode b1 b2 in
     setMemory addr (regX cpu) cpu

fSTY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSTY cpu mode b1 b2 =
     let addr = getAddress cpu mode b1 b2 in
     setMemory addr (regY cpu) cpu

fTAY :: CPUState -> CPUState
fTAY cpu =
    let value = regA cpu
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fTYA :: CPUState -> CPUState
fTYA cpu =
    let value = regY cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s cpu 

fTAX :: CPUState -> CPUState
fTAX cpu =
    let value = regA cpu
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fTXA :: CPUState -> CPUState
fTXA cpu =
    let value = regX cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s cpu 

fTXS :: CPUState -> CPUState
fTXS cpu =
    let value = regX cpu in
    cpuNewSP value cpu

fTSX :: CPUState -> CPUState
fTSX cpu =
    let value = stackPointer cpu
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fINY :: CPUState -> CPUState
fINY cpu =
    let value = regY cpu + 1
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fDEY :: CPUState -> CPUState
fDEY cpu =
    let value = regY cpu - 1
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fINX :: CPUState -> CPUState
fINX cpu =
    let value = regX cpu + 1
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fINC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fINC cpu mode b1 b2 =
    let value = (getValue cpu mode b1 b2) + 1
        addr = getAddress cpu mode b1 b2
        s = setZN value (regS cpu) in
    setMemory addr value $ cpuNewRegS s cpu

fDEC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fDEC cpu mode b1 b2 =
    let value = (getValue cpu mode b1 b2) - 1
        addr = getAddress cpu mode b1 b2
        s = setZN value (regS cpu) in
    setMemory addr value $ cpuNewRegS s cpu

fDEX :: CPUState -> CPUState
fDEX cpu =
    let value = regX cpu - 1
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fCPX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCPX cpu mode b1 b2 =
    compareHelper (regX cpu) mode b1 b2 cpu

fCPY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCPY cpu mode b1 b2 =
    compareHelper (regY cpu) mode b1 b2 cpu
    
fCMP :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCMP cpu mode b1 b2 =
    compareHelper (regA cpu) mode b1 b2 cpu

fBNE :: CPUState ->  Byte -> CPUState
fBNE cpu b1 =
    branchHelper (getFlag flagZBit cpu == False) b1 cpu
    
fBEQ :: CPUState ->  Byte -> CPUState
fBEQ cpu b1 =
    branchHelper (getFlag flagZBit cpu == True) b1 cpu

fBCC :: CPUState ->  Byte -> CPUState
fBCC cpu b1 =
    branchHelper (getFlag flagCBit cpu == False) b1 cpu
    
fBCS :: CPUState ->  Byte -> CPUState
fBCS cpu b1 =
    branchHelper (getFlag flagCBit cpu == True) b1 cpu

fBPL :: CPUState ->  Byte -> CPUState
fBPL cpu b1 =
    branchHelper (getFlag flagNBit cpu == False) b1 cpu

fBMI :: CPUState ->  Byte -> CPUState
fBMI cpu b1 =
    branchHelper (getFlag flagNBit cpu == True) b1 cpu

fBVC :: CPUState ->  Byte -> CPUState
fBVC cpu b1 =
    branchHelper (getFlag flagVBit cpu == False) b1 cpu

fBVS :: CPUState ->  Byte -> CPUState
fBVS cpu b1 =
    branchHelper (getFlag flagVBit cpu == True) b1 cpu

fJMP :: CPUState -> AddressingMode -> Byte -> Byte -> CPUState
fJMP cpu mode b1 b2 =
    let addr = getAddress cpu mode b1 b2 in
    cpuNewPointer (addr-3) cpu 

fJSR :: CPUState -> Byte -> Byte -> CPUState
fJSR cpu b1 b2 =
    let addr = getAddress cpu Absolute b1 b2 in
    cpuNewPointer (addr-3) $ push (intToWord8(pPointer cpu+2 `mod` 256)) $ push (intToWord8((pPointer cpu+2) `div` 256)) cpu 
    
fRTS :: CPUState -> CPUState
fRTS cpu = 
    let low = peek cpu
        hi = peek $ pull cpu in
    cpuNewPointer ((word8ToInt hi)*256 + (word8ToInt low)) $ (pull $ pull cpu)

fRTI :: CPUState -> CPUState
fRTI cpu = 
    let s = peek cpu
        low = peek $ pull cpu
        hi = peek $ pull $ pull cpu in
    cpuNewRegS s $ cpuNewPointer ((word8ToInt hi)*256 + (word8ToInt low)-1) $ (pull $ pull $ pull cpu)
    
fPHA :: CPUState -> CPUState
fPHA cpu =
     push (regA cpu) cpu

fPHP :: CPUState -> CPUState
fPHP cpu =
     push (regS cpu) cpu

fPLA :: CPUState -> CPUState
fPLA cpu =
    let value = peek cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s (pull cpu)

fPLP :: CPUState -> CPUState
fPLP cpu =
    let value = peek cpu in
    cpuNewRegS value $ pull cpu
    
fASL :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fASL cpu mode b1 b2 
        | mode == Accumulator = cpuNewRegA newValue $ setFlagExp flagCBit c $ cpuNewRegS s cpu
        | otherwise = setMemory addr newValue $ setFlagExp flagCBit c $  cpuNewRegS s cpu
        where addr = getAddress cpu mode b1 b2
              value = getValue cpu mode b1 b2
              c = value `testBit` 7
              newValue = shiftL value 1
              s = setZN newValue (regS cpu)

fROL :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fROL cpu mode b1 b2 
        | mode == Accumulator = cpuNewRegA newValue $ setFlagExp flagCBit c $ cpuNewRegS s cpu
        | otherwise = setMemory addr newValue $ setFlagExp flagCBit c $  cpuNewRegS s cpu
        where addr = getAddress cpu mode b1 b2
              value = getValue cpu mode b1 b2
              c = value `testBit` 7
              newValue = (shiftL value 1) + if getFlag flagCBit cpu then 1::Word8 else 0::Word8
              s = setZN newValue (regS cpu)

fROR :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fROR cpu mode b1 b2 
        | mode == Accumulator = cpuNewRegA newValue $ setFlagExp flagCBit c $ cpuNewRegS s cpu
        | otherwise = setMemory addr newValue $ setFlagExp flagCBit c $  cpuNewRegS s cpu
        where addr = getAddress cpu mode b1 b2
              value = getValue cpu mode b1 b2
              c = value `testBit` 0
              newValue = (shiftR value 1) + if getFlag flagCBit cpu then 0x80::Word8 else 0::Word8
              s = setZN newValue (regS cpu)

fLSR :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fLSR cpu mode b1 b2 
        | mode == Accumulator = cpuNewRegA newValue $ setFlagExp flagCBit c $ cpuNewRegS s cpu
        | otherwise = setMemory addr newValue $ setFlagExp flagCBit c $  cpuNewRegS s cpu
        where addr = getAddress cpu mode b1 b2
              value = getValue cpu mode b1 b2
              c = value `testBit` 0
              newValue = shiftR value 1
              s = setZN newValue (regS cpu)
              
fADC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fADC cpu mode b1 b2 =
        let value = getValue cpu mode b1 b2
            carry = if getFlag flagCBit cpu then 1::Int else 0::Int
            result = word8ToInt (regA cpu) + (word8ToInt value) + carry
            newA = intToWord8 result
            v = ((regA cpu) `testBit` 7) /= (newA `testBit` 7) 
            s = (setZN newA (regS cpu))
            c = result > 255
            in
        setFlagExp flagVBit v $ setFlagExp flagCBit c $ cpuNewRegA newA $ cpuNewRegS s cpu

fSBC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSBC cpu mode b1 b2 =
        let value = getValue cpu mode b1 b2
            carry = if getFlag flagCBit cpu then 0::Int else 1::Int
            result = word8ToInt (regA cpu) - (word8ToInt value) - carry
            newA = if result>=0 then intToWord8 result else intToWord8 (result+256)
            v = ((regA cpu) `testBit` 7) /= (newA `testBit` 7) 
            s = (setZN newA (regS cpu))
            c = result >= 0
            in
        setFlagExp flagVBit v $ setFlagExp flagCBit c $ cpuNewRegA newA $ cpuNewRegS s cpu
        
fBIT :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBIT cpu mode b1 b2 =
        let value = getValue cpu mode b1 b2
            result = (regA cpu) Data.Bits..&. value
            n = value `testBit` 7
            v = value `testBit` 6
            z = result == 0
            in
        setFlagExp flagVBit v $ setFlagExp flagNBit n $ setFlagExp flagZBit z cpu
        
fAND cpu mode b1 b2 = 
        let value = (getValue cpu mode b1 b2) Data.Bits..&. (regA cpu)
            s = (setZN value (regS cpu)) in
            cpuNewRegA value $ cpuNewRegS s cpu

fORA cpu mode b1 b2 = 
        let value = (getValue cpu mode b1 b2) Data.Bits..|. (regA cpu)
            s = (setZN value (regS cpu)) in
            cpuNewRegA value $ cpuNewRegS s cpu

fEOR cpu mode b1 b2 = 
        let value = (getValue cpu mode b1 b2) `xor` (regA cpu)
            s = (setZN value (regS cpu)) in
            cpuNewRegA value $ cpuNewRegS s cpu
            
word8ToInt :: Word8 -> Int
word8ToInt x = fromInteger (toInteger x) :: Int

intToWord8 :: Int -> Word8
intToWord8 x = fromInteger (toInteger x) :: Word8
     
getAddress :: CPUState -> AddressingMode -> Byte -> Byte -> Int
getAddress cpu IndexedIndirectX byte1 byte2 =
    let addr = word8ToInt byte1 + word8ToInt (regX cpu) in
    word8ToInt (memory cpu ! addr) + 256 * (word8ToInt (memory cpu ! (addr+1)))
getAddress cpu IndirectIndexedY byte1 byte2 =
    let b1 = memory cpu ! (word8ToInt byte1)
        b2 = memory cpu ! (word8ToInt (byte1+1)) in
    getAddress cpu AbsoluteY b1 b2
getAddress cpu Indirect byte1 byte2 =    
    let addr = word8ToInt byte1 + 256 * word8ToInt byte2 in
    getAddress cpu Absolute (memory cpu ! addr) (memory cpu ! (addr+1))
getAddress cpu Absolute byte1 byte2 =    
    word8ToInt byte1 + 256 * (word8ToInt byte2)
getAddress cpu AbsoluteX byte1 byte2 =    
    word8ToInt byte1 + word8ToInt byte2 * 256 + word8ToInt (regX cpu)
getAddress cpu AbsoluteY byte1 byte2 =    
    word8ToInt byte1 + word8ToInt byte2 * 256 + word8ToInt (regY cpu)
getAddress cpu ZeroPage byte1 byte2 =
    word8ToInt byte1
getAddress cpu ZeroPageX byte1 byte2 =               
    word8ToInt $ byte1 + regX cpu
getAddress cpu ZeroPageY byte1 byte2 =               
    word8ToInt $ byte1 + regY cpu
getAddress cpu mode byte1 byte2 = error $ "Invalid addressing mode." ++ show mode

getValue :: CPUState -> AddressingMode -> Byte -> Byte -> Byte
getValue cpu Immidiate byte1 byte2 = byte1
getValue cpu Accumulator byte1 byte2 = regA cpu
getValue cpu mode byte1 byte2 = memory cpu ! getAddress cpu mode byte1 byte2

step :: CPUState ->  CPUState
step cpu = 
    let byte0 = (memory cpu) ! pPointer cpu 
        byte1 = (memory cpu) ! (pPointer cpu+1)
        byte2 = (memory cpu) ! (pPointer cpu+2)
        instruction = instructionCodes ! byte0 
        m = mode instruction
        inst = iType instruction        
        cpu2 = case inst of
            LDA -> fLDA cpu m byte1 byte2 
            LDX -> fLDX cpu m byte1 byte2 
            LDY -> fLDY cpu m byte1 byte2 
            CLC -> fCLC cpu
            CLI -> fCLI cpu
            CLV -> fCLV cpu
            SEC -> fSEC cpu
            SEI -> fSEI cpu
            STA -> fSTA cpu m byte1 byte2
            STX -> fSTX cpu m byte1 byte2
            STY -> fSTY cpu m byte1 byte2
            CPX -> fCPX cpu m byte1 byte2
            CPY -> fCPY cpu m byte1 byte2
            CMP -> fCMP cpu m byte1 byte2
            INX -> fINX cpu
            INY -> fINY cpu
            INC -> fINC cpu m byte1 byte2
            DEC -> fDEC cpu m byte1 byte2
            DEX -> fDEX cpu
            DEY -> fDEY cpu
            TAY -> fTAY cpu
            TYA -> fTYA cpu
            TAX -> fTAX cpu
            TXA -> fTXA cpu
            TSX -> fTSX cpu
            TXS -> fTXS cpu
            BRK -> fBRK cpu
            BNE -> fBNE cpu byte1
            BEQ -> fBEQ cpu byte1
            BMI -> fBMI cpu byte1
            BPL -> fBPL cpu byte1
            BCC -> fBCC cpu byte1
            BCS -> fBCS cpu byte1
            BVC -> fBVC cpu byte1
            BVS -> fBVS cpu byte1
            JSR -> fJSR cpu byte1 byte2
            JMP -> fJMP cpu m byte1 byte2
            ASL -> fASL cpu m byte1 byte2
            LSR -> fLSR cpu m byte1 byte2
            ROL -> fROL cpu m byte1 byte2
            ROR -> fROR cpu m byte1 byte2
            ADC -> fADC cpu m byte1 byte2
            SBC -> fSBC cpu m byte1 byte2
            AND -> fAND cpu m byte1 byte2
            EOR -> fEOR cpu m byte1 byte2
            ORA -> fORA cpu m byte1 byte2
            BIT -> fBIT cpu m byte1 byte2
            PHA -> fPHA cpu
            PLA -> fPLA cpu
            PHP -> fPHP cpu
            PLP -> fPLP cpu
            RTS -> fRTS cpu
            RTI -> fRTI cpu
            NOP -> cpu
            CLD -> cpu

            otherwise -> error $ "not implemented " ++ show inst
        in
        cpuNewPointer (pPointer cpu2 + instructionLength m) cpu2 
            
clcTest :: Byte -> Bool
clcTest s = 
        let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0xFF 0 False [] in
        not $ getFlag flagCBit (fCLC cpu)
        
fileToByteList :: String -> IO [Byte]
fileToByteList path = do
    contents <- BS.readFile path
    return $ unpack contents
    
loadMemory :: String  -> IO CPUState
loadMemory path = do
    content <- fileToByteList path
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    let memoryL = (L.take address $ repeat 0) ++ (L.tail $ L.tail content) ++ (repeat 0)
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF address False []

loadRom = do
    let path = "d:\\Hackaton\\Phase3\\64c.251913-01.bin"
    content <- fileToByteList path
    let basic_address = 0xA000   
    let kernal_address = 0xE000
    let memoryL = (L.take basic_address $ repeat 0) ++ (L.take 0x2000 content) ++ (L.take 0x2000 $ repeat 0) ++ (L.take 0x2000 $ L.drop 0x2000 content)
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF 0xFCE2 False []

unsafeWrite arr index newValue = do
    mutableArr <- unsafeThawIOArray arr
    Data.Array.Base.unsafeWrite mutableArr index newValue
    newArr <- unsafeFreezeIOArray mutableArr
    return newArr

unsafeWrite2 arr index1 newValue1 index2 newValue2 = do
    mutableArr <- unsafeThawIOArray arr
    Data.Array.Base.unsafeWrite mutableArr index1 newValue1
    Data.Array.Base.unsafeWrite mutableArr index2 newValue2
    newArr <- unsafeFreezeIOArray mutableArr
    return newArr
    
run cpu = do
    --Prelude.putStrLn $ show (pPointer cpu) ++ " A:"++ show  (regA cpu) ++ " P:"++ show  (regS cpu)        
    --print cpu
    newCpu <- updateMemory cpu    
    updateScreen newCpu (changedMemory cpu)
    
    --print newCpu
  --  print (changedMemory newCpu)
    --print (memory cpu ! 56333)
    if (stopped newCpu) then return ()--print cpu
    else run $ step newCpu
        
start = do
--    content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\1_first.prg"
    setTitle "Commodore 64"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\2_loop.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\3_subroutine.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\5_addressingmodes.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\7_snake.prg"
    content <- loadRom    
    cpu <- run content
    return cpu
  
setChar x y ch fgColor bgColor = do
    setCursorPosition y x    
    setSGR [ 
             SetColor Foreground Vivid fgColor
           , SetColor Background Vivid bgColor
           ]
    Prelude.putStr [ch]

setBorder bgColor = do
    sequence $ L.map (\x -> setChar x 0 ' ' bgColor bgColor)[0..41]
    sequence $ L.map (\x -> setChar x 26 ' ' bgColor bgColor) [0..41]
    sequence $ L.map (\y -> setChar 0 y ' ' bgColor bgColor)  [0..26]
    sequence $ L.map (\y -> setChar 41 y ' ' bgColor bgColor) [0..26]
    
c64chr :: Byte -> Char
c64chr b 
    | b >= 1 && b<=26 = chr $ word8ToInt(b)+ 64
    | b==32 = ' '
    | b>=48 && b <=58 = chr $ word8ToInt(b)
    | otherwise  = '.'

    
updateMemory :: CPUState -> IO (CPUState)
updateMemory cpu = do
     let cm = [x | x <- changedMemory cpu, fst x > -1, snd x >= 0]
     case L.length cm of
        0 -> return cpu
        1 -> do
                newMemory <- C64.unsafeWrite (memory cpu) (fst $ cm!!0) (snd $ cm!!0)
                let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) []
                return newCpu
        2 -> do
                newMemory <- C64.unsafeWrite2 (memory cpu) (fst $ cm!!0) (snd $ cm!!0) (fst $ cm!!1) (snd $ cm!!1)
                let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) []
                return newCpu
         
--updateMemory cpu = do
--     print (changedMemory cpu)
--     if (changedMemory cpu) == Nothing then return cpu
--     else do
--        newMemory <- Main.unsafeWrite (memory cpu) (fromJust (changedMemory cpu)) (changedMemoryValue cpu)
        -- print $ newMemory ! (fromJust (changedMemory cpu))
--        let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (changedMemoryValue cpu)
--        return newCpu

updateScreen :: CPUState -> [(Int, Byte)] -> IO ()
updateScreen cpu changedMemory
    | changedMemory == [] = return ()
    | otherwise = do
        updateScreen2 cpu (fst $ L.head $ changedMemory)
        return ()
        

updateScreen2 :: CPUState -> Int -> IO ()
updateScreen2 cpu address
    | address >= 0x400 && address < 0x400+1000 = do 
                    setChar ((address-0x400) `mod` 40 + 1) ((address-0x400) `div` 40 +1) (c64chr $ (memory cpu) ! address) (snd $ colors ! ((memory cpu) ! (address+0x400)  Data.Bits..&. 0x0F)) (snd $ colors ! ((memory cpu) ! 0xD021  Data.Bits..&. 0x0F)) 
                    return ()
    | address == 0xD020 = do 
                    setBorder (snd $ colors ! (((memory cpu) ! 0xD020) Data.Bits..&. 0x0F))
                    return ()
    | otherwise = return ()
    
    
main = do
    clearScreen
    start
--startTest = do
--    content <- fileToByteList "d:\\Hackaton\\Phase1\\Prg\\1_first.prg"
--    let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0 in
--    print "end"
  