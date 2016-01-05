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
--import System.Console.ANSI
import qualified Data.Vector.Unboxed as V
--import Test.QuickCheck
import Data.Array.ST
import Control.Monad.ST
import Numeric (showHex)
import Base
import Instructions
import Keyboard

type Memory = Array Int Byte

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

data CPUState = CPUState { memory :: Memory, regA :: Byte, regX :: Byte, regY :: Byte, regS :: Byte, stackPointer ::Byte, pPointer :: Int, stopped :: Bool, changedMemory :: [(Int, Byte)], characterROM :: Memory }  

instance Show CPUState where
  show cpu = "A: " ++ (show $ regA cpu) ++ "  X: " ++ (show $ regX cpu) ++ "  Y: " ++ (show $ regY cpu) ++ "  P: " ++ (showHex  (pPointer cpu) "") ++ "  SP: " ++ (show $ stackPointer cpu) ++ 
             " C: " ++ (show $ regS cpu `testBit` flagCBit)  ++ " Z: " ++ (show $ regS cpu `testBit` flagZBit)  ++ " N: " ++ (show $ regS cpu `testBit` flagNBit)  ++
               " V: " ++ (show $ regS cpu `testBit` flagVBit)  ++ " Stooped: " ++ (show $ stopped cpu)  ++ 
             " Memory: " ++ (show $ L.take 10 $ L.drop 0x0277  $ Data.Array.elems $ memory cpu) ++ 
             " Stack: " ++ (show $ L.take 5 $ L.drop 0x1FB  $ Data.Array.elems $ memory cpu) ++
             (show $ (iType (instructionCodes ! ((memory cpu) ! pPointer cpu))))
  
initialCPUState pointer = CPUState 
                        (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 0 0xFF pointer False [] (array (0, 0x0400) [(i, 0::Byte) | i <- [0..0x0400]])

cpuNewPointer pointer cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) pointer (stopped cpu) (changedMemory cpu) (characterROM cpu)

cpuNewMemory changedMemoryAddress value cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) ((changedMemoryAddress, value):(changedMemory cpu)) (characterROM cpu)

cpuNewRegA :: Byte -> CPUState -> CPUState                        
cpuNewRegA regA cpu = CPUState 
                        (memory cpu) regA (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (characterROM cpu)

cpuNewRegX :: Byte -> CPUState -> CPUState                        
cpuNewRegX regX cpu = CPUState 
                        (memory cpu) (regA cpu) regX (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (characterROM cpu)

cpuNewRegY :: Byte -> CPUState -> CPUState                        
cpuNewRegY regY cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) regY (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (characterROM cpu)

cpuNewRegS regS cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) regS (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (characterROM cpu)

cpuNewSP :: Byte -> CPUState -> CPUState                        
cpuNewSP sp cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) sp (pPointer cpu) (stopped cpu) (changedMemory cpu) (characterROM cpu)

                        
cpuStop cpu = CPUState 
                        (memory cpu) (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) True [] (characterROM cpu)
                        
setBitValue :: Int -> Bool -> Byte -> Byte
setBitValue flagBit True v = v `setBit` flagBit
setBitValue flagBit False v = v `clearBit` flagBit

    
setZN :: Byte -> Byte -> Byte
--setZN value s = 
--        setBitValue flagZBit (value==0) (setBitValue flagNBit (value `testBit` 7) s)
setZN value s 
    | value == 0 && n == True = s .|. 130
    | value == 0 = (s .|. 2) .&. 127
    | n == True = (s .&. 253) .|. 128
    | otherwise = s .&. 125
    where n = value `testBit` 7

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

fBRK :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBRK cpu mode b1 b2 = cpuStop cpu
 
fCLC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCLC cpu mode b1 b2 =
     clearFlag flagCBit cpu

fCLI :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCLI cpu mode b1 b2 =
     clearFlag flagIBit cpu

fCLV :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fCLV cpu mode b1 b2 =
     clearFlag flagVBit cpu

fSEC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSEC cpu mode b1 b2 =
     setFlag flagCBit cpu

fSEI :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fSEI cpu mode b1 b2 =
     setFlag flagIBit cpu

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

fTAY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTAY cpu mode b1 b2 =
    let value = regA cpu
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fTYA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTYA cpu mode b1 b2 =
    let value = regY cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s cpu 

fTAX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTAX cpu mode b1 b2 =
    let value = regA cpu
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fTXA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTXA cpu mode b1 b2 =
    let value = regX cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s cpu 

fTXS :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTXS cpu mode b1 b2 =
    let value = regX cpu in
    cpuNewSP value cpu

fTSX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fTSX cpu mode b1 b2 =
    let value = stackPointer cpu
        s = setZN value (regS cpu) in
    cpuNewRegX value $ cpuNewRegS s cpu 

fINY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fINY cpu mode b1 b2 =
    let value = regY cpu + 1
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fDEY :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fDEY cpu mode b1 b2 =
    let value = regY cpu - 1
        s = setZN value (regS cpu) in
    cpuNewRegY value $ cpuNewRegS s cpu 

fINX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fINX cpu mode b1 b2 =
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

fDEX :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fDEX cpu mode b1 b2 =
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

fBNE :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBNE cpu mode b1 b2 =
    branchHelper (getFlag flagZBit cpu == False) b1 cpu
    
fBEQ :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBEQ cpu mode b1 b2 =
    branchHelper (getFlag flagZBit cpu == True) b1 cpu

fBCC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBCC cpu mode b1 b2 =
    branchHelper (getFlag flagCBit cpu == False) b1 cpu
    
fBCS :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBCS cpu mode b1 b2 =
    branchHelper (getFlag flagCBit cpu == True) b1 cpu

fBPL :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBPL cpu mode b1 b2 =
    branchHelper (getFlag flagNBit cpu == False) b1 cpu

fBMI :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBMI cpu mode b1 b2 =
    branchHelper (getFlag flagNBit cpu == True) b1 cpu

fBVC :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBVC cpu mode b1 b2 =
    branchHelper (getFlag flagVBit cpu == False) b1 cpu

fBVS :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fBVS cpu mode b1 b2 =
    branchHelper (getFlag flagVBit cpu == True) b1 cpu

fJMP :: CPUState -> AddressingMode -> Byte -> Byte -> CPUState
fJMP cpu mode b1 b2 =
    let addr = getAddress cpu mode b1 b2 in
    cpuNewPointer (addr-3) cpu 

fJSR :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fJSR cpu mode b1 b2 =
    let addr = getAddress cpu Absolute b1 b2 in
    cpuNewPointer (addr-3) $ push (intToWord8(pPointer cpu+2 `mod` 256)) $ push (intToWord8((pPointer cpu+2) `div` 256)) cpu 
     
fRTS :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fRTS cpu mode b1 b2 =
    let low = peek cpu
        hi = peek $ pull cpu in
    cpuNewPointer ((word8ToInt hi)*256 + (word8ToInt low)) $ (pull $ pull cpu)

fRTI :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fRTI cpu mode b1 b2 =
    let s = peek cpu
        low = peek $ pull cpu
        hi = peek $ pull $ pull cpu in
    cpuNewRegS s $ cpuNewPointer ((word8ToInt hi)*256 + (word8ToInt low)-1) $ (pull $ pull $ pull cpu)
    
fPHA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fPHA cpu mode b1 b2 =
     push (regA cpu) cpu

fPHP :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fPHP cpu mode b1 b2 =
     push (regS cpu) cpu

fPLA :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fPLA cpu mode b1 b2 =
    let value = peek cpu
        s = setZN value (regS cpu) in
    cpuNewRegA value $ cpuNewRegS s (pull cpu)

fPLP :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState
fPLP cpu mode b1 b2 =
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
            CLC -> fCLC cpu m byte1 byte2 
            CLI -> fCLI cpu m byte1 byte2 
            CLV -> fCLV cpu m byte1 byte2 
            SEC -> fSEC cpu m byte1 byte2 
            SEI -> fSEI cpu m byte1 byte2 
            STA -> fSTA cpu m byte1 byte2
            STX -> fSTX cpu m byte1 byte2
            STY -> fSTY cpu m byte1 byte2
            CPX -> fCPX cpu m byte1 byte2
            CPY -> fCPY cpu m byte1 byte2
            CMP -> fCMP cpu m byte1 byte2
            INX -> fINX cpu m byte1 byte2 
            INY -> fINY cpu m byte1 byte2 
            INC -> fINC cpu m byte1 byte2
            DEC -> fDEC cpu m byte1 byte2
            DEX -> fDEX cpu m byte1 byte2 
            DEY -> fDEY cpu m byte1 byte2 
            TAY -> fTAY cpu m byte1 byte2 
            TYA -> fTYA cpu m byte1 byte2 
            TAX -> fTAX cpu m byte1 byte2 
            TXA -> fTXA cpu m byte1 byte2 
            TSX -> fTSX cpu m byte1 byte2 
            TXS -> fTXS cpu m byte1 byte2 
            BRK -> fBRK cpu m byte1 byte2 
            BNE -> fBNE cpu m byte1 byte2 
            BEQ -> fBEQ cpu m byte1 byte2 
            BMI -> fBMI cpu m byte1 byte2 
            BPL -> fBPL cpu m byte1 byte2 
            BCC -> fBCC cpu m byte1 byte2 
            BCS -> fBCS cpu m byte1 byte2 
            BVC -> fBVC cpu m byte1 byte2 
            BVS -> fBVS cpu m byte1 byte2 
            JSR -> fJSR cpu m byte1 byte2 
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
            PHA -> fPHA cpu m byte1 byte2 
            PLA -> fPLA cpu m byte1 byte2 
            PHP -> fPHP cpu m byte1 byte2 
            PLP -> fPLP cpu m byte1 byte2 
            RTS -> fRTS cpu m byte1 byte2 
            RTI -> fRTI cpu m byte1 byte2 
            NOP -> cpu
            CLD -> cpu
            SED -> cpu -- not implemented

            otherwise -> error $ "not implemented " ++ (show inst) ++ "  Memory: " ++ (show $ L.take 10 $ L.drop 0x0313  $ Data.Array.elems $ memory cpu)
        in
        cpuNewPointer (pPointer cpu2 + instructionLength m) cpu2 
            
--clcTest :: Byte -> Bool
--clcTest s = 
--        let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0xFF 0 False []  (array (0, 0x0400) [(i, 0::Byte) | i <- [0..0x0400]]) in
--        not $ getFlag flagCBit (fCLC cpu)
        
fileToByteList :: String -> IO [Byte]
fileToByteList path = do
    contents <- BS.readFile path
    return $ unpack contents
    
loadMemory :: String  -> String  -> IO CPUState
loadMemory romPath characterRomPath = do
    content <- fileToByteList romPath
    charContent <- fileToByteList characterRomPath
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    let memoryL = (L.take address $ repeat 0) ++ (L.tail $ L.tail content) ++ (repeat 0)
    
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF address False [] (array (0, 0x0400) $ L.zip [0..0x0400] charContent)

loadRom = do
    let path = "d:\\Hackaton\\Phase3\\64c.251913-01.bin"
    let charPath = "d:\\temp\\Haskell\\C64\\characters.901225-01.bin"
    content <- fileToByteList path
    charContent <- fileToByteList charPath
    let charROM = charContent ++ (L.map (255-) charContent)
    let basic_address = 0xA000   
    let kernal_address = 0xE000
    let memoryL = (L.take basic_address $ repeat 0) ++ (L.take 0x2000 content) ++ (L.take 0x2000 $ repeat 0) ++ (L.take 0x2000 $ L.drop 0x2000 content)
    return $ CPUState (array (0, 0xFFFF) $ L.zip [0..0xFFFF] memoryL) 0 0 0 0 0xFF 0xFCE2 False [] (array (0, 0x0800) $ L.zip [0..0x0800] charROM)

loadGame cpu = do
    print "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    content <- fileToByteList "c:\\temp\\MI\\hunchback.prg"
    let address = word8ToInt (content !! 0) + 256*word8ToInt (content !! 1)   
    mutableArr <- unsafeThawIOArray (memory cpu)
    unsafeWriteList mutableArr address (L.tail $ L.tail content)
    newArr <- unsafeFreezeIOArray mutableArr
    let newCpu = CPUState newArr (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
    return newCpu


unsafeWriteList arr index [] = return ()
unsafeWriteList mutableArr index (x:xs) = do
    Data.Array.Base.unsafeWrite mutableArr index x
    unsafeWriteList mutableArr (index+1) xs
    return ()

unsafeWrite arr index newValue = do
    --print $ show index ++ "  " ++ show newValue
    mutableArr <- unsafeThawIOArray arr
    if index /= 0xDC00 then
        Data.Array.Base.unsafeWrite mutableArr index newValue
    else do
        val <- getKeyMatrixState newValue
        Data.Array.Base.unsafeWrite mutableArr 0xDC01 val
    if (index == 0xDD00 || index == 0xD018 || index == 0xD011 || index == 0xD016) then do
        print $ (showHex index "" ) ++ ": " ++ show newValue
        newArr <- unsafeFreezeIOArray mutableArr
        return newArr
    else do
        newArr <- unsafeFreezeIOArray mutableArr
        return newArr

unsafeWrite2 arr index1 newValue1 index2 newValue2 = do
    mutableArr <- unsafeThawIOArray arr
    Data.Array.Base.unsafeWrite mutableArr index1 newValue1
    Data.Array.Base.unsafeWrite mutableArr index2 newValue2
    newArr <- unsafeFreezeIOArray mutableArr
    return newArr

unsafeWrite3 arr index1 newValue1 index2 newValue2 index3 newValue3 = do
    mutableArr <- unsafeThawIOArray arr
    Data.Array.Base.unsafeWrite mutableArr index1 newValue1
    Data.Array.Base.unsafeWrite mutableArr index2 newValue2
    Data.Array.Base.unsafeWrite mutableArr index3 newValue3
    newArr <- unsafeFreezeIOArray mutableArr
    return newArr
    
run cpu = do
    --Prelude.putStrLn $ show (pPointer cpu) ++ " A:"++ show  (regA cpu) ++ " P:"++ show  (regS cpu)        
   --print cpu\
    --let a = pPointer cpu
    newCpu <- updateMemory cpu    
    
    --print newCpu
  --  print (changedMemory newCpu)
    --print (memory cpu ! 56333)
    if (stopped newCpu) then return ()--print cpu
    else run $ step newCpu

stepN cpu 0 = return cpu
stepN cpu n = do
    --print cpu
    newCpu <- updateMemory cpu
    if  (regS cpu) == -1 then return newCpu            
    else stepN (step newCpu) (n-1)

keyPressed :: CPUState -> Byte -> IO CPUState
keyPressed cpu ch = do
    if (ch == 93) then loadGame cpu -- ']'
    else do
         updateMemory cpu
--        let cpu2 =  setMemory 0x00c6 1 cpu0
--        let cpu3 =  setMemory 0x0277 ch cpu2
--        newCpu <- updateMemory cpu3
--        return newCpu

interrupt cpu
    | getFlag flagIBit cpu == True = cpu
    | otherwise = cpuNewPointer (0xFF48) $ push (regS cpu) $ push (intToWord8(pPointer cpu `mod` 256)) $ push (intToWord8((pPointer cpu) `div` 256)) cpu 

    
start = do
--    content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\1_first.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\2_loop.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase1\\Prg\\3_subroutine.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\5_addressingmodes.prg"
    --content <- loadMemory "d:\\Hackaton\\Phase2\\7_snake.prg"
    content <- loadRom    
    cpu <- run content
    return cpu

--createScreenMemory cpu = 

    
updateMemory :: CPUState -> IO (CPUState)
updateMemory cpu = do
     let cm = [x | x <- changedMemory cpu, fst x > -1, snd x >= 0]
     case L.length cm of
        0 -> return cpu
        1 -> do
                newMemory <- C64.unsafeWrite (memory cpu) (fst $ cm!!0) (snd $ cm!!0)
                let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
                return newCpu
        2 -> do
                newMemory <- C64.unsafeWrite2 (memory cpu) (fst $ cm!!0) (snd $ cm!!0) (fst $ cm!!1) (snd $ cm!!1)
                let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
                return newCpu
        3 -> do
                newMemory <- C64.unsafeWrite3 (memory cpu) (fst $ cm!!0) (snd $ cm!!0) (fst $ cm!!1) (snd $ cm!!1) (fst $ cm!!2) (snd $ cm!!2)
                let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) [] (characterROM cpu)
                return newCpu
        otherwise -> do
                error $ show $ cpu

         
--updateMemory cpu = do
--     print (changedMemory cpu)
--     if (changedMemory cpu) == Nothing then return cpu
--     else do
--        newMemory <- Main.unsafeWrite (memory cpu) (fromJust (changedMemory cpu)) (changedMemoryValue cpu)
        --   $ newMemory ! (fromJust (changedMemory cpu))
--        let newCpu = CPUState newMemory (regA cpu) (regX cpu) (regY cpu) (regS cpu) (stackPointer cpu) (pPointer cpu) (stopped cpu) (changedMemory cpu) (changedMemoryValue cpu)
--        return newCpu

borderWidth = 20
screenWidth :: Int
screenWidth = borderWidth*2 + 320
screenHeight :: Int
screenHeight = borderWidth*2 + 200

xyToIndex x y = x+y*screenWidth

getBorder :: Byte -> [(Int, Byte)]
getBorder value =
        [(xyToIndex x y, value) | x <- [0..borderWidth-1]++[borderWidth+320..screenWidth-1], y <- [0..screenHeight-1]]++
        [(xyToIndex x y, value) | x <- [borderWidth..screenWidth-borderWidth-1], y <- [0..borderWidth-1]++[borderWidth+200..screenHeight-1]]

getGraphics :: CPUState -> [(Int, Byte)]
getGraphics cpu =
        L.concat $ L.map (\c -> copyCharToScreen cpu (fst c) (snd c) 14 7) [(x,y) | x <- [0..39], y <- [0..24]] 

        
charLineToList cpu ch line bkColor fgColor x y = 
        let addr = (word8ToInt (ch Data.Bits..&. 0x7F)) * 8 + line
            
            b = (characterROM cpu) ! (addr)
            l = if ch < 128 then [testBit b a | a <- [7,6..0]] else [not (testBit b a) | a <- [7,6..0]]
            colors = L.map (\a -> if a==True then fgColor else bkColor) l
            memAddr = xyToIndex x (y+line) in
        L.zip [memAddr .. memAddr+7] colors
        
copyCharToScreen cpu x y bkColor fgColor = 
        let screenX = x*8 + borderWidth
            screenY = y*8 + borderWidth
            chAddress = 0x400 + y*40 + x
            ch = (memory cpu) ! chAddress in
        L.concat $ L.map (\l -> charLineToList cpu ch l bkColor fgColor screenX screenY) [0..7]

getScreenBytes :: CPUState -> [(Int, Byte)]
getScreenBytes cpu =
        getBorder ((memory cpu) ! 0xD020) ++ (getGraphics cpu)

            
--main = do
--    clearScreen
--    start
--startTest = do
--    content <- fileToByteList "d:\\Hackaton\\Phase1\\Prg\\1_first.prg"
--    let cpu = CPUState (array (0, 0xFFFF) [(i, 0::Byte) | i <- [0..0xFFFF]]) 0 0 0 s 0 in
--    print "end"
  