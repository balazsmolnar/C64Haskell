
module Instructions where

import Base
import Data.List as L
import Data.Array
import Data.List
import Numeric (showHex)
import Data.Bits
import Data.Char    
import Data.Binary


data AddressingMode =   
     Implied|
     IndexedIndirectX|
     IndirectIndexedY|
     Indirect|
     Absolute|
     AbsoluteX|
     AbsoluteY|
     Immidiate|
     ZeroPage|
     ZeroPageX|
     ZeroPageY|
     Accumulator|
     Relative
    deriving (Read, Show, Enum, Eq)
 
data Instruction = Instruction { iType :: CPUState ->  AddressingMode -> Byte -> Byte -> CPUState, mode :: AddressingMode }  

    
codeList = [
    Instruction fBRK Implied,-- 00 - BRK                        
    Instruction fORA IndexedIndirectX,-- 01 - ORA - (Indirect,X,         
    Instruction fUNDEF Implied,-- 02 - Future Expansion           
    Instruction fUNDEF Implied,-- 03 - Future Expansion           
    Instruction fUNDEF Implied,-- 04 - Future Expansion           
    Instruction fORA ZeroPage,-- 05 - ORA - Zero Page            
    Instruction fASL ZeroPage,-- 06 - ASL - Zero Page            
    Instruction fUNDEF Implied,-- 07 - Future Expansion           
    Instruction fPHP Implied,-- 08 - PHP                        
    Instruction fORA Immidiate,-- 09 - ORA - Immediate            
    Instruction fASL Accumulator,-- 0A - ASL - Accumulator          
    Instruction fUNDEF Implied,-- 0B - Future Expansion           
    Instruction fUNDEF Implied,-- 0C - Future Expansion           
    Instruction fORA Absolute,-- 0D - ORA - Absolute             
    Instruction fASL Absolute,-- 0E - ASL - Absolute             
    Instruction fUNDEF Implied,-- 0F - Future Expansion           
    Instruction fBPL Relative,-- 10 - BPL                        
    Instruction fORA IndirectIndexedY, --11 - ORA - (Indirect,,Y         
    Instruction fUNDEF Implied,-- 12 - Future Expansion           
    Instruction fUNDEF Implied,-- 13 - Future Expansion           
    Instruction fUNDEF Implied,-- 14 - Future Expansion           
    Instruction fORA ZeroPageX,--15 - ORA - Zero Page,X          
    Instruction fASL ZeroPageX,--16 - ASL - Zero Page,X          
    Instruction fUNDEF Implied,-- 17 - Future Expansion           
    Instruction fCLC Implied,--18 - CLC                        
    Instruction fORA AbsoluteY,--19 - ORA - Absolute,Y           
    Instruction fUNDEF Implied,-- 1A - Future Expansion           
    Instruction fUNDEF Implied,-- 1B - Future Expansion           
    Instruction fUNDEF Implied,-- 1C - Future Expansion           
    Instruction fORA AbsoluteX,--1D - ORA - Absolute,X           
    Instruction fASL AbsoluteX,--1E - ASL - Absolute,X           
    Instruction fUNDEF Implied,-- 1F - Future Expansion           
    Instruction fJSR Absolute,--20 - JSR
    Instruction fAND IndexedIndirectX,--21 - AND - (Indirect,X,
    Instruction fUNDEF Implied,-- 22 - Future Expansion
    Instruction fUNDEF Implied,-- 23 - Future Expansion
    Instruction fBIT ZeroPage,-- 24 - BIT - Zero Page
    Instruction fAND ZeroPage,--25 - AND - Zero Page
    Instruction fROL ZeroPage,--26 - ROL - Zero Page
    Instruction fUNDEF Implied,-- 27 - Future Expansion
    Instruction fPLP Implied,-- 28 - PLP
    Instruction fAND Immidiate,--29 - AND - Immediate
    Instruction fROL Accumulator,--2A - ROL - Accumulator
    Instruction fUNDEF Implied,-- 2B - Future Expansion
    Instruction fBIT Absolute,--2C - BIT - Absolute
    Instruction fAND Absolute,--2D - AND - Absolute
    Instruction fROL Absolute,--2E - ROL - Absolute
    Instruction fUNDEF Implied,-- 2F - Future Expansion
    Instruction fBMI Relative,--30 - BMI
    Instruction fAND IndirectIndexedY,--31 - AND - (Indirect,,Y
    Instruction fUNDEF Implied,-- 32 - Future Expansion
    Instruction fUNDEF Implied,-- 33 - Future Expansion
    Instruction fUNDEF Implied,-- 34 - Future Expansion
    Instruction fAND ZeroPageX,-- 35 - AND - Zero Page,X
    Instruction fROL ZeroPageX,--36 - ROL - Zero Page,X
    Instruction fUNDEF Implied,-- 37 - Future Expansion
    Instruction fSEC Implied,--38 - SEC
    Instruction fAND AbsoluteY,--39 - AND - Absolute,Y
    Instruction fUNDEF Implied,-- 3A - Future Expansion
    Instruction fUNDEF Implied,-- 3B - Future Expansion
    Instruction fUNDEF Implied,-- 3C - Future Expansion
    Instruction fAND AbsoluteX,--3D - AND - Absolute,X
    Instruction fROL AbsoluteX,--3E - ROL - Absolute,X
    Instruction fUNDEF Implied,-- 3F - Future Expansion
    Instruction fRTI Implied,--40 - RTI                        
    Instruction fEOR IndexedIndirectX, --41 - EOR - (Indirect,X,         
    Instruction fUNDEF Implied,-- 42 - Future Expansion           
    Instruction fUNDEF Implied,-- 43 - Future Expansion           
    Instruction fUNDEF Implied,-- 44 - Future Expansion           
    Instruction fEOR ZeroPage,--45 - EOR - Zero Page            
    Instruction fLSR ZeroPage,--46 - LSR - Zero Page            
    Instruction fUNDEF Implied,-- 47 - Future Expansion           
    Instruction fPHA Implied,--48 - PHA                        
    Instruction fEOR Immidiate,--49 - EOR - Immediate            
    Instruction fLSR Accumulator,--4A - LSR - Accumulator          
    Instruction fUNDEF Implied,-- 4B - Future Expansion           
    Instruction fJMP Absolute,--4C - JMP - Absolute             
    Instruction fEOR Absolute,--4D - EOR - Absolute             
    Instruction fLSR Absolute,--4E - LSR - Absolute             
    Instruction fUNDEF Implied,-- 4F - Future Expansion           
    Instruction fBVC Relative,--50 - BVC                        
    Instruction fEOR IndirectIndexedY, -- 51 - EOR - (Indirect,,Y         
    Instruction fUNDEF Implied,-- 52 - Future Expansion           
    Instruction fUNDEF Implied,-- 53 - Future Expansion           
    Instruction fUNDEF Implied,-- 54 - Future Expansion           
    Instruction fEOR ZeroPageX,--55 - EOR - Zero Page,X          
    Instruction fLSR ZeroPageX,--56 - LSR - Zero Page,X          
    Instruction fUNDEF Implied,-- 57 - Future Expansion           
    Instruction fCLI Implied,--58 - CLI                        
    Instruction fEOR AbsoluteY,--59 - EOR - Absolute,Y           
    Instruction fUNDEF Implied,-- 5A - Future Expansion           
    Instruction fUNDEF Implied,-- 5B - Future Expansion           
    Instruction fUNDEF Implied,-- 5C - Future Expansion           
    Instruction fEOR AbsoluteX,--50 - EOR - Absolute,X           
    Instruction fLSR AbsoluteX,--5E - LSR - Absolute,X           
    Instruction fUNDEF Implied,-- 5F - Future Expansion           
    Instruction fRTS Implied,--60 - RTS
    Instruction fADC IndexedIndirectX,--61 - ADC - (Indirect,X,
    Instruction fUNDEF Implied,-- 62 - Future Expansion
    Instruction fUNDEF Implied,-- 63 - Future Expansion
    Instruction fUNDEF Implied,-- 64 - Future Expansion
    Instruction fADC ZeroPage,--65 - ADC - Zero Page
    Instruction fROR ZeroPage,--66 - ROR - Zero Page
    Instruction fUNDEF Implied,-- 67 - Future Expansion
    Instruction fPLA Implied,--68 - PLA
    Instruction fADC Immidiate,--69 - ADC - Immediate
    Instruction fROR Accumulator,--6A - ROR - Accumulator
    Instruction fUNDEF Implied,-- 6B - Future Expansion
    Instruction fJMP Indirect,  --6C - JMP - Indirect
    Instruction fADC Absolute,--6D - ADC - Absolute
    Instruction fROR Absolute,--6E - ROR - Absolute
    Instruction fUNDEF Implied,-- 6F - Future Expansion
    Instruction fBVS Relative,--70 - BVS
    Instruction fADC IndirectIndexedY,--71 - ADC - (Indirect,,Y
    Instruction fUNDEF Implied,-- 72 - Future Expansion
    Instruction fUNDEF Implied,-- 73 - Future Expansion
    Instruction fUNDEF Implied,-- 74 - Future Expansion
    Instruction fADC ZeroPageX,--75 - ADC - Zero Page,X
    Instruction fROR ZeroPageX,--76 - ROR - Zero Page,X
    Instruction fUNDEF Implied,-- 77 - Future Expansion
    Instruction fSEI Implied,--78 - SEI
    Instruction fADC AbsoluteY,--79 - ADC - Absolute,Y
    Instruction fUNDEF Implied,-- 7A - Future Expansion
    Instruction fUNDEF Implied,-- 7B - Future Expansion
    Instruction fUNDEF Implied,-- 7C - Future Expansion
    Instruction fADC AbsoluteX,--7D - ADC - Absolute,X
    Instruction fROR AbsoluteX,--7E - ROR - Absolute,X
    Instruction fUNDEF Implied,-- 7F - Future Expansion
    Instruction fUNDEF Implied,-- 80 - Future Expansion           
    Instruction fSTA IndexedIndirectX, -- 81 - STA - (Indirect,X,         
    Instruction fUNDEF Implied,-- 82 - Future Expansion           
    Instruction fUNDEF Implied,-- 83 - Future Expansion           
    Instruction fSTY ZeroPage,--84 - STY - Zero Page            
    Instruction fSTA ZeroPage,--85 - STA - Zero Page            
    Instruction fSTX ZeroPage,--86 - STX - Zero Page            
    Instruction fUNDEF Implied,-- 87 - Future Expansion           
    Instruction fDEY Implied,--88 - DEY                        
    Instruction fUNDEF Implied,-- 89 - Future Expansion           
    Instruction fTXA Implied,--8A - TXA                        
    Instruction fUNDEF Implied,-- 8B - Future Expansion           
    Instruction fSTY Absolute,--8C - STY - Absolute             
    Instruction fSTA Absolute,--8D - STA - Absolute             
    Instruction fSTX Absolute,--8E - STX - Absolute             
    Instruction fUNDEF Implied,-- 8F - Future Expansion           
    Instruction fBCC Relative,--90 - BCC                        
    Instruction fSTA IndirectIndexedY, --91 - STA - (Indirect,,Y         
    Instruction fUNDEF Implied,-- 92 - Future Expansion           
    Instruction fUNDEF Implied,-- 93 - Future Expansion           
    Instruction fSTY ZeroPageX,--94 - STY - Zero Page,X          
    Instruction fSTA ZeroPageX,--95 - STA - Zero Page,X          
    Instruction fSTX ZeroPageY,--96 - STX - Zero Page,Y          
    Instruction fUNDEF Implied,-- 97 - Future Expansion           
    Instruction fTYA Implied,--98 - TYA                        
    Instruction fSTA AbsoluteY,--99 - STA - Absolute,Y           
    Instruction fTXS Implied,--9A - TXS                        
    Instruction fUNDEF Implied,-- 9B - Future Expansion           
    Instruction fUNDEF Implied,-- 9C - Future Expansion           
    Instruction fSTA AbsoluteX,--90 - STA - Absolute,X           
    Instruction fUNDEF Implied,-- 9E - Future Expansion           
    Instruction fUNDEF Implied,-- 9F - Future Expansion           
    Instruction fLDY Immidiate,--A0 - LDY - Immediate
    Instruction fLDA IndexedIndirectX,--A1 - LDA - (Indirect,X,
    Instruction fLDX Immidiate,--A2 - LDX - Immediate
    Instruction fUNDEF Implied,-- A3 - Future Expansion
    Instruction fLDY ZeroPage,--A4 - LDY - Zero Page
    Instruction fLDA ZeroPage,--A5 - LDA - Zero Page
    Instruction fLDX ZeroPage,--A6 - LDX - Zero Page
    Instruction fUNDEF Implied,-- A7 - Future Expansion
    Instruction fTAY Implied,--A8 - TAY
    Instruction fLDA Immidiate,--A9 - LDA - Immediate
    Instruction fTAX Implied,--AA - TAX
    Instruction fUNDEF Implied,-- AB - Future Expansion
    Instruction fLDY Absolute,--AC - LDY - Absolute
    Instruction fLDA Absolute,--AD - LDA - Absolute
    Instruction fLDX Absolute,--AE - LDX - Absolute
    Instruction fUNDEF Implied,-- AF - Future Expansion
    Instruction fBCS Relative,--B0 - BCS
    Instruction fLDA IndirectIndexedY,--B1 - LDA - (Indirect,,Y
    Instruction fUNDEF Implied,-- B2 - Future Expansion
    Instruction fUNDEF Implied,-- B3 - Future Expansion
    Instruction fLDY ZeroPageX,--B4 - LDY - Zero Page,X
    Instruction fLDA ZeroPageX,--B5 - LDA - Zero Page,X
    Instruction fLDX ZeroPageY,--B6 - LDX - Zero Page,Y
    Instruction fUNDEF Implied,-- B7 - Future Expansion
    Instruction fCLV Implied,--B8 - CLV
    Instruction fLDA AbsoluteY,--B9 - LDA - Absolute,Y
    Instruction fTSX Implied,--BA - TSX
    Instruction fUNDEF Implied,-- BB - Future Expansion
    Instruction fLDY AbsoluteX,--BC - LDY - Absolute,X
    Instruction fLDA AbsoluteX,--BD - LDA - Absolute,X
    Instruction fLDX AbsoluteY,--BE - LDX - Absolute,Y
    Instruction fUNDEF Implied,-- BF - Future Expansion
    Instruction fCPY Immidiate,--C0 - Cpy - Immediate            
    Instruction fCMP IndexedIndirectX, -- C1 - CMP - (Indirect,X,         
    Instruction fUNDEF Implied,-- C2 - Future Expansion           
    Instruction fUNDEF Implied,-- C3 - Future Expansion           
    Instruction fCPY ZeroPage,-- C4 - CPY - Zero Page            
    Instruction fCMP ZeroPage,-- C5 - CMP - Zero Page            
    Instruction fDEC ZeroPage,-- C6 - DEC - Zero Page            
    Instruction fUNDEF Implied,-- C7 - Future Expansion           
    Instruction fINY Implied,-- C8 - INY                        
    Instruction fCMP Immidiate,-- C9 - CMP - Immediate            
    Instruction fDEX Implied,-- CA - DEX                        
    Instruction fUNDEF Implied,-- CB - Future Expansion           
    Instruction fCPY Absolute,-- CC - CPY - Absolute             
    Instruction fCMP Absolute,-- CD - CMP - Absolute             
    Instruction fDEC Absolute,-- CE - DEC - Absolute             
    Instruction fUNDEF Implied,-- CF - Future Expansion           
    Instruction fBNE Relative,-- D0 - BNE                        
    Instruction fCMP IndirectIndexedY, -- D1 - CMP   (Indirect@,Y         
    Instruction fUNDEF Implied,-- D2 - Future Expansion           
    Instruction fUNDEF Implied,-- D3 - Future Expansion           
    Instruction fUNDEF Implied,-- D4 - Future Expansion           
    Instruction fCMP ZeroPageX,-- D5 - CMP - Zero Page,X          
    Instruction fDEC ZeroPageX,-- D6 - DEC - Zero Page,X          
    Instruction fUNDEF Implied,-- D7 - Future Expansion           
    Instruction fCLD Implied,--D8 - CLD                        
    Instruction fCMP AbsoluteY,--D9 - CMP - Absolute,Y           
    Instruction fUNDEF Implied,-- DA - Future Expansion           
    Instruction fUNDEF Implied,-- DB - Future Expansion           
    Instruction fUNDEF Implied,-- DC - Future Expansion           
    Instruction fCMP AbsoluteX,--DD - CMP - Absolute,X           
    Instruction fDEC AbsoluteX,--DE - DEC - Absolute,X           
    Instruction fUNDEF Implied,-- DF - Future Expansion           
    Instruction fCPX Immidiate,--E0 - CPX - Immediate
    Instruction fSBC IndexedIndirectX,--E1 - SBC - (Indirect,X,
    Instruction fUNDEF Implied,-- E2 - Future Expansion
    Instruction fUNDEF Implied,-- E3 - Future Expansion
    Instruction fCPX ZeroPage,--E4 - CPX - Zero Page
    Instruction fSBC ZeroPage,--E5 - SBC - Zero Page
    Instruction fINC ZeroPage,--E6 - INC - Zero Page
    Instruction fUNDEF Implied,-- E7 - Future Expansion
    Instruction fINX Implied,-- E8 - INX
    Instruction fSBC Immidiate,-- E9 - SBC - Immediate
    Instruction fNOP Implied,-- EA - NOP
    Instruction fUNDEF Implied,-- EB - Future Expansion
    Instruction fCPX Absolute,--EC - CPX - Absolute
    Instruction fSBC Absolute,--ED - SBC - Absolute
    Instruction fINC Absolute,--EE - INC - Absolute
    Instruction fUNDEF Implied,-- EF - Future Expansion
    Instruction fBEQ Relative,--F0 - BEQ
    Instruction fSBC IndirectIndexedY,--F1 - SBC - (Indirect,,Y
    Instruction fUNDEF Implied,-- F2 - Future Expansion
    Instruction fUNDEF Implied,-- F3 - Future Expansion
    Instruction fUNDEF Implied,-- F4 - Future Expansion
    Instruction fSBC ZeroPageX,--F5 - SBC - Zero Page,X
    Instruction fINC ZeroPageX,-- F6 - INC - Zero Page,X
    Instruction fUNDEF Implied,-- F7 - Future Expansion
    Instruction fSED Implied,--F8 - SED
    Instruction fSBC AbsoluteY,-- F9 - SBC - Absolute,Y
    Instruction fUNDEF Implied,-- FA - Future Expansion
    Instruction fUNDEF Implied,-- FB - Future Expansion
    Instruction fUNDEF Implied,-- FC - Future Expansion
    Instruction fSBC AbsoluteX,--FD - SBC - Absolute,X
    Instruction fINC AbsoluteX,-- FE - INC - Absolute,X
    Instruction fUNDEF Implied-- FF - Future Expansion
           ]

instructionCodes = array (0::Byte,0xFF::Byte) $ zip [0::Byte .. 0xFF ::Byte] codeList

instructionLength :: AddressingMode -> Int
instructionLength Relative = 0
instructionLength Implied = 1
instructionLength Accumulator = 1
instructionLength IndexedIndirectX = 2
instructionLength IndirectIndexedY = 2
instructionLength Immidiate = 2
instructionLength ZeroPage = 2
instructionLength ZeroPageX = 2
instructionLength ZeroPageY = 2
instructionLength Indirect = 3
instructionLength Absolute = 3
instructionLength AbsoluteX = 3
instructionLength AbsoluteY = 3



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
             " Stack: " ++ (show $ L.take 5 $ L.drop 0x1FB  $ Data.Array.elems $ memory cpu) 
             -- ++ (show $ (iType (instructionCodes ! ((memory cpu) ! pPointer cpu))))
  
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

fUNDEF cpu mode b1 b2 = 
        error "UNDEF command"
        
fCLD cpu mode b1 b2 = 
        cpu

fSED cpu mode b1 b2 = 
        cpu
        
fNOP cpu mode b1 b2 = 
        cpu
        
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
