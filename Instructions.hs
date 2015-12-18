
module Instructions where

import Base
import Data.Array
import Data.List

data InstructionType = 
    UNDEF|
    ADC| -- Add Memory to Accumulator with Carry
    AND| -- "AND" Memory with Accumulator
    ASL| -- Shift Left One Bit (Memory or Accumulator)
    BCC| -- Branch on Carry Clear
    BCS| -- Branch on Carry Set
    BEQ| -- Branch on Result Zero
    BIT| -- Test Bits in Memory with Accumulator
    BMI| -- Branch on Result Minus
    BNE| -- Branch on Result not Zero
    BPL| -- Branch on Result Plus
    BRK| -- Force Break
    BVC| -- Branch on Overflow Clear
    BVS| -- Branch on Overflow Set
    CLC| -- Clear Carry Flag
    CLD| -- Clear Decimal Mode
    CLI| -- Clear interrupt Disable Bit
    CLV| -- Clear Overflow Flag
    CMP| -- Compare Memory and Accumulator
    CPX| -- Compare Memory and Index X
    CPY| -- Compare Memory and Index Y
    DEC| -- Decrement Memory by One
    DEX| -- Decrement Index X by One
    DEY| -- Decrement Index Y by One
    EOR| -- "Exclusive-Or" Memory with Accumulator
    INC| -- Increment Memory by One
    INX| -- Increment Index X by One
    INY| -- Increment Index Y by One
    JMP| -- Jump to New Location
    JSR| -- Jump to New Location Saving Return Address
    LDA| -- Load Accumulator with Memory
    LDX| -- Load Index X with Memory
    LDY| -- Load Index Y with Memory
    LSR| -- Shift Right One Bit (Memory or Accumulator)
    NOP| -- No Operation
    ORA| -- "OR" Memory with Accumulator
    PHA| -- Push Accumulator on Stack
    PHP| -- Push Processor Status on Stack
    PLA| -- Pull Accumulator from Stack
    PLP| -- Pull Processor Status from Stack
    ROL| -- Rotate One Bit Left (Memory or Accumulator)
    ROR| -- Rotate One Bit Right (Memory or Accumulator)
    RTI| -- Return from Interrupt
    RTS| -- Return from Subroutine
    SBC| -- Subtract Memory from Accumulator with Borrow
    SEC| -- Set Carry Flag
    SED| -- Set Decimal Mode
    SEI| -- Set Interrupt Disable Status
    STA| -- Store Accumulator in Memory
    STX| -- Store Index X in Memory
    STY| -- Store Index Y in Memory
    TAX| -- Transfer Accumulator to Index X
    TAY| -- Transfer Accumulator to Index Y
    TSX| -- Transfer Stack Pointer to Index X
    TXA| -- Transfer Index X to Accumulator
    TXS| -- Transfer Index X to Stack Pointer
    TYA -- Transfer Index Y to Accumulator
    deriving (Read, Show, Enum, Eq)

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
 
data Instruction = Instruction { iType :: InstructionType, mode :: AddressingMode }  
    deriving (Read, Show, Eq) 

    
codeList = [
    Instruction BRK Implied,-- 00 - BRK                        
    Instruction ORA IndexedIndirectX,-- 01 - ORA - (Indirect,X,         
    Instruction UNDEF Implied,-- 02 - Future Expansion           
    Instruction UNDEF Implied,-- 03 - Future Expansion           
    Instruction UNDEF Implied,-- 04 - Future Expansion           
    Instruction ORA ZeroPage,-- 05 - ORA - Zero Page            
    Instruction ASL ZeroPage,-- 06 - ASL - Zero Page            
    Instruction UNDEF Implied,-- 07 - Future Expansion           
    Instruction PHP Implied,-- 08 - PHP                        
    Instruction ORA Immidiate,-- 09 - ORA - Immediate            
    Instruction ASL Accumulator,-- 0A - ASL - Accumulator          
    Instruction UNDEF Implied,-- 0B - Future Expansion           
    Instruction UNDEF Implied,-- 0C - Future Expansion           
    Instruction ORA Absolute,-- 0D - ORA - Absolute             
    Instruction ASL Absolute,-- 0E - ASL - Absolute             
    Instruction UNDEF Implied,-- 0F - Future Expansion           
    Instruction BPL Relative,-- 10 - BPL                        
    Instruction ORA IndirectIndexedY, --11 - ORA - (Indirect,,Y         
    Instruction UNDEF Implied,-- 12 - Future Expansion           
    Instruction UNDEF Implied,-- 13 - Future Expansion           
    Instruction UNDEF Implied,-- 14 - Future Expansion           
    Instruction ORA ZeroPageX,--15 - ORA - Zero Page,X          
    Instruction ASL ZeroPageX,--16 - ASL - Zero Page,X          
    Instruction UNDEF Implied,-- 17 - Future Expansion           
    Instruction CLC Implied,--18 - CLC                        
    Instruction ORA AbsoluteY,--19 - ORA - Absolute,Y           
    Instruction UNDEF Implied,-- 1A - Future Expansion           
    Instruction UNDEF Implied,-- 1B - Future Expansion           
    Instruction UNDEF Implied,-- 1C - Future Expansion           
    Instruction ORA AbsoluteX,--1D - ORA - Absolute,X           
    Instruction ASL AbsoluteX,--1E - ASL - Absolute,X           
    Instruction UNDEF Implied,-- 1F - Future Expansion           
    Instruction JSR Absolute,--20 - JSR
    Instruction AND IndexedIndirectX,--21 - AND - (Indirect,X,
    Instruction UNDEF Implied,-- 22 - Future Expansion
    Instruction UNDEF Implied,-- 23 - Future Expansion
    Instruction BIT ZeroPage,-- 24 - BIT - Zero Page
    Instruction AND ZeroPage,--25 - AND - Zero Page
    Instruction ROL ZeroPage,--26 - ROL - Zero Page
    Instruction UNDEF Implied,-- 27 - Future Expansion
    Instruction PLP Implied,-- 28 - PLP
    Instruction AND Immidiate,--29 - AND - Immediate
    Instruction ROL Accumulator,--2A - ROL - Accumulator
    Instruction UNDEF Implied,-- 2B - Future Expansion
    Instruction BIT Absolute,--2C - BIT - Absolute
    Instruction AND Absolute,--2D - AND - Absolute
    Instruction ROL Absolute,--2E - ROL - Absolute
    Instruction UNDEF Implied,-- 2F - Future Expansion
    Instruction BMI Relative,--30 - BMI
    Instruction AND IndirectIndexedY,--31 - AND - (Indirect,,Y
    Instruction UNDEF Implied,-- 32 - Future Expansion
    Instruction UNDEF Implied,-- 33 - Future Expansion
    Instruction UNDEF Implied,-- 34 - Future Expansion
    Instruction AND ZeroPageX,-- 35 - AND - Zero Page,X
    Instruction ROL ZeroPageX,--36 - ROL - Zero Page,X
    Instruction UNDEF Implied,-- 37 - Future Expansion
    Instruction SEC Implied,--38 - SEC
    Instruction AND AbsoluteY,--39 - AND - Absolute,Y
    Instruction UNDEF Implied,-- 3A - Future Expansion
    Instruction UNDEF Implied,-- 3B - Future Expansion
    Instruction UNDEF Implied,-- 3C - Future Expansion
    Instruction AND AbsoluteX,--3D - AND - Absolute,X
    Instruction ROL AbsoluteX,--3E - ROL - Absolute,X
    Instruction UNDEF Implied,-- 3F - Future Expansion
    Instruction RTI Implied,--40 - RTI                        
    Instruction EOR IndexedIndirectX, --41 - EOR - (Indirect,X,         
    Instruction UNDEF Implied,-- 42 - Future Expansion           
    Instruction UNDEF Implied,-- 43 - Future Expansion           
    Instruction UNDEF Implied,-- 44 - Future Expansion           
    Instruction EOR ZeroPage,--45 - EOR - Zero Page            
    Instruction LSR ZeroPage,--46 - LSR - Zero Page            
    Instruction UNDEF Implied,-- 47 - Future Expansion           
    Instruction PHA Implied,--48 - PHA                        
    Instruction EOR Immidiate,--49 - EOR - Immediate            
    Instruction LSR Accumulator,--4A - LSR - Accumulator          
    Instruction UNDEF Implied,-- 4B - Future Expansion           
    Instruction JMP Absolute,--4C - JMP - Absolute             
    Instruction EOR Absolute,--4D - EOR - Absolute             
    Instruction LSR Absolute,--4E - LSR - Absolute             
    Instruction UNDEF Implied,-- 4F - Future Expansion           
    Instruction BVC Relative,--50 - BVC                        
    Instruction EOR IndirectIndexedY, -- 51 - EOR - (Indirect,,Y         
    Instruction UNDEF Implied,-- 52 - Future Expansion           
    Instruction UNDEF Implied,-- 53 - Future Expansion           
    Instruction UNDEF Implied,-- 54 - Future Expansion           
    Instruction EOR ZeroPageX,--55 - EOR - Zero Page,X          
    Instruction LSR ZeroPageX,--56 - LSR - Zero Page,X          
    Instruction UNDEF Implied,-- 57 - Future Expansion           
    Instruction CLI Implied,--58 - CLI                        
    Instruction EOR AbsoluteY,--59 - EOR - Absolute,Y           
    Instruction UNDEF Implied,-- 5A - Future Expansion           
    Instruction UNDEF Implied,-- 5B - Future Expansion           
    Instruction UNDEF Implied,-- 5C - Future Expansion           
    Instruction EOR AbsoluteX,--50 - EOR - Absolute,X           
    Instruction LSR AbsoluteX,--5E - LSR - Absolute,X           
    Instruction UNDEF Implied,-- 5F - Future Expansion           
    Instruction RTS Implied,--60 - RTS
    Instruction ADC IndexedIndirectX,--61 - ADC - (Indirect,X,
    Instruction UNDEF Implied,-- 62 - Future Expansion
    Instruction UNDEF Implied,-- 63 - Future Expansion
    Instruction UNDEF Implied,-- 64 - Future Expansion
    Instruction ADC ZeroPage,--65 - ADC - Zero Page
    Instruction ROR ZeroPage,--66 - ROR - Zero Page
    Instruction UNDEF Implied,-- 67 - Future Expansion
    Instruction PLA Implied,--68 - PLA
    Instruction ADC Immidiate,--69 - ADC - Immediate
    Instruction ROR Accumulator,--6A - ROR - Accumulator
    Instruction UNDEF Implied,-- 6B - Future Expansion
    Instruction JMP Indirect,  --6C - JMP - Indirect
    Instruction ADC Absolute,--6D - ADC - Absolute
    Instruction ROR Absolute,--6E - ROR - Absolute
    Instruction UNDEF Implied,-- 6F - Future Expansion
    Instruction BVS Relative,--70 - BVS
    Instruction ADC IndirectIndexedY,--71 - ADC - (Indirect,,Y
    Instruction UNDEF Implied,-- 72 - Future Expansion
    Instruction UNDEF Implied,-- 73 - Future Expansion
    Instruction UNDEF Implied,-- 74 - Future Expansion
    Instruction ADC ZeroPageX,--75 - ADC - Zero Page,X
    Instruction ROR ZeroPageX,--76 - ROR - Zero Page,X
    Instruction UNDEF Implied,-- 77 - Future Expansion
    Instruction SEI Implied,--78 - SEI
    Instruction ADC AbsoluteY,--79 - ADC - Absolute,Y
    Instruction UNDEF Implied,-- 7A - Future Expansion
    Instruction UNDEF Implied,-- 7B - Future Expansion
    Instruction UNDEF Implied,-- 7C - Future Expansion
    Instruction ADC AbsoluteX,--7D - ADC - Absolute,X
    Instruction ROR AbsoluteX,--7E - ROR - Absolute,X
    Instruction UNDEF Implied,-- 7F - Future Expansion
    Instruction UNDEF Implied,-- 80 - Future Expansion           
    Instruction STA IndexedIndirectX, -- 81 - STA - (Indirect,X,         
    Instruction UNDEF Implied,-- 82 - Future Expansion           
    Instruction UNDEF Implied,-- 83 - Future Expansion           
    Instruction STY ZeroPage,--84 - STY - Zero Page            
    Instruction STA ZeroPage,--85 - STA - Zero Page            
    Instruction STX ZeroPage,--86 - STX - Zero Page            
    Instruction UNDEF Implied,-- 87 - Future Expansion           
    Instruction DEY Implied,--88 - DEY                        
    Instruction UNDEF Implied,-- 89 - Future Expansion           
    Instruction TXA Implied,--8A - TXA                        
    Instruction UNDEF Implied,-- 8B - Future Expansion           
    Instruction STY Absolute,--8C - STY - Absolute             
    Instruction STA Absolute,--8D - STA - Absolute             
    Instruction STX Absolute,--8E - STX - Absolute             
    Instruction UNDEF Implied,-- 8F - Future Expansion           
    Instruction BCC Relative,--90 - BCC                        
    Instruction STA IndirectIndexedY, --91 - STA - (Indirect,,Y         
    Instruction UNDEF Implied,-- 92 - Future Expansion           
    Instruction UNDEF Implied,-- 93 - Future Expansion           
    Instruction STY ZeroPageX,--94 - STY - Zero Page,X          
    Instruction STA ZeroPageX,--95 - STA - Zero Page,X          
    Instruction STX ZeroPageY,--96 - STX - Zero Page,Y          
    Instruction UNDEF Implied,-- 97 - Future Expansion           
    Instruction TYA Implied,--98 - TYA                        
    Instruction STA AbsoluteY,--99 - STA - Absolute,Y           
    Instruction TXS Implied,--9A - TXS                        
    Instruction UNDEF Implied,-- 9B - Future Expansion           
    Instruction UNDEF Implied,-- 9C - Future Expansion           
    Instruction STA AbsoluteX,--90 - STA - Absolute,X           
    Instruction UNDEF Implied,-- 9E - Future Expansion           
    Instruction UNDEF Implied,-- 9F - Future Expansion           
    Instruction LDY Immidiate,--A0 - LDY - Immediate
    Instruction LDA IndexedIndirectX,--A1 - LDA - (Indirect,X,
    Instruction LDX Immidiate,--A2 - LDX - Immediate
    Instruction UNDEF Implied,-- A3 - Future Expansion
    Instruction LDY ZeroPage,--A4 - LDY - Zero Page
    Instruction LDA ZeroPage,--A5 - LDA - Zero Page
    Instruction LDX ZeroPage,--A6 - LDX - Zero Page
    Instruction UNDEF Implied,-- A7 - Future Expansion
    Instruction TAY Implied,--A8 - TAY
    Instruction LDA Immidiate,--A9 - LDA - Immediate
    Instruction TAX Implied,--AA - TAX
    Instruction UNDEF Implied,-- AB - Future Expansion
    Instruction LDY Absolute,--AC - LDY - Absolute
    Instruction LDA Absolute,--AD - LDA - Absolute
    Instruction LDX Absolute,--AE - LDX - Absolute
    Instruction UNDEF Implied,-- AF - Future Expansion
    Instruction BCS Relative,--B0 - BCS
    Instruction LDA IndirectIndexedY,--B1 - LDA - (Indirect,,Y
    Instruction UNDEF Implied,-- B2 - Future Expansion
    Instruction UNDEF Implied,-- B3 - Future Expansion
    Instruction LDY ZeroPageX,--B4 - LDY - Zero Page,X
    Instruction LDA ZeroPageX,--B5 - LDA - Zero Page,X
    Instruction LDX ZeroPageY,--B6 - LDX - Zero Page,Y
    Instruction UNDEF Implied,-- B7 - Future Expansion
    Instruction CLV Implied,--B8 - CLV
    Instruction LDA AbsoluteY,--B9 - LDA - Absolute,Y
    Instruction TSX Implied,--BA - TSX
    Instruction UNDEF Implied,-- BB - Future Expansion
    Instruction LDY AbsoluteX,--BC - LDY - Absolute,X
    Instruction LDA AbsoluteX,--BD - LDA - Absolute,X
    Instruction LDX AbsoluteY,--BE - LDX - Absolute,Y
    Instruction UNDEF Implied,-- BF - Future Expansion
    Instruction CPY Immidiate,--C0 - Cpy - Immediate            
    Instruction CMP IndexedIndirectX, -- C1 - CMP - (Indirect,X,         
    Instruction UNDEF Implied,-- C2 - Future Expansion           
    Instruction UNDEF Implied,-- C3 - Future Expansion           
    Instruction CPY ZeroPage,-- C4 - CPY - Zero Page            
    Instruction CMP ZeroPage,-- C5 - CMP - Zero Page            
    Instruction DEC ZeroPage,-- C6 - DEC - Zero Page            
    Instruction UNDEF Implied,-- C7 - Future Expansion           
    Instruction INY Implied,-- C8 - INY                        
    Instruction CMP Immidiate,-- C9 - CMP - Immediate            
    Instruction DEX Implied,-- CA - DEX                        
    Instruction UNDEF Implied,-- CB - Future Expansion           
    Instruction CPY Absolute,-- CC - CPY - Absolute             
    Instruction CMP Absolute,-- CD - CMP - Absolute             
    Instruction DEC Absolute,-- CE - DEC - Absolute             
    Instruction UNDEF Implied,-- CF - Future Expansion           
    Instruction BNE Relative,-- D0 - BNE                        
    Instruction CMP IndirectIndexedY, -- D1 - CMP   (Indirect@,Y         
    Instruction UNDEF Implied,-- D2 - Future Expansion           
    Instruction UNDEF Implied,-- D3 - Future Expansion           
    Instruction UNDEF Implied,-- D4 - Future Expansion           
    Instruction CMP ZeroPageX,-- D5 - CMP - Zero Page,X          
    Instruction DEC ZeroPageX,-- D6 - DEC - Zero Page,X          
    Instruction UNDEF Implied,-- D7 - Future Expansion           
    Instruction CLD Implied,--D8 - CLD                        
    Instruction CMP AbsoluteY,--D9 - CMP - Absolute,Y           
    Instruction UNDEF Implied,-- DA - Future Expansion           
    Instruction UNDEF Implied,-- DB - Future Expansion           
    Instruction UNDEF Implied,-- DC - Future Expansion           
    Instruction CMP AbsoluteX,--DD - CMP - Absolute,X           
    Instruction DEC AbsoluteX,--DE - DEC - Absolute,X           
    Instruction UNDEF Implied,-- DF - Future Expansion           
    Instruction CPX Immidiate,--E0 - CPX - Immediate
    Instruction SBC IndexedIndirectX,--E1 - SBC - (Indirect,X,
    Instruction UNDEF Implied,-- E2 - Future Expansion
    Instruction UNDEF Implied,-- E3 - Future Expansion
    Instruction CPX ZeroPage,--E4 - CPX - Zero Page
    Instruction SBC ZeroPage,--E5 - SBC - Zero Page
    Instruction INC ZeroPage,--E6 - INC - Zero Page
    Instruction UNDEF Implied,-- E7 - Future Expansion
    Instruction INX Implied,-- E8 - INX
    Instruction SBC Immidiate,-- E9 - SBC - Immediate
    Instruction NOP Implied,-- EA - NOP
    Instruction UNDEF Implied,-- EB - Future Expansion
    Instruction CPX Absolute,--EC - CPX - Absolute
    Instruction SBC Absolute,--ED - SBC - Absolute
    Instruction INC Absolute,--EE - INC - Absolute
    Instruction UNDEF Implied,-- EF - Future Expansion
    Instruction BEQ Relative,--F0 - BEQ
    Instruction SBC IndirectIndexedY,--F1 - SBC - (Indirect,,Y
    Instruction UNDEF Implied,-- F2 - Future Expansion
    Instruction UNDEF Implied,-- F3 - Future Expansion
    Instruction UNDEF Implied,-- F4 - Future Expansion
    Instruction SBC ZeroPageX,--F5 - SBC - Zero Page,X
    Instruction INC ZeroPageX,-- F6 - INC - Zero Page,X
    Instruction UNDEF Implied,-- F7 - Future Expansion
    Instruction SED Implied,--F8 - SED
    Instruction SBC AbsoluteY,-- F9 - SBC - Absolute,Y
    Instruction UNDEF Implied,-- FA - Future Expansion
    Instruction UNDEF Implied,-- FB - Future Expansion
    Instruction UNDEF Implied,-- FC - Future Expansion
    Instruction SBC AbsoluteX,--FD - SBC - Absolute,X
    Instruction INC AbsoluteX,-- FE - INC - Absolute,X
    Instruction UNDEF Implied-- FF - Future Expansion
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
