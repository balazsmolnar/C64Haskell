{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Binary

import Base
import Instructions
import MemoryModule
      
test_word8ToInt = 
    TestCase (assertEqual "word8ToInt 3" 3 (word8ToInt 3))

test_intToword8 = 
    TestCase (assertEqual "intToWord8 112" 112 (word8ToInt 112))
    
prop_word8ToInt_intToword8 :: Word8 -> Bool
prop_word8ToInt_intToword8 value = 
        (intToWord8 . word8ToInt) value == value

test_cpuNewPointer = 
    TestCase (assertEqual "cpuNewPointer" 0x1234 (pPointer (cpuNewPointer 0x1234 (initialCPUState 0)))  )

test_cpuNewRegA = 
    TestCase (assertEqual "cpuNewRegA" 0x12 (regA (cpuNewRegA 0x12 (initialCPUState 0)))  )

test_cpuNewRegX = 
    TestCase (assertEqual "cpuNewRegX" 0x12 (regX (cpuNewRegX 0x12 (initialCPUState 0)))  )

test_cpuNewRegY = 
    TestCase (assertEqual "cpuNewRegY" 0x12 (regY (cpuNewRegY 0x12 (initialCPUState 0)))  )

test_cpuNewRegS = 
    TestCase (assertEqual "cpuNewRegS" 0x12 (regS (cpuNewRegS 0x12 (initialCPUState 0)))  )

test_IncreaseCounter_No_Interrupt = 
    TestCase (assertEqual "IncreaseCounter_No_Interrupt" 0 (pPointer (cpuIncreaseCounter (initialCPUState 0)))  )

test_IncreaseCounter_Interrupt = 
    TestCase (assertEqual "IncreaseCounter_Interrupt" 0xFF48 (pPointer (foldr (\x acc -> cpuIncreaseCounter acc) (initialCPUState 0) [0..interruptCount]))  )

test_IncreaseCounter_Interrupt_Resets_Counter = 
    TestCase (assertEqual "IncreaseCounter_Interrupt_Resets_Counter" 0 (counter (foldr (\x acc -> cpuIncreaseCounter acc) (initialCPUState 0) [0..interruptCount]))  )
    
test_Initial_SP_Is_FF = 
    TestCase (assertEqual "Initial_SP_Is_FF" 0xFF (stackPointer (initialCPUState 0))  )
    
--
-- Instructions
--
  
prop_clc :: Bool -> Bool
prop_clc value = 
        let memory = initMemory 10
            cpu = setFlagExp flagCBit value (initialCPUState 0) in
        not $ getFlag flagCBit (fCLC cpu memory Implied 0 0)

prop_sec :: Bool -> Bool
prop_sec value = 
        let memory = initMemory 10
            cpu = setFlagExp flagCBit value (initialCPUState 0) in
        getFlag flagCBit (fSEC cpu memory Implied 0 0)

--
--  LDA
--
        
test_LDA_Immidiate = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,5)]
        cpu = (initialCPUState 0)
        result = (fLDA cpu memory Immidiate 34 0) in
    TestCase (assertEqual "test_LDA_Immidiate" 34  (regA result) )

test_LDA_Absolute = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,5)]
        cpu = (initialCPUState 0)
        result = (fLDA cpu memory Absolute 0x35 0x12) in
    TestCase (assertEqual "test_LDA_Absolute" 6  (regA result) )

test_LDA_AbsoluteX = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,7)]
        cpu = cpuNewRegX 2 (initialCPUState 0)
        result = (fLDA cpu memory AbsoluteX 0x34 0x12) in
    TestCase (assertEqual "test_LDA_AbsoluteX" 7  (regA result) )

test_LDA_AbsoluteY = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,7)]
        cpu = cpuNewRegY 2 (initialCPUState 0)
        result = (fLDA cpu memory AbsoluteY 0x34 0x12) in
    TestCase (assertEqual "test_LDA_AbsoluteY" 7  (regA result) )

test_LDA_ZeroPage = 
    let memory = initMemoryWithValues 0xFFFF [(0x34,5), (0x35,6), (0x36,7)]
        cpu = (initialCPUState 0)
        result = (fLDA cpu memory ZeroPage 0x34 0) in
    TestCase (assertEqual "test_LDA_ZeroPage" 5  (regA result) )

test_LDA_ZeroPageX = 
    let memory = initMemoryWithValues 0xFFFF [(0x34,0x34), (0x35,0x12), (0x1234,5), (0x1235,6), (0x1236,7)]
        cpu = cpuNewRegX 1 (initialCPUState 0)
        result = (fLDA cpu memory ZeroPageX 0x34 0) in
    TestCase (assertEqual "test_LDA_ZeroPageX" 0x12  (regA result) )

test_LDA_ZeroPageY = 
    let memory = initMemoryWithValues 0xFFFF [(0x34,0x34), (0x35,0x12), (0x1234,5), (0x1235,6), (0x1236,7)]
        cpu = cpuNewRegY 1 (initialCPUState 0)
        result = (fLDA cpu memory ZeroPageY 0x34 0) in
    TestCase (assertEqual "test_LDA_ZeroPageY" 0x12  (regA result) )

--
--  LDX
--
        
test_LDX_Immidiate = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,5)]
        cpu = (initialCPUState 0)
        result = (fLDX cpu memory Immidiate 34 0) in
    TestCase (assertEqual "test_LDX_Immidiate" 34  (regX result) )

test_LDX_Absolute = 
    let memory = initMemoryWithValues 0xFFFF [(0x1234,5), (0x1235,6), (0x1236,5)]
        cpu = (initialCPUState 0)
        result = (fLDX cpu memory Absolute 0x35 0x12) in
    TestCase (assertEqual "test_LDX_Absolute" 6  (regX result) )

test_LDX_ZeroPage = 
    let memory = initMemoryWithValues 0xFFFF [(0x34,5), (0x35,6), (0x36,7)]
        cpu = (initialCPUState 0)
        result = (fLDX cpu memory ZeroPage 0x34 0) in
    TestCase (assertEqual "test_LDX_ZeroPage" 5  (regX result) )

--
--  STA
--

test_STA_Absolute = 
    let memory = initMemory 0xFFFF
        cpu = cpuNewRegA 5 (initialCPUState 0)
        result = (fSTA cpu memory Absolute 0x35 0x12) in
    TestCase (assertEqual "test_STA_Absolute" (0x1235, 5)  (head $ changedMemory result) )

test_STA_AbsoluteX = 
    let memory = initMemory 0xFFFF
        cpu = cpuNewRegX 2 $ cpuNewRegA 5 (initialCPUState 0)
        result = (fSTA cpu memory AbsoluteX 0x35 0x12) in
    TestCase (assertEqual "test_STA_AbsoluteX" (0x1237, 5)  (head $ changedMemory result) )

--
-- INY
--

test_INY_5 = 
     let memory = initMemory 10
         cpu = cpuNewRegY 5(initialCPUState 0)
         result = fINY cpu memory Implied 0x00 0x00 in
     TestCase (assertEqual "test_INY_5" 6 (regY result) )

test_INY_FF = 
     let memory = initMemory 10
         cpu = cpuNewRegY 0xFF(initialCPUState 0)
         result = fINY cpu memory Implied 0x00 0x00 in
     TestCase (assertEqual "test_INY_FF" 0 (regY result) )

test_INY_FF_Zero_Flag = 
     let memory = initMemory 10
         cpu = cpuNewRegY 0xFF(initialCPUState 0)
         result = fINY cpu memory Implied 0x00 0x00 in
     TestCase (assertEqual "test_INY_FF_Zero_Flag" True (getFlag flagZBit result) )
     
--
--  JMP
--

test_JMP_Absolute = 
    let memory = initMemory 0xFFFF
        cpu =  (initialCPUState 0)
        result = (fJMP cpu memory Absolute 0x35 0x12) in
    TestCase (assertEqual "test_JMP_Absolute" (0x1235-3)  (pPointer result) )
    
tests = TestList
        [
            test_word8ToInt,
            test_intToword8,
            test_cpuNewPointer,
            test_cpuNewRegA,
            test_cpuNewRegX,
            test_cpuNewRegY,
            test_cpuNewRegS,
            test_IncreaseCounter_No_Interrupt,
            test_IncreaseCounter_Interrupt,
            test_IncreaseCounter_Interrupt_Resets_Counter,
            test_LDA_Immidiate,
            test_LDA_Absolute,
            test_LDA_AbsoluteX,
            test_LDA_AbsoluteY,
            test_LDA_ZeroPage,
            test_LDA_ZeroPageX,
            test_LDA_ZeroPageY,
            test_LDX_Immidiate,
            test_LDX_Absolute,
            test_LDX_ZeroPage,
            test_STA_Absolute,
            test_STA_AbsoluteX,
            test_JMP_Absolute,
            test_INY_5,
            test_INY_FF,
            test_INY_FF_Zero_Flag  
        ]

return []
runTests = $quickCheckAll
        
main = do 
        runTestTT  tests
        runTests
