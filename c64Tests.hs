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
            test_IncreaseCounter_Interrupt_Resets_Counter
        ]

return []
runTests = $quickCheckAll
        
main = do 
        runTestTT  tests
        runTests
