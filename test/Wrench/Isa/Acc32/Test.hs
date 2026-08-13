module Wrench.Isa.Acc32.Test (tests) where

import Data.Default
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Isa.Acc32
import Wrench.Machine.Memory
import Wrench.Machine.Types

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "bgtz taken (acc > 0)" $ do
            runBranch (Bgz 100) 5 @?= 100
        , testCase "bgtz not taken (acc == 0)" $ do
            runBranch (Bgz 100) 0 @?= 10
        , testCase "bgtz not taken (acc < 0)" $ do
            runBranch (Bgz 100) (-1) @?= 10
        , testCase "bltz taken (acc < 0)" $ do
            runBranch (Blz 100) (-1) @?= 100
        , testCase "bltz not taken (acc == 0)" $ do
            runBranch (Blz 100) 0 @?= 10
        , testCase "bltz not taken (acc > 0)" $ do
            runBranch (Blz 100) 5 @?= 10
        , testCase "bgez taken (acc == 0)" $ do
            runBranch (Bgez 100) 0 @?= 100
        , testCase "bgez taken (acc > 0)" $ do
            runBranch (Bgez 100) 5 @?= 100
        , testCase "bgez not taken (acc < 0)" $ do
            runBranch (Bgez 100) (-1) @?= 10
        ]

-- | Build an initial state with a LoadImm at address 0 (5 bytes) followed by
-- the branch instruction at address 5 (5 bytes). After two instructionStep
-- calls, the acc is set and the branch is evaluated.
mkState :: Isa Int32 Int32 -> Int32 -> Acc32State Int32
mkState branchInstr accVal =
    let loadInstr = LoadImm accVal
        mem =
            mkIoMem
                def
                ( Mem
                    { memoryData =
                        fromList $
                            [(0, Instruction loadInstr)]
                                <> [(i, InstructionPart) | i <- [1 .. 4]]
                                <> [(5, Instruction branchInstr)]
                                <> [(i, InstructionPart) | i <- [6 .. 9]]
                    , memorySize = 10
                    }
                )
     in initState 0 mem []

runBranch :: Isa Int32 Int32 -> Int32 -> Int
runBranch instr accVal =
    let st = mkState instr accVal
        st' = execState (instructionStep >> instructionStep) st
     in programCounter st'
