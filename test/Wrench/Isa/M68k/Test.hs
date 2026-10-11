module Wrench.Isa.M68k.Test (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Default
import Data.List (isInfixOf)
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos (initialPos)
import Wrench.Isa.M68k
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (DerefMnemonic (..), Ref (..))

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Byte arithmetic operations" $ do
            let M68kSt{dataRegs} = simulate "add.b D1, D0" st0{dataRegs = insert D1 3 $ insert D0 4 dataRegs0}
             in (dataRegs !? D0) @?= Just 7
            let M68kSt{dataRegs, zFlag} = simulate "add.b D1, D0" st0{dataRegs = insert D1 0xFF $ insert D0 1 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    zFlag @?= True
            let M68kSt{dataRegs} = simulate "sub.b D1, D0" st0{dataRegs = insert D1 2 $ insert D0 7 dataRegs0}
             in (dataRegs !? D0) @?= Just 5
            let M68kSt{dataRegs, nFlag} = simulate "sub.b D1, D0" st0{dataRegs = insert D1 10 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just (-5)
                    nFlag @?= True
            let M68kSt{dataRegs} = simulate "mul.b D1, D0" st0{dataRegs = insert D1 3 $ insert D0 4 dataRegs0}
             in (dataRegs !? D0) @?= Just 12
            let M68kSt{dataRegs} = simulate "mul.b D1, D0" st0{dataRegs = insert D1 10 $ insert D0 10 dataRegs0}
             in (dataRegs !? D0) @?= Just 100
        , testCase "Compare operations" $ do
            let M68kSt{dataRegs, zFlag, nFlag, cFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 5 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 5
                    zFlag @?= True
                    nFlag @?= False
                    cFlag @?= False
            let M68kSt{dataRegs, zFlag, nFlag, cFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 10 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 10
                    zFlag @?= False
                    nFlag @?= True
                    cFlag @?= True
            let M68kSt{dataRegs, zFlag, nFlag, cFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 3 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 3
                    zFlag @?= False
                    nFlag @?= False
                    cFlag @?= False
            let M68kSt{dataRegs, zFlag} =
                    simulate "cmp.b D1, D0" st0{dataRegs = insert D1 0x100 $ insert D0 0 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    (dataRegs !? D1) @?= Just 0x100
                    zFlag @?= True
        , testCase "Logic ops clear V and C flags" $ do
            let M68kSt{vFlag, cFlag} =
                    simulate
                        "and.l D1, D0"
                        st0
                            { dataRegs = insert D1 0xFF $ insert D0 0xFF dataRegs0
                            , vFlag = True
                            , cFlag = True
                            }
             in do
                    vFlag @?= False
                    cFlag @?= False
            let M68kSt{vFlag, cFlag} =
                    simulate
                        "move.l D1, D0"
                        st0
                            { dataRegs = insert D1 1 dataRegs0
                            , vFlag = True
                            , cFlag = True
                            }
             in do
                    vFlag @?= False
                    cFlag @?= False
            let M68kSt{vFlag, cFlag} =
                    simulate
                        "not.l D0"
                        st0
                            { dataRegs = insert D0 0 dataRegs0
                            , vFlag = True
                            , cFlag = True
                            }
             in do
                    vFlag @?= False
                    cFlag @?= False
        , testCase "Translator" $ do
            translate "movea.l D0, A0" @?= Right (MoveA Long (DirectDataReg D0) (DirectAddrReg A0))
            translate "move.l 12, D0" @?= Right (Move Long (Immediate $ valueR 12) (DirectDataReg D0))
            translate "move.l 8(A2), D0" @?= Right (Move Long (IndirectAddrReg 8 A2 Nothing) (DirectDataReg D0))
            translate "move.l -8(A2), D0" @?= Right (Move Long (IndirectAddrReg (-8) A2 Nothing) (DirectDataReg D0))
            translate "move.l -8(A2,D1), D0"
                @?= Right (Move Long (IndirectAddrReg (-8) A2 (Just $ DataIndex D1)) (DirectDataReg D0))
            translate "move.l -8(A2,A1), D0"
                @?= Right (Move Long (IndirectAddrReg (-8) A2 (Just $ AddrIndex A1)) (DirectDataReg D0))
            translate "cmp.l D1, D0" @?= Right (Cmp Long (DirectDataReg D1) (DirectDataReg D0))
            translate "cmp.b 10, D0" @?= Right (Cmp Byte (Immediate $ valueR 10) (DirectDataReg D0))
            translate "cmp.l (A1), D0" @?= Right (Cmp Long (IndirectAddrReg 0 A1 Nothing) (DirectDataReg D0))
            -- movea is the one instruction that legitimately accepts An as source.
            translate "movea.l A2, A0" @?= Right (MoveA Long (DirectAddrReg A2) (DirectAddrReg A0))
            translate "jsr 0x20" @?= Right (Jsr $ valueR 0x20)
            translate "rts" @?= Right Rts
        , testCase "An as source rejected outside movea (issue #143)" $ do
            -- Address Register Direct is not a valid source mode for these
            -- instructions; the parser must fail rather than silently
            -- consuming @A2@ as a label.
            isLeft (parsesM68k "move.l A2, D0") @?= True
            isLeft (parsesM68k "add.l A0, D1") @?= True
            isLeft (parsesM68k "sub.l A0, D1") @?= True
            isLeft (parsesM68k "cmp.l A0, D0") @?= True
            isLeft (parsesM68k "and.l A0, D1") @?= True
            -- @movea@ still accepts An as source.
            isRight (parsesM68k "movea.l A2, A0") @?= True
        , testGroup
            "Displacement field"
            [ testCase "displacements that fit are accepted" $ do
                isRight (parsesM68k "move.l -4(A6), D0") @?= True
                isRight (parsesM68k "move.l 32767(A0), D0") @?= True
                isRight (parsesM68k "move.l -32768(A0), D0") @?= True
                isRight (parsesM68k "link A6, -4") @?= True
            , testCase "displacement above the field" $ do
                rejected "move.l 32768(A0), D0" "-32768..32767"
            , testCase "displacement below the field" $ do
                rejected "move.l -32769(A0), D0" "-32768..32767"
            , -- Shares a brief extension word with the index register.
              testCase "the indexed form carries a byte" $ do
                accepted "move.l 127(A0,D1), D0"
                rejected "move.l 128(A0,D1), D0" "-128..127"
                rejected "move.l -129(A0,D1), D0" "-128..127"
            , testCase "link carries the same field" $ do
                rejected "link A6, 100000" "-32768..32767"
            , testCase "rejection names the operand, the field and the range" $ do
                fieldOutcome "move.l 100000(A0), D0" >>= \case
                    Right m -> assertFailure $ "should be rejected, got: " <> show m
                    Left err -> do
                        assertBool ("displacement in: " <> err) $ "100000" `isInfixOf` err
                        assertBool ("field width in: " <> err) $ "16-bit" `isInfixOf` err
                        assertBool ("carrier in: " <> err) $ "extension word" `isInfixOf` err
                        assertBool ("range in: " <> err) $ "-32768..32767" `isInfixOf` err
            ]
        , testGroup
            "Shift count"
            [ testCase "counts that fit are accepted" $ do
                accepted "asl.l 0, D0"
                accepted "asl.l 31, D0"
                accepted "lsr.b 7, D0"
                accepted "asl.b 8, D0"
            , testCase "an immediate count of 8 or more clears a byte" $ do
                let M68kSt{dataRegs} = simulate "asl.b 8, D0" st0{dataRegs = insert D0 0x10 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "the register form is untouched" $ do
                accepted "asl.l D1, D0"
            , testCase "count above the field" $ do
                rejected "asl.l 32, D0" "0..31"
            , testCase "a negative count" $ do
                rejected "asr.b -1, D0" "0..31"
            , testCase "every shift carries the same field" $ do
                for_ ["asl", "asr", "lsl", "lsr"] $ \op -> do
                    outcome <- fieldOutcome $ op <> ".l 100, D0"
                    case outcome of
                        Right m -> assertFailure $ op <> " should be rejected, got: " <> show m
                        Left err -> do
                            assertBool (op <> " in: " <> err) $ (op <> ".l: shift count") `isInfixOf` err
                            assertBool ("range in: " <> err) $ "0..31" `isInfixOf` err
            , testCase "a label is not a shift count" $ do
                isLeft (parsesM68k "asl.l shamt, D0") @?= True
            , testCase "every other immediate still takes a label" $ do
                isRight (parsesM68k "movea.l array, A0") @?= True
                isRight (parsesM68k "move.l array, D0") @?= True
                isRight (parsesM68k "add.l array, D0") @?= True
            ]
        , testGroup
            "Register shift count"
            [ testCase "a count at the width shifts the datum out" $ do
                let M68kSt{dataRegs} =
                        simulate "asl.l D1, D0" st0{dataRegs = insert D1 32 $ insert D0 1 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "a count above the width shifts the datum out too" $ do
                let M68kSt{dataRegs} =
                        simulate "asl.l D1, D0" st0{dataRegs = insert D1 33 $ insert D0 1 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "a negative count is modulo 64, so it shifts it out too" $ do
                let M68kSt{dataRegs} =
                        simulate "asl.l D1, D0" st0{dataRegs = insert D1 (-1) $ insert D0 1 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "the carry still reports the last bit shifted out" $ do
                let M68kSt{cFlag} =
                        simulate "asl.l D1, D0" st0{dataRegs = insert D1 32 $ insert D0 1 dataRegs0}
                 in cFlag @?= True
            , testCase "byte mode is held at its own width" $ do
                let M68kSt{dataRegs} =
                        simulate "asl.b D1, D0" st0{dataRegs = insert D1 8 $ insert D0 0x10 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "a negative count in byte mode shifts the byte out" $ do
                let M68kSt{dataRegs} =
                        simulate "asr.b D1, D0" st0{dataRegs = insert D1 (-1) $ insert D0 0x10 dataRegs0}
                 in (dataRegs !? D0) @?= Just 0
            , testCase "a count that fits is unaffected" $ do
                let M68kSt{dataRegs} =
                        simulate "lsl.l D1, D0" st0{dataRegs = insert D1 2 $ insert D0 3 dataRegs0}
                 in (dataRegs !? D0) @?= Just 12
            ]
        , testGroup
            "Byte-mode immediate"
            [ testCase "a byte immediate that fits is accepted" $ do
                accepted "move.b 255, D0"
                accepted "move.b -128, D0"
                accepted "add.b 0xFF, D0"
            , testCase "above the byte" $ do
                rejected "move.b 300, D0" "-128..255"
            , testCase "below the byte" $ do
                rejected "move.b -129, D0" "-128..255"
            , testCase "every byte-mode immediate is checked" $ do
                for_ ["move", "and", "or", "xor", "add", "sub", "mul", "div", "cmp"] $ \op -> do
                    outcome <- fieldOutcome $ op <> ".b 300, D0"
                    case outcome of
                        Right m -> assertFailure $ op <> " should be rejected, got: " <> show m
                        Left err -> do
                            assertBool (op <> " in: " <> err) $ (op <> ".b: immediate") `isInfixOf` err
                            assertBool ("range in: " <> err) $ "-128..255" `isInfixOf` err
            , testCase "long mode still takes a whole word" $ do
                accepted "move.l 100000, D0"
                accepted "move.l 0xFFFFFFFF, D0"
            , testCase "only the immediate is checked, not a register or memory source" $ do
                accepted "move.b D1, D0"
                accepted "move.b (A1), D0"
            ]
        , testCase "Read byte from memory by address register" $ do
            let M68kSt{dataRegs, addrRegs} = simulate "move.b (A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 6
                    (addrRegs !? A2) @?= Just 6
            let M68kSt{dataRegs, addrRegs} =
                    simulate
                        "move.b (A2), D0"
                        st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D0 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x10203006
                    (addrRegs !? A2) @?= Just 6
            let M68kSt{dataRegs, addrRegs} = simulate "move.b -(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (addrRegs !? A2) @?= Just 5
            let M68kSt{dataRegs, addrRegs} = simulate "move.b (A2)+, D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 6
                    (addrRegs !? A2) @?= Just 7
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b 2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 8
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [6, 7, 8, 9] @?= [6, 7, 8, 9]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b -2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 4
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [3, 4, 5, 6] @?= [3, 4, 5, 6]
        , testCase "Write byte from memory by address register" $ do
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    fmap snd (readByte mem 5) @?= Right 5
                    fmap snd (readByte mem 6) @?= Right 2
                    fmap snd (readByte mem 7) @?= Right 7
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D2 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D2) @?= Just 0x10203040
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7] @?= [5, 0x40, 7]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, -(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 5
                    readMemBytes mem [5, 6, 7] @?= [2, 6, 7]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)+" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 7
                    readMemBytes mem [5, 6, 7] @?= [5, 2, 7]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, 2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [6, 7, 8, 9] @?= [6, 7, 2, 9]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.b D2, -2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [2, 3, 4, 5, 6] @?= [2, 3, 2, 5, 6]
        , testCase "Read word from memory by address register" $ do
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l (A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x09080706
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x06, 0x07, 0x08, 0x09, 10]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l -(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x05040302
                    (addrRegs !? A2) @?= Just 2
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 2, 3, 4, 5, 6]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l (A2)+, D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x09080706
                    (addrRegs !? A2) @?= Just 10
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 6, 7, 8, 9, 10]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l 2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x0B0A0908
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [10, 11, 12, 13, 14, 15] @?= [0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l -4(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x05040302
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 2, 3, 4, 5, 6]
        , testCase "Write word to memory by address register" $ do
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x02, 0x00, 0x00, 0x00, 10]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D2 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D2) @?= Just 0x10203040
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x40, 0x30, 0x20, 0x10, 10]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D2, -(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 2
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 0x02, 0x00, 0x00, 0x00, 6]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)+" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 10
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x02, 0x00, 0x00, 0x00, 10]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D2, 2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [7, 8, 9, 10, 11, 12] @?= [7, 0x02, 0x00, 0x00, 0x00, 12]
            let M68kSt{dataRegs, addrRegs, mem} = simulate "move.l D3, -2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D3) @?= Just 3
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [3, 4, 5, 6, 7, 8] @?= [3, 0x03, 0x00, 0x00, 0x00, 8]
        , testCase "Read with index register addressing" $ do
            let M68kSt{dataRegs, addrRegs, mem} =
                    simulate
                        "move.b 2(A2,D1), D0"
                        st0
                            { addrRegs = insert A2 6 addrRegs0
                            , dataRegs = insert D1 3 dataRegs0
                            }
             in do
                    (addrRegs !? A2) @?= Just 6
                    (dataRegs !? D1) @?= Just 3
                    (dataRegs !? D0) @?= Just 11
                    readMemBytes mem [10, 11, 12] @?= [10, 11, 12]
            let M68kSt{dataRegs, addrRegs, mem} =
                    simulate
                        "move.b 2(A2,A1), D0"
                        st0{addrRegs = insert A2 6 $ insert A1 3 addrRegs0}
             in do
                    (addrRegs !? A2) @?= Just 6
                    (addrRegs !? A1) @?= Just 3
                    (dataRegs !? D0) @?= Just 11
                    readMemBytes mem [10, 11, 12] @?= [10, 11, 12]
            let M68kSt{dataRegs, addrRegs, mem} =
                    simulate
                        "move.l 4(A2,D1), D0"
                        st0
                            { addrRegs = insert A2 10 addrRegs0
                            , dataRegs = insert D1 2 dataRegs0
                            }
             in do
                    (addrRegs !? A2) @?= Just 10
                    (dataRegs !? D1) @?= Just 2
                    (dataRegs !? D0) @?= Just 0x13121110
                    readMemBytes mem [15, 16, 17, 18, 19, 20] @?= [15, 0x10, 0x11, 0x12, 0x13, 20]
        , testCase "Write with index register addressing" $ do
            let M68kSt{dataRegs, addrRegs, mem} =
                    simulate
                        "move.b D0, 2(A2,D1)"
                        st0
                            { addrRegs = insert A2 6 addrRegs0
                            , dataRegs = insert D1 3 $ insert D0 0x99 dataRegs0
                            }
             in do
                    (dataRegs !? D0) @?= Just 0x99
                    (addrRegs !? A2) @?= Just 6
                    (dataRegs !? D1) @?= Just 3
                    readMemBytes mem [10, 11, 12] @?= [10, 0x99, 12]
            let M68kSt{dataRegs, addrRegs, mem} =
                    simulate
                        "move.l D0, 4(A2,A1)"
                        st0
                            { addrRegs = insert A2 10 $ insert A1 2 addrRegs0
                            , dataRegs = insert D0 0x7BCDEF12 dataRegs0
                            }
             in do
                    (dataRegs !? D0) @?= Just 0x7BCDEF12
                    (addrRegs !? A2) @?= Just 10
                    (addrRegs !? A1) @?= Just 2
                    readMemBytes mem [15, 16, 17, 18, 19, 20] @?= [15, 0x12, 0xEF, 0xCD, 0x7B, 20]
        , testCase "JSR and RTS operations" $ do
            let M68kSt{pc, addrRegs} = simulate "jsr 0x20" st0{addrRegs = insert A7 0x100 addrRegs0}
             in do
                    pc @?= 0x20
                    (addrRegs !? A7) @?= Just 0xFC

            let M68kSt{pc, addrRegs} =
                    simulate
                        "rts"
                        st0
                            { addrRegs = insert A7 0x0F addrRegs0
                            }
             in do
                    pc @?= 0x1211100F
                    (addrRegs !? A7) @?= Just 0x13
        , testCase "Shift carry flag" $ do
            -- LSR: shift right, last bit shifted out is bit 0
            let M68kSt{cFlag} =
                    simulate "lsr.l D1, D0" st0{dataRegs = insert D1 1 $ insert D0 3 dataRegs0}
             in cFlag @?= True -- 3 = ...11, shifting right 1, bit 0 = 1
            let M68kSt{cFlag} =
                    simulate "lsr.l D1, D0" st0{dataRegs = insert D1 1 $ insert D0 2 dataRegs0}
             in cFlag @?= False -- 2 = ...10, shifting right 1, bit 0 = 0
            -- ASL: shift left, last bit shifted out is bit 31
            let M68kSt{cFlag} =
                    simulate "asl.l D1, D0" st0{dataRegs = insert D1 1 $ insert D0 minBound dataRegs0}
             in cFlag @?= True -- minBound has bit 31 set
        , testCase "Negate operations" $ do
            let M68kSt{dataRegs, nFlag} = simulate "neg.l D0" st0{dataRegs = insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just (-5)
                    nFlag @?= True
            let M68kSt{dataRegs, zFlag} = simulate "neg.l D0" st0{dataRegs = insert D0 0 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    zFlag @?= True
            let M68kSt{dataRegs, nFlag} = simulate "neg.b D0" st0{dataRegs = insert D0 3 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just (-3)
                    nFlag @?= True
        , testCase "Clear operations" $ do
            let M68kSt{dataRegs, zFlag, nFlag, vFlag, cFlag} = simulate "clr.l D0" st0{dataRegs = insert D0 42 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    zFlag @?= True
                    nFlag @?= False
                    vFlag @?= False
                    cFlag @?= False
            let M68kSt{dataRegs, zFlag} = simulate "clr.b D0" st0{dataRegs = insert D0 0xFF dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x00
                    zFlag @?= True
        , testCase "Division by zero" $ do
            let M68kSt{internalError} =
                    simulate "div.l D1, D0" st0{dataRegs = insert D1 0 $ insert D0 10 dataRegs0}
             in internalError @?= Just "division by zero"
            let M68kSt{internalError} =
                    simulate "div.b D1, D0" st0{dataRegs = insert D1 0 $ insert D0 10 dataRegs0}
             in internalError @?= Just "division by zero"
        , testCase "LINK and UNLK operations" $ do
            let M68kSt{addrRegs, mem} =
                    simulate
                        "link A6, -8"
                        st0
                            { addrRegs = insert A6 0x20 $ insert A7 0x10 addrRegs0
                            }
             in do
                    (addrRegs !? A6) @?= Just 0x0C
                    (addrRegs !? A7) @?= Just 0x04
                    fmap snd (readWord mem 0x1C) @?= Right 0x1F1E1D1C
                    fmap snd (readWord mem 0x0C) @?= Right 0x00000020
            let M68kSt{addrRegs, mem} =
                    simulate
                        "unlk A6"
                        st0
                            { addrRegs = insert A6 0x0C $ insert A7 0x14 addrRegs0
                            , mem =
                                either error id $ do
                                    mem1 <- writeWord mem0 0x1C 0x1F1E1D1C
                                    writeWord mem1 0x0C 0x00000020
                            }
             in do
                    (addrRegs !? A6) @?= Just 0x20
                    (addrRegs !? A7) @?= Just 0x10
                    fmap snd (readWord mem 0x1C) @?= Right 0x1F1E1D1C
                    fmap snd (readWord mem 0x10) @?= Right 0x13121110
                    fmap snd (readWord mem 0x20) @?= Right 0x23222120
        ]
    where
        memInit = Mem 256 $ fromList $ map (\a -> (fromEnum a, Value a)) [0 .. 255]
        st0 :: M68kSt Int32
        st0@M68kSt{addrRegs = addrRegs0, dataRegs = dataRegs0, mem = mem0} =
            (initState 256 (mkIoMem (fromList []) memInit) [])
                { dataRegs = fromList $ zip dataRegisters [0 ..]
                }

parsesM68k :: String -> Either String (M68kIsa Int32 (Ref Int32))
parsesM68k = translate

-- | A displacement is checked while parsing and a shift count at deref, so
-- both land in 'Left' here.
fieldOutcome :: String -> IO (Either String (M68kIsa Int32 Int32))
fieldOutcome code =
    case parsesM68k code of
        Left err -> return $ Left err
        Right m -> do
            let m' = derefMnemonic (const (Just 0)) 0 m
            outcome <- try @ErrorCall $ evaluate $ length (show m' :: String)
            return $ case outcome of
                Left (ErrorCallWithLocation msg _location) -> Left msg
                Right _ -> Right m'

accepted :: String -> IO ()
accepted code = do
    outcome <- fieldOutcome code
    case outcome of
        Right _ -> return ()
        Left err -> assertFailure $ code <> " should be accepted, got: " <> err

rejected :: String -> String -> IO ()
rejected code range = do
    outcome <- fieldOutcome code
    case outcome of
        Right m -> assertFailure $ code <> " should be rejected, got: " <> show m
        Left err -> assertBool ("expected range " <> range <> " in: " <> err) $ range `isInfixOf` err

readMemBytes :: (Memory a isa w) => a -> [Int] -> [Word8]
readMemBytes mem addrs = map snd $ rights $ map (readByte mem) addrs

-- | 'Ref' equality ignores the source position, so any position does here.
valueR :: Int32 -> Ref Int32
valueR = ValueR id (initialPos "-")

translate :: (MnemonicParser (isa' w (Ref w)), w ~ Int32) => String -> Either String (isa' w (Ref w))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ errorBundlePretty err
        Right m -> Right m

simulate ::
    ( DerefMnemonic (isa' w) w
    , Machine (M68kSt w) isa w
    , MnemonicParser (isa' w (Ref w))
    , isa ~ isa' w w
    , w ~ Int32
    ) =>
    String
    -> M68kSt w
    -> M68kSt w
simulate code st =
    let instr = either (error . show) (derefMnemonic (error "labels not defined") def) (translate code)
     in execState (instructionExecute 0 instr) st
