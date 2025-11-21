module Wrench.Isa.M68k.Test (tests) where

import Data.Default
import Relude
import Relude.Extra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
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
            let State{dataRegs} = simulate "add.b D1, D0" st0{dataRegs = insert D1 3 $ insert D0 4 dataRegs0}
             in (dataRegs !? D0) @?= Just 7
            let State{dataRegs, zFlag} = simulate "add.b D1, D0" st0{dataRegs = insert D1 0xFF $ insert D0 1 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    zFlag @?= True
            let State{dataRegs} = simulate "sub.b D1, D0" st0{dataRegs = insert D1 2 $ insert D0 7 dataRegs0}
             in (dataRegs !? D0) @?= Just 5
            let State{dataRegs, nFlag} = simulate "sub.b D1, D0" st0{dataRegs = insert D1 10 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just (-5)
                    nFlag @?= True
            let State{dataRegs} = simulate "mul.b D1, D0" st0{dataRegs = insert D1 3 $ insert D0 4 dataRegs0}
             in (dataRegs !? D0) @?= Just 12
            let State{dataRegs} = simulate "mul.b D1, D0" st0{dataRegs = insert D1 10 $ insert D0 10 dataRegs0}
             in (dataRegs !? D0) @?= Just 100
        , testCase "Compare operations" $ do
            let State{dataRegs, zFlag, nFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 5 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 5
                    zFlag @?= True
                    nFlag @?= False
            let State{dataRegs, zFlag, nFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 10 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 10
                    zFlag @?= False
                    nFlag @?= True
            let State{dataRegs, zFlag, nFlag} =
                    simulate "cmp.l D1, D0" st0{dataRegs = insert D1 3 $ insert D0 5 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (dataRegs !? D1) @?= Just 3
                    zFlag @?= False
                    nFlag @?= False
            let State{dataRegs, zFlag} =
                    simulate "cmp.b D1, D0" st0{dataRegs = insert D1 0x100 $ insert D0 0 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0
                    (dataRegs !? D1) @?= Just 0x100
                    zFlag @?= True
        , testCase "Translator" $ do
            translate "movea.l D0, A0" @?= Right (MoveA Long (DirectDataReg D0) (DirectAddrReg A0))
            translate "move.l 12, D0" @?= Right (Move Long (Immediate $ ValueR id 12) (DirectDataReg D0))
            translate "move.l 8(A2), D0" @?= Right (Move Long (IndirectAddrReg 8 A2 Nothing) (DirectDataReg D0))
            translate "move.l -8(A2), D0" @?= Right (Move Long (IndirectAddrReg (-8) A2 Nothing) (DirectDataReg D0))
            translate "move.l -8(A2,D1), D0"
                @?= Right (Move Long (IndirectAddrReg (-8) A2 (Just $ DataIndex D1)) (DirectDataReg D0))
            translate "move.l -8(A2,A1), D0"
                @?= Right (Move Long (IndirectAddrReg (-8) A2 (Just $ AddrIndex A1)) (DirectDataReg D0))
            translate "cmp.l D1, D0" @?= Right (Cmp Long (DirectDataReg D1) (DirectDataReg D0))
            translate "cmp.b 10, D0" @?= Right (Cmp Byte (Immediate $ ValueR id 10) (DirectDataReg D0))
            translate "cmp.l (A1), D0" @?= Right (Cmp Long (IndirectAddrReg 0 A1 Nothing) (DirectDataReg D0))
            translate "jsr 0x20" @?= Right (Jsr (ValueR id 0x20))
            translate "rts" @?= Right Rts
        , testCase "Read byte from memory by address register" $ do
            let State{dataRegs, addrRegs} = simulate "move.b (A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 6
                    (addrRegs !? A2) @?= Just 6
            let State{dataRegs, addrRegs} =
                    simulate
                        "move.b (A2), D0"
                        st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D0 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x10203006
                    (addrRegs !? A2) @?= Just 6
            let State{dataRegs, addrRegs} = simulate "move.b -(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 5
                    (addrRegs !? A2) @?= Just 5
            let State{dataRegs, addrRegs} = simulate "move.b (A2)+, D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 6
                    (addrRegs !? A2) @?= Just 7
            let State{dataRegs, addrRegs, mem} = simulate "move.b 2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 8
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [6, 7, 8, 9] @?= [6, 7, 8, 9]
            let State{dataRegs, addrRegs, mem} = simulate "move.b -2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 4
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [3, 4, 5, 6] @?= [3, 4, 5, 6]
        , testCase "Write byte from memory by address register" $ do
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    fmap snd (readByte mem 5) @?= Right 5
                    fmap snd (readByte mem 6) @?= Right 2
                    fmap snd (readByte mem 7) @?= Right 7
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D2 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D2) @?= Just 0x10203040
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7] @?= [5, 0x40, 7]
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, -(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 5
                    readMemBytes mem [5, 6, 7] @?= [2, 6, 7]
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, (A2)+" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 7
                    readMemBytes mem [5, 6, 7] @?= [5, 2, 7]
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, 2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [6, 7, 8, 9] @?= [6, 7, 2, 9]
            let State{dataRegs, addrRegs, mem} = simulate "move.b D2, -2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [2, 3, 4, 5, 6] @?= [2, 3, 2, 5, 6]
        , testCase "Read word from memory by address register" $ do
            let State{dataRegs, addrRegs, mem} = simulate "move.l (A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x09080706
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x06, 0x07, 0x08, 0x09, 10]
            let State{dataRegs, addrRegs, mem} = simulate "move.l -(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x05040302
                    (addrRegs !? A2) @?= Just 2
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 2, 3, 4, 5, 6]
            let State{dataRegs, addrRegs, mem} = simulate "move.l (A2)+, D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x09080706
                    (addrRegs !? A2) @?= Just 10
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 6, 7, 8, 9, 10]
            let State{dataRegs, addrRegs, mem} = simulate "move.l 2(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x0B0A0908
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [10, 11, 12, 13, 14, 15] @?= [0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F]
            let State{dataRegs, addrRegs, mem} = simulate "move.l -4(A2), D0" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D0) @?= Just 0x05040302
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 2, 3, 4, 5, 6]
        , testCase "Write word to memory by address register" $ do
            let State{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x02, 0x00, 0x00, 0x00, 10]
            let State{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)" st0{addrRegs = insert A2 6 addrRegs0, dataRegs = insert D2 0x10203040 dataRegs0}
             in do
                    (dataRegs !? D2) @?= Just 0x10203040
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x40, 0x30, 0x20, 0x10, 10]
            let State{dataRegs, addrRegs, mem} = simulate "move.l D2, -(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 2
                    readMemBytes mem [1, 2, 3, 4, 5, 6] @?= [1, 0x02, 0x00, 0x00, 0x00, 6]
            let State{dataRegs, addrRegs, mem} = simulate "move.l D2, (A2)+" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 10
                    readMemBytes mem [5, 6, 7, 8, 9, 10] @?= [5, 0x02, 0x00, 0x00, 0x00, 10]
            let State{dataRegs, addrRegs, mem} = simulate "move.l D2, 2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D2) @?= Just 2
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [7, 8, 9, 10, 11, 12] @?= [7, 0x02, 0x00, 0x00, 0x00, 12]
            let State{dataRegs, addrRegs, mem} = simulate "move.l D3, -2(A2)" st0{addrRegs = insert A2 6 addrRegs0}
             in do
                    (dataRegs !? D3) @?= Just 3
                    (addrRegs !? A2) @?= Just 6
                    readMemBytes mem [3, 4, 5, 6, 7, 8] @?= [3, 0x03, 0x00, 0x00, 0x00, 8]
        , testCase "Read with index register addressing" $ do
            let State{dataRegs, addrRegs, mem} =
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
            let State{dataRegs, addrRegs, mem} =
                    simulate
                        "move.b 2(A2,A1), D0"
                        st0{addrRegs = insert A2 6 $ insert A1 3 addrRegs0}
             in do
                    (addrRegs !? A2) @?= Just 6
                    (addrRegs !? A1) @?= Just 3
                    (dataRegs !? D0) @?= Just 11
                    readMemBytes mem [10, 11, 12] @?= [10, 11, 12]
            let State{dataRegs, addrRegs, mem} =
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
            let State{dataRegs, addrRegs, mem} =
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
            let State{dataRegs, addrRegs, mem} =
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
            let State{pc, addrRegs} = simulate "jsr 0x20" st0{addrRegs = insert A7 0x100 addrRegs0}
             in do
                    pc @?= 0x20
                    (addrRegs !? A7) @?= Just 0xFC

            let State{pc, addrRegs} =
                    simulate
                        "rts"
                        st0
                            { addrRegs = insert A7 0x0F addrRegs0
                            }
             in do
                    pc @?= 0x1211100F
                    (addrRegs !? A7) @?= Just 0x13
        , testCase "LINK and UNLK operations" $ do
            let State{addrRegs, mem} =
                    simulate
                        "link A6, -8"
                        st0
                            { addrRegs = insert A6 0x20 $ insert A7 0x10 addrRegs0
                            }
             in do
                    (addrRegs !? A6) @?= Just 0x0C
                    (addrRegs !? A7) @?= Just 0x14
                    fmap snd (readWord mem 0x1C) @?= Right 0x1F1E1D1C
                    fmap snd (readWord mem 0x0C) @?= Right 0x00000020
            let State{addrRegs, mem} =
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
        st0 :: M68kState Int32
        st0@State{addrRegs = addrRegs0, dataRegs = dataRegs0, mem = mem0} =
            (initState 256 $ mkIoMem (fromList []) memInit Nothing)
                { dataRegs = fromList $ zip dataRegisters [0 ..]
                }

readMemBytes :: (Memory a isa w) => a -> [Int] -> [Word8]
readMemBytes mem addrs = map snd $ rights $ map (readByte mem) addrs

translate :: (MnemonicParser (isa' w (Ref w)), w ~ Int32) => String -> Either String (isa' w (Ref w))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ errorBundlePretty err
        Right m -> Right m

simulate ::
    ( DerefMnemonic (isa' w) w
    , Machine (st (IoMem isa w) w) isa w
    , MnemonicParser (isa' w (Ref w))
    , isa ~ isa' w w
    , w ~ Int32
    ) =>
    String
    -> st (IoMem isa w) w
    -> st (IoMem isa w) w
simulate code st =
    let instr = either (error . show) (derefMnemonic (error "labels not defined") def) (translate code)
     in execState (instructionExecute 0 instr) st
