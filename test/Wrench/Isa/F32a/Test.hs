module Wrench.Isa.F32a.Test (tests) where

import Data.Default
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Wrench.Isa.F32a
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser.Types (MnemonicParser (..))
import Wrench.Translator.Types (DerefMnemonic (..), Ref (..))

tests :: TestTree
tests =
    testGroup
        "ISA"
        [ testCase "Over copies second element to top" $ do
            let State{dataStack} = simulate "over" st0{dataStack = [10, 20, 30]}
             in dataStack @?= [20, 10, 20, 30]
        , testCase "Over with two elements" $ do
            let State{dataStack} = simulate "over" st0{dataStack = [1, 2]}
             in dataStack @?= [2, 1, 2]
        , testCase "Dup duplicates top" $ do
            let State{dataStack} = simulate "dup" st0{dataStack = [42, 7]}
             in dataStack @?= [42, 42, 7]
        , testCase "Drop removes top" $ do
            let State{dataStack} = simulate "drop" st0{dataStack = [1, 2, 3]}
             in dataStack @?= [2, 3]
        ]
    where
        memInit = Mem 256 $ fromList $ map (\a -> (fromEnum a, Value a)) [0 .. 255]
        st0 :: F32aState Int32
        st0 =
            initState 256 (mkIoMem (fromList []) memInit) []

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
