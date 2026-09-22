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
            let F32aSt{dataStack} = simulate "over" st0{dataStack = [10, 20, 30]}
             in dataStack @?= [20, 10, 20, 30]
        , testCase "Over with two elements" $ do
            let F32aSt{dataStack} = simulate "over" st0{dataStack = [1, 2]}
             in dataStack @?= [2, 1, 2]
        , testCase "Dup duplicates top" $ do
            let F32aSt{dataStack} = simulate "dup" st0{dataStack = [42, 7]}
             in dataStack @?= [42, 42, 7]
        , testCase "Drop removes top" $ do
            let F32aSt{dataStack} = simulate "drop" st0{dataStack = [1, 2, 3]}
             in dataStack @?= [2, 3]
        , testCase "Bare decimal literal pushes to stack" $ do
            let F32aSt{dataStack} = simulate "42" st0
             in dataStack @?= [42]
        , testCase "Bare hex literal pushes to stack" $ do
            let F32aSt{dataStack} = simulate "0xFF" st0
             in dataStack @?= [255]
        , testCase "Bare negative literal pushes to stack" $ do
            let F32aSt{dataStack} = simulate "-1" st0
             in dataStack @?= [-1]
        , testCase "Bare char literal pushes to stack" $ do
            let F32aSt{dataStack} = simulate "'A'" st0
             in dataStack @?= [65]
        , testCase "lit keyword still works" $ do
            let F32aSt{dataStack} = simulate "lit 42" st0
             in dataStack @?= [42]
        ]
    where
        memInit = Mem 256 $ fromList $ map (\a -> (fromEnum a, Value a)) [0 .. 255]
        st0 :: F32aSt Int32
        st0 =
            initState 256 (mkIoMem (fromList []) memInit) []

translate :: (MnemonicParser (isa' w (Ref w)), w ~ Int32) => String -> Either String (isa' w (Ref w))
translate code =
    case parse mnemonic "-" (code <> "\n") of
        Left err -> Left $ errorBundlePretty err
        Right m -> Right m

simulate ::
    ( DerefMnemonic (isa' w) w
    , Machine (F32aSt w) isa w
    , MnemonicParser (isa' w (Ref w))
    , isa ~ isa' w w
    , w ~ Int32
    ) =>
    String
    -> F32aSt w
    -> F32aSt w
simulate code st =
    let instr = either (error . show) (derefMnemonic (error "labels not defined") def) (translate code)
     in execState (instructionExecute 0 instr) st
