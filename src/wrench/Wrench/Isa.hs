{- | Which of the simulated architectures a source file is written for, and how
to guess it when the user did not say.
-}
module Wrench.Isa (
    Isa (..),
    allIsa,
    isaName,
    detectIsa,
) where

import Prelude (Read (..))
import Relude
import Text.Megaparsec (parse)
import Wrench.Isa.Acc32 (Acc32Isa)
import Wrench.Isa.F32a (F32aIsa)
import Wrench.Isa.M68k (M68kIsa)
import Wrench.Isa.RiscIv (RiscIvIsa)
import Wrench.Isa.VliwIv (VliwIvIsa)
import Wrench.Translator.Parser (asmParser)
import Wrench.Translator.Parser.Types (MnemonicParser)
import Wrench.Translator.Types (Ref)

data Isa
    = Acc32
    | F32a
    | RiscIv
    | M68k
    | VliwIv
    deriving (Bounded, Enum, Eq, Show)

allIsa :: [Isa]
allIsa = [minBound .. maxBound]

-- | The name the @--isa@ option and the example directories use.
isaName :: Isa -> Text
isaName Acc32 = "acc32"
isaName F32a = "f32a"
isaName RiscIv = "risc-iv-32"
isaName M68k = "m68k"
isaName VliwIv = "vliw-iv"

instance Read Isa where
    readsPrec _ "risc-iv" = [(RiscIv, "")] -- historical spelling of risc-iv-32
    readsPrec _ s =
        [ (isa, "")
        | isa <- allIsa
        , toString (isaName isa) == s
        ]

-- * Detection

detectIsa :: Text -> Either Text Isa
detectIsa src = case filter (parsesAs src) allIsa of
    [isa] -> Right isa
    [] -> Left "no isa can parse the source"
    several -> Left $ "source parses as " <> show several

parsesAs :: Text -> Isa -> Bool
parsesAs src = \case
    Acc32 -> parses @(Acc32Isa Int32 (Ref Int32))
    F32a -> parses @(F32aIsa Int32 (Ref Int32))
    RiscIv -> parses @(RiscIvIsa Int32 (Ref Int32))
    M68k -> parses @(M68kIsa Int32 (Ref Int32))
    VliwIv -> parses @(VliwIvIsa Int32 (Ref Int32))
    where
        parses :: forall isa. (MnemonicParser isa) => Bool
        parses = isRight $ parse (asmParser @isa @Int32) "" (toString src <> "\n")
