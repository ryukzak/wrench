{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Wrench.Translator (
    translate,
    TranslatorResult (..),
) where

import Relude
import Relude.Extra
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Wrench.Machine.Memory
import Wrench.Machine.Types
import Wrench.Translator.Parser
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

data TranslatorResult mem w = TranslatorResult
    { dump :: !mem
    , labels :: !(HashMap Text w)
    , dumpStats :: !DumpStats
    }
    deriving (Show)

data St w
    = St
    { sOffset :: !w
    , sLabels :: ![(Text, w)]
    }
    deriving (Show)

evaluateLabels ::
    (ByteSize isa, IsWord w) =>
    [Section isa w Text]
    -> Either Text (HashMap Text w)
evaluateLabels sections =
    let markCodeToken st'@St{sOffset, sLabels} token =
            case token of
                Mnemonic m -> st'{sOffset = sOffset + toEnum (byteSize m)}
                Label l -> st'{sLabels = (l, sOffset) : sLabels}
        markDataToken st'@St{sOffset, sLabels} DataToken{dtLabel, dtValue} =
            st'
                { sOffset = sOffset + toEnum (byteSize dtValue)
                , sLabels = (dtLabel, sOffset) : sLabels
                }
        St{sLabels = labels} =
            foldl'
                ( \st@St{sOffset} -> \case
                    Code{org, codeTokens} ->
                        foldl' markCodeToken st{sOffset = maybe sOffset toEnum org} codeTokens
                    Data{org, dataTokens} ->
                        foldl' markDataToken st{sOffset = maybe sOffset toEnum org} dataTokens
                )
                St{sOffset = 0, sLabels = []}
                sections
        collect [] dict = Right dict
        collect ((n, v) : ls) dict
            | n `member` dict = Left $ "Duplicate label: " <> n
            | otherwise = collect ls (insert n v dict)
     in collect labels (fromList [] :: HashMap Text w)

translate ::
    forall isa_ w.
    ( ByteSize (isa_ w (Ref w))
    , ByteSize (isa_ w w)
    , DerefMnemonic (isa_ w) w
    , IsWord w
    , MnemonicParser (isa_ w (Ref w))
    , Show (isa_ w w)
    ) =>
    Int
    -> [Word8]
    -> FilePath
    -> String
    -> Either Text (TranslatorResult (Mem (isa_ w w) w) w)
translate memorySize fillBytes fn src =
    case parse asmParser fn src of
        Right sections ->
            case evaluateLabels sections of
                Left err -> Left err
                (Right labels) ->
                    let resolveLabel l = (labels !? l)
                     in do
                            code <- mapM (uncurry (derefSection resolveLabel)) (markupSectionOffsets 0 sections)
                            dump <- prepareDump memorySize fillBytes code
                            Right $ TranslatorResult dump labels (computeDumpStats code)
        Left err -> Left $ toText $ errorBundlePretty err
