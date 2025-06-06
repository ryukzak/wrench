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
    , labels :: !(HashMap String w)
    }
    deriving (Show)

data St w
    = St
    { sOffset :: !w
    , sLabels :: ![(String, w)]
    }
    deriving (Show)

evaluateLabels ::
    (ByteSize isa, MachineWord w) =>
    [Section isa w String]
    -> Either String (HashMap String w)
evaluateLabels sections =
    let processCode st'@St{sOffset, sLabels} token =
            case token of
                Mnemonic m -> st'{sOffset = sOffset + toEnum (byteSize m)}
                Label l -> st'{sLabels = (l, sOffset) : sLabels}
        processData st'@St{sOffset, sLabels} DataToken{dtLabel, dtValue} =
            st'
                { sOffset = sOffset + toEnum (byteSize dtValue)
                , sLabels = (dtLabel, sOffset) : sLabels
                }
        offsetError org offset = error $ ".org directive set " <> show org <> " but we already at " <> show offset
        St{sLabels = labels} =
            foldl'
                ( \st@St{sOffset} -> \case
                    Code{org = Nothing, codeTokens} -> foldl' processCode st codeTokens
                    Data{org = Nothing, dataTokens} -> foldl' processData st dataTokens
                    Code{org = Just offset, codeTokens}
                        | toEnum offset < sOffset -> offsetError offset sOffset
                        | otherwise -> foldl' processCode st{sOffset = toEnum offset} codeTokens
                    Data{org = Just offset, dataTokens}
                        | toEnum offset < sOffset -> offsetError offset sOffset
                        | otherwise -> foldl' processData st{sOffset = toEnum offset} dataTokens
                )
                St{sOffset = 0, sLabels = []}
                sections
        collect [] dict = Right dict
        collect ((n, v) : ls) dict
            | n `member` dict = Left $ "Duplicate label: " <> n
            | otherwise = collect ls (insert n v dict)
     in collect labels (fromList [] :: HashMap String w)

translate ::
    forall isa_ w.
    ( ByteSize (isa_ w (Ref w))
    , ByteSize (isa_ w w)
    , DerefMnemonic (isa_ w) w
    , MachineWord w
    , MnemonicParser (isa_ w (Ref w))
    ) =>
    Int
    -> FilePath
    -> String
    -> Either Text (TranslatorResult (Mem (isa_ w w) w) w)
translate memorySize fn src =
    case parse asmParser fn src of
        Right sections ->
            case evaluateLabels sections of
                Left err -> Left $ toText err
                (Right labels) ->
                    let resolveLabel l = (labels !? l)
                        code = map (uncurry (derefSection resolveLabel)) (markupSectionOffsets 0 sections)
                        dump = prepareDump memorySize code
                     in Right $ TranslatorResult dump labels
        Left err -> Left $ toText $ errorBundlePretty err
