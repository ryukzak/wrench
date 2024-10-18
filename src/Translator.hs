{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Translator (
    translate,
    TranslatorResult (..),
) where

import Machine.Memory
import Machine.Types
import Relude
import Relude.Extra
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Translator.Parser
import Translator.Parser.Types
import Translator.Types

data TranslatorResult mem w = TranslatorResult
    { dump :: !mem
    , labels :: !(HashMap String w)
    }
    deriving (Show)

data St w
    = St
    { sOffset :: !w
    , sLabels :: !(HashMap String w)
    }
    deriving (Show)

evaluateLabels ::
    (ByteLength isa, MachineWord w) =>
    [Section isa w String]
    -> HashMap String w
evaluateLabels sections =
    sLabels
        $ foldl'
            ( \st -> \case
                Code{codeTokens} ->
                    foldl'
                        ( \st'@St{sOffset, sLabels} token ->
                            case token of
                                Mnemonic m -> st'{sOffset = sOffset + toEnum (byteLength m)}
                                Label l -> st'{sLabels = insert l sOffset sLabels}
                                Comment _ -> st'
                        )
                        st
                        codeTokens
                Data{dataTokens} ->
                    foldl'
                        ( \st'@St{sOffset, sLabels} DataToken{dtLabel, dtValue} ->
                            st'
                                { sOffset = sOffset + toEnum (byteLength dtValue)
                                , sLabels = insert dtLabel sOffset sLabels
                                }
                        )
                        st
                        dataTokens
            )
            St{sOffset = 0, sLabels = fromList []}
            sections

translate ::
    forall isa_ w.
    ( MnemonicParser (isa_ w (Ref w))
    , ByteLength (isa_ w (Ref w))
    , ByteLength (isa_ w w)
    , MachineWord w
    , DerefMnemonic (isa_ w) w
    ) =>
    Maybe Int
    -> FilePath
    -> String
    -> Either Text (TranslatorResult (Mem (isa_ w w) w) w)
translate memorySize fn src =
    case parse asmParser fn src of
        Right (sections :: [Section isa1 w String]) ->
            let labels = evaluateLabels sections
                resolveLabel l = (labels !? l)
                code = map (uncurry (derefSection resolveLabel)) (markupOffsets 0 sections)
                dump = prepareDump memorySize code
             in Right $ TranslatorResult dump labels
        Left err -> Left $ toText $ errorBundlePretty err
