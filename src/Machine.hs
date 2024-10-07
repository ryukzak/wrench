{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Machine (journal, powerOn) where

import Control.Monad.Writer
import Machine.Types
import Relude
import Relude.Extra

journal :: (Machine st isa w) => Int -> HashMap Int String -> st -> [Trace st isa]
journal limit pc2label st = execWriter $ journal' limit pc2label st

journal' 0 _ _ = tell [TWarn "Instruction limit reached"]
journal' limit pc2label st
    | Just (pc, instruction) <- evalState instructionFetch st = do
        tell [TInstruction pc (pc2label !? pc) instruction]
        let st' = execState instructionStep st
        tell [TState st']
        journal' (limit - 1) pc2label st'
    | otherwise = return ()

powerOn ::
    (Machine st isa w, MachineWord w) =>
    Int
    -> HashMap String w
    -> st
    -> Either Text [Trace st isa]
powerOn limit labels st = do
    let pc2label = fromList $ map (\(a, b) -> (fromEnum b, a)) $ toPairs labels
    Right $ journal limit pc2label st
