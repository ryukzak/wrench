{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Wrench.Machine.BitPack.Test (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Wrench.Machine.BitPack

data Color = Red | Green | Blue
    deriving (Bounded, Enum, Eq, Show)

instance BitPack Color where
    unpack = getEnum 2 <?> "color"
    pack color = putEnum 2 color <?> "color"

data Pixel = Pixel {pxColor :: Color, pxLevel :: Int}
    deriving (Eq, Show)

instance BitPack Pixel where
    unpack = word $ do
        pxColor <- unpack
        pxLevel <- getField 30
        return Pixel{pxColor, pxLevel}
    pack Pixel{pxColor, pxLevel} = word $ do
        pack pxColor
        putField 30 pxLevel <?> "level"

tests :: TestTree
tests =
    testGroup
        "Machine.BitPack"
        [ testCase "Fields pack most significant bit first and may cross a word" $ do
            let encode = do
                    putField 4 0xA
                    putField 8 0xBC
                    putField 20 0xDEF01
                    putField 32 0x12345678
                decode = do
                    a <- getField 4
                    b <- getField 8
                    c <- getField 20
                    d <- getField 32
                    return (a, b, c, d)
            runBitPut encode @?= Right [0xABCDEF01 :: Int32, 0x12345678]
            decodeList [0xABCDEF01, 0x12345678] decode @?= Right (0xA, 0xBC, 0xDEF01, 0x12345678)
        , testCase "A field spanning two words reads both" $ do
            let encode = do
                    putPad 28
                    putField 8 0xFF
                    putPad 28
                decode = do
                    getPad 28
                    getField 8
            runBitPut encode @?= Right [0x0000000F :: Int32, 0xF0000000]
            decodeList [0x0000000F, 0xF0000000] decode @?= Right 0xFF
        , testCase "A 42-bit field is read from and written to two words" $ do
            let encode = do
                    putField 3 0b101
                    putField 42 0x2ABCDEF0123
                    putPad 19
                decode = do
                    getPad 3
                    getField 42
            runBitPut encode @?= Right [0xB55E6F78 :: Int32, 0x09180000]
            decodeList [0xB55E6F78, 0x09180000] decode @?= Right 0x2ABCDEF0123
        , testCase "Word layouts group the fields of one word each" $ do
            let encode = do
                    word $ do
                        putField 4 0xA
                        putField 28 0xBCDEF01
                    word $ do
                        putField 16 0x1234
                        putField 16 0x5678
                decode = do
                    (a, b) <- word $ do
                        a <- getField 4
                        b <- getField 28
                        return (a, b)
                    (c, d) <- word $ do
                        c <- getField 16
                        d <- getField 16
                        return (c, d)
                    return (a, b, c, d)
            runBitPut encode @?= Right [0xABCDEF01 :: Int32, 0x12345678]
            decodeList [0xABCDEF01, 0x12345678] decode @?= Right (0xA, 0xBCDEF01, 0x1234, 0x5678)
        , testCase "A word layout must fill exactly one word from a word boundary" $ do
            let misaligned = do
                    putPad 1
                    word $ putPad 32
                    putPad 31
                misalignedGet = do
                    getPad 1
                    word $ getPad 32
            runBitPut @Int32 (word $ putPad 31) @?= Left "word layout doesn't fill exactly one 32-bit word"
            runBitPut @Int32 (word $ putPad 64) @?= Left "word layout doesn't fill exactly one 32-bit word"
            runBitPut @Int32 misaligned @?= Left "word layout doesn't start on a word boundary"
            decodeList [0, 0] (word $ getPad 31) @?= Left "word layout doesn't fill exactly one 32-bit word"
            decodeList [0, 0] (word $ getPad 33) @?= Left "word layout doesn't fill exactly one 32-bit word"
            decodeList [0, 0] misalignedGet @?= Left "word layout doesn't start on a word boundary"
        , testCase "A value that doesn't fit its field is rejected" $ do
            let encode x = do
                    putField 4 x
                    putPad 28
            runBitPut @Int32 (encode 16) @?= Left "16 doesn't fit in 4 bits"
            runBitPut @Int32 (encode (-1)) @?= Left "-1 doesn't fit in 4 bits"
        , testCase "Labels name where a layout failed, outermost first" $ do
            let encode = label "record" $ do
                    putField 4 16 <?> "x"
                    putPad 28
            runBitPut @Int32 encode @?= Left "record: x: 16 doesn't fit in 4 bits"
            decodeList [] (label "record" $ getField 1 <?> "x") @?= Left "record: x: out of words"
        , testCase "Padding may be any width; a field at most 63 bits" $ do
            runBitPut (putPad 96) @?= Right [0, 0, 0 :: Int32]
            decodeList [0, 0, 0] (getPad 96) @?= Right ()
            runBitPut @Int32 (putField 64 0) @?= Left "field width 64 is outside 0..63"
            decodeList [0, 0] (getField 64) @?= Left "field width 64 is outside 0..63"
        , testCase "Fields must fill whole words" $
            runBitPut @Int32 (putPad 31) @?= Left "31 bits don't fill whole 32-bit words"
        , testCase "An enum field rejects a value no constructor has" $ do
            let encode width = do
                    putEnum width Blue <?> "color"
                    putPad (32 - width)
            runBitPut (encode 2) @?= Right [0x80000000 :: Int32]
            runBitPut @Int32 (encode 1) @?= Left "color: 2 doesn't fit in 1 bits"
            decodeList [0x80000000] (getEnum @Color 2) @?= Right Blue
            decodeList [0xC0000000] (getEnum @Color 2 <?> "color") @?= Left "color: unknown tag 3"
        , testCase "A decoder fetches only the words it reaches" $ do
            let fetched decoder = reverse $ execState (runBitGetWith fetch decoder) []
                fetch k = do
                    modify (k :)
                    return (0 :: Int32)
                secondWord = do
                    getPad 32
                    getField 32
                tagged = do
                    tag <- getField 2
                    if tag == 0 then return 0 else getField 32
            fetched (getField 2) @?= [0]
            fetched (getField 32) @?= [0]
            fetched (getField 33) @?= [0, 1]
            fetched secondWord @?= [0, 1]
            fetched tagged @?= [0]
        , testCase "A BitPack instance composes inside another's layout" $ do
            let pixel = Pixel{pxColor = Blue, pxLevel = 0x5A5}
            packWords @Int32 pixel @?= Right [0x800005A5]
            unpackWords [0x800005A5 :: Int32] @?= Right pixel
            unpackWords @Pixel [0xC0000000 :: Int32] @?= Left "color: unknown tag 3"
            packWords @Int32 pixel{pxLevel = 2 ^ (30 :: Int)} @?= Left "level: 1073741824 doesn't fit in 30 bits"
        , testCase "unpackWords rejects words left over" $
            unpackWords @Pixel [0x800005A5 :: Int32, 0] @?= Left "words left over after decoding: 1"
        , testCase "A list decoder fails past the end of its words" $ do
            runBitGet @Int32 (getField 33) [0] @?= Left "out of words"
            runBitGet @Int32 (getField 1) [] @?= Left "out of words"
        ]

decodeList :: [Int32] -> BitGet Int32 a -> Either Text a
decodeList = flip runBitGet
