module Main (main) where

import Lib

main :: IO ()
main = do
    src <- readFile "test/fib.asm"
    mapM_ putStrLn $ someFunc "test/fib.asm" src
