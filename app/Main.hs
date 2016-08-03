module Main where

import PoE.Parser

main :: IO ()
main = do
    s <- getContents
    putStrLn $ parseInput s
