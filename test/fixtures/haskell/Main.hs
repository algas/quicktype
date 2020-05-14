module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    content <- readFile filePath
    putStr content
