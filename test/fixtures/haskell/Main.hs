module Main where

import QuickType
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    content <- readFile filePath
    putStr content
