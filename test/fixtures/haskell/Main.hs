{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode, decode)
import QuickType
import System.Environment
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    content <- BS.readFile filePath
    BS.putStr content
    let dec = decode content :: Maybe TopLevel
    BS.putStr $ encode dec
