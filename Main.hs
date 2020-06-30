{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)

main :: IO ()
main = readPdf "./assets/receipt.png" >>= print

readPdf :: FilePath -> IO [Text]
readPdf file = do
  dump <- strict $ inproc tesseract args empty
  return $ T.lines dump
  where
    tesseract = "tesseract"
    args = [format fp file, "stdout", "-l", "eng"]
