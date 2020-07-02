{-# LANGUAGE OverloadedStrings #-}

module OCR where

import           Prelude                           hiding ( lines )
import           Turtle
import qualified Turtle.Bytes                  as TB
import           Data.Text                                ( Text
                                                          , lines
                                                          )
import           Data.Text.Encoding                       ( decodeUtf8 )
import           Data.ByteString                          ( ByteString )

extractTextFromImage :: ByteString -> IO [Text]
extractTextFromImage bs = do
  dump <- TB.strict $ TB.inproc tesseract args input
  return . lines . decodeUtf8 $ dump
 where
  input     = pure bs
  tesseract = "tesseract"
  args      = ["stdin", "stdout", "-l", "eng"]
