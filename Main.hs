{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Text                         hiding ( empty )
import qualified Data.Text                     as T
import           Network.Wai.Handler.Warp                 ( run )
import           Turtle
import           Yesod
import           Prelude                           hiding ( FilePath )
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.ByteString                          ( ByteString )
import           Data.Time.Clock
import           Control.Monad.Logger                     ( LoggingT
                                                          , runStdoutLoggingT
                                                          , runStderrLoggingT
                                                          )
import           UnliftIO.Resource                        ( runResourceT )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Receipt
  name Text
  raw ByteString
  date UTCTime default=CURRENT_TIME
  deriving Show Eq
|]

newtype App = App ConnectionPool

mkYesod "App" [parseRoutes|
  /         RootR     GET
  /receipts ReceiptsR POST
|]

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
    <html lang="en">
      <head>
        <title>Receipt Manager
      <body>
        ^{pageBody pc}
  |]

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Routes

getRootR :: Handler Html
getRootR = defaultLayout [whamlet|
  <div>
    <span>Todo...
|]

postReceiptsR :: Handler Html
postReceiptsR = do
  ((result, widget), enctype) <- runFormPost receiptForm
  case result of
    FormSuccess receiptForm -> do
      receipt <- receiptFormToReceipt receiptForm
      defaultLayout [whamlet|<p>#{show receipt}|]
    FormFailure errors -> do
      defaultLayout [whamlet|<p>errors|]

-- Forms

data ReceiptForm = ReceiptForm
  { name :: Text
  , fileInfo :: FileInfo
  }

receiptForm :: Html -> MForm Handler (FormResult ReceiptForm, Widget)
receiptForm =
  renderDivs
    $   ReceiptForm
    <$> areq textField "Name" Nothing
    <*> areq fileField "File" Nothing

receiptFormToReceipt :: (MonadResource m, MonadIO m) => ReceiptForm -> m Receipt
receiptFormToReceipt (ReceiptForm name info) = do
  bs   <- fileSourceByteString info
  time <- liftIO getCurrentTime
  return $ Receipt name bs time

readPdf :: FilePath -> IO [Text]
readPdf file = do
  dump <- strict $ inproc tesseract args empty
  return $ T.lines dump
 where
  tesseract = "tesseract"
  args      = [format fp file, "stdout", "-l", "eng"]

runApp :: MonadIO m => ConnectionPool -> m ()
runApp pool = liftIO $ warp 3000 $ App pool

main :: IO ()
main = runResourceT $ runStderrLoggingT $ withSqlitePool dbName
                                                         connectionCount
                                                         runApp
 where
  dbName          = "test.db"
  connectionCount = 10
