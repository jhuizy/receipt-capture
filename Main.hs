{-# LANGUAGE ViewPatterns #-}
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
import qualified Data.Text.Encoding            as T
import qualified Data.Text                     as T
import           Network.Wai.Handler.Warp                 ( run )
import           Turtle hiding (select, limit)
import qualified Turtle.Bytes                  as TB
import           Yesod hiding ((==.))
import           Prelude                           hiding ( FilePath )
import           Database.Persist hiding ((==.))
import           Database.Persist.Sqlite hiding ((==.))
import           Data.ByteString                          ( ByteString )

import           Data.Time.Clock
import           Control.Monad.Logger                     ( LoggingT
                                                          , runStdoutLoggingT
                                                          , runStderrLoggingT
                                                          )
import           UnliftIO.Resource                        ( runResourceT )
import           Control.Monad.Reader                     ( ReaderT(runReaderT)
                                                          )
import           Data.ByteString.Base64                   ( encode )
import           GHC.IO.Encoding                          ( setLocaleEncoding
                                                          , utf8
                                                          )
import Database.Esqueleto

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Receipt
  name Text
  raw ByteString
  date UTCTime default=CURRENT_TIME
  deriving Show Eq
ReceiptInfo
  receipt ReceiptId
  info [Text]
  deriving Show Eq
|]

newtype App = App ConnectionPool

mkYesod "App" [parseRoutes|
  /                         RootR         GET
  /receipts                 ReceiptsR     GET POST
  !/receipts/new            ReceiptsNewR  GET
  !/receipts/#ReceiptId     ReceiptsShowR GET
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
    <span>List of receipts
|]

getReceiptsNewR :: Handler Html
getReceiptsNewR = do
  (widget, enctype) <- generateFormPost receiptForm
  defaultLayout [whamlet|
    <div>
      <p>Enter a new receipt
      <form method=post action=@{ReceiptsR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
  |]


getReceiptsR :: Handler Html
getReceiptsR = do
  receipts <- runDB $ selectList [] [Desc ReceiptDate]
  defaultLayout [whamlet|
    <div>
      $forall Entity id (Receipt name raw date) <- receipts
        <li><a href=@{ReceiptsShowR id}>#{show name}
  |]

getReceiptsShowR :: ReceiptId -> Handler Html
getReceiptsShowR receiptId = do
  results <- runDB $ select $ from $ \(r, ri) -> do
    where_ ((r ^. ReceiptId ==. ri ^. ReceiptInfoReceipt) &&. (r ^. ReceiptId ==. val receiptId))
    limit 1
    return (r, ri)
  case results of
    [((Entity _ receipt), (Entity _ (ReceiptInfo _ info)))] -> do
      renderReceipt receipt info
    _ -> notFound

 where
  base64png raw = T.decodeUtf8 . encode $ raw

  renderReceipt (Receipt name raw date) infoList = defaultLayout [whamlet|
        <div>
          <h1>#{name}
          <span>Uploaded: #{formatRFC822 date}
          <img src="data:image/png;base64,#{base64png raw}">
          $forall info <- infoList
            <div>#{info}
      |]

postReceiptsR :: Handler Html
postReceiptsR = do
  ((result, widget), enctype) <- runFormPost receiptForm
  case result of
    FormSuccess receiptForm -> do
      receipt   <- receiptFormToReceipt receiptForm
      info      <- liftIO $ extractTextFromImage (receiptRaw receipt)
      receiptId <- runDB $ insert receipt
      infoId    <- runDB $ insert $ ReceiptInfo receiptId info
      redirect $ ReceiptsShowR receiptId
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

rawPng :: IO ByteString
rawPng = TB.strict $ TB.input "./assets/receipt.png"

extractTextFromImage :: ByteString -> IO [Text]
extractTextFromImage bs = do
  dump <- TB.strict $ TB.inproc tesseract args input
  return . T.lines . T.decodeUtf8 $ dump
 where
  input     = pure bs
  tesseract = "tesseract"
  args      = ["stdin", "stdout", "-l", "eng"]

runApp :: MonadIO m => ConnectionPool -> m ()
runApp pool = liftIO $ do
  flip runSqlPool pool $ runMigration migrateAll
  warp 3000 $ App pool

main :: IO ()
main = runResourceT $ runStderrLoggingT $ withSqlitePool dbName
                                                         connectionCount
                                                         runApp
 where
  dbName          = "test.db"
  connectionCount = 10


