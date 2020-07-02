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
import           Turtle                            hiding ( select
                                                          , limit
                                                          )
import qualified Turtle.Bytes                  as TB
import           Yesod                             hiding ( (==.) )
import           Prelude                           hiding ( FilePath )
import           Database.Persist                  hiding ( (==.) )
import           Database.Persist.Sqlite           hiding ( (==.) )
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
import           Database.Esqueleto
import           Model
import           OCR                                      ( extractTextFromImage
                                                          )
import           Receipt                                  ( insertReceipt
                                                          , fetchReceipt
                                                          , fetchReceipts
                                                          , deleteReceipt
                                                          )


newtype App = App ConnectionPool

mkYesod "App" [parseRoutes|
  /                              RootR             GET
  /receipts                      ListReceiptsR     GET POST
  !/receipts/new                 NewReceiptR       GET
  !/receipts/#ReceiptId/edit     EditReceiptR      GET
  !/receipts/#ReceiptId          ReceiptR          GET DELETE PUT
|]

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
    <html lang="en">
      <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
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

getNewReceiptR :: Handler Html
getNewReceiptR = do
  (widget, enctype) <- generateFormPost receiptForm
  defaultLayout [whamlet|
    <div>
      <p>Enter a new receipt
      <form method=post action=@{ListReceiptsR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
  |]


getListReceiptsR :: Handler Html
getListReceiptsR = do
  receipts <- runDB fetchReceipts
  defaultLayout [whamlet|
    <div>
        <table>
          <thead>
            <th>Name
            <th>Date Created
            <th>Actions
          <tbody>
            $forall (id, Receipt name raw date) <- receipts
              <tr>
                <td><a href=@{ReceiptR id}>#{name}
                <td>#{formatRFC1123 date}      
                <td>delete | <a href=@{EditReceiptR id}>edit
  |]

methodLink :: Text -> Text -> Route site -> WidgetFor site ()
methodLink name method link = [whamlet|
  $newline never
  <form method=#{method} action="@{link}">
    <button>#{name}
|]

putReceiptR :: ReceiptId -> Handler Html
putReceiptR = undefined

getEditReceiptR  :: ReceiptId -> Handler Html
getEditReceiptR = undefined

deleteReceiptR :: ReceiptId -> Handler Html
deleteReceiptR id = do
  runDB $ deleteReceipt id
  redirect ListReceiptsR

getReceiptR :: ReceiptId -> Handler Html
getReceiptR receiptId = do
  mReceiptAndInfo <- runDB $ fetchReceipt receiptId
  case mReceiptAndInfo of
    Just (receipt, info) -> do
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

postListReceiptsR :: Handler Html
postListReceiptsR = do
  ((result, widget), enctype) <- runFormPost receiptForm
  case result of
    FormSuccess receiptForm -> do
      receipt   <- receiptFormToReceipt receiptForm
      info      <- liftIO $ extractTextFromImage (receiptRaw receipt)
      receiptId <- runDB $ insertReceipt receipt info
      redirect $ ReceiptR receiptId
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


