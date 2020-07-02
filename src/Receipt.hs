{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Receipt where

import           Database.Persist                         ( selectList )
import           Data.Text                                ( Text )
import           Database.Esqueleto
import           Model
import           Control.Monad.Reader                     ( ReaderT )
import           Control.Monad.IO.Class                   ( MonadIO(liftIO) )

insertReceipt
  :: (MonadIO m, PersistStoreWrite backend, BaseBackend backend ~ SqlBackend)
  => Receipt
  -> [Text]
  -> ReaderT backend m (Key Receipt)
insertReceipt receipt info = do
  id <- insert receipt
  insert $ ReceiptInfo id info
  return id

deleteReceipt
  :: ( MonadIO m
     , BackendCompatible SqlBackend backend
     , PersistQueryWrite backend
     , PersistUniqueWrite backend
     )
  => Key Receipt
  -> ReaderT backend m ()
deleteReceipt id = do
  delete $ from $ \r -> do
    where_ (r ^. ReceiptId ==. val id)

fetchReceipts
  :: ( MonadIO m
     , BackendCompatible SqlBackend backend
     , PersistQueryRead backend
     , PersistUniqueRead backend
     )
  => ReaderT backend m [(ReceiptId, Receipt)]
fetchReceipts = do
  entities <- select $ from $ \r -> do
    orderBy [desc (r ^. ReceiptDate)]
    return r
  return (f <$> entities)
  where f (Entity id r) = (id, r)

fetchReceipt
  :: ( MonadIO m
     , BackendCompatible SqlBackend backend
     , PersistQueryRead backend
     , PersistUniqueRead backend
     )
  => Key Receipt
  -> ReaderT backend m (Maybe (Receipt, [Text]))
fetchReceipt id = do
  results <- select $ from $ \(receipt, receiptInfo) -> do
    where_
      (   (receipt ^. ReceiptId ==. val id)
      &&. (receipt ^. ReceiptId ==. receiptInfo ^. ReceiptInfoReceipt)
      )
    return (receipt, receiptInfo)
  return $ case results of
    [(Entity _ receipt, Entity _ (ReceiptInfo _ info))] -> Just (receipt, info)
    _ -> Nothing
