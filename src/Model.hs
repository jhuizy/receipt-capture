{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import           Database.Persist
import           Data.ByteString                          ( ByteString )
import           Data.Text                                ( Text )
import           Data.Time                                ( UTCTime )
import           Database.Persist.TH

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
