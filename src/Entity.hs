{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}

module Entity
  ( Task (..)
  , TaskId
  , Entity (..)
  , migrateAll
  , runDB
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Database.Persist.TH (
  mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Config (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    content String
    deriving Show
|]

-- | Make query to a database.
runDB
  :: (MonadReader Config m, MonadIO m)
  => SqlPersistT IO a -> m a
runDB query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

-- Task instances

instance ToJSON Task where
  toJSON task = object ["content" .= taskContent task]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task
    <$> v .: "content"

instance ToJSON (Entity Task) where
  toJSON (Entity taskId task) = object
    [ "id"   .= taskId
    , "content" .= taskContent task
    ]
