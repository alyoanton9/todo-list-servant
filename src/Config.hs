{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config
  ( AppT(..)
  , App
  , Config(..)
  , makePool
  , katipLogger
  , defaultPort
  ) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Logger (MonadLogger(..), MonadLoggerIO(..))
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, liftIO)
import Data.Maybe (fromMaybe)
import Database.Persist.Postgresql (
  ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai (Middleware, rawPathInfo, requestHeaderUserAgent)
import Network.Wai.Handler.Warp (Port)
import Servant

import Logger

newtype AppT m a =
  AppT
    { runApp ::
      ReaderT Config (ExceptT ServerError m) a
    }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader Config, MonadError ServerError
    )

type App = AppT IO

data Config
  = Config
  { configPool   :: ConnectionPool
  , configLogEnv :: LogEnv
  , configPort   :: Port
  }

makePool :: LogEnv -> IO ConnectionPool
makePool logEnv = runKatipT logEnv $
  createPostgresqlPool connectionStr connectionsNumber

katipLogger :: LogEnv -> Middleware
katipLogger env app request responseFunc = runKatipT env $ do
  let userAgent = fromMaybe "undefined" $ requestHeaderUserAgent request
  let requestedPath = rawPathInfo request
  let msg = "raw path=" <> requestedPath <> " user agent=" <> userAgent
  logMsg "middleware" InfoS $ logStr msg
  liftIO $ app request responseFunc

-- Instances

-- | @MonadLogger@ instance required to define @MonadLoggerIO@ instance.
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = convertLogMsg logMsg

-- | @IO@ instance required to define @MonadLoggerIO@ instance.
instance Katip IO where
  getLogEnv = defaultLogEnv
  localLogEnv _ ioAction = ioAction

-- | @MonadLoggerIO@ instance required by @createPostgresqlPool@.
instance MonadIO m => MonadLoggerIO (KatipT m) where
  askLoggerIO = KatipT . return $ convertLogMsg logMsg

-- Config constants

connectionStr :: ConnectionString
connectionStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

connectionsNumber :: Int
connectionsNumber = 10

defaultPort :: Int
defaultPort = 4000
