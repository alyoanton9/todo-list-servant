{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handlers
  ( App(..)
  , taskListServerT
  , taskListAPI
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Database.Persist.Postgresql
import Servant

import Api
import Entity
import Config

-- | Monad to use in handlers instead of the default 'Handler'.
newtype App a = App { runApp :: ReaderT Config Handler a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader Config, MonadError ServerError
    )

taskListAPI :: Proxy TaskListAPI
taskListAPI = Proxy

-- | Function enumerating handlers to handle requests to the API.
taskListServerT :: ServerT TaskListAPI App
taskListServerT =
      (getEntityTasks
  :<|> createEntityTask)
  :<|> taskOperations
  where
    taskOperations taskId =
           getEntityTaskById taskId
      :<|> replaceEntityTaskById taskId
      :<|> deleteEntityTaskById taskId

-- Handlers implementations

getEntityTasks ::  App [Entity TaskS]
getEntityTasks = runDB $ selectList [] []

createEntityTask :: TaskS -> App (Entity TaskS)
createEntityTask task = do
  taskId <- runDB $ insert task
  return $ Entity taskId task

getEntityTaskById :: TaskSId -> App (Entity TaskS)
getEntityTaskById taskId = do
  maybeTask <- runDB $ get taskId
  case maybeTask of
    Just task -> return $ Entity taskId task
    Nothing   -> throwError err404

deleteEntityTaskById :: TaskSId -> App (Entity TaskS)
deleteEntityTaskById taskId = do
  entityTask <- getEntityTaskById taskId
  runDB $ delete taskId
  return $ entityTask

replaceEntityTaskById :: TaskSId -> TaskS -> App (Entity TaskS)
replaceEntityTaskById taskId task = do
  _ <- getEntityTaskById taskId
  runDB $ replace taskId task
  return $ Entity taskId task
