{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( TaskListAPI
  ) where

import Servant

import Entity

type TaskListAPI
  =   "api" :> "task" :>
    (    Get '[JSON] [Entity Task]
    :<|> ReqBody '[JSON] Task :> Post '[JSON] (Entity Task)
    )
  :<|> "api" :> "task" :> Capture "taskId" TaskId :>
    (    Get '[JSON] (Entity Task)
    :<|> ReqBody '[JSON] Task :> Put '[JSON] (Entity Task)
    :<|> Delete '[JSON] (Entity Task)
    )
