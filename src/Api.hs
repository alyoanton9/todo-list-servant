{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( TaskListAPI
  ) where

import Servant

import Entity

type TaskListAPI
  =   "api" :> "task" :>
    (    Get '[JSON] [Entity TaskS]
    :<|> ReqBody '[JSON] TaskS :> Post '[JSON] (Entity TaskS)
    )
  :<|> "api" :> "task" :> Capture "taskId" TaskSId :>
    (    Get '[JSON] (Entity TaskS)
    :<|> ReqBody '[JSON] TaskS :> Put '[JSON] (Entity TaskS)
    :<|> Delete '[JSON] (Entity TaskS)
    )
