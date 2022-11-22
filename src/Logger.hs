{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( convertLogMsg
  , defaultLogEnv
  , logMsg
  , runKatipT
  , logStr
  , KatipT(..)
  , Katip(..)
  , LogEnv
  , Severity(..)
  ) where

import Control.Monad.Logger
import Data.List (singleton)
import Katip
import System.IO (stdout)

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
  let permitFunc = permitItem DebugS
  handleScribe <- mkHandleScribe ColorIfTerminal stdout permitFunc V2
  logEnv <- initLogEnv "todo-list-servant" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv

-- | Convert @Katip.logMsg@ to @Control.Monad.Logger.monadLoggerLog@.
convertLogMsg
  :: ToLogStr msg
  => (Namespace -> Severity -> Katip.LogStr -> m ())
  -> (Loc -> LogSource -> LogLevel -> msg -> m ())
convertLogMsg logMsg' _ logSrc logLvl msg =
  logMsg' namespace severity logStrMsg
  where
    namespace = Namespace $ singleton logSrc
    severity = logLvlToSeverity logLvl
    logStrMsg = logStr . fromLogStr . toLogStr $ msg

logLvlToSeverity :: LogLevel -> Severity
logLvlToSeverity = \case
  LevelDebug     -> DebugS
  LevelInfo      -> InfoS
  LevelWarn      -> WarningS
  LevelError     -> ErrorS
  (LevelOther _) -> NoticeS
