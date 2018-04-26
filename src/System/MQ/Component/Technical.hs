{-# LANGUAGE RecordWildCards #-}

module System.MQ.Component.Technical
  (
    runTech
  , processKill
  , processMonitoring
  ) where

import           Control.Concurrent                                (forkIO)
import           Control.Monad.IO.Class                            (liftIO)
import           System.MQ.Component.Env                           (Env (..))
import           System.MQ.Component.Technical.Internal.Kill       (processKill)
import           System.MQ.Component.Technical.Internal.Monitoring (processMonitoring)
import           System.MQ.Monad                                   (MQMonad,
                                                                    runMQMonad)

runTech :: Env -> MQMonad ()
runTech env@Env{..} = do
        _ <- liftIO . forkIO . runMQMonad $ processKill env
        processMonitoring env
