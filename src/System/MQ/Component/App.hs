{-# LANGUAGE RecordWildCards #-}

module System.MQ.Component.App
  (
    runApp
  , runAppWithTech
  ) where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.MVar       (isEmptyMVar, putMVar)
import           Control.Monad                 (forever, when)
import           Control.Monad.Except          (catchError)
import           System.Log.Formatter          (simpleLogFormatter)
import           System.Log.Handler            (setFormatter)
import           System.Log.Handler.Simple     (fileHandler)
import           System.Log.Logger             (Priority (..), addHandler,
                                                infoM, setLevel,
                                                updateGlobalLogger)
import           System.MQ.Component.Atomic    (createAtomic)
import           System.MQ.Component.Config    (loadEnv)
import           System.MQ.Component.Env       (Env (..), Name)
import           System.MQ.Component.Technical (runTech)
import           System.MQ.Monad               (MQMonad, errorHandler,
                                                runMQMonad)


runApp :: Name -> (Env -> MQMonad ()) -> IO ()
runApp name' runComm = runAppWithTech name' runComm runTech
  
runAppWithTech :: Name -> (Env -> MQMonad ()) -> (Env -> MQMonad ()) -> IO ()
runAppWithTech name' runComm runCustomTech = do
    env@Env{..} <- loadEnv name'

    setupLogger env

    infoM name "running component..."

    infoM name "running technical fork..."
    _ <- forkIO $ processMQError name $ runCustomTech env

    forever $ do
        atomicIsEmpty <- isEmptyMVar atomic

        when atomicIsEmpty $ do
            infoM name "there is not communication thread, creating new one..."
            commThreadId <- forkIO $ processMQError name $ runComm env

            let newAtomic = createAtomic commThreadId
            putMVar atomic newAtomic

        threadDelay oneSecond

  where
    oneSecond :: Int
    oneSecond = 10^(6 :: Int)



processMQError :: Name -> MQMonad () -> IO ()
processMQError name'= runMQMonad . flip catchError (errorHandler name')

setupLogger :: Env -> IO ()
setupLogger Env{..} = do
    h <- fileHandler logfile INFO >>= \lh -> return $
             setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger name (addHandler h)
    updateGlobalLogger name (setLevel INFO)
