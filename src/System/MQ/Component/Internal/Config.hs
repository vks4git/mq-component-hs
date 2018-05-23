{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.MQ.Component.Internal.Config
  (
    loadEnv
  , load3Channels
  , load3ChannelsWithContext
  , load2Channels
  , load2ChannelsWithContext
  , loadTechChannels
  , loadTechChannelsWithContext
  ) where

import           Control.Concurrent.MVar          (newEmptyMVar)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson.Picker                ((|--))
import qualified Data.Text                        as T (Text, pack)
import           System.BCD.Config                (getConfigText)
import           System.MQ.Component.Internal.Env (Env (..), Name,
                                                   ThreeChannels (..),
                                                   TwoChannels (..))
import           System.MQ.Monad                  (MQMonad)
import           System.MQ.Transport              (ConnectTo (..), Context,
                                                   Host, HostPort (..), Port,
                                                   contextM)


loadEnv :: Name -> IO Env
loadEnv name' = do
    config         <- getConfigText
    let creator'   = config |-- ["params", T.pack name', "creator"]
    let logfile'   = config |-- ["params", T.pack name', "logfile"]
    let frequency' = config |-- ["params", T.pack name', "frequency"]
    Env name' creator' logfile' frequency' <$> liftIO newEmptyMVar


load3Channels :: Name -> MQMonad ThreeChannels
load3Channels name' = contextM >>= flip load3ChannelsWithContext name'

load3ChannelsWithContext :: Context -> Name -> MQMonad ThreeChannels
load3ChannelsWithContext context' name' = do
    sIn   <- liftIO schedulerInFromConfig
    sOut  <- liftIO schedulerOutFromConfig
    (ControllerCfg contr) <- liftIO $ controllerFromConfig name'

    fromScheduler  <- connectTo (comHelper sOut) context'
    toScheduler    <- connectTo (comHelper sIn) context'
    fromController <- connectTo contr context'
    pure ThreeChannels{..}

load2Channels :: MQMonad TwoChannels
load2Channels = contextM >>= load2ChannelsWithContext

load2ChannelsWithContext :: Context -> MQMonad TwoChannels
load2ChannelsWithContext = flip configHelper comHelper

loadTechChannels :: MQMonad TwoChannels
loadTechChannels = contextM >>= loadTechChannelsWithContext

loadTechChannelsWithContext :: Context -> MQMonad TwoChannels
loadTechChannelsWithContext = flip configHelper techHelper

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

data SchedulerCfg = SchedulerCfg { host     :: Host
                                 , comport  :: Port
                                 , techport :: Port
                                 }

newtype ControllerCfg = ControllerCfg HostPort

configHelper :: Context -> (SchedulerCfg -> HostPort) -> MQMonad TwoChannels
configHelper context' helper = do
    sIn  <- liftIO schedulerInFromConfig
    sOut <- liftIO schedulerOutFromConfig

    fromScheduler <- connectTo (helper sOut) context'
    toScheduler   <- connectTo (helper sIn) context'
    pure TwoChannels{..}

comHelper :: SchedulerCfg -> HostPort
comHelper SchedulerCfg{..} = HostPort host comport

techHelper :: SchedulerCfg -> HostPort
techHelper SchedulerCfg{..} = HostPort host techport


controllerFromConfig :: Name -> IO ControllerCfg
controllerFromConfig name' = do
    config <- getConfigText
    let host' = config |-- ["deploy", "monique", "controller", "host"]
    let port' = config |-- ["params", T.pack name', "port"]
    pure . ControllerCfg $ HostPort host' port'

schedulerInFromConfig :: IO SchedulerCfg
schedulerInFromConfig = schedulerFromJSON ["deploy", "monique", "scheduler-in"]

schedulerOutFromConfig :: IO SchedulerCfg
schedulerOutFromConfig = schedulerFromJSON ["deploy", "monique", "scheduler-out"]

schedulerFromJSON :: [T.Text] -> IO SchedulerCfg
schedulerFromJSON route = do
    config <- getConfigText
    let getField f = config |-- (route ++ [f])
    pure $ SchedulerCfg (getField "host")
                        (getField "comport")
                        (getField "techport")





