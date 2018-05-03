{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.MQ.Component.Template.Listener
  ( DataAnalyser
  , listenerComm
  , listenerTech
  ) where

import           System.MQ.Component.Config                (load2Channels,
                                                            loadTechChannels)
import           System.MQ.Component.Env                   (Env (..),
                                                            TwoChannels (..))
import           System.MQ.Component.Template.DataObtainer (DataAnalyser,
                                                            obtainData)
import           System.MQ.Monad                           (MQMonad,
                                                            foreverSafe)
import           System.MQ.Protocol.Class                  (MessageLike (..))

-- | Listener of communication level
--
listenerComm :: MessageLike a => DataAnalyser a -> Env -> MQMonad ()
listenerComm analyser env = load2Channels >>= listenerWithChannels analyser env

-- | Listener of technical level
--
listenerTech :: MessageLike a => DataAnalyser a -> Env -> MQMonad ()
listenerTech analyser env = loadTechChannels >>= listenerWithChannels analyser env

-- | Template `listener` allows user to gather data from queue and perform different actions with it
--
listenerWithChannels :: MessageLike a =>  DataAnalyser a -> Env -> TwoChannels -> MQMonad ()
listenerWithChannels analyser Env{..} TwoChannels{..} = foreverSafe name $ obtainData fromScheduler analyser

