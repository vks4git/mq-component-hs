{-# LANGUAGE DuplicateRecordFields #-}

module System.MQ.Component.Env
  (
    Name
  , Env (..)
  , TwoChannels (..)
  , ThreeChannels (..)
  ) where

import           Control.Concurrent.MVar
import           System.MQ.Component.Atomic (Atomic)
import           System.MQ.Protocol
import           System.MQ.Transport


type Name = String

data Env = Env { name      :: Name
               , creator   :: Creator
               , logfile   :: FilePath
               , frequency :: Int
               , atomic    :: MVar Atomic
               }

data TwoChannels = TwoChannels { fromScheduler :: SubChannel
                               , toScheduler   :: PushChannel
                               }

data ThreeChannels = ThreeChannels { fromScheduler  :: SubChannel
                                   , toScheduler    :: PushChannel
                                   , fromController :: PullChannel
                                   }
