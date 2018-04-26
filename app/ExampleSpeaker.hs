{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           GHC.Generics                  (Generic)
import           System.MQ.Component           (Env (..), TwoChannels (..),
                                                load2Channels, runApp)
import           System.MQ.Component.Transport (push)
import           System.MQ.Encoding.JSON       as JSON (pack, unpack)
import           System.MQ.Monad               (MQMonad)
import           System.MQ.Protocol            (MessageLike (..),
                                                MessageType (..), Props (..),
                                                createMessage, emptyHash,
                                                jsonEncoding, notExpires)

main :: IO ()
main = runApp "example_speaker" simpleSpeaker

newtype ExampleSimpleConfig = ExampleSimpleConfig {config :: Int}
  deriving (Show, Generic)

instance ToJSON ExampleSimpleConfig

instance FromJSON ExampleSimpleConfig

instance MessageLike ExampleSimpleConfig where
  props = Props "example_simple" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

simpleSpeaker :: Env -> MQMonad ()
simpleSpeaker env@Env{..} = do
    TwoChannels{..} <- load2Channels
    forever $ do
        msg <- createMessage emptyHash creator notExpires $ ExampleSimpleConfig 15

        push toScheduler env msg
        liftIO $ print "sent"
        liftIO $ threadDelay 1000000
