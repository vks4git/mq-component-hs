{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad                 (forever, when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           GHC.Generics                  (Generic)
import           System.MQ.Component           (Env (..), TwoChannels (..),
                                                load2Channels, runApp)
import           System.MQ.Component.Transport (sub)
import qualified System.MQ.Encoding.JSON       as JSON (pack, unpack)
import           System.MQ.Monad               (MQMonad)
import           System.MQ.Protocol            (Condition (..), Message (..),
                                                MessageLike (..),
                                                MessageType (..), Props (..),
                                                jsonEncoding, matches,
                                                messageSpec)
import           Text.Printf                   (printf)

main :: IO ()
main = runApp "example_listener-hs" simpleListener

newtype ExampleSimpleConfig = ExampleSimpleConfig {message :: String}
  deriving (Show, Generic)

instance ToJSON ExampleSimpleConfig

instance FromJSON ExampleSimpleConfig

instance MessageLike ExampleSimpleConfig where
  props = Props "example_simple" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

simpleListener :: Env -> MQMonad ()
simpleListener env@Env{..} = do
    TwoChannels{..} <- load2Channels
    forever $ do
        (tag, msg) <- sub fromScheduler env
        when (tag `matches` (messageSpec :== "example_simple")) $
            liftIO $ printf "\nTag: %s\nMessage: %s" (show tag) (show (unpack . msgData $ msg :: Maybe ExampleSimpleConfig))
