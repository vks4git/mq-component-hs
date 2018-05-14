{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.IO.Class                (liftIO)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..))
import           GHC.Generics                          (Generic)
import           System.MQ.Component                   (Env (..),
                                                        callForeignComponent,
                                                        getLastMsgId, runApp)
import           System.MQ.Component.Template.Listener (MQActionVoid,
                                                        listenerComm)
import qualified System.MQ.Encoding.JSON               as JSON (pack, unpack)
import           System.MQ.Monad                       (MQMonad)
import           System.MQ.Protocol                    (MessageLike (..),
                                                        MessageType (..),
                                                        Props (..),
                                                        jsonEncoding,
                                                        notExpires)

main :: IO ()
main = runApp "example_foreign-hs" $ listenerComm simpleForeignCall

newtype ExampleSimpleConfig = ExampleSimpleConfig {message :: String}
  deriving (Show, Generic)

instance ToJSON ExampleSimpleConfig

instance FromJSON ExampleSimpleConfig

instance MessageLike ExampleSimpleConfig where
  props = Props "example_simple" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

newtype ExampleSimpleResponse = ExampleSimpleResponse {result :: String}
  deriving (Show, Generic)

instance ToJSON ExampleSimpleResponse

instance FromJSON ExampleSimpleResponse

instance MessageLike ExampleSimpleResponse where
  props = Props "example_simple" Result jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

simpleForeignCall :: MQActionVoid ExampleSimpleConfig
simpleForeignCall env@Env{..} config = do
  lastId <- getLastMsgId atomic
  foreignRes <- callForeignComponent env lastId notExpires config :: MQMonad ExampleSimpleResponse
  liftIO $ print foreignRes
