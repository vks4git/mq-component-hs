{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module ExampleRadioTypes
  ( RadioData (..)
  ) where

import           Control.Monad                  ((>=>))
import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import           Data.Map.Strict                (Map, fromList, member, (!))
import           Data.MessagePack.Types.Class   (MessagePack (..))
import           Data.MessagePack.Types.Object  (Object)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           System.MQ.Encoding.MessagePack (makeMsgPackDictionary)
import           System.MQ.Protocol             (MessageLike (..),
                                                 MessageType (..), Props (..))

-- | 'RadioData' represents some data that can be trasfered from one component to another.
--
newtype RadioData = RadioData { message :: String }
  deriving (Show, Generic)

makeMsgPackDictionary ''RadioData

instance ToJSON RadioData
instance FromJSON RadioData
instance MessageLike RadioData where
  props = Props "example_radio" Data


