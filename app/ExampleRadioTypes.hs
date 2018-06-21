{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleRadioTypes
  ( RadioData (..)
  ) where

import           Control.Monad                 ((>=>))
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Data.Map.Strict               (Map, fromList, member, (!))
import           Data.MessagePack.Types.Class  (MessagePack (..))
import           Data.MessagePack.Types.Object (Object)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           System.MQ.Protocol            (Dictionary (..),
                                                MessageLike (..),
                                                MessageType (..), Props (..))

-- | 'RadioData' represents some data that can be trasfered from one component to another.
--
newtype RadioData = RadioData { message :: String }
  deriving (Show, Generic)

instance ToJSON RadioData
instance FromJSON RadioData
instance MessageLike RadioData where
  props = Props "example_radio" Data


instance Dictionary RadioData where
  toDictionary (RadioData msg) = fromList [ "message" .= msg]
  fromDictionary dict = do
    (msg :: String)  <- dict .! "message"
    pure $ RadioData msg

instance MessagePack RadioData where
  toObject = toObject . toDictionary
  fromObject = fromObject >=> fromDictionary


infix .=
(.=) :: (Ord a, MessagePack b) => a -> b -> (a, Object)
a .= b = (a, toObject b)

infix .!
(.!) :: (Monad m, MessagePack b) => Map Text Object -> Text -> m b
dict .! key | key `member` dict = fromObject $ dict ! key
            | otherwise = error $ "System.MQ.Protocol.Internal.Instances: .! :: key " ++ show key ++ " is not an element of the dictionary."
