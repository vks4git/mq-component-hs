{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.MQ.Component.Template.DataObtainer
 ( DataAnalyser
 , obtainData
 ) where

import           Control.Monad            (when)
import           Data.String              (fromString)
import           System.MQ.Monad          (MQMonad)
import           System.MQ.Protocol       (Condition (..), Message (..),
                                           MessageTag, matches, messageSpec,
                                           messageType)
import           System.MQ.Protocol.Class (MessageLike (..), Props (..))
import           System.MQ.Transport      (SubChannel, sub)

type DataAnalyser a = a -> MQMonad ()

-- | Receive from queue message with given type, spec and pId and analyse it using 'DataAnalyser'
--
obtainData :: forall a . MessageLike a => SubChannel -> DataAnalyser a -> MQMonad ()
obtainData subChannel analyser = do
    (tag, Message{..}) <- sub subChannel

    when (filterTag tag) $ do
      subData <- unpackM msgData
      analyser subData

  where
    Props{..} = props :: Props a

    mType = mtype
    mSpec = fromString $ spec

    filterTag :: MessageTag -> Bool
    filterTag = (`matches` (messageType :== mType :&& messageSpec :== mSpec))
