{-# LANGUAGE RecordWildCards #-}

module System.MQ.Component.Internal.Transport
  ( PubChannel
  , PullChannel
  , PushChannel
  , SubChannel
  , pub
  , pull
  , push
  , sub
  ) where

import           System.MQ.Component.Internal.Atomic (updateLastMsgId)
import           System.MQ.Component.Internal.Env    (Env (..))
import           System.MQ.Monad                     (MQMonad)
import           System.MQ.Protocol                  (Message (..), MessageTag)
import           System.MQ.Transport                 (PubChannel, PullChannel,
                                                      PushChannel, SubChannel)
import qualified System.MQ.Transport                 as T (pub, pull, push, sub)

-- During creation of custom logic on communication level user MUST use ONLY THESE
-- functions for receiving/sending messages from/to queue.

-- | Pushes @(tag, content)@ to the 'PushChannel'.
-- @tag::'MessageTag'@ is generated automatically from @content@.
--
push :: PushChannel -> Env -> Message -> MQMonad ()
push channel _ msg = T.push channel msg

-- | Pulls @(tag, content)@ from the 'PullChannel'.
-- Writes id of received message to @atomic@.
--
pull :: PullChannel -> Env -> MQMonad (MessageTag, Message)
pull channel Env{..} = do
  (tag, msg@Message{..})  <- T.pull channel
  updateLastMsgId msgId atomic
  pure (tag, msg)

-- | Publishes @(tag, content)@ to the 'PubChannel'.
-- @tag::'MessageTag'@ is generated automatically from @content@.
--
pub :: PubChannel -> Env -> Message -> MQMonad ()
pub channel _ msg = T.pub channel msg

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
-- Writes id of received message to @atomic@.
--
sub :: SubChannel -> Env -> MQMonad (MessageTag, Message)
sub channel Env{..} = do
  (tag, msg@Message{..})  <- T.sub channel
  updateLastMsgId msgId atomic
  pure (tag, msg)


