module System.MQ.Component.Extras.Template.Types
  (
    MQAction
  , MQActionVoid
  ) where

import           System.MQ.Component.Internal.Env (Env)
import           System.MQ.Monad                  (MQMonad)

-- | Type of function that user needs to define in order to use some templates.
-- Function takes component's 'Env' and data of type 'a', processes given data
-- and returns result of type 'b'.
--
type MQAction a b = Env -> a -> MQMonad b

-- | Type of function that user needs to define in order to use some templates.
-- Type is similar to 'MQAction' apart from the fact that this action doesn't
-- produce any results.
--
type MQActionVoid a = MQAction a ()
