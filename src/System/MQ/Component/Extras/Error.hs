{-# LANGUAGE RecordWildCards #-}

module System.MQ.Component.Extras.Error
  ( throwComponentError
  ) where

import           Control.Monad.Except (throwError)
import           System.MQ.Monad      (MQError (..), MQMonad)

throwComponentError :: String -> MQMonad a
throwComponentError = throwError . MQComponentError
