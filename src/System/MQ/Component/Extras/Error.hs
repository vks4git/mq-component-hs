module System.MQ.Component.Extras.Error
  ( throwComponentError
  , throwForeignError
  ) where

import           Control.Monad.Except (throwError)
import           System.MQ.Error      (MQError (..), errorComponent,
                                       errorForeign)
import           System.MQ.Monad      (MQMonad)

throwComponentError :: String -> MQMonad a
throwComponentError = throwError . MQError errorComponent

throwForeignError :: String -> MQMonad a
throwForeignError = throwError . MQError errorForeign
