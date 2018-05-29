module System.MQ.Component.Extras.Validation
  ( validateCondition
  , validateConditionMaybe
  , catchRuntimeError
  ) where

import           System.IO.Unsafe                 (unsafePerformIO)

import           Control.Exception                (catch, SomeException)

import           System.MQ.Component.Extras.Error (throwErrorIncorrectInput)
import           System.MQ.Monad                  (MQMonadS)

validateCondition :: (a -> Bool) -> a -> String -> MQMonadS s a
validateCondition condition val msg = if (condition val)
                                        then return val
                                        else throwErrorIncorrectInput msg

validateConditionMaybe :: (a -> Bool) -> Maybe a -> String -> MQMonadS s (Maybe a)
validateConditionMaybe condition (Just val) msg = if (condition val)
                                                    then return $ Just val
                                                    else throwErrorIncorrectInput msg
validateConditionMaybe _ Nothing _              = return Nothing

catchRuntimeError :: a -> (String -> MQMonadS s a) -> String -> MQMonadS s a
catchRuntimeError val throwFunc msg = unsafePerformIO $ catch (val `seq` return . return $ val)
                                             (\e -> return . throwFunc $ msg ++ ": " ++ show (e::SomeException))
