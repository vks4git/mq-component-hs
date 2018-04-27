{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (liftIO)
import           Data.String                   (fromString)
import           System.MQ.Component           (Env (..), TwoChannels (..),
                                                loadTechChannels,
                                                runAppWithTech)
import           System.MQ.Component.Transport (push)
import           System.MQ.Monad               (MQMonad)
import           System.MQ.Protocol            (createMessage, notExpires)
import           System.MQ.Protocol.Technical  (KillConfig (..))

main :: IO ()
main = runAppWithTech "killer" (const $ return ()) killer

killer :: Env -> MQMonad ()
killer env@Env{..} = do
    TwoChannels{..} <- loadTechChannels
    forever $ do
        liftIO $ putStrLn "Enter id of task to kill"

        taskId <- fromString <$> liftIO getLine

        msg <- createMessage "" name notExpires $ KillConfig taskId
        push toScheduler env msg
