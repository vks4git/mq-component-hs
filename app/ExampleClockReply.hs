{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           ExampleClockTypes          (ClockConfig (..), ClockResult (..))
import           System.MQ.Component        (runApp)
import           System.MQ.Component.Extras (MQAction, workerController)
import           System.MQ.Protocol         (getTimeMillis)

main :: IO ()
main = runApp "example_clock-reply-hs" $ workerController clockReplyAction

-- | This is logic for "clustered" example. This function doesn't changed in regular and "cluster" modes.
-- The only difference is how we run them. In this example function 'workerController' is used
-- to run this component in "cluster" mode.
-- See @config.json@ for the details about connections to the controller.
--
clockReplyAction :: MQAction ClockConfig ClockResult
clockReplyAction _ ClockConfig {..} = do
    liftIO . putStrLn $ "Got the question: " ++ question
    now <- liftIO getTimeMillis
    liftIO . putStrLn $ "My time is " ++ show now
    return $ ClockResult now
