{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (liftIO)
import           System.MQ.Component           (Env (..), TwoChannels (..),
                                                load2Channels, runApp)
import           System.MQ.Component.Transport (sub)
import           System.MQ.Monad               (MQMonad)
import           System.MQ.Protocol            (Message (..))

main :: IO ()
main = runApp "example_killable-hs" exampleKillable

exampleKillable :: Env -> MQMonad ()
exampleKillable env@Env{..} = do
    TwoChannels{..} <- load2Channels
    forever $ do
        liftIO $ putStrLn "I'm running"
        (_, Message{..}) <- sub fromScheduler env

        liftIO $ print msgId
        liftIO $ putStrLn "Going into loop"

        forever $ liftIO $ threadDelay 1000000
