{-# LANGUAGE RecordWildCards #-}

module Main where

import           ExampleCalculatorTypes     (CalculatorConfig (..),
                                             CalculatorResult (..))
import           System.MQ.Component        (runApp)
import           System.MQ.Component.Extras (MQAction, throwComponentError,
                                             workerScheduler)

main :: IO ()
main = runApp "example_calculator-hs" $ workerScheduler calculatorWorkerAction

-- | To use template 'Worker' we just need to define function of type 'MQAction'
-- that will process messages received from queue and return results of such processing.
-- Our calculator multiplies every number received in 'CalculatorConfig' by 228 and
-- returns result of that multiplication in 'CalculatorResult'.
--
calculatorWorkerAction :: MQAction CalculatorConfig CalculatorResult
calculatorWorkerAction _ CalculatorConfig{..} =
  case action of
    "+" -> return $ CalculatorResult $ first + second
    "*" -> return $ CalculatorResult $ first * second
    m   -> throwComponentError $ "Unknown calculator action: " ++ m

