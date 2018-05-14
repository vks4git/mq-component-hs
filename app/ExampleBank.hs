{-# LANGUAGE RecordWildCards #-}

module Main where

import           ExampleBankTypes           (BankConfig (..), BankResult (..))
import           ExampleCalculatorTypes     (CalculatorConfig (..),
                                             CalculatorResult (..))
import           System.MQ.Component        (Env (..), getLastMsgId, runApp)
import           System.MQ.Component.Extras (MQAction, callForeignComponent,
                                             workerScheduler)
import           System.MQ.Protocol         (notExpires)

main :: IO ()
main = runApp "example_bank-hs" $ workerScheduler bankWorkerAction

-- | Our bank receives in 'BankConfig' two parameters: number of months and amount
-- of money paid per month. Then bank calls calculator to multiply number of months
-- by amount of money, receives result of that action and wraps it into 'BankResult'.
--
bankWorkerAction :: MQAction BankConfig BankResult
bankWorkerAction env@Env{..} BankConfig{..} = do
    let calculatorConfig = CalculatorConfig (fromIntegral months) payment "multiplys"
    taskId <- getLastMsgId atomic

    BankResult . answer <$> callForeignComponent env taskId notExpires calculatorConfig
