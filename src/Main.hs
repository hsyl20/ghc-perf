module Main where

import GHC.Profiler.UI
import GHC.Profiler.State

main :: IO ()
main = do
  _finalState <- runState =<< initialState

  pure ()
