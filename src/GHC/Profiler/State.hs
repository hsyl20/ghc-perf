-- | App state
module GHC.Profiler.State
  ( AppState (..)
  , initAppState
  )
where

data AppState = AppState

initAppState :: IO AppState
initAppState = pure AppState
