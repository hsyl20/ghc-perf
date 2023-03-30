-- | App state
module GHC.Profiler.State
  ( AppState
  , initialState
  )
where

type AppState = ()

initialState :: IO AppState
initialState = pure ()
