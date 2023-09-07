-- | App state
module GHC.Profiler.State
  ( S
  , initState
  )
where

type S = ()

initState :: IO S
initState = pure ()
