module GHC.Profiler.Component
  ( Component(..)
  )
where

import Network.Wai
import Data.Text (Text)

import GHC.Profiler.Responder

data Component = Component
  { compName    :: !Text -- ^ Component name (used as path prefix)
  , compRespond :: Request -> Responder -> IO ResponseReceived
  }
