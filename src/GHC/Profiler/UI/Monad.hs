module GHC.Profiler.UI.Monad
  ( H
  , runH
  , MState (..)
  , initMState
  )
where

import GHC.Profiler.UI.Html
import GHC.Profiler.UI.Events
import GHC.Profiler.State
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

-- | Main monad alias
type H a = HtmlT (ReaderT MState IO) a

runH :: MState -> H a -> IO (Html a)
runH s m = runReaderT (commuteHtmlT2 m) s

-- | Web UI state
data MState = MState
  { mSSE      :: !SSE
  , mAppState :: !AppState
  }

initMState :: AppState -> IO MState
initMState state = do
  -- Initialize global server-sent events
  sse <- initSSE
  -- TODO: we need to get events from somewhere.
  --  - add listener to events produced by the app (another Chan in State?)
  --  - poke State regularly?
  -- We don't want to have too much application logic in the UI in case we
  -- decide to have a TUI too.

  pure $ MState
    { mSSE      = sse
    , mAppState = state
    }

