module GHC.Profiler.UI.Html
  ( module Lucid.Html5
  , module Lucid.Htmx
  , module Lucid.Base
  , sseConnect_
  , sseSwap_
  )
where

import Lucid.Base
import Lucid.Html5
import Lucid.Htmx

------------------------------------
-- To upstream in lucid2-htmx

sseSwap_ :: Term arg result => arg -> result
sseSwap_ = term "sse-swap"

sseConnect_ :: Term arg result => arg -> result
sseConnect_ = term "sse-connect"

------------------------------------
