{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-- | App web UI
module GHC.Profiler.UI
  ( httpApp
  , initUIState
  , UIState
  )
where

import GHC.Profiler.State
import GHC.Profiler.UI.Style
import GHC.Profiler.UI.Events

import Network.Wai
import Network.HTTP.Types.Status
import Lucid.Base
import Lucid.Html5
import Lucid.Htmx
import Data.Text.Lazy.Encoding (encodeUtf8)
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.ByteString.Builder

------------------------------------
-- To upstream in lucid2-htmx
--
sseSwap_ :: Term arg result => arg -> result
sseSwap_ = term "sse-swap"

sseConnect_ :: Term arg result => arg -> result
sseConnect_ = term "sse-connect"

------------------------------------

data UIState = UIState
  { uiSSE :: SSE
  , uiCount :: IORef Int
  }

initUIState :: IO UIState
initUIState = do
  -- Initialize global server-sent events
  sse <- initSSE
  -- TODO: we need to get events from somewhere.
  --  - add listener to events produced by the app (another Chan in State?)
  --  - poke State regularly?
  -- We don't want to have too much application logic in the UI in case we
  -- decide to have a TUI too.

  -- some dumb counter for the example
  count <- newIORef 0

  pure $ UIState
    { uiSSE = sse
    , uiCount = count
    }


httpApp :: UIState -> S -> Application
httpApp uistate state req respond = do

  let
    respondHtml' status headers html = do
      let bs = Lucid.Base.renderBS html
      respond (responseLBS status headers bs)

    respondHtml html = respondHtml' ok200 [] html
    respondLBS status headers bs = respond (responseLBS status headers bs)
    respondText status headers t = respondLBS status headers (encodeUtf8 t)

    sse = uiSSE uistate

  -- match on request and respond
  case pathInfo req of
    [] -> respondHtml (homePage state)
    ["style.css"] -> respondText ok200 [] renderedCss
    ["events"]    -> respond =<< responseSSE sse

    ["status"]    -> do
      v <- readIORef (uiCount uistate)
      respondHtml $ "Counted " <> toHtml (show v)
    ["button"]    -> respondHtml clickButton
    ["clicked"]   -> do
      -- don't spawn the thread twice...
      old <- readIORef (uiCount uistate)
      when (old == 0) $ do
        -- send some events, just for fun
        void $ forkIO $ forever do
          v <- atomicModifyIORef (uiCount uistate) (\x -> (x+1,x+1))
          sendEvent sse $ ServerEvent
            { eventName = Just $ byteString "status_update"
            , eventId   = Nothing
            , eventData = [stringUtf8 $ "<div>Set " ++ show v ++ "</div>"]
            }
          threadDelay 1000000

      respondHtml (clickedHtml state)
    _             -> respondLBS status404 [] ""


clickButton :: Html ()
clickButton =
  button_
    [ hxPost_ "/clicked"
    , hxSwap_ "outerHTML"
    ] do
    "Click me!"


homePage :: S -> Html ()
homePage _state = full $ do
  clickButton


clickedHtml :: S -> Html ()
clickedHtml _state = do
  div_
    [ id_ "dynamic"
    ] do
      button_
        [ hxSwap_ "outerHTML"
        , hxTarget_ "#dynamic"
        , hxPost_ "/button"
        ] "STOP!"
      div_
        [ hxExt_ "sse"
        , sseConnect_ "/events"
        ] do
          -- triggered
          div_
            [ hxGet_ "/status"
            , hxTrigger_ "sse:status_update"
            ] do
            "Triggered div"
          -- show received event data
          div_
            [ sseSwap_ "status_update"
            ] do
            "Received event data"

-- | Full page: send HTML headers
full :: Monad m => HtmlT m () -> HtmlT m ()
full p = doctypehtml_ $ do
  head_ do
    title_ "GHC Profiler"
    script_ [ src_ "https://unpkg.com/htmx.org@1.9.5" ] emptyHtml
    script_ [ src_ "https://unpkg.com/htmx.org/dist/ext/sse.js" ] emptyHtml
    -- CSS style
    link_ [ href_ "/style.css", rel_ "stylesheet", type_ "text/css"]
  body_
    -- body listens to SSE events, hence inner elements can use hxTrigger
    -- "sse:event_name" to be triggered by an event and fetch an updated
    -- information (hxGet/hxPost...) or be replaced by the event data directly
    -- (sseSwap).
    [ hxExt_ "sse"
    , sseConnect_ "/events"
    ] do
      div_ [id_ "container"] do
        div_ [id_ "header"] do
          div_ [class_ "logo"] do
            "GHC profiler"
        div_ [id_ "sidenav"] do
          "Navigation"
        div_ [id_ "main" ] do
          p

emptyHtml :: Monad m => HtmlT m ()
emptyHtml = mempty
