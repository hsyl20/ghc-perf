{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-- | App web UI
module GHC.Profiler.UI
  ( httpApp
  )
where

import GHC.Profiler.State
import GHC.Profiler.UI.Style

import Network.Wai
import Network.HTTP.Types.Status
import Lucid.Base
import Lucid.Html5
import Lucid.Htmx
import Data.Text.Lazy.Encoding (encodeUtf8)

httpApp :: S -> Application
httpApp state req respond = do

  let
    respondHtmlM' status headers html = do
      bs <- Lucid.Base.renderBS <$> html
      respond (responseLBS status headers bs)

    respondHtml' status headers html = respondHtmlM' status headers (pure html)

    respondHtmlM html = respondHtmlM' ok200 [] html
    respondHtml  html = respondHtml'  ok200 [] html
    respondLBS status headers bs = respond (responseLBS status headers bs)
    respondText status headers t = respondLBS status headers (encodeUtf8 t)

  -- match on request and respond
  case pathInfo req of
    [] -> respondHtml (homePage state)
    ["style.css"] -> respondText ok200 [] renderedCss
    ["clicked"]   -> respondHtml (clickedHtml state)
    _             -> respondLBS status404 [] ""


homePage :: S -> Html ()
homePage state = full $ do
  button_
    [ hxPost_ "/clicked"
    , hxSwap_ "outerHTML"
    ] do
    "Click me!"

clickedHtml :: S -> Html ()
clickedHtml state = do
  div_ "Clicked!"

-- | Full page: send HTML headers
full :: Monad m => HtmlT m () -> HtmlT m ()
full p = doctypehtml_ $ do
  head_ do
    title_ "GHC Profiler"
    script_ [ src_ "https://unpkg.com/htmx.org@1.9.5" ] emptyHtml
    -- CSS style
    link_ [ href_ "/style.css", rel_ "stylesheet", type_ "text/css"]
  body_ do
    div_ [id_ "container"] do
      div_ [id_ "header"] do
        "GHC profiler"
      div_ [id_ "sidenav"] do
        "Navigation"
      div_ [id_ "main" ] do
        p
      div_ [id_ "footer"] do
        "GHC profiler rocks"

emptyHtml :: Monad m => HtmlT m ()
emptyHtml = mempty
