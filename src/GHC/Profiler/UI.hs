-- | App web UI
module GHC.Profiler.UI
  ( httpApp
  )
where

import GHC.Profiler.UI.Style
import GHC.Profiler.UI.Events
import GHC.Profiler.UI.Html
import GHC.Profiler.UI.Monad
import GHC.Profiler.Responder
import GHC.Profiler.Component
import GHC.Profiler.Component.RtsStats

import Network.Wai
import Network.HTTP.Types.Status
import Control.Monad
import Data.Text (Text,pack,unpack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

componentMap :: Map Text Component
componentMap = Map.fromList $ map (\c -> (compName c,c)) components

-- | All the enabled components
components :: [Component]
components =
  [ rtsStats
  ]

httpApp :: MState -> Application
httpApp state req respond = do

  let
    sse = mSSE state
    res = Responder respond state

  -- match on request and respond
  case pathInfo req of
    []            -> respondH res (full welcomeHtml)
    ["style.css"] -> respondText res ok200 [] renderedCss
    ["events"]    -> respond =<< responseSSE sse

    "nav" : path  -> respondH res $ navHtml True (fmap (read . unpack) path)

    (c:_)
      | Just comp <- Map.lookup c componentMap
      -> compRespond comp req res

    _             -> respondLBS res status404 [] ""


welcomeHtml :: H ()
welcomeHtml = do
  h1_ "Welcome to the GHC profiler (alpha)"
  p_ "The purpose of this profiler is twofold:"
  ul_ do
    li_ "Profiling and optimizing GHC itself"
    li_ "Profiling and optimizing programs built with GHC"

  p_ do
    "This tool allows you to perform several analyses directly from this interface. "
    "Use the navigation panel on the left to start producing and exploring profiling data. "

  p_ do
    "Bug reports and suggestions welcome at "
    a_ [ href_   "https://github.com/hsyl20/ghc-profiler/issues"
       , target_ "blank_"
       ]
       "https://github.com/hsyl20/ghc-profiler/issues"

  p_ "Happy profiling!"



-- | Full page: send HTML headers
full :: H () -> H ()
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
    -- The latter doesn't fully work yet for sse-swap elements installed after
    -- initialization of the connection (see
    -- https://github.com/bigskysoftware/htmx/issues/916)
    [ hxExt_ "sse"
    , sseConnect_ "/events"
    ] do
      div_ [id_ "container"] do
        div_ [id_ "header"] do
          div_ [class_ "logo"] do
            "GHC profiler"
        div_ [id_ "sidenav"] do
          navHtml False [0]
        div_ [id_ "main" ] do
          p

emptyHtml :: Monad m => HtmlT m ()
emptyHtml = mempty

helloHtml :: H ()
helloHtml = do
  p_ "Hello World!"

  rtsStatsButton




data Nav = Nav
  { navTitle    :: Text  -- ^ Menu title
  , navContents :: H ()  -- ^ Page to show
  , navSubs     :: [Nav] -- ^ Sub menu entries
  }

navs :: [Nav]
navs =
  [ Nav "Welcome" welcomeHtml []
  , Nav "Hello World" helloHtml
      [ Nav "Sub item 1" "sub1" []
      , Nav "Sub item 2" "sub2" []
      , Nav "Sub item 3" "sub3" []
      ]
  ]


-- | Display the menu
navHtml :: Bool -> [Int] -> H ()
navHtml oob path = do
  let (sel1,sel2,main_html) = case path of
        []    -> (Nothing,Nothing, emptyHtml)
        [a]   -> (Just a, Nothing, navContents (navs !! a))
        a:b:_ -> (Just a, Just b,  navContents (navSubs (navs !! a) !! b))

  -- out-of-band swap of main contents
  when oob do
    div_
      [ id_ "main"
      , hxSwapOob_ "true"
      ] main_html

  -- display the whole menu with style for selected elements
  div_
    [ id_ "sidemenu"
    ] do
    forM_ (navs `zip` [0..]) \(nav,i) -> do
      let is_selected1 = sel1 == Just i
      div_
        [ hxTarget_ "#sidenav"
        , hxGet_    ("/nav/" <> pack (show i))
        , class_    "navitem"
        , if is_selected1 then class_ "selected" else mempty
        ] $ toHtml (navTitle nav)
      when is_selected1 $ do
        div_
          [ class_ "navsub"
          ] do
          forM_ (navSubs nav `zip` [0..]) \(snav,j) -> do
            let is_selected2 = sel2 == Just j
            div_
              [ hxTarget_ "#sidenav"
              , hxGet_    ("nav/"<> pack (show i) <> "/" <> pack (show j))
              , class_    "navsubitem"
              , if is_selected2 then class_ "selected" else mempty
              ] $ toHtml (navTitle snav)
              -- We don't support third level nesting (yet)
