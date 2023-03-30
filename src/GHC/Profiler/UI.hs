-- | App UI
module GHC.Profiler.UI
  ( runState
  )
where

import Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

import GHC.Profiler.State

type AppEvent = ()

data Name
  = Viewport1
  | Viewport2
  deriving (Eq,Ord,Show)

-- | Run Brick application on the given state
runState :: AppState -> IO AppState
runState = defaultMain app

app :: App AppState AppEvent Name
app = App
  { appDraw         = draw
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = startEvent
  , appAttrMap      = attributeMap
  }

ui :: Widget Name
ui = str "Hello World"

draw :: AppState -> [Widget Name]
draw _ = [ui]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent ev = do
  case ev of
    VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt
    _                                    -> pure ()
  pure ()

startEvent :: EventM Name AppState ()
startEvent = pure ()

attributeMap :: AppState -> AttrMap
attributeMap _ = attrMap defAttr []

