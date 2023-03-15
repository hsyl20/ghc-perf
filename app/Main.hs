module Main where

import Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

type AppState = ()
type AppEvent = ()

data Name
  = Viewport1
  | Viewport2
  deriving (Eq,Ord,Show)

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

main :: IO ()
main = do
  let app = App
        { appDraw         = draw
        , appChooseCursor = chooseCursor
        , appHandleEvent  = handleEvent
        , appStartEvent   = startEvent
        , appAttrMap      = attributeMap
        }
  let initialState = ()
  _finalState <- defaultMain app initialState

  pure ()
