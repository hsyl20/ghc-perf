module GHC.Profiler.Responder
  ( Responder(..)
  , respondH
  , respondHtml
  , respondHtml'
  , respondLBS
  , respondText
  , ServerEvent(..)
  , sendEvent
  )
where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified GHC.Profiler.UI.Events as SSE
import GHC.Profiler.UI.Events (ServerEvent(..))
import GHC.Profiler.UI.Monad

import Lucid.Base
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

data Responder = Responder
  { resRespond :: !(Response -> IO ResponseReceived)
  , resMState  :: !MState
  }

respondHtml'
  :: Responder
  -> Status
  -> ResponseHeaders
  -> Html a
  -> IO ResponseReceived
respondHtml' res status headers html = do
  let bs = Lucid.Base.renderBS html
  resRespond res (responseLBS status headers bs)

respondHtml
  :: Responder
  -> Html a
  -> IO ResponseReceived
respondHtml res html = respondHtml' res ok200 [] html

respondH
  :: Responder
  -> H a
  -> IO ResponseReceived
respondH res h = respondHtml res =<< runH (resMState res) h

respondLBS
  :: Responder
  -> Status
  -> ResponseHeaders
  -> LBS.ByteString
  -> IO ResponseReceived
respondLBS res status headers bs = resRespond res (responseLBS status headers bs)

respondText
  :: Responder
  -> Status
  -> ResponseHeaders
  -> LT.Text
  -> IO ResponseReceived
respondText res status headers t = respondLBS res status headers (encodeUtf8 t)

sendEvent
  :: Responder
  -> ServerEvent
  -> IO ()
sendEvent res e = SSE.sendEvent (mSSE (resMState res)) e
