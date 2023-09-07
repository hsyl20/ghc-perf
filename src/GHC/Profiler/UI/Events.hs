{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server-sent events
--
-- Adapted wai-extra:Network.Wai.EventSource
module GHC.Profiler.UI.Events
  ( ServerEvent(..)
  , SSE
  , initSSE
  , responseSSE
  , sendEvent
  )
where

import Data.ByteString.Builder
import Control.Concurrent.Chan
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types
import Control.Monad
import Control.Monad.IO.Class

-- Server-sent events
data ServerEvent
  = ServerEvent
    { eventName :: Maybe Builder
    , eventId   :: Maybe Builder
    , eventData :: [Builder]
    }
  | CommentEvent Builder
  | RetryEvent Int
  deriving (Show)

newtype SSE = SSE (Chan ServerEvent)

initSSE :: IO SSE
initSSE = SSE <$> newChan

sendEvent :: SSE -> ServerEvent -> IO ()
sendEvent (SSE ch) se = writeChan ch se

-- | Send events from the given SSE as a streamed response
responseSSE :: SSE -> IO Response
responseSSE (SSE ch') = do
  ch <- liftIO $ dupChan ch'
  pure $ responseStream status200 [(hContentType, "text/event-stream")]
    \write flush -> void do
      write (eventToBuilder (CommentEvent "Ready!"))
      flush
      let loop = do
            se <- readChan ch
            write (eventToBuilder se)
            flush
            loop
      loop

nl, nameField, idField, dataField, retryField, commentField :: Builder
nl            = char7 '\n'
nameField     = string7 "event:"
idField       = string7 "id:"
dataField     = string7 "data:"
retryField    = string7 "retry:"
commentField  = char7 ':'

field :: Builder -> Builder -> Builder
field l b = l `mappend` b `mappend` nl


eventToBuilder :: ServerEvent -> Builder
eventToBuilder (CommentEvent txt) = field commentField txt
eventToBuilder (RetryEvent   n)   = field retryField (string8 . show $ n)
eventToBuilder (ServerEvent n i d)= name n (evid i $ mconcat (map (field dataField) d)) `mappend` nl
  where
    name Nothing   = id
    name (Just n') = mappend (field nameField n')
    evid Nothing   = id
    evid (Just i') = mappend (field idField   i')
