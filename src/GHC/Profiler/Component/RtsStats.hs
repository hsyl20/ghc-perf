module GHC.Profiler.Component.RtsStats
  ( rtsStats
  , rtsStatsButton
  )
where

import GHC.Profiler.Component
import GHC.Profiler.Responder
import GHC.Profiler.State
import GHC.Profiler.UI.Monad
import GHC.Profiler.UI.Html

import Network.Wai
import Network.HTTP.Types.Status
import Data.Text (Text,pack)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Process
import System.FilePath
import System.IO.Temp
import Database.SQLite.Simple
import Data.ByteString.Builder

rtsStats :: Component
rtsStats = Component
  { compName    = comp_name
  , compRespond = comp_respond
  }

comp_name :: Text
comp_name = "rts_stats"

comp_respond :: Request -> Responder -> IO ResponseReceived
comp_respond req res = do
  -- match on request and respond
  case pathInfo req of
    [_, "status", n] -> do
      withDB \db -> do
        rs <- queryNamed db "SELECT done,code,stdout,stderr FROM rts_stats WHERE id = :id"
                [ ":id" := n
                ]
        case rs of
          [((done :: Integer),(code :: Text),(out :: Text), (err :: Text))] -> case done of
            0 -> respondHtml res "No GHC result"
            _ -> respondHtml res do
              p_ [] do
                "GHC terminated with: "
                toHtml code
              "Stdout:"
              pre_ [] (toHtml out)
              "Stderr:"
              pre_ [] (toHtml err)
          []  -> respondHtml res "Invalid ID"
          _   -> respondHtml res "Too many results. Expected one."

    [_, "start"]   -> do
      res_id <- withDB \db -> do
        execute_ db "CREATE TABLE IF NOT EXISTS rts_stats \n\
                    \( id INTEGER PRIMARY KEY\
                    \, project INTEGER\
                    \, done INTEGER\
                    \, code TEXT\
                    \, stdout TEXT\
                    \, stderr TEXT\
                    \)"
        execute db "INSERT INTO rts_stats (done) VALUES (0)" ()
        lastInsertRowId db

      -- spawn the request
      void $ forkIO $ withSystemTempDirectory "ghc-prof" \fp -> do
        let p = fp </> "HelloWorld.hs"
        Prelude.writeFile p
          "module Main where\n\
          \main :: IO ()\n\
          \main = putStrLn \"Hello World\""
        (code,out,err) <- readCreateProcessWithExitCode ((shell ("ghc " <> p <> " +RTS -s"))
          { cwd = Just fp
          })
          ""
        -- store result in DB
        withDB \db -> do
          executeNamed db "UPDATE rts_stats SET done = 1, code = :code, stdout = :stdout, stderr = :stderr WHERE id = :id"
            [ ":code"   := show code
            , ":stdout" := out
            , ":stderr" := err
            , ":id"     := res_id
            ]

        -- signal that result arrived
        let event = ServerEvent
              { eventName = Just $ byteString "status_update_" <> integerDec (fromIntegral res_id)
              , eventId   = Nothing
              , eventData = [""]
              }
        sendEvent res event

      respondHtml res do
        -- this is the html that is sent first and that will then be updated
        -- with the result
        div_
          [ hxGet_ ("/" <> comp_name <> "/status/" <> pack (show res_id))
          , hxTrigger_ ("sse:status_update_" <> pack (show res_id))
          ] do
          "Processing... Please wait."

    _             -> respondLBS res status404 [] ""


rtsStatsButton :: H ()
rtsStatsButton = do
  button_
    [ hxPost_ $ "/" <> comp_name <> "/start"
    , hxSwap_ "outerHTML"
    ] do
    "Build HelloWorld with GHC"

  rs <- liftIO $ withDB \db -> do
    query_ db "SELECT id FROM rts_stats"

  when (not (null rs)) do
    p_ "Previous runs:"
    ul_ [] do
      forM_ rs \(Only (rowid :: Integer)) -> do
        li_ do
          toHtml (show rowid)
          " - "
          a_
            [ hxGet_  ("/" <> comp_name <> "/status/" <> pack (show rowid))
            , hxSwap_ "outerHTML"
            , href_ "#"
            ]
            "Details"
