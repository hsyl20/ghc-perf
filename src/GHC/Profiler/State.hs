{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-- | App state
module GHC.Profiler.State
  ( AppState (..)
  , initAppState
  , withDB
  )
where

import Database.SQLite.Simple
import Control.Exception

-- | The AppState contains the dynamic runtime state.
-- The database contains the static state.
--
-- For now the runtime state is empty but it could be used to reference threads
-- currently processing a request.
--  In the DB we would have a "request" table with (req_id,req_details)
--  In the runtime state we would have "Map red_id ThreadId"
-- At program start we should cleanup the "request" table: either remove the
-- requests or restart processing them.
data AppState = AppState

initAppState :: IO AppState
initAppState = pure AppState

withDB :: (Connection -> IO a) -> IO a
withDB act = do
  db <- open "prof.db"
  act db `finally` close db
