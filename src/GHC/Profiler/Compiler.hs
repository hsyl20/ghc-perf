{-# LANGUAGE BlockArguments #-}

module GHC.Profiler.Compiler
  ( Revision (..)
  , withRevision
  -- * Revisions
  , ghcRecent
  )
where

import System.Process
import System.IO.Temp
import System.FilePath

data Revision = Revision
  { ghcRepository :: !String -- ^ Git repository url
  , ghcHash       :: !String -- ^ Git commit
  }

ghcGitlab :: String
ghcGitlab = "https://gitlab.haskell.org/ghc/ghc.git"

-- | A recent GHC
ghcRecent :: Revision
ghcRecent = Revision ghcGitlab "98b5cf67f8428b0daefcbf5df121df0b8a126654"

-- | Checkout a compiler revision in a temporary directory
withRevision :: Revision -> (FilePath -> IO a) -> IO a
withRevision rev act = do
  -- store in system temporary directory for now. We may want to offer control
  -- over this to the user...
  withSystemTempDirectory "ghc-repo" \fp -> do

    -- it would be better to download a shallow clone if possible
    let clone = (shell ("git clone " <> ghcRepository rev))
                { cwd = Just fp
                }
    let checkout = (shell ("git checkout " <> ghcHash rev))
                { cwd = Just (fp </> "ghc")
                }

    putStrLn $ "Cloning GHC in " ++ fp

    putStrLn =<< readCreateProcess clone ""
    putStrLn =<< readCreateProcess checkout ""
    act (fp </> "ghc")
