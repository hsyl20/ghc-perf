{-# LANGUAGE BlockArguments #-}

module GHC.Profiler.Compiler
  ( -- * Git revisions
    Revision (..)
  , withRevision
  , ghcRecent
  -- * Building GHC
  , GhcBuildOpts (..)
  , withGhcBindist
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

    let sh x     = (shell x)
                    { cwd = Just fp
                    }
    let ghc_sh x = (shell x)
                    { cwd = Just (fp </> "ghc")
                    }

    -- Perform shallow clone (submodules too)
    let clone    = sh     $ "git clone --depth=1 --recurse-submodules --shallow-submodules " <> ghcRepository rev
    let checkout = ghc_sh $ "git checkout " <> ghcHash rev

    putStrLn $ "Cloning GHC in " ++ fp

    putStrLn =<< readCreateProcess clone ""
    putStrLn =<< readCreateProcess checkout ""
    act (fp </> "ghc")

data GhcBuildOpts = GhcBuildOpts
  { buildFlavour        :: !String          -- ^ Hadrian flavour
  , buildTarget         :: !(Maybe String)  -- ^ Configure target
  , buildUseEmconfigure :: !Bool            -- ^ Wrap configure with emconfigure
  }

-- | Checkout a compiler revision in a temporary directory
withGhcBindist :: Revision -> GhcBuildOpts -> (FilePath -> IO a) -> IO a
withGhcBindist rev opts act = withRevision rev $ \fp -> do
    let sh x     = (shell x)
                    { cwd = Just fp
                    }

    let boot      = sh $ "./boot"
    let configure = sh $ mconcat
                      [ if buildUseEmconfigure opts then "emconfigure " else ""
                      , "./configure"
                      , case buildTarget opts of
                          Nothing  -> ""
                          Just tgt -> " --target=" <> tgt
                      ]
    let build     = sh $ mconcat
                      [ "./hadrian/build binary-dist-dir --docs=none"
                      , " -j"
                      ]


    putStrLn $ "Building GHC in " ++ fp

    putStrLn =<< readCreateProcess boot ""
    putStrLn =<< readCreateProcess configure ""
    putStrLn =<< readCreateProcess build ""

    act (fp </> "bindist")
