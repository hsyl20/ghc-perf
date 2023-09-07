{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module GHC.Profiler.UI.Style
  ( renderedCss
  )
where

import Clay
import Data.Text.Lazy (Text)

-- | Rendered CSS style
--
-- We render it here to ensure it's a CAF
renderedCss :: Text
renderedCss = Clay.renderWith format [] css
  where
    format = compact
    -- format = pretty -- enable this for debugging CSS

-- | CSS style
css :: Css
css = do
  body ? background red
