{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module GHC.Profiler.UI.Style
  ( renderedCss
  )
where

import Clay
import Clay.Stylesheet
import Data.Text.Lazy (Text)
import qualified Data.Text

-----------------------------
-- To upstream in Clay
gridTemplateRows :: [Size a] -> Css
gridTemplateRows = key "grid-template-rows" . noCommas

gridArea :: Data.Text.Text -> Css
gridArea = key "grid-area" . value

gridTemplateAreas :: [[Data.Text.Text]] -> Css
gridTemplateAreas = key "grid-template-areas" . intercalate " " . Prelude.map (bracket . noCommas)
  where
    bracket x = "\"" <> x <> "\""

resize :: Data.Text.Text -> Css
resize = key "resize"
-----------------------------

-- | Rendered CSS style
--
-- We render it here to ensure it's a CAF
renderedCss :: Text
renderedCss = Clay.renderWith format [] css
  where
    --format = compact
    format = pretty -- enable this for debugging CSS

-- | CSS style
css :: Css
css = do
  body ? do
    backgroundColor "#cac9c9"
    margin (px 0) (px 0) (px 0) (px 0)

  "#container" ? do
    border (px 0) solid black
    display grid
    height (vh 100)
    -- see https://itnext.io/simple-web-layout-with-css-grid-ec6be5086531
    -- For tablet/mobile, change grid template with:
    --  @media(max-width: 768px) {
    --    grid-template-...
    --  }
    gridTemplateRows    [ fr 2, fr 20, fr 1]
    gridTemplateColumns [ fr 1, fr 4]
    gridTemplateAreas   [ [ "header", "header"]
                        , [ "sidenav","main"]
                        , [ "footer", "footer"]
                        ]
    gridGap (px 5)

  "#header" ? do
    background green
    gridArea "header"

  "#footer" ? do
    background gray
    gridArea "footer"

  "#sidenav" ? do
    overflow auto
    background yellow
    gridArea "sidenav"
    boxSizing borderBox
    -- resize "horizontal"
      -- TODO: Doesn't support reducing the horizontal size of the menu well...
      -- Increasing the size works well though.
      --
      -- I've tried resizable from interactjs but it doesn't work with grid
      -- layout apparently.

  "#main" ? do
    background orange
    gridArea "main"
