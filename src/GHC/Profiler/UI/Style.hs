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
-----------------------------

-- | Rendered CSS style
--
-- We render it here to ensure it's a CAF
renderedCss :: Text
renderedCss = Clay.renderWith format [] css
  where
    format = compact
    --format = pretty -- enable this for debugging CSS

-- | CSS style
css :: Css
css = do
  let
    head_bg = "#0074B7"
    side_fg = black
    main_bg = white
    main_fg = black
    side_bg = lightgray
    head_fg = white

    valignCenter = do
      -- vertical alignment trick from
      -- https://stackoverflow.com/a/4416166
      position relative
      top (pct 50)
      transform (translate (px 0) (pct (-50)))
      left (px 15)

  body ? do
    fontFamily [] [sansSerif]
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
    gridTemplateRows    [ px 50, auto]
    gridTemplateColumns [ px 200, fr 4]
    gridTemplateAreas   [ [ "header", "header"]
                        , [ "sidenav","main"]
                        ]
    gridGap (px 0)

  "#header" ? do
    backgroundColor head_bg
    fontColor head_fg
    gridArea "header"

    ".logo" ? do
      fontWeight bold
      fontSize (pt 18)
      valignCenter
      

  "#sidenav" ? do
    overflow auto
    backgroundColor side_bg
    fontColor side_fg
    gridArea "sidenav"
    boxSizing borderBox
    -- resize "horizontal"
      -- TODO: Doesn't support reducing the horizontal size of the menu well...
      -- Increasing the size works well though.
      --
      -- I've tried resizable from interactjs but it doesn't work with grid
      -- layout apparently.

    ".navitem" ? do

      width (pct 100)
      textAlign center
      paddingTop    (em 0.5)
      paddingBottom (em 0.5)
      cursor pointer

      ":hover" & do
        backgroundColor "#5e9eee"

    ".navsubitem" ? do

      width (pct 100)
      textAlign center
      paddingTop    (em 0.5)
      paddingBottom (em 0.5)
      cursor pointer

      ":hover" & do
        backgroundColor "#90bcf2"

  "#main" ? do
    backgroundColor main_bg
    fontColor main_fg
    gridArea "main"
    paddingLeft (px 10)
