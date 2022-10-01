{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Arrow (second)
import SvgIcons.Core.Geometry
import SvgIcons.Core.Render
import SvgIcons.Core.Style
import SvgIcons.Core.Utils
import SvgIcons.Icons
import SvgIcons.Images
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A



main :: IO ()
main = do
  makeSvgStandalone
  makeIcons
  makeImages



makeSvgStandalone :: IO ()
makeSvgStandalone =
  renderSvgFiles
    "./frontend/assets/img/"
    [ (,) "logo"         (svg logo ! A.viewbox "-1 -1 2 2")
    , (,) "lemonsMosaic" (lemonsMosaic "rgb(235, 225, 225)")
    , (,) "peopleMosaic" (peopleMosaic "lightgray" "white" ! A.preserveaspectratio "none")
    , (,) "hexMosaic"    (hexMosaic    "silver")
    , (,) "nazariMosaic" (nazariMosaic "rgb(245,245,245)" "transparent")
    , (,) "arabicMosaic" (arabicMosaic "rgb(200, 200, 200)" "rgb(190, 190, 250)")
    ]



makeIcons :: IO ()
makeIcons =
  renderSvgReact
    "./frontend/assets/svg/"
    $ map 
      ( stdDims  `second` )
      [ (,) "eyeClosed" (eyeStriked  ! A.strokeWidth "0.2" )
      , (,) "eyeOpened" (eyeOpened   ! A.strokeWidth "0.2" )
      , (,) "cancel"    (cancel      ! A.strokeWidth "0"   )
      , (,) "tick"      (accept      ! A.strokeWidth "0"   )
      , (,) "warning"   (warning     ! A.strokeWidth "0"   )
      , (,) "company"   (company     ! A.strokeWidth "0.05" ! A.fill "white")
      , (,) "people"    (people      ! A.strokeWidth "0.1"  ! A.fill "white")
      , (,) "key"       (keyWithArc  ! A.strokeWidth "0"   )
      , (,) "document"  (document    ! A.strokeWidth "0.1" )
      , (,) "cogwheel"  (cog9        ! A.strokeWidth "0.1" )
      , (,) "archive"   (archive     ! A.strokeWidth "0.1" )
      , (,) "cogset"    (cog6        ! A.strokeWidth "0.1"  ! A.fill "none")
      , (,) "carnet"    (carnet      ! A.strokeWidth "0.05")
      , (,) "pencil"    (pencil      ! A.strokeWidth "0.1"  ! A.fill "none" ! A.transform (rotateAround 45 0 0))
      , (,) "envelope"  (envelope    ! A.strokeWidth "0.1"  ! A.fill "none")
      , (,) "minimize"  (minimize    ! A.strokeWidth "0.1")
      , (,) "maximize"  (maximize    ! A.strokeWidth "0.1")
      , (,) "lock"      (lock        ! A.strokeWidth "0"  )
      , (,) "pin"       (pin         ! A.strokeWidth "0.1"  ! A.fill "none")
      ]



makeImages :: IO ()
makeImages =
  renderSvgReact
    "./frontend/assets/svg/"
    [ (,) "flagES" (es ! A.preserveaspectratio "xMidYMid slice")
    , (,) "flagGB" (uk ! A.preserveaspectratio "xMidYMid slice")
    ]



logo :: Svg
logo = 
    do
      cornerNE
      cornerNE ! A.transform (rotateAround  90 0 0)
      cornerNE ! A.transform (rotateAround 180 0 0)
      cornerNE ! A.transform (rotateAround 270 0 0)
      columns
      starPolygonFirstSpecies 8 0.5 (0,0)
        ! A.fill "none"
        ! A.stroke "black"
        ! (A.strokeWidth .: 0.02)
  where
    w = 0.05
    cornerNE = 
      S.path 
        ! A.stroke "black"
        ! (A.strokeWidth .: 2*w)
        ! A.fill "none"
        ! A.d cornerDirs
    cornerDirs = mkPath $ do
      m   0.15     (w - 1)
      l   (1 - w)  (w - 1)
      l   (1 - w)  (-0.15)
    columns = 
      S.path 
        ! A.stroke "black"
        ! (A.strokeWidth .: w)
        ! A.strokeLinejoin "round"
        ! A.strokeLinecap  "round"
        ! A.strokeDasharray "0 0.2"
        ! A.fill "none"
        ! A.d columnsDirs
    columnsDirs = mkPath $ do
      m     0.7    0.7
      l   (-0.7)   0.7
      l   (-0.7) (-0.7)
      l     0.7  (-0.7)
      S.z
