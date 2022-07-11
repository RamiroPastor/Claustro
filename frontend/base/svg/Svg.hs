{-# LANGUAGE     OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module Svg where

import           GHC.IO.Encoding
import qualified Data.Text as T
import           Text.Blaze (customAttribute)
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A
import           Text.Blaze.Svg.Renderer.Pretty



infixl 5 .:
(.:) :: (S.AttributeValue -> S.Attribute ) -> Float -> S.Attribute
f .: x = f $ S.toValue x


main :: IO ()
main = do
  setLocaleEncoding utf8
  compileSvgStandalone
  compileSvgReact


compileSvgStandalone :: IO ()
compileSvgStandalone =
    mapM_ f svgStandalone
  where
    f (name , svgCode) =
      writeFile 
        ("../../assets/img/" ++ name ++ ".svg") 
        (renderSvg $ S.docType >> svgBullshit svgCode)


compileSvgReact :: IO ()
compileSvgReact =
    mapM_ f svgReact
  where
    f (name , svgCode) =
      writeFile 
        ("../../assets/svg/" ++ name ++ ".jsx") 
        (svgToReact name $ svgBullshit svgCode)




svgStandalone :: [ (FilePath , Svg) ]
svgStandalone =
  [ (,) "logo"          logo 
  , (,) "lemonsMosaic"  lemonsMosaic
  , (,) "squaresMosaic" interlacedSquaresMosaic
  ]


svgReact :: [ (FilePath , Svg) ]
svgReact =
  [ (,) "logo"      logo
  , (,) "eyeOpened" eyeOpened
  , (,) "eyeClosed" eyeClosed
  , (,) "tick"      tick
  , (,) "warning"   exclamationSignal
  , (,) "flagES"    flagES
  , (,) "flagGB"    flagGB
  ]




svgBullshit :: Svg -> Svg
svgBullshit svg =
  svg
    ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
    ! customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
    ! A.preserveaspectratio "xMidYMid slice"


svgToReact :: String -> Svg -> String
svgToReact name svgCode =
    "import React from 'react';"
    ++ "\n\n" ++
    "export const " ++ name ++ " = \n" ++ render svgCode
  where
    render = T.unpack . adaptToReact . T.pack . renderSvg 
    adaptToReact =
        (T.replace "xmlns:xlink"       "xmlnsXlink")
      . (T.replace "stroke-width"      "strokeWidth")
      . (T.replace "stroke-dasharray"  "strokeDasharray")
      . (T.replace "stroke-dashoffset" "strokeDashoffset")
      . (T.replace "stroke-linejoin"   "strokeLinejoin")
      . (T.replace "stroke-linecap"    "strokeLinecap")
      . (T.replace "font-family"       "fontFamily")
      . (T.replace "font-size"         "fontSize")
      . (T.replace "text-anchor"       "textAnchor")
      . (T.replace "dominant-baseline" "dominantBaseline")





--------------------------------------------------------------------------------


evenOddSplit :: [a] -> ([a], [a])
evenOddSplit [] = ([], [])
evenOddSplit (x:xs) = (x:o, e)
  where (e,o) = evenOddSplit xs



starPolygonFirstSpecies :: 
  Int -> Float -> Float -> (Float, Float) -> Svg
starPolygonFirstSpecies n strokeW radius (c1,c2) =
    S.path
      ! d directions
      ! (strokeWidth .: (2*strokeW))
  where
    α  = 2 * pi / (fromIntegral n)
    r  = radius - strokeW
    vertice k' = 
      let k = fromIntegral k'
      in 
        (,) (c1 + r * cos (k*α))
            (c2 + r * sin (k*α))
    verticesList = map vertice [0 .. (n-1)]
    directions =
      if even n 
        then 
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (fst $ evenOddSplit verticesList)
            l   (fst $ head verticesList)  (snd $ head verticesList)
            S.z
            m   (fst $ verticesList !! 1)  (snd $ verticesList !! 1)
            mapM_ (uncurry S.l) (snd $ evenOddSplit verticesList)
            l   (fst $ verticesList !! 1)  (snd $ verticesList !! 1)
            S.z
        else
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (tail $ fst $ evenOddSplit $ verticesList ++ verticesList)
            S.z


--------------------------------------------------------------------------------


logo :: Svg
logo = 
  svg
    ! A.viewbox "-1 -1 2 2"
    $ do
      cornerNE
      cornerNE ! A.transform (rotateAround  90 0 0)
      cornerNE ! A.transform (rotateAround 180 0 0)
      cornerNE ! A.transform (rotateAround 270 0 0)
      columns
      starPolygonFirstSpecies 8 0.02 0.5 (0,0)
        ! A.fill "none"
        ! A.stroke "black"
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


--------------------------------------------------------------------------------


lemonsMosaic :: Svg
lemonsMosaic =
    svg
      ! A.viewbox "0.15 0 0.85 1"
      $ do
        -- lemon
        lemon ! A.transform (                             rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate      0   (-0.5) <> rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate      0     0.5  <> rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate   0.43  (-0.25) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate   0.43    0.25  <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate   0.43    0.75  <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43) (-0.75) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43) (-0.25) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43)   0.25  <> rotateAround (-29) 0.5 0.5)
  where
    r1 = 0.24
    r2 = r1
    k  = 0.2                    -- k must be lower than r1
    x0 = 0.5
    y0 = 0.5
    y1 = y0 - k
    y2 = y0 + k
    f1 y = x0 - sqrt(r1^2 - (y - y0)^2)
    f2 y = x0 + sqrt(r1^2 - (y - y0)^2)
    lemon =
      S.path
        ! (A.strokeWidth .: 0)
        ! A.fill "rgb(235, 225, 225)"
        ! A.d lemonDirs
    lemonDirs = mkPath $ do
      m   0.5      0.15
      aa  r2       r2     0  False  True   (f1 y1)  y1
      aa  r1       r1     0  False  False  (f1 y2)  y2  
      aa  r2       r2     0  False  True   0.5      0.85
      aa  r2       r2     0  False  True   (f2 y2)  y2
      aa  r1       r1     0  False  False  (f2 y1)  y1
      aa  r2       r2     0  False  True   0.5      0.15
      S.z


--------------------------------------------------------------------------------


interlacedSquaresMosaic :: Svg
interlacedSquaresMosaic =
    svg
      ! A.viewbox "0 0 1 1"
      $ do
        cornerTopLeft
        cornerTopLeft ! A.transform (rotateAround 180 0.5 0.5)
        cornerTopLeft ! A.transform (rotateAround 90  0.25 0.25 <> translate 0.5 0  )
        cornerTopLeft ! A.transform (rotateAround 270 0.25 0.25 <> translate 0   0.5)
  where
    color1 = "rgb(100, 200, 255)"
    color2 = "rgb(150, 170, 255)"
    s = 0.03
    k1 = 0.08
    k2 = (1/3) * (0.25 - k1 + s)
    (ax, ay) = (0.25 - k1, 0.25 + k1)
    (bx, by) = (0.25 + k1, 0.25 - k1)
    cornerTopLeft =
      S.g 
        ! A.fill "none"
        ! (A.strokeWidth .: 2*s)
        ! A.strokeLinecap "round"
        $ do
          S.path ! A.stroke color1 ! A.d dirs1
          S.path ! A.stroke color2 ! A.d dirs2
          S.path ! A.stroke color1 ! A.d dirs3
          S.rect 
            ! (A.x .: ax - s)
            ! (A.y .: ay - s)
            ! (A.width  .: 2*s)
            ! (A.height .: 2*s)
            ! A.fill color1
    -----------------------------------------------------------
    dirs1 = mkPath $ do
      m   0.25  0
      l   ax    k2
      l   ax    ay
    dirs2 = mkPath $ do
      m   0     0.25
      l   k2    by
      l   bx    by
      l   bx    (0.5 - k2)
      l   0.25  0.5
    dirs3 = mkPath $ do
      m   ax    ay
      l   (0.5 - k2)  ay
      l   0.5  0.25


--------------------------------------------------------------------------------

eyeOpened :: S.Svg
eyeOpened =
  S.svg openEye ! A.viewbox "0 0 1 1"

eyeClosed :: S.Svg
eyeClosed =
  S.svg closedEye ! A.viewbox "0 0 1 1"


openEye :: S.Svg
openEye =
  do
    eyeBorder
    pupil
    glow
  where
    w  = 0.04
    c1 = 0.5
    c2 = 0.43
    cr = 0.24
    k  = 0.15
    eyeBorder =
      S.path
        ! A.d eyePath
        ! A.fill "none"
        ! (A.strokeWidth .: 2*w)
        ! A.strokeLinejoin   "round"
    eyePath = S.mkPath $ do
      S.m   w    0.5
      S.c   0.25  0.1  0.75  0.1   (1-w)  0.5
      S.c   0.75  0.9  0.25  0.9   w      0.5
      S.z
    pupil =
      S.circle
        ! (A.cx .: c1)
        ! (A.cy .: c2)
        ! (A.r  .: cr)
        ! A.stroke "none"
    glow =
      S.path
        ! A.d glowPath
        ! A.fill "none"
        ! A.stroke "white"
        ! (A.strokeWidth .: 1.5*w)
        ! A.strokeLinecap "round"
    glowPath = S.mkPath $ do
      S.m   (c1 - k)   c2
      S.aa  k  k  0  False  True  c1  (c2 - k)



closedEye :: S.Svg
closedEye =
  do
    openEye
    bar
  where
    k   = 0.1
    bar =
      S.path
        ! A.d    barPath
        ! A.fill "none"
        -- ! A.stroke "rgb(70,70,70)"
        ! A.strokeWidth "0.12"
        ! A.strokeLinecap "round"
    barPath = S.mkPath $ do
      S.m   k     (1-k)
      S.l   (1-k)  k


--------------------------------------------------------------------------------


tick :: Svg
tick =
  svg
    ! A.viewbox "0 0 1 1"
    $ S.path
      ! d directions
      ! fill "none"
      ! (strokeWidth .: (2*w))
      ! strokeLinecap  "round"
      ! strokeLinejoin "round"
  where
    w = 0.1
    k = 0.11
    directions = mkPath $ do
      m   k     0.5
      l   0.3   0.85
      l   (1-k) k


exclamationSignal :: Svg
exclamationSignal = 
  svg
    ! A.viewbox "0 0 1 1"
    $ do
      triangle
      stick
      dot
  where
    k1 = 0.1
    k2 = 0.06
    k3 = 0.035
    h1 = (1 - 2*k1)
    y1 = 0.4
    y2 = 0.65
    y3 = 0.78
    triangle =
      S.path
        ! d trianglePath
        ! (strokeWidth .: 1.5 * k1)
        ! strokeLinejoin "round"
    trianglePath = mkPath $ do
      m   0.5    (1 - k1 - h1)
      l   k1     (1-k1)
      l   (1-k1) (1-k1)
      S.z
    stick =
      S.path
        ! d stickPath
        ! fill "white"
        ! (strokeWidth .: 0)
    stickPath = mkPath $ do
      m   (0.5 - k3)  y2
      l   (0.5 + k3)  y2
      l   (0.5 + k2)  y1
      aa  k2  k2  0  True  False  (0.5 - k2)  y1
      S.z
    dot =
      S.circle
        ! (cx .: 0.5)
        ! (cy .: y3)
        ! (r  .: k2)
        ! fill "white"
        ! (strokeWidth .: 0)


--------------------------------------------------------------------------------


flagES :: Svg
flagES =
    S.svg
      ! A.viewbox "0 0 3 2"
      $ do
        redBandTop
        yellowBand
        redBandBot
  where
    colRed = "rgb(198,11,30)"
    colYellow = "rgb(255,196,0)"
    redBandTop =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 3)
        ! (A.height .: 0.5)
        ! A.stroke "none"
        ! A.fill colRed
    yellowBand =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0.5)
        ! (A.width  .: 3)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill colYellow
    redBandBot =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 1.5)
        ! (A.width  .: 3)
        ! (A.height .: 0.5)
        ! A.stroke "none"
        ! A.fill colRed



flagGB :: S.Svg
flagGB =
    S.svg
      ! A.viewbox "0 0 50 30"
      $ do
        scotland
        irelandBase
        irelandBase ! A.transform (rotateAround 180 mx my)
        englandRed
        englandWhite
  where
    w = 50
    h = 30
    mx = w / 2
    my = h / 2
    -- x0 = 3 / sin (atan (3/5))
    x1 = 2 / sin (atan (3/5))
    -- y0 = 3 / sin (atan (5/3))
    y1 = 2 / sin (atan (5/3))
    colWhite = "white"
    colBlue = "rgb(1,33,105)"
    colRed = "rgb(200,16,46)"
    scotland = do
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: w)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill colBlue
      S.path
        ! (A.strokeWidth .: 6)
        ! A.stroke colWhite
        ! A.fill "none"
        ! A.d scotlandDirs
    scotlandDirs = mkPath $ do
      m   0  0
      l   w  h
      m   0  h
      l   w  0
    irelandBase =
      S.path
        ! A.stroke "none"
        ! A.fill colRed
        ! A.d irelandDirs
    irelandDirs = mkPath $ do
      m   0         0
      l   0         y1
      l   (mx - x1) my
      l   mx        my
      S.z
      m   0         h
      l   x1        h
      l   mx        (my + y1)
      l   mx        my
      S.z
    englandRed =
      S.path 
        ! (A.strokeWidth .: 6)
        ! A.stroke colRed
        ! A.fill "none"
        ! A.d englandDirsRed
    englandWhite =
      S.path
        ! (A.strokeWidth .: 2)
        ! A.stroke colWhite
        ! A.fill "none"
        ! A.d englandDirsWhite
    englandDirsRed = mkPath $ do
      m   0   my
      l   w   my
      m   mx  0
      l   mx  h
    englandDirsWhite = mkPath $ do
      m   0         (my + 4)
      l   (mx - 4)  (my + 4)
      l   (mx - 4)  h
      m   (mx + 4)  h
      l   (mx + 4)  (my + 4)
      l   w         (my + 4)
      m   w         (my - 4)
      l   (mx + 4)  (my - 4)
      l   (mx + 4)  0
      m   (mx - 4)  0
      l   (mx - 4)  (my - 4)
      l   0         (my - 4)


--------------------------------------------------------------------------------