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
  [ (,) "logo"         logo 
  , (,) "lemonsMosaic" lemonsMosaic
  ]


svgReact :: [ (FilePath , Svg) ]
svgReact =
  [ (,) "logo"      logo
  , (,) "eyeOpened" eyeOpened
  , (,) "eyeClosed" eyeClosed
  , (,) "tick"      tick
  , (,) "warning"   exclamationSignal
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