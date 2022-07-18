{-# LANGUAGE     OverloadedStrings       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module Svg where

import           GHC.IO.Encoding
import           Data.List (intersperse)
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
  , (,) "peopleMosaic"  peopleMosaic
  , (,) "hexMosaic1"    hexMosaic1
  ]


svgReact :: [ (FilePath , Svg) ]
svgReact =
  [ (,) "logo"      logo
  , (,) "eyeOpened" eyeOpened
  , (,) "eyeClosed" eyeClosed
  , (,) "cancel"    cancel
  , (,) "tick"      tick
  , (,) "warning"   exclamationSignal
  , (,) "flagES"    flagES
  , (,) "flagGB"    flagGB
  , (,) "document"  documentWithPencil
  , (,) "key"       keyWithCircle
  , (,) "people"    people
  , (,) "carnet"    carnet
  , (,) "cogwheel"  cogwheel9
  , (,) "cogset"    cogwheelSet
  , (,) "envelope"  envelope
  , (,) "pencil"    bigLeanedPencil
  , (,) "company"   company
  , (,) "archive"   archive
  , (,) "minimize"  minimize
  , (,) "maximize"  maximize
  , (,) "pin"       pin
  , (,) "lock"      lock
  ]




svgBullshit :: Svg -> Svg
svgBullshit svg =
  svg
    ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
    ! customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"


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
      . (T.replace "stroke-miterlimit" "strokeMiterlimit")



horizontalMirrorSymmetry :: S.Svg -> S.Svg
horizontalMirrorSymmetry s =
  s ! A.transform (S.matrix (-1) 0 0 1 1 0)

verticalMirrorSymmetry :: S.Svg -> S.Svg
verticalMirrorSymmetry s =
  s ! A.transform (S.matrix 1 0 0 (-1) 0 1)


-- frame takes the parameters of the viewbox
frame :: Float -> Float -> Float -> Float -> S.Svg
frame x y w h =
    S.path
      ! A.fill "none"
      ! A.stroke "black"
      ! A.strokeWidth "0.01"
      ! A.d frameDirs
  where
    frameDirs = mkPath $ do
      m   x       y
      l   x      (y + h)    
      l  (x + w) (y + h)
      l  (x + w)  y
      S.z
      m  (x + w/2)  y
      l  (x + w/2) (y + h)
      m   x        (y + h/2)
      l  (x + w)   (y + h/2)



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
    color1 = "rgb(200, 200, 200)"
    color2 = "rgb(190, 190, 250)"
    s = 0.035
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


peopleMosaic :: Svg
peopleMosaic =
  S.svg
    ! A.viewbox "0 0 1 1"
    ! A.preserveaspectratio "none"
    $ do
      part1
      part2
  where
    part1 = S.g $ do
      mainLine
      mainLine ! A.transform (translate 0.5 0.5) 
    part2 = 
      horizontalMirrorSymmetry part1
    mainLine =
      S.path
        ! A.fill "none"
        ! A.stroke "lightgray"
        ! (A.strokeWidth .: 0.025)
        ! A.d mainPath
    mainPath = mkPath $ do
      m   0.5  0
      q   0.2  0.1  0.3  0.25
      q   0    0.3  0    0.5


--------------------------------------------------------------------------------


hexMosaic1 :: Svg
hexMosaic1 =
  S.svg
    ! A.viewbox (S.toValue $ concat $ intersperse " " $ map show [vbX, vbY, vbW, vbH])
    $ do
      -- frame vbX vbY vbW vbH
      S.defs baseHexDef
      baseHex  ! A.transform (translate 0 ((-3) * k))
      baseTile
      baseTile ! A.transform (translate ((-3) * k * cos30) ((-3) * k * sin30))
      baseTile ! A.transform (translate (  3  * k * cos30) ((-3) * k * sin30))
      baseTile ! A.transform (translate ((-3) * k * cos30) (  3  * k * sin30))
      baseTile ! A.transform (translate (  3  * k * cos30) (  3  * k * sin30))
  where
    vbX = (-1) * 0.5 * vbW
    vbY = (-1) * 0.5 * vbH
    vbW = 6 * k * cos30
    vbH = 3 * k
    k = 1  -- side of base hex
    cos30 = 0.5 * sqrt 3
    sin30 = 0.5
    baseHex = S.use ! A.xlinkHref "#hexMosaic1_baseHex"
    baseTile = 
      S.g $ do
        baseHex
        baseHex ! A.transform (rotateAround 120 0 0)
        baseHex ! A.transform (rotateAround 240 0 0)
    baseHexDef =
      S.path 
        ! A.id_ "hexMosaic1_baseHex"
        ! A.fill "none"
        ! A.stroke "silver"
        ! A.strokeWidth "0.05"
        ! A.strokeLinecap "round"
        ! A.strokeLinejoin "round"
        ! A.d baseHexDirs
    baseHexDirs = S.mkPath $ do
      m   ( k * cos30) (k * sin30)
      l       0           0
      l       0           k
      lr  ( k * cos30) (k * sin30)
      m       0           0
      l   (-k * cos30) (k * sin30)
      lr      0           k
      lr  ( k * cos30) (k * sin30)
      ---
      m   (      k * cos30) (      k * sin30 + 1/3 * k)
      l   (1/3 * k * cos30) (1/3 * k * sin30 + 1/3 * k)
      l   (1/3 * k * cos30) (1/3 * k * sin30 + 2/3 * k)
      l   (      k * cos30) (      k * sin30 + 2/3 * k)
      ---
      m   (2/3 * k * cos30)        (2/3 * k * sin30 + 4/3 * k)
      lr  ( -1 * k * cos30)        ( -1 * k * sin30          )
      lr         0                 (2/3 * k * (-1)           )
      lr  (1/3 * k * cos30 * (-1)) (1/3 * k * sin30          )
      lr         0                 (2/3 * k                  )
      lr  (      k * cos30)        (      k * sin30)




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


cancel :: Svg
cancel =
  svg equis
    ! A.viewbox "0 0 1 1"
    ! A.preserveaspectratio "xMidYMid slice"


cancelSmall :: Svg
cancelSmall =
  svg equis
    ! A.viewbox "-0.2 -0.2 1.4 1.4"
    ! A.preserveaspectratio "xMidYMid slice"


equis :: Svg
equis =
  S.path
    ! d directions
    ! fill "none"
    ! (strokeWidth .: (2*w))
    ! strokeLinecap "round"
  where
    w = 0.1
    directions = mkPath $ do
      m   w      w
      l   (1-w)  (1-w)
      m   (1-w)  w
      l   w      (1-w)


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
      ! A.preserveaspectratio "xMidYMid slice"
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
      ! A.preserveaspectratio "xMidYMid slice"
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


pencil :: Svg
pencil =
    S.path
      ! d pencilPath
      ! (strokeWidth .: 2*s)
      ! strokeLinejoin "round"
      ! fill "none"
  where
    s  = 0.008  -- half-width of the stroke
    w  = 0.1   -- width of the pencil
    x1 = 0.5 - w/2
    x2 = 0.5 + w/2
    y1 = 0.3
    y2 = 0.34
    y3 = 0.36
    y4 = 0.6
    y5 = y6 - 0.04
    y6 = 0.7
    pencilPath = mkPath $ do
      m   0.5  y5
      l   0.5  y6
      l   x1   y4
      l   x2   y4
      l   0.5  y6
      m   x1   y4
      l   x1   y3
      l   x2   y3
      l   x2   y4
      m   0.5  y4
      l   0.5  y3
      m   x1   y3
      l   x1   y2
      l   x2   y2
      l   x2   y3
      m   x1   y2
      l   x1   y1
      m   x2   y1
      l   x2   y2
      m   x1   y1
      aa  (w/2) 0.03 0 True True x2 y1



documentWithPencil :: Svg
documentWithPencil = 
    S.svg 
      ! A.viewbox "0 0 1 1"
      $ do
        positionedPencil
        paperBorder
        textLines
        xMark
  where
    positionedPencil =
      pencil
        ! transform ( (translate 0.31 0.19) <> (rotateAround 45 0.5 0.5) )
    --------------------------------------------------
    paperW = 0.72 - paperS
    paperS = 0.025
    paperBorder =
      S.path
        ! (strokeWidth .: 2*paperS)
        ! strokeLinejoin "round"
        ! strokeLinecap  "round"
        ! fill "none"
        ! d paperPath
    paperPath = mkPath $ do
      m   (0.5 + paperW/2)  0.42
      l   (0.5 + paperW/2)  paperS
      l   (0.5 - paperW/2)  paperS
      l   (0.5 - paperW/2)  (1-paperS)
      l   (0.5 + paperW/2)  (1-paperS)
      l   (0.5 + paperW/2)  0.85
    --------------------------------------------------
    textLines =
      S.path
        ! (strokeWidth .: 0.04)
        ! fill "none"
        ! d textPath
    x1 = 0.28
    x2 = 1 - x1
    y1 = 0.2
    y2 = 0.35
    y3 = 0.5
    textPath = mkPath $ do
      m  x1  y1   >>   l  x2  y1
      m  x1  y2   >>   l  x2  y2
      m  x1  y3   >>   l  x2  y3
    --------------------------------------------------
    mX = 0.33
    mY = 0.71
    k  = 0.1
    xMark =
      S.path
        ! (strokeWidth .: 0.04)
        ! strokeLinecap "round"
        ! fill "none"
        ! d xMarkPath
    xMarkPath = mkPath $ do
      m  mX        mY
      l  (mX + k)  (mY + k)
      m  (mX + k)  mY
      l  mX        (mY + k)


--------------------------------------------------------------------------------


keyWithCircle :: S.Svg
keyWithCircle =
    svg
      ! A.viewbox "0 0 1 1"
      $ do
        key
        arc
  where
    w  = 0.05
    x0 = 0.25
    x1 = 0.3
    x2 = 0.65
    y1 = 0.6
    r1 = (x1 - w) / 2
    r2 = 0.5 - w
    y2 = 0.5 - ( sqrt $ r2^2 - (0.5 - x0)^2 )
    key =
      S.path
        ! (A.strokeWidth .: 0.09)
        ! A.fill "none"
        ! A.strokeLinecap "round"
        ! A.d keyPath
    keyPath = mkPath $ do
      m   x1   0.499
      aa  r1   r1   0  True False  x1  0.5
      l   x2   0.5
      l   x2   y1
      m   0.5  0.5
      l   0.5  y1
    arc =
      S.path
        ! (A.strokeWidth .: w)
        ! A.fill "none"
        ! A.strokeLinecap "round"
        ! A.d arcPath
    arcPath = mkPath $ do
      m   x0  y2
      aa  r2  r2   0  True True  x0 (1-y2)


--------------------------------------------------------------------------------


people :: S.Svg
people =
  svg
    ! A.viewbox "0 0 1 1"
    $ do
      person ! A.transform (translate   0.2  (-0.2))
      person ! A.transform (translate (-0.2) (-0.2))
      person
  where
    s  = 0.025
    kx = 0.25
    ky = 0.8
    kr = (1 - 2*kx) / 2
    person = 
      S.g $ do
        simpleShoulders
        simpleHead
    simpleHead =
      circle
        ! cx "0.5"
        ! cy "0.45"
        ! r  "0.15"
        ! (strokeWidth .: 2*s)
        ! fill "white"
    ----------------------------------------
    simpleShoulders =
      S.path
        ! d    shouldersPath
        ! (strokeWidth .: 2*s)
        ! fill "white"
    shouldersPath =
      mkPath $ do
        m  kx  ky
        aa kr  kr   0 True True (1 - kx) ky
        aa kr  0.15 0 True True kx       ky


--------------------------------------------------------------------------------


carnet :: S.Svg
carnet =
  svg
    ! A.viewbox "0 0 1 1"
    $ do
      cardBorder
      textLines
      photoHead
      photoShoulders
  where
    -- commonColor = "#000"
    ----------------------------------------
    w1 = 0.02
    x1 = 1.618 * y1
    y1 = 0.29
    cardBorder =
      S.path
        ! strokeLinejoin "round"
        ! (strokeWidth .: 2*w1)
        -- ! stroke commonColor
        ! fill   "transparent"
        ! d cardBorderPath
    cardBorderPath =
      mkPath $ do
        m   (0.5 - x1)  (0.5 - y1)
        l   (0.5 + x1)  (0.5 - y1)
        l   (0.5 + x1)  (0.5 + y1)
        l   (0.5 - x1)  (0.5 + y1)
        S.z
    ----------------------------------------
    h1 = 0.38
    h2 = 0.5
    h3 = 0.62
    k1 = 0.58
    k2 = 0.83
    textLines =
      S.path
        ! strokeWidth "0.035"
        -- ! stroke commonColor
        ! strokeLinecap "round"
        ! d textLinesPath
    textLinesPath =
      mkPath $ do
        m k1 h1   >>   l k2 h1
        m k1 h2   >>   l k2 h2
        m k1 h3   >>   l k2 h3
    ----------------------------------------
    q1 = 1 - k2
    q2 = 1 - k1
    qm = (q1+q2)/2
    photoHead =
      circle
        ! (cx .: qm)
        ! (cy .: ((h1+h2)/2))
        ! r "0.07"
        -- ! fill commonColor
        ! stroke "none"
    photoShoulders =
      S.path
        -- ! fill commonColor
        ! d shoulders
        ! stroke "none"
    shoulders =
      mkPath $ do
        m   q1  h3
        aa  (q2 - qm) (q2 - qm) 0 True True q2 h3
        aa  (q2 - qm) 0.025     0 True True q1 h3


--------------------------------------------------------------------------------


cogwheel :: Int -> Float -> S.Svg
cogwheel n eps =
    S.path
      ! A.fill "none"
      ! (A.strokeWidth .: 0.04)
      ! A.d cogPath
  where
    r1 = 0.20 :: Float
    r2 = 0.36 :: Float
    r3 = 0.47 :: Float
    a  = (2 * pi) / (2 * fromIntegral n)
    makeAngles k'  =
      let k = fromIntegral k'
      in [ k*a - eps, k*a + eps ]
    makePoint r α = ( 0.5 + r * cos α , 0.5 + r * sin α)
    outer = map (makePoint r3) $ concatMap makeAngles $ filter even [0 .. 2*n]
    inner = map (makePoint r2) $ concatMap makeAngles $ filter odd  [0 .. 2*n]
    f ((a1,a2):(b1,b2):outs) ((c1,c2):(d1,d2):ins) = do
      l  a1 a2
      l  b1 b2
      l  c1 c2
      aa r2 r2 0 False True d1 d2
      f outs ins
    f _ _ = S.z
    cogPath = mkPath $ do
      m (0.5 + r1) 0.5
      aa r1 r1 0 True False (0.5 - r1) 0.5
      aa r1 r1 0 True False (0.5 + r1) 0.5
      m (fst $ head outer) (snd $ head outer)
      f outer inner


cogwheel9 :: S.Svg
cogwheel9 =
  S.svg (cogwheel 9 0.15)
    ! A.viewbox "0 0 1 1"


cogwheelSet :: S.Svg
cogwheelSet = 
  S.svg 
    ! A.viewbox "0 0 1 1"
    $ do
      cogwheel 21 0.04 ! A.transform ( translate 0    0    <> S.scale 0.65 0.65 )
      cogwheel 17 0.04 ! A.transform ( translate 0.5  0.42 <> S.scale 0.5  0.5  )
      cogwheel 13 0.05 ! A.transform ( translate 0.18 0.62 <> S.scale 0.35 0.35 )

--------------------------------------------------------------------------------


envelope :: Svg
envelope =
    S.svg 
      ! A.viewbox "0 0 1 1"
      $ S.path
        ! d envelopePath
        ! fill "none"
        ! (strokeWidth .: 2*s)
        ! strokeLinejoin "round"
  where
    s = 0.03         -- half-width of the stroke
    h = 0.618 - 2*s  -- height of the whole envelope
    k = 0.58         -- y-coordinate of the middle point
    ohShit = (/)
      (0.309 - 2*s + 2*k*s - 0.618*s + 4*s*s - 4*k*s*s)
      (2*k - 2*s - 0.382 - 4*k*s + 4*s*s + 0.764*s)
    (a1,a2) = (,)  s      ((1-h)/2)      -- top left corner
    (b1,b2) = (,)  s      (1 - (1-h)/2)  -- bottom left corner
    (c1,c2) = (,)  (1-s)  (1 - (1-h)/2)  -- bottom right corner
    (d1,d2) = (,)  (1-s)  ((1-h)/2)      -- top right corner
    (e1,e2) = (,)  0.5    k              -- middle point
    (f1,f2) = (,)  ohShit 0.5
    (g1,g2) = (,)  (1-f1) 0.5
    envelopePath = mkPath $ do
      m   a1 a2
      l   b1 b2
      l   c1 c2
      l   d1 d2
      l   a1 a2
      l   e1 e2
      l   d1 d2
      m   b1 b2
      l   f1 f2
      m   c1 c2
      l   g1 g2


--------------------------------------------------------------------------------


bigPencil :: Svg
bigPencil =
    S.path
      ! d pencilPath
      ! (strokeWidth .: 2*s)
      ! strokeLinejoin "round"
      ! fill "none"
  where
    s  = 0.03  -- half-width of the stroke
    w  = 0.3   -- width of the pencil
    x1 = 0.5 - w/2
    x2 = 0.5 + w/2
    y1 = 0.05
    y2 = 0.12
    y3 = 0.20
    y4 = 0.7
    y5 = y6 - 0.09
    y6 = 0.97
    pencilPath = mkPath $ do
      m   0.5  y5
      l   0.5  y6
      l   x1   y4
      l   x2   y4
      l   0.5  y6
      m   x1   y4
      l   x1   y3
      l   x2   y3
      l   x2   y4
      m   0.5  y4
      l   0.5  y3
      m   x1   y3
      l   x1   y2
      l   x2   y2
      l   x2   y3
      m   x1   y2
      l   x1   y1
      m   x2   y1
      l   x2   y2
      m   x1   y1
      aa  (w/2) 0.03 0 True True x2 y1



bigLeanedPencil :: S.Svg
bigLeanedPencil =
  svg
    ! A.viewbox "0 0 1 1"
    $ bigPencil ! transform (rotateAround 45 0.5 0.5)


--------------------------------------------------------------------------------


company :: S.Svg
company =
    svg
      ! A.viewbox "0.05 0.05 0.9 0.9"
      $ do
        leftBuilding
        leftWindows
        leftDoor
        rightBuilding
        rightWindows
  where
    w = 0.02
    x1 = 0.12
    x2 = x1 + 0.1
    x3 = x4 - 0.1
    x4 = 0.6
    x5 = 1 - x1
    y1 = 0.10
    y2 = 0.15
    y3 = (y1 + y4) / 2
    y4 = 0.35
    y5 = y4 + 0.05
    y6 = y7 - 0.05
    y7 = 1 - y1
    doorH = 0.12
    ----------------------------------------
    leftBuilding =
      S.path
        ! d leftBuildingPath
        ! (strokeWidth .: 2*w)
        ! fill "none"
    leftBuildingPath =
      mkPath $ do
        m   x1  y7
        l   x1  y2
        l   x2  y2
        l   x2  y1
        l   x3  y1
        l   x3  y2
        l   x4  y2
        l   x4  y7
        S.z
    rightBuilding =
      S.path
        ! d rightBuildingPath
        ! (strokeWidth .: 2*w)
        ! fill "none"
    rightBuildingPath =
      mkPath $ do
        m   x4  y4
        l   x5  y4
        l   x5  y7
        l   x4  y7
    ----------------------------------------
    leftDoor =
      S.path
        ! d (mkPath $   m ((x1+x4)/2) y7 >> l ((x1+x4)/2) (y7 - doorH) )
        ! strokeWidth "0.08"
        ! fill "none"
    ----------------------------------------
    k1 = (x3 - x2) / 3
    leftWindows =
      S.path
        ! d leftWindowsPath
        ! (strokeWidth .: 2*w)
        ! strokeDasharray (S.toValue $ (show $ 3*w) ++ " " ++ (show w))
        ! fill "none"
    leftWindowsPath =
      mkPath $ do
        m   (x2 + 0*k1)  y3
        l   (x2 + 0*k1)  y6
        m   (x2 + 1*k1)  y3
        l   (x2 + 1*k1)  (y7 - doorH)
        m   (x2 + 2*k1)  y3
        l   (x2 + 2*k1)  (y7 - doorH)
        m   (x2 + 3*k1)  y3
        l   (x2 + 3*k1)  y6
    ----------------------------------------
    k2 = ((x5-w) - (x4+w)) / 4
    rightWindows =
      S.path
        ! d rightWindowsPath
        ! (strokeWidth .: 2*w)
        ! (strokeDasharray .: 2*w)
        ! fill "none"
    rightWindowsPath =
      mkPath $ do
        m   (x4 + w + 1*k2)  y5
        l   (x4 + w + 1*k2)  y6
        m   (x4 + w + 2*k2)  y5
        l   (x4 + w + 2*k2)  y6
        m   (x4 + w + 3*k2)  y5
        l   (x4 + w + 3*k2)  y6


--------------------------------------------------------------------------------


archive :: S.Svg
archive = 
  S.svg
    ! A.viewbox "0 0 1 1"
    $ do
      archiveBody
      archiveHandle (0.5 - (1 - s)/3)
      archiveHandle 0.5
      archiveHandle (0.5 + (1 - s)/3)
  where
    s = 0.02
    x1 = 0.4
    x2 = 0.1
    y0 = 0.02
    archiveBody =
      S.path
        ! A.fill "none"
        ! (A.strokeWidth .: 2*s)
        ! A.d archiveDirs
    archiveDirs = mkPath $ do
      m   (0.5 - x1)  s
      l   (0.5 - x1)  (1 - s)
      l   (0.5 + x1)  (1 - s)
      l   (0.5 + x1)  s
      S.z
      m   (0.5 - x1)  (s + 1/3 * (1 - s))
      l   (0.5 + x1)  (s + 1/3 * (1 - s))
      m   (0.5 - x1)  (s + 2/3 * (1 - s))
      l   (0.5 + x1)  (s + 2/3 * (1 - s))
    archiveHandle ky =
      S.path
        ! A.fill "none"
        ! (A.strokeWidth .: 2*s)
        ! A.d (handleDirs ky)
    handleDirs ky = mkPath $ do
      m   (0.5 - x2)  (ky - y0)
      aa  y0  y0  0  True  False  (0.5 - x2 + y0)  (ky + y0)
      l   (0.5 + x2 - y0)  (ky + y0)
      aa  y0  y0  0  True  False  (0.5 + x2)  (ky - y0)


--------------------------------------------------------------------------------


minimize :: Svg
minimize =
  S.svg 
    ! A.viewbox "-1 -1 2 2"
    $ S.path
      ! A.fill "none"
      ! A.strokeWidth "0.16"
      ! A.d (mkPath $ m (-0.7) 0  >>  l (0.7) 0)


--------------------------------------------------------------------------------


maximize :: Svg
maximize =
  S.svg
    ! A.viewbox "-1 -1 2 2"
    $ do
      frontSquare
  where
    s = 0.07
    w = 1.2
    k = 0.25
    frontSquare =
      S.path
        ! A.fill "none"
        ! (A.strokeWidth .: 2*s)
        ! A.transform (translate (-0.1) 0.1)
        ! A.d frontDirs
    frontDirs = mkPath $ do
      m   (-0.5 * w)  (-0.5 * w)
      l   ( 0.5 * w)  (-0.5 * w)
      l   ( 0.5 * w)  ( 0.5 * w)
      l   (-0.5 * w)  ( 0.5 * w)
      S.z
      m   (-0.5*w + k)  (-0.5*w - k)
      l   ( 0.5*w + k)  (-0.5*w - k)
      l   ( 0.5*w + k)  ( 0.5*w - k) 


--------------------------------------------------------------------------------


pin :: Svg
pin =
  S.svg 
    ! A.viewbox "0 0 1 1"
    $ S.path
        ! A.fill "none"
        ! (A.strokeWidth .: 0.05)
        ! A.strokeLinejoin "arcs"
        ! A.strokeMiterlimit "8"
        ! A.d (mkPath $ topPath >> bodyPath >> needlePath)
        ! A.transform (S.rotateAround 45 0.5 0.5)
  where
    w1 = 0.13
    w2 = 0.04
    y1 = 0.05
    y2 = 0.15
    y3 = 0.45
    y4 = 0.58
    y5 = 0.8
    y6 = 1.03
    r1 = (y2 - y1) / 2
    r2 = (y4 - y3)
    topPath = do
      m   (0.5 - w1)  y1
      aa  r1 r1 0 True False (0.5 - w1) y2
      l   (0.5 + w1)  y2
      aa  r1 r1 0 True False (0.5 + w1) y1
      l   (0.5 - w1)  y1
    bodyPath = do
      m   (0.5 - w1)  y2
      l   (0.5 - w1)  y3
      aa  r2 r2 0 False False (0.5 - w1 - r2) y4
      l   (0.5 + w1 + r2) y4
      aa  r2 r2 0 False False (0.5 + w1) y3
      l   (0.5 + w1)  y2
    needlePath = do
      m   (0.5 - w2)  y4
      l   (0.5 - w2)  y5
      l   0.5         y6
      l   (0.5 + w2)  y5
      l   (0.5 + w2)  y4


--------------------------------------------------------------------------------


lock :: S.Svg
lock =
  S.svg
    ! A.viewbox "0 0 1 1"
    $ do
      arm
      body
      keyhole
  where
    aw  = 0.035
    ax1 = 0.3
    ax2 = 1 - ax1
    axr = (ax2 - ax1) / 2
    ay1 = 0.5
    ay2 = 0.25
    arm =
      S.path
        ! (strokeWidth .: 2*aw)
        ! fill "none"
        ! d armPath
        -- ! stroke "black"
    armPath =
      mkPath $ do
        m   ax1 ay1
        l   ax1 ay2
        aa  axr axr 0 True True ax2 ay2
        l   ax2 ay1
    ----------------------------------------
    bx1 = 0.15
    bx2 = 1 - bx1
    by1 = 0.38
    by2 = 0.95
    body =
      S.path
        ! stroke "none"
        ! d bodyPath
    bodyPath =
      mkPath $ do
        m bx1 by1
        l bx2 by1
        l bx2 by2
        l bx1 by2
        S.z
    ----------------------------------------
    kr  = 0.07
    kw  = 0.038
    ky1 = 0.6
    ky2 = 0.78
    keyhole = do
      S.circle
        ! fill "#fff"
        ! (cx .: 0.5)
        ! (cy .: ky1)
        ! (r  .: kr)
        ! stroke "none"
      S.path
        ! d (mkPath $   m 0.5 ky1  >>  l 0.5 ky2)
        ! stroke "#fff"
        ! (strokeWidth .: 2*kw)
        ! strokeLinecap "round"


--------------------------------------------------------------------------------