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
  [ (,) "logo" logo 
  ]


svgReact :: [ (FilePath , Svg) ]
svgReact =
  [ (,) "logo" logo
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

