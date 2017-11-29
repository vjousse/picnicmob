module Lib
    ( picnic
    ) where

import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)

picnic :: IO ()
picnic = do
  putStrLn "Hello World! Let's have a picnic! \n"
  people_text <- readFile "people.txt"
  let people :: [Person]
      people  = read people_text

  putStr "Number of people coming: "
  print (length people)

  let writePoint :: Point -> String
      writePoint (x,y) = (show x)++","++(show y)++" "

  let writePolygon :: (Color,Polygon) -> String
      writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

  let writePolygons :: [(Color,Polygon)] -> String
      writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

  let colorize :: Color -> [Polygon] -> [(Color,Polygon)]
      colorize = zip.repeat

  -- The @ syntax magically define a list (befor the @) and pattern match
  -- it's content (colors after the @)
  -- See https://stackoverflow.com/questions/1153465/what-does-the-symbol-mean-in-reference-to-lists-in-haskell
  let rainbow :: [[Polygon] -> [(Color, Polygon)]]
      rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  writeFile "tut0.svg" $ writePolygons (blue [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]])
