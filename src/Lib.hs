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
