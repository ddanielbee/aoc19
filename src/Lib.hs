module Lib ( mainFunc ) where

  import DayOne
  import DayTwo
  import Data.List.Split


  mainFunc :: IO ()
  mainFunc = sequence_ [ runDay1 calculateRequiredFuelForMassAndFuel, runDay2 compute ]

  runDay1 :: Show a => ([Int] -> a) -> IO ()
  runDay1 fn = do 
                content <- readFile "data/day1.txt"
                print $ fn $ read <$> lines content

  runDay2 :: Show a => ([Int] -> a) -> IO ()
  runDay2 fn = do
                content <- readFile "data/day2.txt"
                print $ fn $ read <$> splitOn "," content