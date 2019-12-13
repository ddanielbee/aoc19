module Lib ( mainFunc ) where

  import DayOne

  mainFunc :: IO ()
  mainFunc = runDay1 calculateRequiredFuel
  
  runDay1 :: Show a => ([Int] -> a) -> IO ()
  runDay1 fn = do 
                content <- readFile "data/day1.txt"
                print $ fn $ read <$> lines content