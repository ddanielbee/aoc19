module DayOne where 

  calculateRequiredFuel :: [Int] -> Int
  calculateRequiredFuel = sum . fmap calculateFuelForMass
  
  calculateFuelForMass :: Int -> Int
  calculateFuelForMass a = div a 3 - 2