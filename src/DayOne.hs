module DayOne where 

  calculateRequiredFuel :: [Int] -> Int
  calculateRequiredFuel = sum . fmap calculateFuelForMass

  calculateRequiredFuelForMassAndFuel :: [Int] -> Int
  calculateRequiredFuelForMassAndFuel = sum . fmap (sum . calculateFuelForMassAndFuel)

  calculateFuelForMass :: Int -> Int
  calculateFuelForMass a = div a 3 - 2

  calculateFuelForMassAndFuel :: Int -> [Int]
  calculateFuelForMassAndFuel = tail . takeWhile (> 0) . iterate calculateFuelForMass