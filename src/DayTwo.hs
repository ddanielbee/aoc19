module DayTwo where

  import           Data.List.Index                ( setAt )
  import           Safe                           ( atMay )
  import Control.Applicative

  data OpCode = Add Int Int Int | Multiply Int Int Int | Finish deriving (Eq)

  compute :: [Int] -> Either String [Int]
  compute input = computeOpCode input 0

  parseOpCode :: [Int] -> Either String OpCode
  parseOpCode [1, first, second, third] = Right $ Add first second third
  parseOpCode [2, first, second, third] = Right $ Multiply first second third
  parseOpCode (99 : _) = Right Finish
  parseOpCode _ = Left "Invalid Code"

  runAddition :: Maybe Int -> Maybe Int -> Either String Int
  runAddition (Just x) (Just y) = Right (x + y)
  runAddition Nothing _ = Left "Value is not in the list"
  runAddition _ Nothing = Left "Value is not in the list"

  runMultiplication :: Maybe Int -> Maybe Int -> Either String Int
  runMultiplication (Just x) (Just y) = Right (x * y)
  runMultiplication Nothing _ = Left "Value is not in the list"
  runMultiplication _ Nothing = Left "Value is not in the list"

  setValueAt :: [Int] -> Either String Int -> Int -> Either String [Int]
  setValueAt intCodes (Right result) pos = Right (setAt pos result intCodes)
  setValueAt _ (Left prob) _ = Left prob
  
  computeOpCode :: [Int] -> Int -> Either String [Int]
  computeOpCode intCodes pos = 
    case parseOpCode (take 4 (drop pos intCodes)) of 
      Right (Add first second third) -> case setValueAt intCodes (runAddition (atMay intCodes first) (atMay intCodes second)) third of
        Right addCodes -> computeOpCode addCodes (pos + 4)
        Left _ -> Left "Error somewhere"
      Right (Multiply first second third) -> case setValueAt intCodes (runMultiplication (atMay intCodes first) (atMay intCodes second)) third of
        Right multCodes -> computeOpCode multCodes (pos + 4)
        Left _ -> Left "Error somewhere"
      Right Finish -> Right intCodes
      _ -> Left "Error somewhere"

  magicNumber :: Int
  magicNumber = 19690720

  possibilities :: [(Int, Int)]
  possibilities = liftA2 (,) [0 .. 99] [0 .. 99]

  replaceInitialValues :: Int -> Int -> [Int] -> [Int]
  replaceInitialValues first second = setAt 1 first . setAt 2 second

  findValues :: Int -> [(Int, Int)] -> [Int] -> Maybe (Int, Int)
  findValues valueToFind ((first, second) : rest) input =
    case computeOpCode (replaceInitialValues first second input) 0 of 
      Right intCodes ->  if head intCodes == valueToFind 
                         then Just (first, second)
                         else findValues valueToFind rest input
      Left _ -> Nothing
  findValues _ [] _ = Nothing

  findValuesApplied :: [Int] -> Maybe (Int, Int)
  findValuesApplied = findValues magicNumber possibilities