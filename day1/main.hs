import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

data PositionChangeEvent = Decrease | Increase | Equal | Ignore deriving (Enum, Eq)

toInt :: Text.Text -> Int
toInt x = case Text.decimal x of
  Left a -> 0
  Right b -> fst b

--- Ignore the '0' values for they can be just some rest (ending of list
changeEvent :: Int -> Int -> PositionChangeEvent
changeEvent 0 _ = Ignore
changeEvent _ 0 = Ignore
changeEvent a b | a > b = Increase 
                | a < b = Decrease
                | otherwise  = Equal

compareAcc :: Int -> Int -> [Int] -> [PositionChangeEvent] -> [PositionChangeEvent]
compareAcc fst snd [y] acc =
  let compared = changeEvent fst snd in
    compareAcc snd y [] (acc  ++ [compared])
compareAcc fst snd (_:y:xs) acc = 
  let compared = changeEvent fst snd in 
    compareAcc snd y (y:xs) (acc ++ [compared])
compareAcc _ _ [] acc = acc

extractWindow :: [Int] -> Int
extractWindow (x:y:z:xs) = x + y + z
extractWindow [x,y] = 0
extractWindow [x] = 0
extractWindow [] = 0

windowGrouping :: [Int] -> [Int] -> [Int]
windowGrouping (x:xs) acc = windowGrouping xs (acc ++ [extractWindow (x:xs)])
windowGrouping [] acc = acc

countEvents :: PositionChangeEvent -> [PositionChangeEvent] -> Int
countEvents a = length . filter (\x -> x == a)

countDescends :: [PositionChangeEvent] -> Int
countDescends = countEvents Decrease


main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map toInt ls
  let fst = head mapped
  let snd = (head . tail) mapped
  let compared = compareAcc fst snd (tail mapped) []
  let windowGroup = windowGrouping mapped []
  let comparedSecond = compareAcc (head windowGroup) ((head . tail) windowGroup) (tail windowGroup) [] 
  print (countDescends compared, windowGroup, countDescends comparedSecond)
