import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Char(digitToInt)

addValues :: Char -> (Int, Int) -> (Int, Int)
addValues '0' values = ((fst values) + 1, snd values)
addValues '1' values = (fst values, (snd values) + 1)

countValues :: [Char] -> (Int, Int) -> (Int, Int)
countValues [] acc = acc
countValues (x:xs) acc = countValues xs (addValues x acc)

processValues :: Int -> [String] -> String
processValues idx values = 
  if idx == 0 then 
    map head values 
  else 
    processValues (idx-1) (map tail values)

getNumberCount :: [[Char]] -> Int -> (Int, Int)
getNumberCount ls idx = countValues (processValues idx ls) (0,0)

mkList :: [Int] -> Int -> [Int]
mkList acc v = 
  if v == 0 then 
    reverse (map (\z -> z - 1) acc)
  else 
    mkList (acc ++ [v]) (v-1)

mkListStr :: [Text.Text] -> [Int]
mkListStr = mkList [] . Text.length . head 

getFinalNumber :: [(Int, Int)] -> [Char]
getFinalNumber = map toCharMax

toChar :: ((Int, Int) -> Bool) -> (Int, Int) -> Char
toChar strat v = if (strat v) then '0' else '1'

toCharMax :: (Int, Int) -> Char
toCharMax = toChar maxP

toCharMin :: (Int, Int) -> Char
toCharMin = toChar minP

maxP :: (Int, Int) -> Bool
maxP v = fst v > snd v

minP :: (Int, Int) -> Bool
minP v = fst v <= snd v

convert :: [Char] -> Int
convert [] = 0
convert (x : xs) = (digitToInt x) + 2 * convert xs

flipB :: [Char] -> [Char]
flipB = map (\z -> if z == '0' then '1' else '0')

getValue :: [Char] -> Int -> Char
getValue ls 0 = head ls
getValue ls idx = getValue (tail ls) (idx-1)

filterValuesFromNumber :: Char -> Int -> [[Char]] -> [[Char]]
filterValuesFromNumber bit idx = filter (\z -> (getValue z idx) == bit)

findNumberValue :: [[Char]] -> ((Int, Int) -> Char) -> Int -> [Char]
findNumberValue [x] _ idx = x
findNumberValue ls strat idx =
  let numberValid = strat (getNumberCount ls idx) in
    findNumberValue (filterValuesFromNumber numberValid idx ls) strat (idx+1)

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  -- im really lazy today
  let gamma = convert (reverse (getFinalNumber (map (getNumberCount (map Text.unpack ls)) (mkListStr ls)))) in
    let eps = convert (flipB (reverse (getFinalNumber (map (getNumberCount (map Text.unpack ls)) (mkListStr ls))))) in
      print (gamma * eps)
  -- pt b now
  let oxygen = convert (reverse (findNumberValue (map Text.unpack ls) toCharMax 0)) in
    let co2 = convert (reverse (findNumberValue (map Text.unpack ls) toCharMin 0)) in
      print (oxygen * co2)
