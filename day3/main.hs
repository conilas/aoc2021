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

getNumberCount :: [Text.Text] -> Int -> (Int, Int)
getNumberCount ls idx = countValues (processValues idx (map Text.unpack ls)) (0,0)

mkList :: [Int] -> Int -> [Int]
mkList acc v = 
  if v == 0 then 
    reverse (map (\z -> z - 1) acc)
  else 
    mkList (acc ++ [v]) (v-1)

mkListStr :: [Text.Text] -> [Int]
mkListStr = mkList [] . Text.length . head 

getFinalNumber :: [(Int, Int)] -> [Char]
getFinalNumber = map (\z -> if (fst z) > (snd z) then '0' else '1')

convert :: [Char] -> Int
convert [] = 0
convert (x : xs) = (digitToInt x) + 2 * convert xs

flipB :: [Char] -> [Char]
flipB = map (\z -> if z == '0' then '1' else '0')

main = do
  ls <- fmap Text.lines (Text.readFile "input2")
  -- im really lazy today
  let gamma = convert (reverse (getFinalNumber (map (getNumberCount ls) (mkListStr ls)))) in
    let eps = convert (flipB (reverse (getFinalNumber (map (getNumberCount ls) (mkListStr ls))))) in
      print (gamma * eps)
