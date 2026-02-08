import Data.Char (digitToInt)
import Text.Printf (printf)

getLargestDigit "" = error "getLargestDigit called with empty string"
getLargestDigit str = f Nothing str
  where
    f (Just (num, str)) "" = (digitToInt num, str)
    f m (c : str) =
      case m of
        (Just (result, workingStr))
          | c > result -> f (Just (c, str)) str
        Nothing -> f (Just (c, str)) str
        _ -> f m str

getMaxJoltage count str | count > length str = error "getMaxJoltage called with insufficient string length"
getMaxJoltage count str = f 0 count str
  where
    f acc 0 str = acc
    f acc count "" = acc
    f acc count str =
      let count' = count - 1
          (num, newStr) = getLargestDigit $ doTail (drop count') str
       in f (acc + num * 10 ^ count') count' (newStr ++ doTail (take count') str)
    doTail f s = reverse $ f $ reverse s

main :: IO ()
main =
  do
    text <- readFile "3/input.txt"
    let s1 = sumLines (getMaxJoltage 2) text
        s2 = sumLines (getMaxJoltage 12) text
    printf "step1: %d\nstep2: %d\n" s1 s2
  where
    sumLines f text = sum $ map f (lines text)
