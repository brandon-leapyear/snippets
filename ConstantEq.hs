import Data.Bits (xor, (.|.))
import Data.Char (ord)
import Data.Function (on)
import Data.List (foldl1)

-- https://security.stackexchange.com/a/83671
constantEq :: String -> String -> Bool
constantEq a b = length a == length b && foldl1 (.|.) joined == 0
  where
    joined = zipWith (xor `on` ord) a b
