{- stack script --resolver lts-14.12 --package pretty-show -}

import Text.Show.Pretty

main :: IO ()
main = do
  contents <- getLine
  pPrint $ parseValue contents
