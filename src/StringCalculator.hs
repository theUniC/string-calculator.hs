module StringCalculator
    (
        calculate
    )
where

import Data.List.Split
import Data.String.Utils(startswith)

calculate :: String -> Int
calculate ""            = 0
calculate s =
    sum . map readInteger . splitWhen (\c -> c == separatorFrom s || c == '\n') $ trim s
        where separatorFrom :: String -> Char
              separatorFrom s
                | hasSeparator s        = s !! 2
                | otherwise             = ','

              readInteger :: String -> Int
              readInteger s
                | (read s :: Int) < 0   = error "negatives not allowed"
                | otherwise             = read s :: Int

trim :: String -> String
trim s
    | hasSeparator s    = drop 4 s
    | otherwise         = s

hasSeparator :: String -> Bool
hasSeparator s = startswith "//" s