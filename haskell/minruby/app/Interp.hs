module Main
    ( main
    ) where

import MinRuby (minrubyParse)

main :: IO ()
main = putStrLn =<< show . minrubyParse <$> getLine
