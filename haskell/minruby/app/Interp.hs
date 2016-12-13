module Main
    ( main
    ) where

import MinRuby (minrubyParse)
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  print tree
  print $ evaluate tree

evaluate :: Tree String -> Int
evaluate (Node v ls) =
  if null ls then
    read v
  else
    let [v1,v2] = fmap evaluate ls in
    case v of
      "+" -> v1 + v2
      "-" -> v1 - v2
      "*" -> v1 * v2
      "/" -> v1 `div` v2
      "%" -> v1 `mod` v2
      _ -> error ("undefined binaty op: " `mappend` v)
