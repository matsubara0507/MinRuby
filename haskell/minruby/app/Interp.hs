module Main
    ( main
    ) where

import MinRuby (minrubyParse)
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  -- print tree
  print $ evaluate tree

evaluate :: Tree String -> Int
evaluate (Node v ls) =
  if null ls then
    read v
  else
    let exps = fmap evaluate ls in
    case v of
      "lit" -> exps !! 0
      "+" -> exps !! 0 + exps !! 1
      "-" -> exps !! 0 - exps !! 1
      "*" -> exps !! 0 * exps !! 1
      "/" -> exps !! 0 `div` exps !! 1
      "%" -> exps !! 0 `mod` exps !! 1
      "**" -> exps !! 0 ^ exps !! 1
      _ -> error ("undefined binaty op: " `mappend` v)
