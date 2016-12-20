module Main
    ( main
    ) where

import MinRuby (Value(..), minrubyParse, FromValue(..), toValue)
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  -- print tree
  print $ evaluate tree

evaluate :: Tree Value -> Value
evaluate (Node v ls) =
  let exps = fmap evaluate ls in
  if null ls then v else
    case v of
      (SVal "lit") -> exps !! 0
      (SVal "+")  -> toValue ((fromValues exps 0) + (fromValues exps 1) :: Int)
      (SVal "-")  -> toValue ((fromValues exps 0) - (fromValues exps 1) :: Int)
      (SVal "*")  -> toValue ((fromValues exps 0) * (fromValues exps 1) :: Int)
      (SVal "/")  -> toValue ((fromValues exps 0) `div` (fromValues exps 1) :: Int)
      (SVal "%")  -> toValue ((fromValues exps 0) `mod` (fromValues exps 1) :: Int)
      (SVal "**") -> toValue $ (fromValues exps 0 :: Int) ^ (fromValues exps 1 :: Int)
      _ -> error ("undefined : " `mappend` show v)

fromValues :: FromValue a => [Value] -> Int -> a
fromValues vls idx = let v = vls !! idx in
  maybe (error $ "unmatch type : " `mappend` show v) id (fromValue v)
