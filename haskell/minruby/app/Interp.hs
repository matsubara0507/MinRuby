{-# LANGUAGE Rank2Types #-}

module Main
    ( main
    ) where

import MinRuby (Value(..), minrubyParse, ToValue(..))
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  print $ evaluate tree

evaluate :: Tree Value -> Value
evaluate (Node v ls) =
  let exps = fmap evaluate ls in
  if null ls then v else
    case getString v of
      "lit" -> exps !! 0
      "+"   -> IVal $ getInt (exps !! 0) + getInt (exps !! 1)
      "-"   -> IVal $ getInt (exps !! 0) - getInt (exps !! 1)
      "*"   -> IVal $ getInt (exps !! 0) * getInt (exps !! 1)
      "/"   -> IVal $ getInt (exps !! 0) `div` getInt (exps !! 1)
      "%"   -> IVal $ getInt (exps !! 0) `mod` getInt (exps !! 1)
      "**"  -> IVal $ getInt (exps !! 0) ^ getInt (exps !! 1)
      "<"   -> BVal $ boolOp (<) (exps !! 0) (exps !! 1)
      "<="  -> BVal $ boolOp (<=) (exps !! 0) (exps !! 1)
      "=="  -> BVal $ boolOp (==) (exps !! 0) (exps !! 1)
      "!="  -> BVal $ boolOp (/=) (exps !! 0) (exps !! 1)
      ">"   -> BVal $ boolOp (>) (exps !! 0) (exps !! 1)
      ">="  -> BVal $ boolOp (>=) (exps !! 0) (exps !! 1)
      _     -> error ("undefined : " `mappend` show v)

boolOp :: (forall a . Ord a => a -> a -> Bool) -> Value -> Value -> Bool
boolOp f (SVal v1) (SVal v2) = f v1 v2
boolOp f (IVal v1) (IVal v2) = f v1 v2
boolOp f (BVal v1) (BVal v2) = f v1 v2
boolOp _ _ _ = False
