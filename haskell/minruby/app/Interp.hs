{-# LANGUAGE Rank2Types #-}

module Main
    ( main
    ) where

import MinRuby
import Data.Tree (Tree(..), drawTree)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  input <- minrubyLoad
  let tree = minrubyParse input
  -- print tree
  -- hFlush stdout
  evaluate tree
  return ()

evaluate :: Tree Value -> IO Value
evaluate (Node v ls) = do
  exps <- mapM evaluate ls
  if null ls then
    return v
  else
    case getString v of
      "func_call" -> print (exps !! 1) *> return (UVal ())
      str -> return $ pevaluate str exps

pevaluate :: String -> [Value] -> Value
pevaluate v exps =
  case v of
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
    "stmt" -> foldl seq (UVal ()) exps
    _     -> error ("undefined : " `mappend` show v)

boolOp :: (forall a . Ord a => a -> a -> Bool) -> Value -> Value -> Bool
boolOp f (SVal v1) (SVal v2) = f v1 v2
boolOp f (IVal v1) (IVal v2) = f v1 v2
boolOp f (BVal v1) (BVal v2) = f v1 v2
boolOp _ _ _ = False
