{-# LANGUAGE Rank2Types #-}

module Main
    ( main
    ) where

import MinRuby
import Control.Monad.State (StateT, evalStateT, modify, gets)
import Control.Monad.Trans (lift)
import Data.Tree (Tree(..), drawTree)
import Data.HashMap (Map, empty, insert, findWithDefault)
import System.IO (hFlush, stdout)

type Env = Map String Value
type Eval a = StateT Env IO a

main :: IO ()
main = do
  input <- minrubyLoad
  let tree = minrubyParse input
  -- print tree
  -- hFlush stdout
  evalStateT (evaluate tree) empty
  return ()

evaluate :: Tree Value -> Eval Value
evaluate (Node v ls) = do
  exps <- mapM evaluate ls
  if null ls then
    return v
  else
    case getString v of
      "func_call"  -> lift (print $ exps !! 1) *> return (UVal ())
      "var_assign" -> assign (exps !! 0) (exps !! 1)
      "var_ref"    -> refer (exps !! 0)
      str -> return $ pevaluate exps str

pevaluate :: [Value] ->  String -> Value
pevaluate exps v =
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

assign :: Value -> Value -> Eval Value
assign k v = modify (insert (getString k) v) *> return v

refer :: Value -> Eval Value
refer k = gets $ findWithDefault emassage (getString k)
  where
    emassage = error $ "undefined : " `mappend` getString k
