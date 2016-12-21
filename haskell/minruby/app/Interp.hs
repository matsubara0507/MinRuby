{-# LANGUAGE Rank2Types #-}

module Main
    ( main
    ) where

import MinRuby
import Control.Monad (join, foldM)
import Control.Monad.State.Strict (StateT, evalStateT, modify, gets)
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
  if null ls then
    return v
  else
    case getString v of
      "func_call" -> UVal <$> (lift . print =<< evaluate (ls !! 1))
      "var_assign" -> join $ assign <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "var_ref" -> refer =<< evaluate (ls !! 0)
      "lit" -> evaluate (ls !! 0)
      "+" -> intOp (+) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "-" -> intOp (-) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "*" -> intOp (*) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "/" -> intOp div <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "%" -> intOp mod <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "**" -> intOp (^) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "<" -> boolOp (<) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "<=" -> boolOp (<=) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "==" -> boolOp (==) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "!=" -> boolOp (/=) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      ">" -> boolOp (>) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      ">=" -> boolOp (>=) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
      "stmt" -> foldM (const evaluate) (UVal ()) ls
      "if"   -> evaluate (ls !! 0) >>=
                  \b -> evaluate $ if getBool b then ls !! 1 else ls !! 2
      _     -> error ("undefined : " `mappend` show v)

intOp :: (Int -> Int -> Int) -> Value -> Value -> Value
intOp f (IVal v1) (IVal v2) = IVal $ f v1 v2
intOp f v1 v2 = error $ "unmatch type " `mappend` concatMap show [v1,v2]

boolOp :: (forall a . Ord a => a -> a -> Bool) -> Value -> Value -> Value
boolOp f (SVal v1) (SVal v2) = BVal $ f v1 v2
boolOp f (IVal v1) (IVal v2) = BVal $ f v1 v2
boolOp f (BVal v1) (BVal v2) = BVal $ f v1 v2
boolOp _ _ _ = BVal False

assign :: Value -> Value -> Eval Value
assign k v = modify (insert (getString k) v) *> return v

refer :: Value -> Eval Value
refer k = gets $ findWithDefault emassage (getString k)
  where
    emassage = error $ "undefined : " `mappend` getString k
