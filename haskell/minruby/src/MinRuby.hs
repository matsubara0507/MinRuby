{-# LANGUAGE FlexibleInstances #-}

module MinRuby
    ( Value(..)
    , ToValue(..)
    , minrubyParse
    , minrubyLoad
    ) where

import Control.Applicative (empty)
import Data.Either (either)
import Data.Tree (Tree(..))
import System.Environment (getArgs)
import Text.Megaparsec ( Parsec, Dec, parse, parseErrorPretty, many, some, try
                       , string, space, (<|>), between, sepBy, letterChar
                       , char, alphaNumChar)
import Text.Megaparsec.Char (digitChar)

type MinRubyParser = Parsec Dec String

data Value = SVal { getString :: String }
           | IVal { getInt :: Int }
           | BVal { getBool :: Bool }
           | UVal ()
           deriving (Eq)

instance Show Value where
  show (SVal v) = show v
  show (IVal v) = show v
  show (BVal True) = "true"
  show (BVal False) = "false"
  show (UVal ()) = "()"

class Ord a => ToValue a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance ToValue String where
  toValue = SVal
  fromValue (SVal v) = Just v
  fromValue _        = Nothing

instance ToValue Int where
  toValue = IVal
  fromValue (IVal v) = Just v
  fromValue _        = Nothing

instance ToValue Bool where
  toValue = BVal
  fromValue (BVal v) = Just v
  fromValue _        = Nothing

minrubyParse :: String -> Tree Value
minrubyParse = either (error . parseErrorPretty) id
             . parse minrubyParser "MinRuby Parser"

minrubyLoad :: IO String
minrubyLoad = do
  args <- getArgs
  if null args then
    getContents
  else
    readFile (args !! 0)

minrubyParser :: MinRubyParser (Tree Value)
minrubyParser = parseStmt

{-
  stmt   := exp (newline stmt | epsilon)
  exp    := op02
  op02   := ident "=" exp <|> op07
  op07   := op08 op07'
  op07'  := ("!=" | "==") op07 | epsilon
  op08   := op12 op08'
  op08'  := (">" | ">=" | "<" | "<=") op08 | epsilon
  op12   := op13 op12'
  op12'  := ("+" | "-") op12 | epsilon
  op13   := op15 op13'
  op13'  := ("*" | "/" | "%") op13 | epsilon
  op14   := "-" op14 | op15
  op15   := op16 op15'
  op15'  := "**" op15 | epsilon
  op16   := ("+" \ "!") op16 | factor
  factor := "(" exp ")" | number | boolean | ident | ident "(" args ")"
  args   := exp ("," args | epsilon)
-}

parseStmt :: MinRubyParser (Tree Value)
parseStmt = Node (SVal "stmt") <$> many parseExp

parseExp :: MinRubyParser (Tree Value)
parseExp = parseOp02

parseOp02 :: MinRubyParser (Tree Value)
parseOp02 = try (mkVassignNode <$> token ident <* stringToken "=" <*> parseExp)
        <|> parseOp07

parseOp07 :: MinRubyParser (Tree Value)
parseOp07 = parseOp08 >>= parseOp07'
  where
    parseOp07' left = mkBinOpNode left
                        <$> (stringToken "!=" <|> stringToken "==")
                        <*> parseOp07
                  <|> return left

parseOp08 :: MinRubyParser (Tree Value)
parseOp08 = parseOp12 >>= parseOp08'
  where
    parseOp08' left = mkBinOpNode left
                        <$> foldl (<|>) empty (fmap stringToken bops)
                        <*> parseOp07
                  <|> return left
    bops = [">=",">","<=","<"]

parseOp12 :: MinRubyParser (Tree Value)
parseOp12 = parseOp13 >>= parseOp12'
  where
    parseOp12' left = mkBinOpNode left
                        <$> (stringToken "+" <|> stringToken "-")
                        <*> parseOp12
                  <|> return left

parseOp13 :: MinRubyParser (Tree Value)
parseOp13 = parseOp14 >>= parseOp13'
  where
    parseOp13' left = mkBinOpNode left
                        <$> foldl (<|>) empty (fmap stringToken aops)
                        <*> parseOp13
                  <|> return left
    aops = ["*","/","%"]

parseOp14 :: MinRubyParser (Tree Value)
parseOp14 = stringToken "-" *> (mkBinOpNode minusOne "*" <$> parseOp14)
        <|> parseOp15
  where
    minusOne = mkLitNode (negate 1 :: Int)

parseOp15 :: MinRubyParser (Tree Value)
parseOp15 = parseOp16 >>= parseOp15'
  where
    parseOp15' left = mkBinOpNode left
                        <$> stringToken "**"
                        <*> parseOp15
                  <|> return left

parseOp16 :: MinRubyParser (Tree Value)
parseOp16 = stringToken "+" *> parseOp16
        <|> stringToken "!" *> (mkBinOpNode false "==" <$> parseOp16)
        <|> parseFactor
  where
    false = mkLitNode False

parseFactor :: MinRubyParser (Tree Value)
parseFactor = between (stringToken "(") (stringToken ")") parseExp
          <|> mkLitNode <$> token digit
          <|> mkLitNode <$> token boolean
          <|> (token ident >>= parseFcall)

parseFcall :: String -> MinRubyParser (Tree Value)
parseFcall name = mkFcallNode name <$>
                    between (stringToken "(") (stringToken ")") parseArgs
              <|> return (mkVrefNode name)

parseArgs :: MinRubyParser [Tree Value]
parseArgs = parseExp `sepBy` char ','

digit :: MinRubyParser Int
digit = read <$> some digitChar

boolean :: MinRubyParser Bool
boolean = stringToken "true" *> return True
      <|> stringToken "false" *> return False

ident :: MinRubyParser String
ident = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

token :: MinRubyParser a -> MinRubyParser a
token p = space *> p <* space

stringToken :: String -> MinRubyParser String
stringToken = token . string

-- Tree generator

leaf :: ToValue a => a -> Tree Value
leaf = flip Node [] . toValue

mkLitNode :: ToValue a => a -> Tree Value
mkLitNode = Node (SVal "lit") . (: []) . leaf

mkVrefNode :: String -> Tree Value
mkVrefNode = Node (SVal "var_ref") . (: []) . leaf

mkBinOpNode :: Tree Value -> String -> Tree Value -> Tree Value
mkBinOpNode e1 op e2 = Node (SVal op) [e1, e2]

mkFcallNode :: String -> [Tree Value] -> Tree Value
mkFcallNode fname = Node (SVal "func_call") . (leaf fname :)

mkVassignNode :: String -> Tree Value -> Tree Value
mkVassignNode vname = Node (SVal "var_assign") . (leaf vname :) . (: [])
