module MinRuby
    ( minrubyParse
    , minrubyLoad
    ) where

import Control.Applicative (empty)
import Data.Either (either)
import Data.Tree (Tree(..))
import Text.Megaparsec ( Parsec, Dec, parse, parseErrorPretty, some
                       , string, space, (<|>), between)
import Text.Megaparsec.Char (digitChar)

type MinRubyParser = Parsec Dec String

minrubyParse :: String -> Tree String
minrubyParse = either (error . parseErrorPretty) id
             . parse minrubyParser "MinRuby Parser"

minrubyLoad :: IO String
minrubyLoad = undefined

minrubyParser :: MinRubyParser (Tree String)
minrubyParser = parseExp

{-
  op12  := op13 op12'
  op12' := ("+" | "-") op12 | epsilon
  op13  := op15 op13'
  op13' := ("*" | "/" | "%") op13 | epsilon
  op15  := factor op15'
  op15' := "**" op15 | epsilon
  factor  := "(" exp ")" | number
-}

parseExp :: MinRubyParser (Tree String)
parseExp = parseOp12

parseOp12 :: MinRubyParser (Tree String)
parseOp12 = parseOp13 >>= parseOp12'
  where
    parseOp12' left = mkBinOpNode left
                        <$> (stringToken "+" <|> stringToken "-")
                        <*> parseOp12
                  <|> return left

parseOp13 :: MinRubyParser (Tree String)
parseOp13 = parseOp15 >>= parseOp13'
  where
    parseOp13' left = mkBinOpNode left
                        <$> (stringToken "*" <|> stringToken "/" <|> stringToken "%")
                        <*> parseOp13
                  <|> return left

parseOp15 :: MinRubyParser (Tree String)
parseOp15 = parseFactor >>= parseOp15'
  where
    parseOp15' left = mkBinOpNode left
                        <$> stringToken "**"
                        <*> parseOp15
                  <|> return left

parseFactor :: MinRubyParser (Tree String)
parseFactor = between (stringToken "(") (stringToken ")") parseExp
          <|> mkLitNode <$> token digit

digit :: MinRubyParser String
digit = some digitChar

token :: MinRubyParser a -> MinRubyParser a
token p = space *> p <* space

stringToken :: String -> MinRubyParser String
stringToken = token . string

-- Tree generator

leaf :: a -> Tree a
leaf = flip Node []

mkLitNode :: String -> Tree String
mkLitNode = Node "lit" . (: []) . leaf

mkBinOpNode :: Tree String -> String -> Tree String -> Tree String
mkBinOpNode e1 op e2 = Node op [e1, e2]
