module MinRuby
    ( minrubyParse
    , minrubyLoad
    ) where

import Control.Applicative (empty)
import Data.Either (either)
import Data.Tree (Tree(..))
import Text.Megaparsec (Parsec, Dec, parse, parseErrorPretty, some, string, space, (<|>))
import Text.Megaparsec.Char (digitChar)

type MinRubyParser = Parsec Dec String

minrubyParse :: String -> Tree String
minrubyParse = either (error . parseErrorPretty) id
             . parse minrubyParser "MinRuby Parser"

minrubyLoad :: IO String
minrubyLoad = undefined

minrubyParser :: MinRubyParser (Tree String)
minrubyParser = space *> parseBinaryOp <* space

parseBinaryOp :: MinRubyParser (Tree String)
parseBinaryOp = do
  n1 <- leaf <$> digit
  space
  op <- operator
  space
  n2 <- leaf <$> digit
  return $ Node op [n1,n2]

digit :: MinRubyParser String
digit = some digitChar

operator :: MinRubyParser String
operator = foldl (<|>) empty $ fmap string binaryOps

binaryOps :: [String]
binaryOps = ["+", "-", "*", "/", "%"]

leaf :: a -> Tree a
leaf = flip Node []
