module QueryBuilder.Parser.Internal
  ( parseOnly
  , int
  , double
  , parens
  , quotes
  ) where

import           Control.Monad (void)
import qualified Data.Bifunctor as BF
import           Data.Scientific (toRealFloat)
import           Data.Text (Text)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first parseErrorPretty . runParser p ""

int :: Parser Int
int = fromInteger <$> L.signed sc integer
  where
    integer = lexeme L.integer

double :: Parser Double
double = L.signed sc $ toRealFloat <$> lexeme L.number

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser () -- space consumer
sc = L.space (void $ char ' ') lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc
