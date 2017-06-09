module QueryBuilder.Parser
    ( parseConstraints
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           QueryBuilder.Parser.Internal (parens, quotes, int, double, parseOnly)
import           QueryBuilder.Types
import           Text.Megaparsec
import           Text.Megaparsec.Text

parseConstraints :: String -> Either String Constraints
parseConstraints = parseOnly (constraintsParser <* eof) . T.strip . T.pack

constraintsParser :: Parser Constraints
constraintsParser = do
    n <- singleConstraintParser <|> parens constraintsParser
    operator <- optional operatorParser

    case operator of
        Nothing -> return n
        Just op -> buildConstraints op <$> sequence [return n, constraintsParser]
  where
    singleConstraintParser = Single <$> constraintParser

constraintParser :: Parser Constraint
constraintParser = Constraint
    <$> constraintNameParser
    <*> constraintValuesParser
    <*> pure Equals

operatorParser :: Parser (Constraints -> Constraints -> Constraints)
operatorParser = try andOperator <|> orOperator
  where
    andOperator = op "AND" *> pure And
    orOperator = op "OR" *> pure Or
    op :: String -> Parser String
    op w = space *> string w <* space

constraintNameParser :: Parser Text
constraintNameParser = T.pack <$> someTill (alphaNumChar <|> char '.') (char ':')

constraintValuesParser :: Parser [Value]
constraintValuesParser = try parseLiteralValue <|> parseValues
  where
    parseValues = explodeValue <$> choice [remainingDouble, remainingInt, remainingText]
    parseLiteralValue = explodeValue . VString <$> quotedStringParser
    remainingDouble = VDouble <$> double
    remainingInt = VInt <$> int
    remainingText = VString <$> some alphaNumChar

quotedStringParser :: Parser String
quotedStringParser = quotes remainingText
  where
    remainingText = some notQuote
    notQuote = noneOf ("\"" :: String)
