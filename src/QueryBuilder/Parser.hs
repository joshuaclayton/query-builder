module QueryBuilder.Parser
    ( parseConstraints
    ) where

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
    allOperations = [(":", Equals), (">", GreaterThan), (">=", GreaterThanOrEqual), ("<", LessThan), ("<=", LessThanOrEqual)]
    singleConstraintParser = Single <$> choice (map (try . uncurry buildConstraint) allOperations)

buildConstraint :: String -> ComparisonOperator -> Parser Constraint
buildConstraint operatorString operator = Constraint
    <$> constraintNameParser operatorString
    <*> constraintValuesParser
    <*> pure operator

operatorParser :: Parser (Constraints -> Constraints -> Constraints)
operatorParser = try andOperator <|> orOperator
  where
    andOperator = op "AND" *> pure And
    orOperator = op "OR" *> pure Or
    op :: String -> Parser String
    op w = space *> string w <* space

constraintNameParser :: String -> Parser String
constraintNameParser split = someTill (alphaNumChar <|> char '.') (string split)

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
