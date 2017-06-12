module QueryBuilder.Parser (parseConstraints, parseConstraints') where

import qualified Data.Text as T
import           QueryBuilder.Parser.Internal (parens, quotes, int, double, parseOnly)
import           QueryBuilder.Types
import           Text.Megaparsec
import           Text.Megaparsec.Text

parseConstraints :: String -> Either String Constraints
parseConstraints = parseOnly (constraintsParser <* eof) . T.strip . T.pack

parseConstraints' :: String -> Either (ParseError Char Dec) Constraints
parseConstraints' = runParser (constraintsParser <* eof) "" . T.strip . T.pack

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

constraintValuesParser :: Parser ExpressionContainer
constraintValuesParser = try parseLiteralValue <|> parseValues
  where
    parseValues = explodeValueToContainer . explodeValue <$> choice [remainingDouble, remainingInt, remainingText]
    parseLiteralValue = explodeValueToContainer . explodeValue . VString <$> quotedStringParser
    remainingDouble = VDouble <$> double
    remainingInt = VInt <$> int
    remainingText = VString <$> some alphaNumChar

quotedStringParser :: Parser String
quotedStringParser = quotes remainingText
  where
    remainingText = some notQuote
    notQuote = noneOf ("\"" :: String)

explodeValueToContainer :: [Value] -> ExpressionContainer
explodeValueToContainer xs =
    foldl1 mappend $ concat [map toS xs, map toI xs, map toD xs, map toB xs]
  where
    toS (VString v) = stringExpr v
    toS _ = mempty
    toI (VInt v) = intExpr v
    toI _ = mempty
    toD (VDouble v) = doubleExpr v
    toD _ = mempty
    toB (VBool v) = boolExpr v
    toB _ = mempty

data Value
    = VString String
    | VInt Int
    | VDouble Double
    | VBool Bool

explodeValue :: Value -> [Value]
explodeValue v@(VString s) = v : additional
  where
    additional =
        case s of
            "T" -> [VBool True]
            "F" -> [VBool False]
            "True" -> [VBool True]
            "False" -> [VBool False]
            "true" -> [VBool True]
            "false" -> [VBool False]
            _ -> []
explodeValue v@(VBool b) = [v, VString $ show b]
explodeValue v@(VInt i) = [v, VString $ show i]
explodeValue v@(VDouble d) =
    baseList ++ [VString $ displayValue $ head baseList]
  where
    baseList = includedInt ++ [v]
    includedInt =
        [VInt $ round d | recip (fromInteger $ round d) == recip d]

displayValue :: Value -> String
displayValue (VString s) = s
displayValue (VInt i) = show i
displayValue (VDouble d) = show d
displayValue (VBool b) = show b
