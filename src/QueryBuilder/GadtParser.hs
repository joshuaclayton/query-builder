module QueryBuilder.GadtParser (parseGadtConstraints) where

import qualified Data.Maybe as M
import qualified Data.Text as T
import           QueryBuilder.GadtTypes
import           QueryBuilder.Parser.Internal (parens, quotes, int, double, parseOnly)
import           QueryBuilder.Types (Value(..), explodeValue)
import           Text.Megaparsec
import           Text.Megaparsec.Text

parseGadtConstraints :: String -> Either String GadtConstraints
parseGadtConstraints = parseOnly (constraintsParser <* eof) . T.strip . T.pack

constraintsParser :: Parser GadtConstraints
constraintsParser = do
    n <- singleConstraintParser <|> parens constraintsParser
    operator <- optional operatorParser

    case operator of
        Nothing -> return n
        Just op -> buildConstraints op <$> sequence [return n, constraintsParser]
  where
    allOperations = [(":", Equals), (">", GreaterThan), (">=", GreaterThanOrEqual), ("<", LessThan), ("<=", LessThanOrEqual)]
    singleConstraintParser = Single <$> choice (map (try . uncurry buildConstraint) allOperations)

buildConstraint :: String -> ComparisonOperator -> Parser GadtConstraint
buildConstraint operatorString operator = GadtConstraint
    <$> constraintNameParser operatorString
    <*> constraintValuesParser
    <*> pure operator

operatorParser :: Parser (GadtConstraints -> GadtConstraints -> GadtConstraints)
operatorParser = try andOperator <|> orOperator
  where
    andOperator = op "AND" *> pure And
    orOperator = op "OR" *> pure Or
    op :: String -> Parser String
    op w = space *> string w <* space

constraintNameParser :: String -> Parser String
constraintNameParser split = someTill (alphaNumChar <|> char '.') (string split)

constraintValuesParser :: Parser ExprContainer
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

explodeValueToContainer :: [Value] -> ExprContainer
explodeValueToContainer xs = ExprContainer mintval mdoubleval mstringval Nothing Nothing Nothing
  where
    safeHead [] = Nothing
    safeHead xs' = Just $ head xs'
    mstringval = safeHead $ M.mapMaybe toS xs
    mintval = safeHead $ M.mapMaybe toI xs
    mdoubleval = safeHead $ M.mapMaybe toD xs
    toS (VString v) = Just $ S v
    toS _ = Nothing
    toI (VInt v) = Just $ I v
    toI _ = Nothing
    toD (VDouble v) = Just $ D v
    toD _ = Nothing
