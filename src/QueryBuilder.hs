module QueryBuilder
    ( module X
    , ResultsWithContext
    , QueryBuilderError(..)
    , ExpressionContainer
    , search
    , intExpr
    , intExprs
    , stringExpr
    , stringExprs
    , doubleExpr
    , doubleExprs
    , boolExpr
    , boolExprs
    ) where

import qualified Data.List as L
import           QueryBuilder.ExpressionContainer
import           QueryBuilder.Parser as X
import           QueryBuilder.Types
import           Text.Megaparsec (ParseError, Dec)

data QueryBuilderError
    = ParserError (ParseError Char Dec)
    | MissingFieldDeclaration [String]
    deriving Show

search :: Eq a => [a] -> (String -> a -> Maybe ExpressionContainer) -> String -> Either QueryBuilderError [a]
search xs f s =
    case parseConstraints' s of
        Left parserError -> Left $ ParserError parserError
        Right constraints ->
          let
            outcome = applyConstraints f constraints xs
          in
            if not (null $ rwcUnmatchedConstraints outcome)
            then Left $ MissingFieldDeclaration $ L.nub $ map cName $ rwcUnmatchedConstraints outcome
            else Right $ rwcResults outcome
