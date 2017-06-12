module QueryBuilder
    ( module X
    , QueryResult(..)
    , QueryBuilderError(..)
    , search
    ) where

import qualified Data.Bifunctor as BF
import           QueryBuilder.Parser as X
import           QueryBuilder.Types (ExpressionContainer, applyConstraints)
import           Text.Megaparsec (ParseError, Dec)

data QueryBuilderError
    = ParserError (ParseError Char Dec)
    | UnmatchedQueryError
    deriving Show

data QueryResult a = QueryResult
    { qrResults :: [a]
    }

search :: Eq a => [a] -> (String -> a -> Maybe ExpressionContainer) -> String -> Either QueryBuilderError (QueryResult a)
search xs f =
    BF.bimap ParserError (QueryResult . flip (applyConstraints f) xs) . parseConstraints'
