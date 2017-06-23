{-# LANGUAGE GADTs #-}

module QueryBuilder.Types where

import QueryBuilder.ExpressionContainer

data ResultsWithContext a = ResultsWithContext
    { rwcResults :: [a]
    , rwcUnmatchedConstraints :: [Constraint]
    }

type PropertyEvaluator a = String -> (a -> Maybe ExpressionContainer)

instance Show a => Show (ResultsWithContext a) where
    show (ResultsWithContext a b) =
        "ResultsWithContext " ++ show a ++ " " ++ show b

applyConstraints :: Eq a => PropertyEvaluator a -> Constraints -> [a] -> ResultsWithContext a
applyConstraints f cs xs = ResultsWithContext filteredXs []
  where
    filteredXs = filter finalFilter xs
    finalFilter = recordToFinalReal f cs

recordToFinalReal :: PropertyEvaluator a -> Constraints -> a -> Bool
recordToFinalReal _ NoConstraints _ = True
recordToFinalReal l (And c c') r = recordToFinalReal l c r && recordToFinalReal l c' r
recordToFinalReal l (Or c c') r = recordToFinalReal l c r || recordToFinalReal l c' r
recordToFinalReal l (Single c) r =
    case l (cName c) r of
        Nothing -> False
        Just container ->
            evalExpr $ compareExpressions (cComparison c) (cContainer c) container
