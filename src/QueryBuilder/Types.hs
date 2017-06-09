{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module QueryBuilder.Types
    ( Value(..)
    , Constraints(..)
    , Constraint(..)
    , ComparisonOperator(..)
    , ToValue(..)
    , Values(..)
    , applyConstraints
    , buildConstraints
    , explodeValue
    , toOperator
    ) where

import Data.List (intersect, union)
import GHC.Float (float2Double)

data Value
    = VString String
    | VInt Int
    | VDouble Double
    deriving (Show, Eq)

data Values
    = SingleValue Value
    | MultipleValues [Value]

instance Ord Value where
    compare (VString v) (VString v') = compare v v'
    compare (VInt v) (VInt v') = compare v v'
    compare (VDouble v) (VDouble v') = compare v v'
    compare _ _ = LT

class ToValue a where
    toValue :: a -> Value

instance ToValue String where
    toValue = VString

instance ToValue Int where
    toValue = VInt

instance ToValue Double where
    toValue = VDouble

instance ToValue Float where
    toValue = VDouble . float2Double

instance ToValue Integer where
    toValue = VInt . fromIntegral

data Constraints
    = Single Constraint
    | And Constraints Constraints
    | Or Constraints Constraints
    | NoConstraints
    deriving Show

data Constraint = Constraint
    { cName :: String
    , cValues :: [Value]
    , cComparison :: ComparisonOperator
    } deriving (Show)

data ComparisonOperator
    = Equals
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    deriving Show

toOperator :: Ord a => ComparisonOperator -> (a -> a -> Bool)
toOperator Equals = (==)
toOperator GreaterThan = (>)
toOperator GreaterThanOrEqual = (>=)
toOperator LessThan = (<)
toOperator LessThanOrEqual = (<=)

buildConstraints :: (Constraints -> Constraints -> Constraints) -> [Constraints] -> Constraints
buildConstraints _ [] = NoConstraints
buildConstraints _ [a] = a
buildConstraints f [a, b] = f a b
buildConstraints f (a:b:xs) = buildConstraints f $ f a b : xs

explodeValue :: Value -> [Value]
explodeValue v@(VString _) = [v]
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

applyConstraints :: Eq a => (String -> a -> Maybe Values) -> Constraints -> [a] -> [a]
applyConstraints _ NoConstraints xs = xs
applyConstraints f (Single constraint) xs = filter (handleConstraint constraint f) xs
applyConstraints f (And lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `intersect` applyConstraints f rconstraints xs
applyConstraints f (Or lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `union` applyConstraints f rconstraints xs

handleConstraint :: Constraint -> (String -> a -> Maybe Values) -> (a -> Bool)
handleConstraint constraint f = go
  where
    xToValue = f $ cName constraint
    allValues = cValues constraint
    compareValues op = any (\v -> any (v `op`) allValues)
    compareValue op v = any (v `op`) allValues
    go x =
        case xToValue x of
            Nothing -> False
            Just value ->
                case (value, cComparison constraint) of
                    (SingleValue value', op) -> compareValue (toOperator op) value'
                    (MultipleValues values, op) -> compareValues (toOperator op) values
