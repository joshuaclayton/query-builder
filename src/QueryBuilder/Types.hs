{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module QueryBuilder.Types
    ( Value(..)
    , Constraints(..)
    , Constraint(..)
    , ComparisonOperator(..)
    , ToValue(..)
    , buildConstraints
    , explodeValue
    , displayValue
    ) where

import Data.Text (Text)
import GHC.Float (float2Double)

data Value
    = VString String
    | VInt Int
    | VDouble Double
    deriving (Show, Eq)

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
    deriving Show

data Constraint = Constraint
    { cName :: Text
    , cValues :: [Value]
    , cComparison :: ComparisonOperator
    } deriving (Show)

data ComparisonOperator
    = Equals
    deriving Show

buildConstraints :: (Constraints -> Constraints -> Constraints) -> [Constraints] -> Constraints
buildConstraints _ [a] = a
buildConstraints f [a, b] = f a b
buildConstraints f (a:b:xs) = buildConstraints f $ f a b : xs
buildConstraints _ _ = error "broken"

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
