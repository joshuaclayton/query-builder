{-# LANGUAGE GADTs #-}

module QueryBuilder.Types where

import Data.List (intersect, union)

data Constraints
    = Single Constraint
    | And Constraints Constraints
    | Or Constraints Constraints
    | NoConstraints
    deriving Show

data Constraint = Constraint
    { cName :: String
    , cContainer :: ExpressionContainer
    , cComparison :: ComparisonOperator
    } deriving (Show)

data ExpressionContainer = ExpressionContainer
    { ecInt :: Expressions Int
    , ecDouble :: Expressions Double
    , ecString :: Expressions String
    , ecBool :: Expressions Bool
    } deriving Show

instance Monoid ExpressionContainer where
    mempty = ExpressionContainer NoExpression NoExpression NoExpression NoExpression
    (ExpressionContainer i d s b) `mappend` (ExpressionContainer i' d' s' b') =
        ExpressionContainer (i `mappend` i') (d `mappend` d') (s `mappend` s') (b `mappend` b')

data Expressions a
    = NoExpression
    | SingleExpression (Expr a)
    | MultipleExpressions [Expr a]
    deriving Show

instance Monoid (Expressions a) where
    mempty = NoExpression
    a `mappend` NoExpression = a
    NoExpression `mappend` a = a
    (SingleExpression a) `mappend` (SingleExpression a') = MultipleExpressions [a, a']
    (MultipleExpressions as) `mappend` (SingleExpression a) = MultipleExpressions $ as ++ pure a
    (SingleExpression a) `mappend` (MultipleExpressions as) = MultipleExpressions $ pure a ++ as
    (MultipleExpressions as) `mappend` (MultipleExpressions as') = MultipleExpressions $ as ++ as'

data ComparisonOperator
    = Equals
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    deriving Show

data Expr a where
    I :: Int -> Expr Int
    D :: Double -> Expr Double
    S :: String -> Expr String
    B :: Bool -> Expr Bool
    Orr :: Expr Bool -> Expr Bool -> Expr Bool
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
    Lt :: Ord a => Expr a -> Expr a -> Expr Bool
    Lte :: Ord a => Expr a -> Expr a -> Expr Bool
    Gt :: Ord a => Expr a -> Expr a -> Expr Bool
    Gte :: Ord a => Expr a -> Expr a -> Expr Bool

instance Show (Expr a) where
    show (I v) = show v
    show (D v) = show v
    show (S v) = show v
    show (B v) = show v
    show (Orr v v') = show v ++ " || " ++ show v'
    show (Eq v v') = show v ++ " == " ++ show v'
    show (Lt v v') = show v ++ " < " ++ show v'
    show (Lte v v') = show v ++ " <= " ++ show v'
    show (Gt v v') = show v ++ " > " ++ show v'
    show (Gte v v') = show v ++ " >= " ++ show v'

stringExpr :: String -> ExpressionContainer
stringExpr x = mempty { ecString = SingleExpression $ S x }

stringExprs :: [String] -> ExpressionContainer
stringExprs xs = mempty { ecString = MultipleExpressions $ map S xs }

intExpr :: Int -> ExpressionContainer
intExpr x = mempty { ecInt = SingleExpression $ I x }

intExprs :: [Int] -> ExpressionContainer
intExprs xs = mempty { ecInt = MultipleExpressions $ map I xs }

doubleExpr :: Double -> ExpressionContainer
doubleExpr x = mempty { ecDouble = SingleExpression $ D x }

doubleExprs :: [Double] -> ExpressionContainer
doubleExprs xs = mempty { ecDouble = MultipleExpressions $ map D xs }

boolExpr :: Bool -> ExpressionContainer
boolExpr x = mempty { ecBool = SingleExpression $ B x }

boolExprs :: [Bool] -> ExpressionContainer
boolExprs xs = mempty { ecBool = MultipleExpressions $ map B xs }

applyConstraints :: Eq a => (String -> a -> Maybe ExpressionContainer) -> Constraints -> [a] -> [a]
applyConstraints _ NoConstraints xs = xs
applyConstraints f (Single constraint) xs = filter (handleConstraint constraint f) xs
applyConstraints f (And lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `intersect` applyConstraints f rconstraints xs
applyConstraints f (Or lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `union` applyConstraints f rconstraints xs

handleConstraint :: Constraint -> (String -> a -> Maybe ExpressionContainer) -> (a -> Bool)
handleConstraint constraint f = maybe False processContainer . xToValue
  where
    xToValue = f $ cName constraint
    exprContainer = cContainer constraint
    op = cComparison constraint
    processContainer = evalExpr . compareExpressions op exprContainer

buildConstraints :: (Constraints -> Constraints -> Constraints) -> [Constraints] -> Constraints
buildConstraints _ [] = NoConstraints
buildConstraints _ [a] = a
buildConstraints f [a, b] = f a b
buildConstraints f (a:b:xs) = buildConstraints f $ f a b : xs

evalExpr :: Expr a -> a
evalExpr (I i) = i
evalExpr (D d) = d
evalExpr (S s) = s
evalExpr (B b) = b
evalExpr (Orr e1 e2) = evalExpr e1 || evalExpr e2
evalExpr (Eq e1 e2) = evalExpr e1 == evalExpr e2
evalExpr (Lt e2 e1) = evalExpr e1 < evalExpr e2
evalExpr (Lte e2 e1) = evalExpr e1 <= evalExpr e2
evalExpr (Gt e2 e1) = evalExpr e1 > evalExpr e2
evalExpr (Gte e2 e1) = evalExpr e1 >= evalExpr e2

compareExpressions :: ComparisonOperator -> ExpressionContainer -> ExpressionContainer -> Expr Bool
compareExpressions op ec1 ec2 = foldl1 Orr [go ecDouble, go ecInt, go ecString, go ecBool]
  where
    go :: Ord a => (ExpressionContainer -> Expressions a) -> Expr Bool
    go f = handleExpressions op (f ec1) (f ec2)

handle :: (Eq a, Ord a) => ComparisonOperator -> Expr a -> Expr a -> Expr Bool
handle Equals = Eq
handle GreaterThan = Gt
handle GreaterThanOrEqual = Gte
handle LessThan = Lt
handle LessThanOrEqual = Lte

handleList :: Ord a => ComparisonOperator -> [Expr a] -> Expr a -> Expr Bool
handleList _ [] _ = B False
handleList op xs x = foldl1 Orr $ map (handle op x) xs

handleExpressions :: Ord a => ComparisonOperator -> Expressions a -> Expressions a -> Expr Bool
handleExpressions _ NoExpression _ = B False
handleExpressions _ _ NoExpression = B False
handleExpressions op (SingleExpression a) (SingleExpression b) = handle op a b
handleExpressions op (SingleExpression a) (MultipleExpressions as) = handleList op as a
handleExpressions op (MultipleExpressions as) (SingleExpression a) = handleList op as a
handleExpressions op (MultipleExpressions as) (MultipleExpressions bs) = foldl1 Orr $ map (handleList op as) bs
