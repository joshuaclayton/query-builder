{-# LANGUAGE GADTs #-}

module QueryBuilder.ExpressionContainer
    ( ExpressionContainer(..)
    , Expressions(..)
    , Expr(..)
    , Constraints(..)
    , Constraint(..)
    , ComparisonOperator(..)

    , evalExpr

    , stringExpr
    , stringExprs
    , intExpr
    , intExprs
    , doubleExpr
    , doubleExprs
    , boolExpr
    , boolExprs

    , compareExpressions
    ) where

data Constraints
    = Single Constraint
    | And Constraints Constraints
    | Or Constraints Constraints
    | NoConstraints
    deriving (Eq, Show)

data Constraint = Constraint
    { cName :: String
    , cContainer :: ExpressionContainer
    , cComparison :: ComparisonOperator
    } deriving (Eq, Show)

data ComparisonOperator
    = Equals
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    deriving (Eq, Show)

data Expr a where
    I   ::          Int       -> Expr Int
    D   ::          Double    -> Expr Double
    S   ::          String    -> Expr String
    B   ::          Bool      -> Expr Bool
    Orr ::          Expr Bool -> Expr Bool -> Expr Bool
    Eq  :: Eq  a => Expr a    -> Expr a    -> Expr Bool
    Lt  :: Ord a => Expr a    -> Expr a    -> Expr Bool
    Lte :: Ord a => Expr a    -> Expr a    -> Expr Bool
    Gt  :: Ord a => Expr a    -> Expr a    -> Expr Bool
    Gte :: Ord a => Expr a    -> Expr a    -> Expr Bool

instance Eq (Expr a) where
    (==) = heq
      where
        heq :: Expr a -> Expr b -> Bool
        heq (I v)      (I v')       = v == v'
        heq (D v)      (D v')       = v == v'
        heq (S v)      (S v')       = v == v'
        heq (B v)      (B v')       = v == v'
        heq (Orr v v') (Orr v1 v1') = v `heq` v1 && v' `heq` v1'
        heq (Eq  v v') (Eq  v1 v1') = v `heq` v1 && v' `heq` v1'
        heq (Lt  v v') (Lt  v1 v1') = v `heq` v1 && v' `heq` v1'
        heq (Lte v v') (Lte v1 v1') = v `heq` v1 && v' `heq` v1'
        heq (Gt  v v') (Gt  v1 v1') = v `heq` v1 && v' `heq` v1'
        heq (Gte v v') (Gte v1 v1') = v `heq` v1 && v' `heq` v1'
        heq _ _ = False

instance Show (Expr a) where
    show (I v)      = show v
    show (D v)      = show v
    show (S v)      = show v
    show (B v)      = show v
    show (Orr v v') = show v ++ " || " ++ show v'
    show (Eq  v v') = show v ++ " == " ++ show v'
    show (Lt  v v') = show v ++ " < "  ++ show v'
    show (Lte v v') = show v ++ " <= " ++ show v'
    show (Gt  v v') = show v ++ " > "  ++ show v'
    show (Gte v v') = show v ++ " >= " ++ show v'

data ExpressionContainer = ExpressionContainer
    { ecInt :: Expressions Int
    , ecDouble :: Expressions Double
    , ecString :: Expressions String
    , ecBool :: Expressions Bool
    } deriving (Eq, Show)

instance Monoid ExpressionContainer where
    mempty = ExpressionContainer NoExpression NoExpression NoExpression NoExpression
    (ExpressionContainer i d s b) `mappend` (ExpressionContainer i' d' s' b') =
        ExpressionContainer (i `mappend` i') (d `mappend` d') (s `mappend` s') (b `mappend` b')

data Expressions a
    = NoExpression
    | SingleExpression (Expr a)
    | MultipleExpressions [Expr a]
    deriving (Eq, Show)

instance Monoid (Expressions a) where
    mempty = NoExpression
    a `mappend` NoExpression = a
    NoExpression `mappend` a = a
    (SingleExpression a)     `mappend` (SingleExpression a')     = MultipleExpressions [a, a']
    (MultipleExpressions as) `mappend` (SingleExpression a)      = MultipleExpressions $ as ++ pure a
    (SingleExpression a)     `mappend` (MultipleExpressions as)  = MultipleExpressions $ pure a ++ as
    (MultipleExpressions as) `mappend` (MultipleExpressions as') = MultipleExpressions $ as ++ as'

evalExpr :: Expr a -> a
evalExpr (I i) = i
evalExpr (D d) = d
evalExpr (S s) = s
evalExpr (B b) = b
evalExpr (Orr e1 e2) = evalExpr e1 || evalExpr e2
evalExpr (Eq  e1 e2) = evalExpr e1 == evalExpr e2
evalExpr (Lt  e1 e2) = evalExpr e1 <  evalExpr e2
evalExpr (Lte e1 e2) = evalExpr e1 <= evalExpr e2
evalExpr (Gt  e1 e2) = evalExpr e1 >  evalExpr e2
evalExpr (Gte e1 e2) = evalExpr e1 >= evalExpr e2


handle :: (Eq a, Ord a) => ComparisonOperator -> Expr a -> Expr a -> Expr Bool
handle Equals = Eq
handle GreaterThan = Gt
handle GreaterThanOrEqual = Gte
handle LessThan = Lt
handle LessThanOrEqual = Lte

handleList :: Ord a => ComparisonOperator -> [Expr a] -> Expr a -> Expr Bool
handleList _ [] _ = B False
handleList op xs x = foldl1 Orr $ map (handle op x) xs

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

compareExpressions :: ComparisonOperator -> ExpressionContainer -> ExpressionContainer -> Expr Bool
compareExpressions op ec1 ec2 = foldl1 Orr [go ecDouble, go ecInt, go ecString, go ecBool]
  where
    go :: Ord a => (ExpressionContainer -> Expressions a) -> Expr Bool
    go f = handleExpressions op (f ec1) (f ec2)

handleExpressions :: Ord a => ComparisonOperator -> Expressions a -> Expressions a -> Expr Bool
handleExpressions _   NoExpression             _                       = B False
handleExpressions _   _                        NoExpression            = B False
handleExpressions op (SingleExpression a)     (SingleExpression b)     = handle op b a
handleExpressions op (SingleExpression a)     (MultipleExpressions as) = handleList op as a
handleExpressions op (MultipleExpressions as) (SingleExpression a)     = handleList op as a
handleExpressions op (MultipleExpressions as) (MultipleExpressions bs) = foldl1 Orr $ map (handleList op as) bs
