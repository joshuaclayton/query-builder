{-# LANGUAGE GADTs #-}

module QueryBuilder.GadtTypes where

import Control.Applicative ((<|>))
import Data.List (intersect, union)

data GadtConstraints
    = Single GadtConstraint
    | And GadtConstraints GadtConstraints
    | Or GadtConstraints GadtConstraints
    | NoGadtConstraints
    deriving Show

data GadtConstraint = GadtConstraint
    { gcName :: String
    , gcContainer :: ExprContainer
    , gcComparison :: ComparisonOperator
    } deriving (Show)

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

data ExprContainer = ExprContainer
    { ecI :: Maybe (Expr Int)
    , ecD :: Maybe (Expr Double)
    , ecS :: Maybe (Expr String)
    , ecIs :: Maybe [Expr Int]
    , ecDs :: Maybe [Expr Double]
    , ecSs :: Maybe [Expr String]
    } deriving Show

instance Monoid ExprContainer where
    mempty = ExprContainer Nothing Nothing Nothing Nothing Nothing Nothing
    (ExprContainer a b c d e f) `mappend` (ExprContainer a' b' c' d' e' f') =
        ExprContainer a'' b'' c'' d'' e'' f''
      where
        a'' = a' <|> a
        b'' = b' <|> b
        c'' = c' <|> c
        d'' = d' <|> d
        e'' = e' <|> e
        f'' = f' <|> f

stringExpr :: String -> ExprContainer
stringExpr s = ExprContainer Nothing Nothing (Just $ S s) Nothing Nothing Nothing

stringExprs :: [String] -> ExprContainer
stringExprs ss = ExprContainer Nothing Nothing Nothing Nothing Nothing (Just $ map S ss)

intExpr :: Int -> ExprContainer
intExpr i = ExprContainer (Just $ I i) Nothing Nothing Nothing Nothing Nothing

intExprs :: [Int] -> ExprContainer
intExprs is = ExprContainer Nothing Nothing Nothing (Just $ map I is) Nothing Nothing

doubleExpr :: Double -> ExprContainer
doubleExpr d = ExprContainer Nothing (Just $ D d) Nothing Nothing Nothing Nothing

doubleExprs :: [Double] -> ExprContainer
doubleExprs ds = ExprContainer Nothing Nothing Nothing Nothing (Just $ map D ds) Nothing

data ComparisonOperator
    = Equals
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    deriving Show

applyConstraints :: Eq a => (String -> a -> Maybe ExprContainer) -> GadtConstraints -> [a] -> [a]
applyConstraints _ NoGadtConstraints xs = xs
applyConstraints f (Single constraint) xs = filter (handleConstraint constraint f) xs
applyConstraints f (And lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `intersect` applyConstraints f rconstraints xs
applyConstraints f (Or lconstraints rconstraints) xs =
    applyConstraints f lconstraints xs `union` applyConstraints f rconstraints xs

handleConstraint :: GadtConstraint -> (String -> a -> Maybe ExprContainer) -> (a -> Bool)
handleConstraint constraint f = go
  where
    xToValue = f $ gcName constraint
    exprContainer = gcContainer constraint
    op = gcComparison constraint
    go x =
        case xToValue x of
            Nothing -> False
            Just exprContainer' ->
                evalExpr $ comparableExpr' exprContainer exprContainer' op

buildConstraints :: (GadtConstraints -> GadtConstraints -> GadtConstraints) -> [GadtConstraints] -> GadtConstraints
buildConstraints _ [] = NoGadtConstraints
buildConstraints _ [a] = a
buildConstraints f [a, b] = f a b
buildConstraints f (a:b:xs) = buildConstraints f $ f a b : xs

test1, test2, test3, test4, test5, test6, test7 :: ExprContainer
test1 = mempty `mappend` intExpr 1
test2 = mempty `mappend` intExpr 1
test3 = mempty `mappend` intExpr 3
test4 = mempty `mappend` stringExpr "foo"
test5 = mempty `mappend` stringExpr "bar"
test6 = mempty `mappend` stringExpr "foo"
test7 = mempty `mappend` doubleExpr 1.2

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

comparableExpr' :: ExprContainer -> ExprContainer -> ComparisonOperator -> Expr Bool
comparableExpr' ec1 ec2 op = foldl1 Orr [singleValues, listValues, listValues']
  where
    listValues = case (combineMaybes (ecDs ec1) (ecD ec2), combineMaybes (ecIs ec1) (ecI ec2), combineMaybes (ecSs ec1) (ecS ec2)) of
        ( Just (v, v'), _, _ ) -> handleList op v v'
        ( _, Just (v, v'), _ ) -> handleList op v v'
        ( _, _, Just (v, v') ) -> handleList op v v'
        ( _, _, _ ) -> B False
    listValues' = case (combineMaybes (ecD ec1) (ecDs ec2), combineMaybes (ecI ec1) (ecIs ec2), combineMaybes (ecS ec1) (ecSs ec2)) of
        ( Just (v, v'), _, _ ) -> handleList op v' v
        ( _, Just (v, v'), _ ) -> handleList op v' v
        ( _, _, Just (v, v') ) -> handleList op v' v
        ( _, _, _ ) -> B False
    singleValues = case (combineMaybes (ecD ec1) (ecD ec2), combineMaybes (ecI ec1) (ecI ec2), combineMaybes (ecS ec1) (ecS ec2)) of
        ( Just (v, v'), _, _ ) -> handle op v v'
        ( _, Just (v, v'), _ ) -> handle op v v'
        ( _, _, Just (v, v') ) -> handle op v v'
        ( _, _, _ ) -> B False

handle :: (Eq a, Ord a) => ComparisonOperator -> Expr a -> Expr a -> Expr Bool
handle Equals = Eq
handle GreaterThan = Gt
handle GreaterThanOrEqual = Gte
handle LessThan = Lt
handle LessThanOrEqual = Lte

handleList :: Ord a => ComparisonOperator -> [Expr a] -> Expr a -> Expr Bool
handleList _ [] _ = B False
handleList op xs x = foldl1 Orr $ map (handle op x) xs

combineMaybes :: Maybe a -> Maybe b -> Maybe (a, b)
combineMaybes (Just v) (Just v') = Just (v, v')
combineMaybes _ _ = Nothing
