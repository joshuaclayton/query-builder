module QueryBuilder.ExpressionContainerSpec where

import QueryBuilder.ExpressionContainer
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "evalExpr" $
        it "works" $ do
            evalExpr (Eq (I 123) (I 456)) `shouldBe` False
            evalExpr (Eq (I 123) (I 123)) `shouldBe` True
            evalExpr ((I 123) `Gte` (I 321)) `shouldBe` False

    describe "compareExpressions" $ do
        it "works for single values" $ do
            evalExpr (compareExpressions Equals (stringExpr "foo") (intExpr 1)) `shouldBe` False
            evalExpr (compareExpressions Equals (stringExpr "foo") (stringExpr "foo")) `shouldBe` True
            evalExpr (compareExpressions Equals (stringExpr "foo") (stringExpr "bar")) `shouldBe` False

        it "works when one expression is a list" $ do
            evalExpr (compareExpressions Equals (stringExpr "foo") (stringExprs ["foo", "bar"])) `shouldBe` True
            evalExpr (compareExpressions Equals (stringExpr "no match") (stringExprs ["foo", "bar"])) `shouldBe` False
            evalExpr (compareExpressions Equals (intExpr 1) (stringExprs ["foo", "bar"])) `shouldBe` False
            evalExpr (compareExpressions GreaterThanOrEqual (intExpr 1) (intExprs [2,3,4])) `shouldBe` False
            evalExpr (compareExpressions GreaterThanOrEqual (intExpr 2) (intExprs [2,3,4])) `shouldBe` True
