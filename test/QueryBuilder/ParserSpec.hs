module QueryBuilder.ParserSpec where

import Data.Monoid ((<>))
import QueryBuilder.ExpressionContainer
import QueryBuilder.Parser
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseConstraints" $ do
        it "works with single constraints" $ do
            let (Right (Single constraint)) = parseConstraints "foo:bar"
            cName constraint `shouldBe` "foo"
            cComparison constraint `shouldBe` Equals
            cContainer constraint `shouldBe` stringExpr "bar"

        it "works with multiple constraints" $ do
            let (Right result) = parseConstraints "foo:bar AND baz:1"
            let firstExpression = Single $ Constraint "foo" (stringExpr "bar") Equals
            let secondExpression = Single $ Constraint "baz" (intExpr 1 <> stringExpr "1" <> doubleExpr 1) Equals
            result `shouldBe` And firstExpression secondExpression

        it "works with parenthesis" $ do
            let (Right result) = parseConstraints "foo:bar AND (baz:buzz OR baz:boo)"
            let firstExpression = Single $ Constraint "foo" (stringExpr "bar") Equals
            let secondExpression = Single $ Constraint "baz" (stringExpr "buzz") Equals
            let thirdExpression = Single $ Constraint "baz" (stringExpr "boo") Equals
            result `shouldBe` And firstExpression (Or secondExpression thirdExpression)

        it "works with operators" $ do
            let (Right result) = parseConstraints "foo>1 AND bar<=5"
            let firstExpression = Single $ Constraint "foo" (intExpr 1 <> doubleExpr 1) GreaterThan
            let secondExpression = Single $ Constraint "bar" (intExpr 5 <> doubleExpr 5) LessThanOrEqual
            result `shouldBe` And firstExpression secondExpression

        it "works with literals" $ do
            let (Right result) = parseConstraints "bar:\"5\""
            let firstExpression = Single $ Constraint "bar" (stringExpr "5") Equals
            result `shouldBe` firstExpression
