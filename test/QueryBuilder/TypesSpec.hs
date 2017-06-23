module QueryBuilder.TypesSpec where

import QueryBuilder.ExpressionContainer
import QueryBuilder.Parser
import QueryBuilder.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "applyConstraints" $ do
        it "works" $ do
            let results = search "hobby.name:Running"
            map pId (rwcResults results) `shouldBe` [PersonId 1]

        it "also works" $ do
            let results = search "hobby.name:Running AND id>=2"
            map pId (rwcResults results) `shouldBe` []

        it "works a third time" $ do
            let results' = search "hobby.name:Running AND id>=0"
            map pId (rwcResults results') `shouldBe` [PersonId 1]

        it "works with multiple things" $ do
            let (Right results) = parseConstraints "(value>50 AND value<100) OR (value>150 AND value<200) OR (value>25050 AND value<25100)"

            let appliedResults = applyConstraints intValueComparisonLookup results [1..50000]
            length (rwcResults appliedResults) `shouldBe` (49*3)


newtype PersonId = PersonId { fromPersonId :: Int } deriving (Eq, Show)
newtype HobbyId = HobbyId { fromHobbyId :: Int } deriving (Eq, Show)

data Hobby = Hobby
    { hId :: HobbyId
    , hName :: String
    , hActive :: Bool
    } deriving (Eq, Show)

data Person = Person
    { pId :: PersonId
    , pName :: String
    , pHobbies :: [Hobby]
    } deriving (Eq, Show)

running, biking, fishing, rowing, climbing :: Hobby
running = Hobby (HobbyId 1) "Running" True
biking = Hobby (HobbyId 2) "Biking" True
fishing = Hobby (HobbyId 3) "Fishing" True
rowing = Hobby (HobbyId 4) "Rowing" True
climbing = Hobby (HobbyId 5) "Climbing" False

people :: [Person]
people =
    [ Person (PersonId 1) "John" [running, biking]
    , Person (PersonId 2) "Jane" [rowing]
    , Person (PersonId 3) "Jim" [climbing]
    , Person (PersonId 4) "Jill" [fishing, rowing, biking]
    ]

search :: String -> ResultsWithContext Person
search s =
    case parseConstraints s of
        Left e -> error "broken"
        Right constraints ->
            applyConstraints comparisonLookup constraints people

comparisonLookup :: String -> Person -> Maybe ExpressionContainer
comparisonLookup "name" p = Just $ stringExpr (pName p)
comparisonLookup "id" p = Just $ intExpr (fromPersonId $ pId p)
comparisonLookup "hobby.id" p = Just $ intExprs $ map (fromHobbyId . hId) (pHobbies p)
comparisonLookup "hobby.name" p = Just $ stringExprs $ map hName (pHobbies p)
comparisonLookup "hobby.active" p = Just $ boolExprs $ map hActive (pHobbies p)
comparisonLookup _ _ = Nothing

intValueComparisonLookup :: String -> Int -> Maybe ExpressionContainer
intValueComparisonLookup "value" = Just . intExpr
intValueComparisonLookup _ = const Nothing
