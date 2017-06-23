module QueryBuilder.Demo where

import QueryBuilder.ExpressionContainer

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

comparisonLookup :: String -> Person -> Maybe ExpressionContainer
comparisonLookup "name" p = Just $ stringExpr (pName p)
comparisonLookup "id" p = Just $ intExpr (fromPersonId $ pId p)
comparisonLookup "hobby.id" p = Just $ intExprs $ map (fromHobbyId . hId) (pHobbies p)
comparisonLookup "hobby.name" p = Just $ stringExprs $ map hName (pHobbies p)
comparisonLookup "hobby.active" p = Just $ boolExprs $ map hActive (pHobbies p)
comparisonLookup _ _ = Nothing
