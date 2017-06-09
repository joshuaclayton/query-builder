module QueryBuilder.Demo where

import           Data.List (intersect, union)
import qualified Data.Text as T
import           QueryBuilder.Types (ToValue, Value(..), Constraints(..), Constraint(..), ComparisonOperator(..), toValue)

newtype PersonId = PersonId Int deriving (Eq, Show)
newtype HobbyId = HobbyId Int deriving (Eq, Show)
data Hobby = Hobby
    { hId :: HobbyId
    , hName :: String
    } deriving (Eq, Show)

data Person = Person
    { pId :: PersonId
    , pName :: String
    , pHobbies :: [Hobby]
    } deriving (Eq, Show)

instance ToValue PersonId where
    toValue (PersonId i) = VInt i

instance ToValue HobbyId where
    toValue (HobbyId i) = VInt i

running, biking, fishing, rowing :: Hobby
running = Hobby (HobbyId 1) "Running"
biking = Hobby (HobbyId 2) "Biking"
fishing = Hobby (HobbyId 3) "Fishing"
rowing = Hobby (HobbyId 4) "Rowing"

people :: [Person]
people =
    [ Person (PersonId 1) "John" [running, biking]
    , Person (PersonId 2) "Jane" [rowing]
    , Person (PersonId 3) "Jim" []
    , Person (PersonId 4) "Jill" [fishing, rowing]
    ]

go :: Eq a => (String -> a -> Maybe Values) -> Constraints -> [a] -> [a]
go f (Single constraint) xs = filter (handleConstraint constraint f) xs
go f (And lconstraints rconstraints) xs =
    go f lconstraints xs `intersect` go f rconstraints xs
go f (Or lconstraints rconstraints) xs =
    go f lconstraints xs `union` go f rconstraints xs

handleConstraint :: Constraint -> (String -> a -> Maybe Values) -> (a -> Bool)
handleConstraint constraint f = go'
  where
    xToValue = f $ T.unpack $ cName constraint
    allValues = cValues constraint
    go' x =
        case xToValue x of
            Nothing -> False
            Just value ->
                case (value, cComparison constraint) of
                    (SingleValue value', Equals) -> value' `elem` allValues
                    (MultipleValues values, Equals) -> not $ null $ values `intersect` allValues

data Values
    = SingleValue Value
    | MultipleValues [Value]

comparisonLookup :: String -> Person -> Maybe Values
comparisonLookup "name" p = Just $ SingleValue $ toValue (pName p)
comparisonLookup "id" p = Just $ SingleValue $ toValue (pId p)
comparisonLookup "hobby.id" p = Just $ MultipleValues $ map (toValue . hId) (pHobbies p)
comparisonLookup "hobby.name" p = Just $ MultipleValues $ map (toValue . hName) (pHobbies p)
comparisonLookup _ _ = Nothing
