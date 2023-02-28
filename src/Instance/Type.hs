

module Instance.Type
  (InstanceSize
  , Distance (..)
  , ProblemType (..)
  , Capacity (..)
  , Instance (..)
  , normalDistance
  , upgradedDistance
  , upgradedCost
  , nrVertices
  , instanceSize
  )
where

type InstanceSize = Int


data Distance =
  Distance
    { distNormal   :: Double
    , distUpgraded :: Double
    }
  deriving (Show, Eq, Ord)

data Capacity =
  Capacity
    { capNormal   :: Double
    , capUpgraded :: Double
    }
  deriving (Show, Eq, Ord)


data ProblemType
  = Narrow
  | Wide
  | Euclidean
  deriving (Show, Eq, Ord, Bounded, Enum)


data Instance =
  Instance
    { problemType         :: ProblemType
    , minCapacityRequired :: Double -- B
    , maxBudget           :: Double -- C
    , maxUpgrades         :: Int
    , capacity            :: [Capacity]
    , distance            :: [[Distance]]
    , upgradeCost         :: [[Double]]
    }
  deriving (Show, Eq, Ord)


distFun :: (Show a) => (Instance -> [[a]]) -> (a -> Double) -> Instance -> Int -> Int -> Double
distFun sel f inst x y
  | x == y = 0
  | x > y = distFun sel f inst y x
  | otherwise = f ((sel inst !? x) !? y)
  where (!?) xs nr
          | nr < length xs = xs !! nr
          | otherwise = error $ "unexpected index in normalDistance: " ++ show xs ++ " !! " ++ show nr ++ " with (x,y) = " ++ show (x,y)

normalDistance :: Instance -> Int -> Int -> Double
normalDistance = distFun distance distNormal

upgradedDistance :: Instance -> Int -> Int -> Double
upgradedDistance = distFun distance distUpgraded

upgradedCost :: Instance -> Int -> Int -> Double
upgradedCost = distFun upgradeCost id


nrVertices :: Instance -> Int
nrVertices = length . distance

instanceSize :: Instance -> InstanceSize
instanceSize = nrVertices
