{-# LANGUAGE ScopedTypeVariables #-}
module Instance.Generator
    ( generateInstance
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.List
import           Data.Ord
import           System.IO.Unsafe

import           Instance.Type
import           Instance.Writer

import           Debug.Trace


getAcceptableInstance :: Symmetric -> Rand StdGen Instance -> IO Instance
getAcceptableInstance sym mkInstance = do
   inst <- evalRandIO mkInstance
   if checkInstance sym inst
     then return inst
     else getAcceptableInstance sym mkInstance

type Symmetric = Bool

makeSymmetric :: Instance -> Instance
makeSymmetric inst@(Instance minReq maxBud maxUpg caps dists upgCosts) = Instance minReq maxBud maxUpg caps dists' upgCosts'
  where
    dists' = zipWith makeSymmetric' dists [0 ..]
    upgCosts' = upgCosts
    makeSymmetric' :: [Distance] -> Int -> [Distance]
    makeSymmetric' xs 0 = xs
    makeSymmetric' xs nr = map (\idx -> Distance (normalDistance inst nr idx) (upgradedDistance inst nr idx)) [0 .. nr-1] ++ drop nr xs


generateInstance :: Symmetric -> Double -> InstanceSize -> IO Instance
generateInstance sym maxPct instSize =
  getAcceptableInstance sym $
  fmap mkSym $ do
    caps <- randCapacities
    let maxCap = maximum [minCap, maxPct * sum (map capNormal caps)]
        -- minCap = minimum (map capNormal caps) + 0.1
        minCap = sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capUpgraded caps)
    trace ("print (pct * sumCaps, pctPlus * sumCaps): " ++ show (minCap, maxCap))
      Instance <$> getRandomR (minCap, maxCap) <*> dbl <*> int <*> return caps <*> randDistances <*> randUpgradeCosts
  where
    mkSym
      | sym = makeSymmetric
      | otherwise = id
    maxSizeD = 10 ^ 3 :: Double
    maxSizeI = round maxSizeD :: Int
    dbl :: (RandomGen g) => Rand g Double
    dbl = getRandomR (1, maxSizeD)
    dbls = getRandomRs (1, maxSizeD)
    int = getRandomR (1, maxSizeI)
    randCapacities = replicateM instSize randCapacity
    randCapacity = fmap checkCapValues $ Capacity <$> dbl <*> dbl
    checkCapValues cap@(Capacity n u)
      | n < u = cap
      | otherwise = Capacity u n
    randDistances = mapM randDistance [0 .. instSize - 1]
    randDistance :: (RandomGen g) => Int -> Rand g [Distance]
    randDistance idx = replaceIndex idx (Distance 0 0) <$> replicateM instSize (fmap checkDistance $ Distance <$> dbl <*> dbl)
    checkDistance = id
    -- checkDistance dist@(Distance n u)
    --   | u < n = dist
    --   | otherwise = Distance u n
    replaceIndex idxx v xs = take idxx xs ++ v : drop (idxx + 1) xs
    randUpgradeCosts = mapM randUpgradeCost [0 .. instSize - 1]
    randUpgradeCost idx = fmap (replaceIndex idx 0 . take instSize) dbls


-- Needs to consider symmetry!!!!
checkInstance :: Bool -> Instance -> Bool
checkInstance True inst = checkInstance False (dropSym inst)
  where
    dropSym (Instance minCapReq maxBud maxUpg caps dists upgCosts) =
      Instance minCapReq maxBud maxUpg caps (zipWith (dropSym' (Distance 0 0)) dists [0 ..]) (zipWith (dropSym' 0) upgCosts [0 ..])
    dropSym' zero xs nr = replicate nr zero ++ drop nr xs
checkInstance _ inst@(Instance minCapReq maxBud maxUpg caps dists upgCosts) =
  minCapReq <= sum (map capUpgraded caps) && -- invariant: capUpgraded >= capNormal
  -- minCapReq >= sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capNormal caps) &&
  minCapReq >= sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capUpgraded caps) &&
  maxBud < sum (map sum upgCosts) && maxBud >= sum (take 2 $ dropWhile (== 0) $ sort $ concat upgCosts) && maxUpg < instSize
  where
    instSize = instanceSize inst
