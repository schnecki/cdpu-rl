{-# LANGUAGE ScopedTypeVariables #-}
module Instance.Generator
    ( generateInstance
    ) where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

import           Instance.Type


getAcceptableInstance :: Rand StdGen Instance -> IO Instance
getAcceptableInstance mkInstance = do
   inst <- evalRandIO mkInstance
   if checkInstance inst
     then return inst
     else getAcceptableInstance mkInstance

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
generateInstance sym pct instSize =  getAcceptableInstance $ fmap mkSym $ Instance <$> dbl <*> dbl <*> int <*> randCapacities <*> randDistances <*> randUpgradeCosts
  -- gInt <- newStdGen
  -- let intRs = randomRs (1, round maxSizeI) gInt :: [Int]
  -- gDbl <- newStdGen
  -- let dblRs = randomRs (0.1, maxSizeD) gDbl :: [Double]
  -- let minCapReq caps = sum caps * pct
  -- let res = [(r1, r2) | r1 <- take 10 intRs, r2 <- take 10 (drop 10 intRs), 2 * r1 > r2]
  -- print $ take 10 intRs
  -- print res
  where
    mkSym | sym = makeSymmetric
          | otherwise = id
    maxSizeD = 10 ^ 1 :: Double
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
    checkDistance dist@(Distance n u)
      | u < n = dist
      | otherwise = Distance u n
    replaceIndex idxx v xs = take idxx xs ++ v : drop (idxx + 1) xs
    randUpgradeCosts = mapM randUpgradeCost [0 .. instSize - 1]
    randUpgradeCost idx = fmap (replaceIndex idx 0 . take instSize) $ dbls


checkInstance :: Instance -> Bool
checkInstance inst@(Instance minCapReq maxBudget maxUpg caps dists upgCosts) =
  minCapReq <= sumCapsUpgraded && minCapReq >= sum (take 2 capsNormalSorted) &&
  maxBudget < sum (map sum upgCosts) && maxBudget >= sum (take 2 $ sort $ concat upgCosts) &&
  maxUpg < instSize


  where sumCapsNormal = sum . map capNormal $ caps
        capsNormal = map capNormal caps
        capsNormalSorted = sort $ map capNormal caps
        sumCapsUpgraded = sum . map capUpgraded $ caps
        instSize = instanceSize inst


