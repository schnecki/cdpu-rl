{-# LANGUAGE ScopedTypeVariables #-}
module Instance.Generator
    ( generateInstance
    , generateInstanceOfType
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
makeSymmetric inst@(Instance tp minReq maxBud maxUpg caps dists upgCosts) = Instance tp minReq maxBud maxUpg caps dists' upgCosts'
  where
    dists' = zipWith makeSymmetric' dists [0 ..]
    upgCosts' = upgCosts
    makeSymmetric' :: [Distance] -> Int -> [Distance]
    makeSymmetric' xs 0  = xs
    makeSymmetric' xs nr = map (\idx -> Distance (normalDistance inst nr idx) (upgradedDistance inst nr idx)) [0 .. nr-1] ++ drop nr xs


generateInstance :: Symmetric -> Double -> Double -> InstanceSize -> IO Instance
generateInstance sym maxPct deltaUpgradeCosts instSize = do
  let tps = [minBound .. maxBound] :: [ProblemType]
  idxTp <- randomRIO (0, length tps - 1)
  let tp = tps !! idxTp
  generateInstanceOfType sym tp maxPct deltaUpgradeCosts instSize


generateInstanceOfType :: Symmetric -> ProblemType -> Double -> Double -> InstanceSize -> IO Instance
generateInstanceOfType sym tp maxPct deltaUpgradeCosts instSize = do
  getAcceptableInstance sym $
    fmap mkSym $ do
      caps <- randCapacities
      let maxCap = maximum [minCap, maxPct * sum (map capNormal caps)]
        -- minCap = minimum (map capNormal caps) + 0.1
          minCap = sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capUpgraded caps)
      upCosts <- randUpgradeCosts
      let budget = (deltaUpgradeCosts *) . (/2) . sum . map sum $ upCosts

      --trace ("print (pct * sumCaps, pctPlus * sumCaps): " ++ show (minCap, maxCap))
      Instance tp <$> getRandomR (minCap, maxCap) <*> pure budget <*> int <*> pure caps <*> randDistances tp <*> pure upCosts
  where
    mkSym
      | sym = makeSymmetric
      | otherwise = id
    maxSizeD = fromIntegral instSize ^ (2 :: Int) :: Double
    maxSizeI = round maxSizeD :: Int
    -- dbl :: (RandomGen g) => Rand g Double
    -- dbl = getRandomR (1, maxSizeD)
    dbls = getRandomRs (1, maxSizeD)
    int = getRandomR (1, maxSizeI)
    randCapacities = replicateM instSize randCapacity
    randCapacity = fmap checkCapValues $ Capacity <$> getRandomR (1, n ^ (2 :: Int)) <*> getRandomR (1, n ^ (2 :: Int))
      where
        n = fromIntegral instSize :: Double
    checkCapValues cap@(Capacity n u)
      | n < u = cap
      | otherwise = Capacity u n
    randDistances :: (RandomGen g) => ProblemType -> Rand g [[Distance]]
    randDistances Euclidean = do
      coords <- generateCoordinates xMax yMax
      return $ map (\x -> map (euclid coords x) [0 .. instSize - 1]) [0 .. instSize - 1]
      where
        xMax = fromIntegral instSize ^ (2 :: Int)
        yMax = xMax
    randDistances tp = mapM (randDistance tp) [0 .. instSize - 1]
    randDistance :: (RandomGen g) => ProblemType -> Int -> Rand g [Distance]
    randDistance tp idx = replaceIndex idx (Distance 0 0) <$> replicateM instSize (fmap checkDistance genDist)
      where
        genDist =
          case tp of
            Narrow -> Distance <$> fmap fromIntegral rand <*> fmap fromIntegral rand
              where n = floor . sqrt . fromIntegral $ instSize
                    rand = getRandomR (1, n :: Int)
            Wide -> Distance <$> fmap fromIntegral rand <*> fmap fromIntegral rand
              where n = fromIntegral instSize ^ 2
                    rand = getRandomR (1, n :: Int)
            _ -> error "generateInstance: Euclidean in randDistance"
    euclid coords i j =
      let (xi, xj) = (coords !! i) !! j
          (yi, yj) = (coords !! j) !! i
          d = (sqrt $ (xi - yi) ^ 2 + (xj - yj) ^ 2)
       in Distance d d -- TODO: fix upgraded distance
    generateCoordinates :: (RandomGen g) => Double -> Double -> Rand g [[(Double, Double)]]
    generateCoordinates xMax yMax = replicateM instSize (replicateM instSize ((,) <$> getRandomR (0, xMax) <*> getRandomR (0, yMax)))
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
    dropSym (Instance tp minCapReq maxBud maxUpg caps dists upgCosts) =
      Instance tp minCapReq maxBud maxUpg caps (zipWith (dropSym' (Distance 0 0)) dists [0 ..]) (zipWith (dropSym' 0) upgCosts [0 ..])
    dropSym' zero xs nr = replicate nr zero ++ drop nr xs
checkInstance _ inst@(Instance _ minCapReq maxBud maxUpg caps dists upgCosts) =
  minCapReq <= sum (map capUpgraded caps) && -- invariant: capUpgraded >= capNormal
  -- minCapReq >= sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capNormal caps) &&
  minCapReq >= sum (take 2 $ dropWhile (== 0) $ sortOn Down $ map capUpgraded caps) &&
  maxBud < sum (map sum upgCosts) && maxBud >= sum (take 2 $ dropWhile (== 0) $ sort $ concat upgCosts) && maxUpg < instSize
  where
    instSize = instanceSize inst
