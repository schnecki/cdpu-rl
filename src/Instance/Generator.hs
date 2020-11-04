{-# LANGUAGE ScopedTypeVariables #-}
module Instance.Generator
    ( generateInstance
    ) where

import           Data.List
import           System.Random

import           Instance.Type


generateInstance :: Double -> InstanceSize -> IO () -- Instance
generateInstance pct instSize = do
  gInt <- newStdGen
  let intRs = randomRs (1, round maxSize) gInt :: [Int]
  gDbl <- newStdGen
  let dblRs = randomRs (0.1, maxSize) gDbl :: [Double]
  let minCapReq caps = sum caps * pct
  let res = [(r1, r2) | r1 <- take 10 intRs, r2 <- take 10 (drop 10 intRs), 2 * r1 > r2]
  print $ take 10 intRs
  print res
  where
    maxSize = 10 ^ 6 :: Double


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


