{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Search.SearchInstances
    ( searchInstances

    ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Default
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           EasyLogger
import           System.Directory
import           System.FilePath.Posix
import           System.Random

import           GenOptions
import           Instance.Generator    as G
import           Instance.Type
import           Instance.Writer
import           Testing.TestCPLEX

type MinRuntime = Double -- in seconds

type Runtime = Double

data St = St
  { curSize            :: !Int        -- Current problem size
  , curPct             :: !Double     -- Pct of maximum capacity
  , curDeltaUpgrade    :: !Double     -- Budget is fraction of sum of upgrading costs
  , minTime            :: !MinRuntime -- Minimum Runtime threshold in seconds
  , pcts               :: !(M.Map Double [(Int, Runtime)])
  , sizes              :: !(M.Map Int [(Double, Runtime)])
  , goodInstances      :: Int
  , generatedInstances :: Int
  , minGoodInstances   :: Int
  , problemType        :: ProblemType
  } deriving (Show)

-- | Maximum size
maxSize :: Int
maxSize = 500

-- | Max pct
maxPct :: Double
maxPct = 0.4

-- | Max delta upgrad
maxDeltaUpgrade :: Double
maxDeltaUpgrade = 0.5


-- | Searches for instances and writes valid ones to the disk
searchInstances :: GenOptions -> Int -> IO ()
searchInstances ops minInstances = do
  $(logDebugText) $ "Options: " <> T.pack (show ops)
  evalStateT (searchInstances' ops) (St (optSize ops) 0.2 0.2 (fromIntegral . optMinSecs $ ops) M.empty M.empty 0 0 minInstances (optProbType ops))


isGood :: MinRuntime -> TestResult -> Bool
isGood mT (TestResult time mLb mUb _) = mT <= time && maybe False (> 0) mLb && (Just True == ((>) . (50 *) <$> mLb <*> mUb))

searchInstances' :: GenOptions -> StateT St IO ()
searchInstances' ops = do
  st@(St size pct deltaUp minTime pcts sizes nrGood nrInsts _ tp) <- get
  $(logDebugText) $ "Starting search. St: " <> T.pack (show st)
  inst <-
    lift $
    generateInstanceOfType
      (def
         { G.symmetric = True
         , G.problemType = tp
         , upgradedCapacityFactor = fromMaybe (upgradedCapacityFactor def) (optCap'Factor ops)
         , upgradedDistanceFactor = fromMaybe (upgradedDistanceFactor def) (optCap'Dist ops)
         })
      pct
      deltaUp
      size
  let directory = "/tmp/"
  let instanceName = show size
  let fp = directory </> instanceName
  $(logDebugText) $ "Starting CPLEX on " <> T.pack fp
  let t = floor $ optMaxUp'Factor ops * (fromIntegral size * (fromIntegral size - 1) / 2)
  res <- liftIO $ writeInstance fp inst >> testCPLEX (round . (2.1 *) $ minTime) t fp
  $(logInfoText) $ "Try " <> T.pack (show $ nrInsts + 1) <> " w/ size, pct: " <> T.pack (show (size, pct, deltaUp)) <> ". Result: " <> T.pack (show res)
  -- Save and iterater
  addCounter (isGood minTime res)
  if isGood minTime res
    then saveInstanceToDisk inst res >> adaptAndRepeat ops res
    else adaptAndRepeat ops res

addCounter :: Bool -> StateT St IO ()
addCounter True = do
  St size pct deltaUp minTime pcts sizes nrGood nrInsts nrMinInsts tp <- get
  put $ St size pct deltaUp minTime pcts sizes (nrGood + 1) (nrInsts + 1) nrMinInsts tp
addCounter False = do
  St size pct deltaUp minTime pcts sizes nrGood nrInsts nrMinInsts tp <- get
  put $ St size pct deltaUp minTime pcts sizes nrGood (nrInsts + 1) nrMinInsts tp


(<<) :: (Num a, Ord a) => a -> a -> Bool
x << y = 10 * x < y


adaptAndRepeat :: GenOptions -> TestResult -> StateT St IO ()
adaptAndRepeat ops (TestResult secs lb up out) = do
  St sz pct delta minTime _ _ nrGood _ nrInstances tp <- get
  $(logInfoText) $ "Instances: " <> T.pack (show nrGood <> "/" <> show nrInstances)
  when (nrGood < nrInstances) $ do
    if secs << minTime
      then oneOf [wRand (1.5, 2.5) changeSz, wRand (0.5, 1.5) changePct, wRand (0.5, 1.5) changeDeltaUp]
      else if secs < minTime
             then oneOf [wRand (1.0, 1.1) changeSz, wRand (0.9, 1.0) changePct, wRand (0.9, 1.0) changeDeltaUp]
             else if secs >= 2 * minTime
                    then oneOf [wRand (0.9, 1) changeSz, wRand (1, 1.1) changePct, wRand (1.0, 1.1) changeDeltaUp]
                    else wRand (0.9, 1.1) changeSz >> wRand (0.9, 1.1) changePct >> wRand (0.9, 1.1) changeDeltaUp
    -- Always randomize a little
    St sz' pct' delta' _ _ _ _ _ _ _ <- get
    when (sz == sz') $ wRand (0.95, 1.05) changeSz
    when (pct == pct') $ wRand (0.95, 1.05) changePct
    when (delta == delta') $ wRand (0.95, 1.05) changeDeltaUp
    searchInstances' ops
  where
    oneOf xs = liftIO (randomRIO (0, length xs - 1)) >>= \i -> xs !! i
    wRand bounds f = liftIO (randomRIO bounds) >>= f
    changeSz :: Double -> StateT St IO ()
    changeSz pct = do
      $(logDebugText) $ "Modifying size to " <> T.pack (show . round' . (100 *) $ pct) <> "%"
      modify (\x -> x {curSize = min maxSize . round . (pct *) . fromIntegral . curSize $ x})
    changePct :: Double -> StateT St IO ()
    changePct pct = do
      $(logDebugText) $ "Modifying pct to " <> T.pack (show . round' . (100 *) $ pct) <> "%"
      modify (\x -> x {curPct = min maxPct . (/100) . round' . (pct *) . (100*) . curPct $ x})
    changeDeltaUp :: Double -> StateT St IO ()
    changeDeltaUp pct = do
      $(logDebugText) $ "Modifying delta upgradings to " <> T.pack (show . round' . (100 *) $ pct) <> "%"
      modify (\x -> x {curDeltaUpgrade  = min maxDeltaUpgrade . (/100) . round' . (pct *) . (100*) . curDeltaUpgrade $ x})
    round' = fromIntegral . round

saveInstanceToDisk :: Instance -> TestResult -> StateT St IO ()
saveInstanceToDisk inst res@(TestResult secs lb ub out) = do
  liftIO $ createDirIfMissing searchDir
  liftIO $ createDirIfMissing resultDir
  liftIO $ createDirIfMissing instanceDir
  St size _ _ _ _ _ nrGood _ _ tp <- get
  let instanceName = show tp <> "_nr" <> show nrGood <> "_sz" <> show size <> "_cplexTime" <> show secs <> "_lb" <> maybe "-" show lb <> "_ub" <> maybe "-" show ub
  liftIO $ writeInstance (instanceDir </> instanceName) inst
  liftIO $ appendFile (instanceDir </> instanceName) ("\n// " <> show res)
  liftIO $ writeFile (resultDir </> "output_" <> instanceName <> ".txt") (T.unpack out)
  liftIO $ copyFile (cplexOutDir </> "problem.lp") (resultDir </> "problem_" <> instanceName <> ".lp")
  liftIO $ copyFile (cplexOutDir </> "results.txt") (resultDir </> "results_" <> instanceName <> ".txt")
  liftIO $ copyFile (cplexOutDir </> "results.txt_details.txt") (resultDir </> "results_details_" <> instanceName <> ".txt")
  where searchDir = "search_result"
        instanceDir = searchDir </> "instances"
        resultDir = searchDir </> "results"
        createDirIfMissing dir = doesDirectoryExist dir >>= \x -> unless x (createDirectory dir)
        cplexOutDir = cplexDir </> "results"
