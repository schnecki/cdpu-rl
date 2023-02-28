{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Search.SearchInstances
    ( searchInstances

    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           EasyLogger
import           System.Directory
import           System.FilePath.Posix
import           System.Random

import           Instance.Generator
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
  }

maxSize :: Int
maxSize = 500

maxPct :: Double
maxPct = 0.4

maxDeltaUpgrade :: Double
maxDeltaUpgrade = 0.5


-- | Searches for instances and writes valid ones to the disk
searchInstances :: ProblemType -> Int -> Double -> Int -> IO ()
searchInstances tp sz minTime minInstances = evalStateT searchInstances' (St sz 0.2 0.2 minTime M.empty M.empty 0 0 minInstances tp)


isGood :: MinRuntime -> TestResult -> Bool
isGood mT = (mT <=) . seconds

searchInstances' :: StateT St IO ()
searchInstances' = do
  St size pct deltaUp minTime pcts sizes nrGood nrInsts _ tp <- get
  inst <- lift $ generateInstanceOfType True tp pct deltaUp size
  let directory = "/tmp/"
  let instanceName = show size
  let fp = directory </> instanceName
  res <- liftIO $ writeInstance fp inst >> testCPLEX 1 fp -- (2*minTime) fp
  $(logInfoText) $ "Try " <> T.pack (show $ nrInsts + 1) <> " w/ size, pct: " <> T.pack (show (size, pct, deltaUp)) <> ". Result: " <> T.pack (show res)
  -- Save and iterater
  addCounter (isGood minTime res)
  if isGood minTime res
    then saveInstanceToDisk inst res >> adaptAndRepeat res
    else adaptAndRepeat res

addCounter :: Bool -> StateT St IO ()
addCounter True = do
  St size pct deltaUp minTime pcts sizes nrGood nrInsts nrMinInsts tp <- get
  put $ St size pct deltaUp minTime pcts sizes (nrGood + 1) (nrInsts + 1) nrMinInsts tp
addCounter False = do
  St size pct deltaUp minTime pcts sizes nrGood nrInsts nrMinInsts tp <- get
  put $ St size pct deltaUp minTime pcts sizes nrGood (nrInsts + 1) nrMinInsts tp


(<<) :: (Num a, Ord a) => a -> a -> Bool
x << y = 10 * x < y


adaptAndRepeat :: TestResult -> StateT St IO ()
adaptAndRepeat (TestResult secs lb up out) = do
  St _ _ _ minTime _ _ nrGood _ nrInstances tp <- get
  $(logInfoText) $ "Instances: " <> T.pack (show nrGood <> "/" <> show nrInstances)
  when (nrGood < nrInstances) $ do
    if secs << minTime
      then oneOf [wRand (1.5, 2.5) changeSz, wRand (0.5, 1.5) changePct, wRand (0.5, 1.5) changeDeltaUp]
      else if secs < minTime
             then oneOf [wRand (1.0, 1.1) changeSz, wRand (0.9, 1.0) changePct, wRand (0.9, 1.0) changeDeltaUp]
             else if secs > 2 * minTime
                    then oneOf [wRand (0.9, 1) changeSz, wRand (1, 1.1) changePct, wRand (1.0, 1.1) changeDeltaUp]
                    else wRand (0.9, 1.1) changeSz >> wRand (0.9, 1.1) changePct >> wRand (0.9, 1.1) changeDeltaUp
    searchInstances'
  where
    oneOf xs = liftIO (randomRIO (0, length xs - 1)) >>= \i -> xs !! i
    wRand bounds f = liftIO (randomRIO bounds) >>= f
    changeSz :: Double -> StateT St IO ()
    changeSz pct = do
      $(logDebugText) $ "Modifying size to " <> T.pack (show . (100 *) $ pct) <> "%"
      modify (\x -> x {curSize = min maxSize . round . (pct *) . fromIntegral . curSize $ x})
    changePct :: Double -> StateT St IO ()
    changePct pct = do
      $(logDebugText) $ "Modifying pct to " <> T.pack (show . (100 *) $ pct) <> "%"
      modify (\x -> x {curPct = min maxPct . (/100) . (pct *) . (100*) . curPct $ x})
    changeDeltaUp :: Double -> StateT St IO ()
    changeDeltaUp pct = do
      $(logDebugText) $ "Modifying delta upgradings to " <> T.pack (show . (100 *) $ pct) <> "%"
      modify (\x -> x {curDeltaUpgrade  = min maxDeltaUpgrade . (/100) . (pct *) . (100*) . curDeltaUpgrade $ x})


saveInstanceToDisk :: Instance -> TestResult -> StateT St IO ()
saveInstanceToDisk inst res@(TestResult secs lb ub out) = do
  liftIO $ createDirIfMissing searchDir
  liftIO $ createDirIfMissing resultDir
  liftIO $ createDirIfMissing instanceDir
  St size _ _ _ _ _ nrGood _ _ tp <- get
  let instanceName = show tp <> "_nr" <> show nrGood <> "_sz" <> show size <> "_cplexTime" <> show secs <> "_lb" <> maybe "-" show lb <> "_ub" <> maybe "-" show ub
  liftIO $ writeInstance (instanceDir </> instanceName) inst
  liftIO $ appendFile (instanceDir </> instanceName) ("\n" <> show res)
  liftIO $ writeFile (resultDir </> "output_" <> instanceName <> ".txt") (T.unpack out)
  liftIO $ copyFile (basePath </> "problem.lp") (resultDir </> "problem_" <> instanceName <> ".lp")
  liftIO $ copyFile (basePath </> "results.txt") (resultDir </> "results_" <> instanceName <> ".txt")
  liftIO $ copyFile (basePath </> "results.txt_details.txt") (resultDir </> "results_details_" <> instanceName <> ".txt")
  where searchDir = "search_result"
        instanceDir = searchDir </> "instances"
        resultDir = searchDir </> "results"
        createDirIfMissing dir = doesDirectoryExist dir >>= \x -> unless x (createDirectory dir)


        basePath = "results/"
