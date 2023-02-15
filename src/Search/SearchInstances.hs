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
  , minTime            :: !MinRuntime -- Minimum Runtime threshold in seconds
  , pcts               :: !(M.Map Double [(Int, Runtime)])
  , sizes              :: !(M.Map Int [(Double, Runtime)])
  , goodInstances      :: Int
  , generatedInstances :: Int
  , minGoodInstances   :: Int
  }

maxSize :: Int
maxSize = 500

maxPct :: Double
maxPct = 0.9

-- | Searches for instances and writes valid ones to the disk
searchInstances :: Int -> Double -> Int -> IO ()
searchInstances sz minTime minInstances = evalStateT searchInstances' (St sz 0.3 minTime M.empty M.empty 0 0 minInstances)


isGood :: MinRuntime -> TestResult -> Bool
isGood mT = (mT <=) . seconds

searchInstances' :: StateT St IO ()
searchInstances' = do
  St size pct minTime pcts sizes nrGood nrInsts _ <- get
  inst <- lift $ generateInstance True pct size
  let directory = "/tmp/"
  let instanceName = show size
  let fp = directory </> instanceName
  res <- liftIO $ writeInstance fp inst >> testCPLEX fp
  $(logInfoText) $ "Try " <> T.pack (show $ nrInsts + 1) <> " w/ size, pct: " <> T.pack (show (size, pct)) <> ". Result: " <> T.pack (show res)
  -- Save and iterater
  addCounter (isGood minTime res)
  if isGood minTime res
    then saveInstanceToDisk inst res >> adaptAndRepeat res
    else adaptAndRepeat res

addCounter :: Bool -> StateT St IO ()
addCounter True = do
  St size pct minTime pcts sizes nrGood nrInsts nrMinInsts <- get
  put $ St size pct minTime pcts sizes (nrGood + 1) (nrInsts + 1) nrMinInsts
addCounter False = do
  St size pct minTime pcts sizes nrGood nrInsts nrMinInsts <- get
  put $ St size pct minTime pcts sizes nrGood (nrInsts + 1) nrMinInsts


(<<) :: (Num a, Ord a) => a -> a -> Bool
x << y = 10 * x < y


adaptAndRepeat :: TestResult -> StateT St IO ()
adaptAndRepeat (TestResult secs lb up out) = do
  St _ _ minTime _ _ nrGood _ nrInstances <- get
  $(logInfoText) $ "Instances: " <> T.pack (show nrGood <> "/" <> show nrInstances)
  when (nrGood < nrInstances) $ do
    if secs << minTime
      then oneOf [wRand (1.5, 2.5) changeSz, wRand (1.5, 2.5) changePct]
      else if secs < minTime
             then oneOf [wRand (1.0, 1.1) changeSz, wRand (1.0, 1.1) changePct]
             else if secs > 2 * minTime
                    then oneOf [wRand (0.9, 1) changeSz, wRand (0.9, 1) changePct]
                    else wRand (0.9, 1.1) changeSz >> wRand (0.9, 1.1) changePct
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
    -- increaseSz = modify (\x -> x {curSize = min maxSize . round . (1.1 *) . fromIntegral . curSize $ x})
    -- increasePct = modify (\x -> x {curPct = min maxPct . (1.1 *) . curPct $ x})


saveInstanceToDisk :: Instance -> TestResult -> StateT St IO ()
saveInstanceToDisk inst res@(TestResult secs lb ub out) = do
  liftIO $ createDirIfMissing searchDir
  liftIO $ createDirIfMissing resultDir
  liftIO $ createDirIfMissing instanceDir
  St size _ _ _ _ nrGood _ _ <- get
  let instanceName = "nr" <> show nrGood <> "_sz" <> show size <> "_cplexTime" <> show secs <> "_lb" <> maybe "-" show lb <> "_ub" <> maybe "-" show ub
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
