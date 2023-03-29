module Testing.TestCPLEX
    ( testCPLEX
    , TestResult (..)
    , isOptimal
    ) where

import           Data.Maybe
import qualified Data.Text      as T
import           System.Process
import           Text.Read      (readMaybe)
import           Text.Regex

data TestResult =
  TestResult
    { seconds    :: Double -- TODO: change to Int if output always in seconds
    , lowerBound :: Maybe Double
    , upperBound :: Maybe Double
    , output     :: T.Text
    } deriving (Eq, Ord)

instance Show TestResult where
  show (TestResult sec lb ub _) = show sec <> "s LB:" ++ show lb ++ " <= "  ++ show ub


isOptimal :: TestResult -> Bool
isOptimal (TestResult _ (Just lb) (Just up) _) = lb == up
isOptimal _                                    = False


testCPLEX :: Int -> Int -> FilePath -> IO TestResult
testCPLEX timeLimit t file = do
  let basePath = "/home/schnecki/Documents/projects/capacitated_dispertion_problem/code"
  out <- readCreateProcess ((shell $ basePath ++ "/cplex/main -c -i " ++ file ++ " -m " ++ show timeLimit ++ " -t " ++ show t) {cwd = Just $ basePath ++ "/cplex"}) ""
  let lbReg = mkRegexWithOpts "LB: ([^\t ]*)" True True
      ubReg = mkRegexWithOpts "UB: ([^\t ]*)" True True
      timeReg = mkRegexWithOpts "TIEMPO: ([0-9]*)" True True
      parseBound :: [String] -> Maybe Double
      parseBound xs =
        case xs of
          [x] -> readMaybe x
          _   -> Nothing
      parseTime :: [String] -> Maybe Double -- Is this actually an Int?
      parseTime xs =
        case xs of
          [x] -> readMaybe x
          _   -> Nothing
      lb = matchRegex lbReg out >>= parseBound
      ub = matchRegex ubReg out >>= parseBound
      time = fromMaybe (-1) (matchRegex timeReg out >>= parseTime)
  return $ TestResult time lb ub (T.pack out)
