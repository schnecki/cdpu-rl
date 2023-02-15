{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Data.Text              as T
import           EasyLogger
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

import           Instance.Generator
import           Instance.Writer
import           Logging
import           Search.SearchInstances

main :: IO ()
main = do
  $(initLogger) LogStdOut
  setMinLogLevel LogDebug -- LogWarning -- LogInfo
  enableGeneratorLogging LogStdOut

  args <- getArgs
  let startSize
        | null args = 10
        | otherwise = read (head args)
  let directory
        | length args <= 1 = "../code/instances"
        | otherwise = args !! 1
  searchInstances startSize 10 5
  flushLoggers
  --
  --
  -- let path = directory </> instanceName
