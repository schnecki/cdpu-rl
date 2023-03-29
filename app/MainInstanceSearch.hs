{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Data.Text              as T
import           EasyLogger
import           Options.Applicative    as Opt
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

import           GenOptions
import           Instance.Generator
import           Instance.Type
import           Instance.Writer
import           Logging
import           Search.SearchInstances

main :: IO ()
main = do
  $(initLogger) LogStdOut
  setMinLogLevel LogDebug -- LogWarning -- LogInfo
  enableGeneratorLogging LogStdOut
  ops <- execParser parseGenOptions

  let sz = size ops
  searchInstances ops 5
  flushLoggers
  --
  --
  -- let path = directory </> instanceName

