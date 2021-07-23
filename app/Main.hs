{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text             as T
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

import           Instance.Generator
import           Instance.Writer

main :: IO ()
main = do
  args <- getArgs
  let size
        | null args = 10
        | otherwise = read (head args)
  let directory
        | length args <= 1 = "../code/instances"
        | otherwise = args !! 1
  inst <- generateInstance True 0.30 size
  dLs <- map T.pack <$> listDirectory directory
  let dLsFiltered = filter (T.isPrefixOf (T.pack (show size) <> "_")) dLs
      nr = 1 + length dLsFiltered
      instanceName = show size ++ "_" ++ show nr ++ ".txt"

  writeInstance (directory </> instanceName) inst
