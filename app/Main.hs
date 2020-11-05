module Main where

import           Instance.Generator
import           Instance.Writer

main :: IO ()
main = generateInstance True 0.30 100 >>= writeInstance "testInstance"
