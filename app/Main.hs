module Main where

import           Instance.Generator
import           Instance.Writer

main :: IO ()
main = generateInstance True 0.5 4 >>= writeInstance "testInstance"
