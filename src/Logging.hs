{-# LANGUAGE TemplateHaskell #-}
module Logging
    ( enableGeneratorLogging
    , disableGeneratorLogging
    ) where

import           EasyLogger

enableGeneratorLogging :: LogDestination -> IO ()
enableGeneratorLogging = $(initLogger)

disableGeneratorLogging :: IO ()
disableGeneratorLogging = $(finalizeLogger)

