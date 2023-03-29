module GenOptions where

import           Options.Applicative as Opt

import           Instance.Type


data GenOptions =
  GenOptions
   { optProbType     :: ProblemType
   , optMinSecs      :: Int
   , optSize         :: Int
   , optCap'Factor   :: Maybe Double
   , optCap'Dist     :: Maybe Double
   , optMaxUp'Factor :: Double
   } deriving (Show)


parseGenOptions :: ParserInfo GenOptions
parseGenOptions = info (parseOptions <**> helper) fullDesc
  where
    parseOptions = GenOptions Narrow <$>
      Opt.option auto (long "time" <> short 't' <> metavar "MIN TIME" <>
                                     help "Minimum time for CPLEX, e.g. 10") <*>
      Opt.option auto (long "size" <> short 's' <> metavar "PROBLEM START SIZE" <>
                                     help "Specify problem start size, e.g. 10") <*>
      -- Opt.strOption (long "path" <> short 'p' <> metavar "PATH" <> help "Problem folder path.") <*> -- <> "../code/instances") <*>
      Opt.optional (Opt.option auto (long "cap" <> short 'c' <> metavar "CAPACITY FACTOR" <>
                                     help "Specify capacity factor, e.g. 1.2")) <*>
      Opt.optional (Opt.option auto (long "dist" <> short 'd' <> metavar "DISTANCE FACTOR" <>
                                     help "Distance factor, e.g. 0.75")) <*>
      Opt.option auto (long "up" <> short 'u' <> metavar "UPGRADE FACTOR" <>
                                     help "Pct of upgraded edges wrt to complete graph, e.g. 0.75")

      -- TODO

      -- (read <$> Opt.strOption (long "type" <> short 't' <> metavar "PROBLEM TYPE" <>
      --                                help "One of 'Narrow', 'Wide', 'Euclid'"))
