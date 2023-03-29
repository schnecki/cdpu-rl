

module Instance.Writer
    ( writeInstance

    ) where


import           Data.List     (intercalate)
import           Instance.Type
import           Text.Printf

dblPrecision :: Int
dblPrecision = 4

dbl :: Double -> String
dbl = printf ("%." ++ show dblPrecision ++ "f")

list :: (a -> String) -> [a] -> String
list f = unwords . map f

writeInstance :: FilePath -> Instance -> IO ()
writeInstance fp inst@(Instance tp minCapReq -- maxBud
                       maxUpg caps dists upgCosts) =

  writeFile fp $ unlines $
  [ "// Problem type: " ++ show tp
  , "// number of vertices"
  , show (nrVertices inst)
  , ""
  , "// minimum capacity required"
  , dbl minCapReq
  , ""
  , "// capacity when opened"
  , list (dbl . capNormal) caps
  , ""
  , "// normal distances"
  ] ++
  map (list (dbl . distNormal)) dists ++
  [ ""
  , "// upgraded capacity"
  , list (dbl . capUpgraded) caps
  , ""
  , "// upgraded distances"
  ] ++
  map (list (dbl. distUpgraded)) dists ++
  [ ""
  -- , "// budget"
  -- , dbl maxBud
  -- , ""
  -- , "// max upgrades"
  -- , show maxUpg
  -- , ""
  , "// upgrading costs"
  ] ++
  map (list dbl) upgCosts


