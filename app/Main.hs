module Main (main) where

import CLM.Constants.PhysicalConstants (tfrz, nlevsoi, nlevsno)

main :: IO ()
main = do
  putStrLn "CLM.hs — Community Land Model (Haskell port)"
  putStrLn $ "  Freezing point: " ++ show tfrz ++ " K"
  putStrLn $ "  Soil layers:    " ++ show nlevsoi
  putStrLn $ "  Snow layers:    " ++ show nlevsno
