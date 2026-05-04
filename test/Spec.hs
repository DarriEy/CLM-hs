import Test.Hspec
import qualified Data.Vector.Unboxed as VU
import System.Directory (doesDirectoryExist)

import CLM.Constants.PhysicalConstants
import CLM.Infrastructure.Tridiagonal (tridiagonalSolve)
import CLM.Infrastructure.Filters (maskToIndices)
import CLM.Driver.PipelineRunner
  ( PipelineConfig(..), defaultPipelineConfig, runPipeline, DailyDiag(..) )

main :: IO ()
main = hspec $ do
  describe "PhysicalConstants" $ do
    it "freezing point is 273.15 K" $
      tfrz `shouldBe` 273.15

    it "latent heat of sublimation = hvap + hfus" $
      hsub `shouldBe` (hvap + hfus)

    it "grid dimensions are positive" $ do
      nlevsoi `shouldSatisfy` (> 0)
      nlevgrnd `shouldSatisfy` (> nlevsoi)
      nlevsno `shouldSatisfy` (> 0)

  describe "Tridiagonal solver" $ do
    it "solves a simple 3x3 system" $ do
      -- System: [2 -1 0; -1 2 -1; 0 -1 2] x = [1; 0; 1]
      -- Solution: x = [1; 1; 1]
      let a = VU.fromList [0.0, -1.0, -1.0]
          b = VU.fromList [2.0,  2.0,  2.0]
          c = VU.fromList [-1.0, -1.0, 0.0]
          r = VU.fromList [1.0,  0.0,  1.0]
          x = tridiagonalSolve a b c r
      VU.length x `shouldBe` 3
      abs (x VU.! 0 - 1.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 1 - 1.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 2 - 1.0) `shouldSatisfy` (< 1e-12)

    it "solves identity system" $ do
      let a = VU.fromList [0.0, 0.0, 0.0]
          b = VU.fromList [1.0, 1.0, 1.0]
          c = VU.fromList [0.0, 0.0, 0.0]
          r = VU.fromList [3.0, 7.0, 2.0]
          x = tridiagonalSolve a b c r
      abs (x VU.! 0 - 3.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 1 - 7.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 2 - 2.0) `shouldSatisfy` (< 1e-12)

  describe "Filters" $ do
    it "maskToIndices returns correct indices" $ do
      let mask = VU.fromList [True, False, True, False, True]
          idxs = maskToIndices mask
      idxs `shouldBe` VU.fromList [0, 2, 4]

    it "empty mask gives empty indices" $ do
      let mask = VU.fromList [False, False, False]
          idxs = maskToIndices mask
      VU.length idxs `shouldBe` 0

  describe "Pipeline integration (vs Julia reference)" $ do
    it "runs 10 days without crashing" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data"
            , pcNdays   = 10
            , pcVerbose = False
            }
          length dailies `shouldBe` 10

    it "Day 1 T_GRND within 15K of Julia reference (259.8K)" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data"
            , pcNdays   = 1
            , pcVerbose = False
            }
          let tg = dd_t_grnd (head dailies)
          -- Julia Day 1: T_GRND = 259.81 K
          abs (tg - 259.81) `shouldSatisfy` (< 15.0)

    it "T_GRND stays in physical range (200-320K) for 30 days" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data"
            , pcNdays   = 30
            , pcVerbose = False
            }
          let tgs = map dd_t_grnd dailies
          all (\t -> t > 200.0 && t < 320.0) tgs `shouldBe` True
