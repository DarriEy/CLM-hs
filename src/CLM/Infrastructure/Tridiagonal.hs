-- | Tridiagonal and band-diagonal solvers.
-- Fortran: TridiagonalMod, BandDiagonalMod
module CLM.Infrastructure.Tridiagonal
  ( tridiagonalSolve
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)

-- | Solve a tridiagonal system Ax = r using Thomas algorithm.
-- a = sub-diagonal, b = diagonal, c = super-diagonal, r = RHS.
-- Returns solution vector x.
tridiagonalSolve
  :: VU.Vector Double  -- ^ Sub-diagonal (length n, a[0] unused)
  -> VU.Vector Double  -- ^ Main diagonal (length n)
  -> VU.Vector Double  -- ^ Super-diagonal (length n, c[n-1] unused)
  -> VU.Vector Double  -- ^ Right-hand side (length n)
  -> VU.Vector Double  -- ^ Solution
tridiagonalSolve a b c r = runST $ do
  let n = VU.length b
  gam <- VUM.new n
  u   <- VUM.new n

  -- Forward sweep
  let bet0 = b VU.! 0
  VUM.write u 0 (r VU.! 0 / bet0)

  let forward bet i
        | i >= n    = return ()
        | otherwise = do
            let gamI = (c VU.! (i - 1)) / bet
            VUM.write gam i gamI
            let bet' = (b VU.! i) - (a VU.! i) * gamI
            uPrev <- VUM.read u (i - 1)
            VUM.write u i (((r VU.! i) - (a VU.! i) * uPrev) / bet')
            forward bet' (i + 1)
  forward bet0 1

  -- Back substitution
  let backward i
        | i < 0     = return ()
        | otherwise = do
            gamI1 <- VUM.read gam (i + 1)
            uI1   <- VUM.read u (i + 1)
            uI    <- VUM.read u i
            VUM.write u i (uI - gamI1 * uI1)
            backward (i - 1)
  backward (n - 2)

  VU.freeze u
