-- | Band diagonal matrix solver for soil water movement.
-- Fortran: BandDiagonalMod.F90
-- Converts band storage to dense and solves via Gaussian elimination
-- with partial pivoting in the ST monad.
module CLM.Infrastructure.BandDiagonal
  ( bandDiagonalColumn
  , bandDiagonalSolve
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)

-- | Solve a single-column band diagonal system.
--
-- @bMatrix@: Band matrix stored as Vector of nband rows, each a VU.Vector
-- of length nlev. Band index @(nband+1)/2 - 1@ (0-based) is the diagonal.
--
-- @r@: Right-hand side vector (length nlev).
-- @jtop@: Top active level (0-based).
-- @jbot@: Bottom active level (0-based).
-- @nband@: Band width (must be odd, e.g. 5 for pentadiagonal).
--
-- Returns the solution vector (same length as r). On singular matrix,
-- returns the input r unchanged.
bandDiagonalColumn
  :: V.Vector (VU.Vector Double)  -- ^ Band matrix [nband][nlev]
  -> VU.Vector Double             -- ^ Right-hand side [nlev]
  -> Int                          -- ^ jtop (0-based)
  -> Int                          -- ^ jbot (0-based)
  -> Int                          -- ^ nband
  -> VU.Vector Double             -- ^ Solution [nlev]
bandDiagonalColumn bMatrix r jtop jbot nband
  | n <= 0    = r
  | otherwise = runST $ do
      let kl = (nband - 1) `div` 2

      -- Build dense n x n matrix from band storage
      mat <- VUM.replicate (n * n) 0.0
      rhs <- VUM.new n

      -- Fill RHS
      forM_ [0 .. n - 1] $ \j ->
        VUM.write rhs j (r VU.! (jtop + j))

      -- Fill dense matrix from band storage
      -- Band index bandIdx corresponds to diagonal offset (bandIdx - kl)
      forM_ [0 .. nband - 1] $ \bandIdx -> do
        let dOff = bandIdx - kl  -- diagonal offset: negative = sub, positive = super
        forM_ [0 .. n - 1] $ \col -> do
          let row = col + dOff
              srcIdx = jtop + col + dOff
          when (row >= 0 && row < n && srcIdx >= 0 && srcIdx < VU.length (bMatrix V.! bandIdx)) $
            VUM.write mat (row * n + col) ((bMatrix V.! bandIdx) VU.! srcIdx)

      -- Gaussian elimination with partial pivoting
      ok <- gaussElim mat rhs n

      if ok
        then do
          -- Build result: copy original r, overwrite active range
          result <- VUM.new (VU.length r)
          forM_ [0 .. VU.length r - 1] $ \i ->
            VUM.write result i (r VU.! i)
          forM_ [0 .. n - 1] $ \j -> do
            v <- VUM.read rhs j
            VUM.write result (jtop + j) v
          VU.freeze result
        else
          return r
  where
    n = jbot - jtop + 1

-- | Solve band diagonal systems for multiple columns.
bandDiagonalSolve
  :: V.Vector (V.Vector (VU.Vector Double))  -- ^ Band matrices [ncol][nband][nlev]
  -> V.Vector (VU.Vector Double)             -- ^ RHS vectors [ncol][nlev]
  -> VU.Vector Int                           -- ^ jtop per column (0-based)
  -> VU.Vector Int                           -- ^ jbot per column (0-based)
  -> Int                                     -- ^ nband
  -> V.Vector (VU.Vector Double)             -- ^ Solution vectors [ncol][nlev]
bandDiagonalSolve bMatrices rVecs jtops jbots nband =
  V.imap (\col rVec ->
    bandDiagonalColumn
      (bMatrices V.! col)
      rVec
      (jtops VU.! col)
      (jbots VU.! col)
      nband
  ) rVecs

-- | Gaussian elimination with partial pivoting on a dense n x n matrix
-- stored row-major in a mutable vector. Modifies mat and rhs in place.
-- Returns True on success, False if matrix is singular.
gaussElim
  :: VUM.MVector s Double  -- ^ Dense matrix [n*n], row-major
  -> VUM.MVector s Double  -- ^ RHS [n]
  -> Int                   -- ^ n
  -> ST s Bool
gaussElim mat rhs n = forward 0
  where
    rd i j = VUM.read mat (i * n + j)
    wr i j v = VUM.write mat (i * n + j) v

    forward k
      | k >= n = do backSub (n - 1); return True
      | otherwise = do
          -- Find pivot row
          pivRow <- findPivot k k
          when (pivRow /= k) $ do
            -- Swap rows
            forM_ [k .. n - 1] $ \j -> do
              vk <- rd k j
              vp <- rd pivRow j
              wr k j vp
              wr pivRow j vk
            -- Swap RHS
            rk <- VUM.read rhs k
            rp <- VUM.read rhs pivRow
            VUM.write rhs k rp
            VUM.write rhs pivRow rk

          diag <- rd k k
          if abs diag < 1.0e-30
            then return False
            else do
              -- Eliminate below
              forM_ [k + 1 .. n - 1] $ \i -> do
                aik <- rd i k
                let factor = aik / diag
                forM_ [k + 1 .. n - 1] $ \j -> do
                  aij <- rd i j
                  akj <- rd k j
                  wr i j (aij - factor * akj)
                wr i k 0.0
                ri <- VUM.read rhs i
                rk' <- VUM.read rhs k
                VUM.write rhs i (ri - factor * rk')
              forward (k + 1)

    findPivot col startRow = findPivotFrom col (startRow + 1) startRow

    findPivotFrom col i cur
      | i >= n = return cur
      | otherwise = do
          vi <- rd i col
          vc <- rd cur col
          if abs vi > abs vc
            then findPivotFrom col (i + 1) i
            else findPivotFrom col (i + 1) cur

    backSub k
      | k < 0 = return ()
      | otherwise = do
          rk <- VUM.read rhs k
          s <- sumFrom (k + 1) k 0.0
          diag <- rd k k
          VUM.write rhs k ((rk - s) / diag)
          backSub (k - 1)

    sumFrom j k acc
      | j >= n = return acc
      | otherwise = do
          akj <- rd k j
          xj <- VUM.read rhs j
          sumFrom (j + 1) k (acc + akj * xj)
