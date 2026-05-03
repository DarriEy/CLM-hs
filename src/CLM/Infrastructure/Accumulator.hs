-- | Accumulator/averaging for daily/monthly time accumulations.
-- Fortran: accumulMod.F90
-- Three types: timeavg, runmean, runaccum.
module CLM.Infrastructure.Accumulator
  ( -- * Types
    AccumType(..)
  , AccumField(..)
  , AccumManager(..)
  , emptyAccumManager
    -- * Initialization
  , initAccumField
    -- * Update
  , updateAccumField
    -- * Extract
  , extractAccumField
    -- * Reset
  , markResetAccumField
  , markResetAll
    -- * Query
  , findField
  , accumTypeToString
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map

-- | Accumulation type.
data AccumType
  = AccumTimeAvg    -- ^ Average over time interval
  | AccumRunMean    -- ^ Running mean
  | AccumRunAccum   -- ^ Running accumulation
  deriving (Eq, Show)

accumTypeToString :: AccumType -> String
accumTypeToString AccumTimeAvg  = "timeavg"
accumTypeToString AccumRunMean  = "runmean"
accumTypeToString AccumRunAccum = "runaccum"

-- | One accumulated field.
-- Ported from @accum_field@ in @accumulMod.F90@.
data AccumField = AccumField
  { afName      :: !String
  , afDesc      :: !String
  , afUnits     :: !String
  , afAccType   :: !AccumType
  , afBeg1d     :: !Int                    -- ^ Subgrid beginning index
  , afEnd1d     :: !Int                    -- ^ Subgrid ending index
  , afNumlev    :: !Int                    -- ^ Number of vertical levels
  , afActive    :: !(VU.Vector Bool)       -- ^ Whether each point is active
  , afInitval   :: !Double                 -- ^ Initial/reset value
  , afVal       :: !(VU.Vector Double)     -- ^ Accumulated values [npts * numlev] (row-major)
  , afReset     :: !(VU.Vector Bool)       -- ^ Reset flags [npts * numlev]
  , afPeriod    :: !Int                    -- ^ Accumulation period (timesteps)
  , afNsteps    :: !(VU.Vector Int)        -- ^ Steps since last reset [npts * numlev]
  , afNdaysShift :: !(VU.Vector Int)       -- ^ Days that reset has shifted timeavg
  } deriving (Show)

-- | Container for all accumulation fields.
data AccumManager = AccumManager
  { amFields :: !(Map.Map String AccumField)
  } deriving (Show)

emptyAccumManager :: AccumManager
emptyAccumManager = AccumManager Map.empty

-- | 2D index helper: (point, level) -> flat index. Row-major: point varies fastest.
idx :: Int -> Int -> Int -> Int
idx npts pt lev = lev * npts + pt

-- | Find a field by name.
findField :: AccumManager -> String -> Maybe AccumField
findField mgr name = Map.lookup name (amFields mgr)

-- | Initialize a new accumulation field and add it to the manager.
--
-- @accumPeriod@: if positive, period in timesteps; if negative, period in
-- days (converted using @stepSize@).
initAccumField
  :: AccumManager
  -> String          -- ^ name
  -> String          -- ^ units
  -> String          -- ^ description
  -> AccumType       -- ^ accumulation type
  -> Int             -- ^ accumPeriod
  -> Int             -- ^ numlev
  -> VU.Vector Bool  -- ^ active mask
  -> Double          -- ^ initValue
  -> Int             -- ^ npts
  -> Int             -- ^ stepSize (seconds)
  -> AccumManager
initAccumField mgr name units desc accType accumPeriod numlev active initValue npts stepSize =
  let period = if accumPeriod < 0
                 then (negate accumPeriod) * secondsPerDay `div` stepSize
                 else accumPeriod
      totalSize = npts * numlev
      af = AccumField
        { afName      = name
        , afDesc      = desc
        , afUnits     = units
        , afAccType   = accType
        , afBeg1d     = 0
        , afEnd1d     = npts - 1
        , afNumlev    = numlev
        , afActive    = active
        , afInitval   = initValue
        , afVal       = VU.replicate totalSize initValue
        , afReset     = VU.replicate totalSize False
        , afPeriod    = period
        , afNsteps    = VU.replicate totalSize 0
        , afNdaysShift = VU.replicate totalSize 0
        }
  in mgr { amFields = Map.insert name af (amFields mgr) }
  where
    secondsPerDay = 86400

-- | Update a single-level accumulation field with new values.
-- Returns updated manager.
updateAccumField :: AccumManager -> String -> VU.Vector Double -> Int -> AccumManager
updateAccumField mgr name field nstep =
  case Map.lookup name (amFields mgr) of
    Nothing -> mgr
    Just af ->
      let af' = case afAccType af of
                  AccumTimeAvg  -> updateTimeAvg af field nstep
                  AccumRunMean  -> updateRunMean af field nstep
                  AccumRunAccum -> updateRunAccum af field nstep
      in mgr { amFields = Map.insert name af' (amFields mgr) }

-- | Extract single-level accumulated field values.
extractAccumField :: AccumManager -> String -> Int -> Maybe (VU.Vector Double)
extractAccumField mgr name nstep =
  case Map.lookup name (amFields mgr) of
    Nothing -> Nothing
    Just af ->
      let npts = afEnd1d af - afBeg1d af + 1
      in Just $ case afAccType af of
           AccumTimeAvg -> extractTimeAvg af npts nstep
           _            -> extractBasic af npts

-- | Mark all points/levels for reset.
markResetAll :: AccumManager -> String -> AccumManager
markResetAll mgr name =
  case Map.lookup name (amFields mgr) of
    Nothing -> mgr
    Just af ->
      let af' = af { afReset = VU.replicate (VU.length (afReset af)) True }
      in mgr { amFields = Map.insert name af' (amFields mgr) }

-- | Mark a specific point for reset (all levels).
markResetAccumField :: AccumManager -> String -> Int -> AccumManager
markResetAccumField mgr name kf =
  case Map.lookup name (amFields mgr) of
    Nothing -> mgr
    Just af ->
      let npts = afEnd1d af - afBeg1d af + 1
          af' = af { afReset = VU.imap (\i old ->
                       let lev = i `div` npts
                           pt  = i `mod` npts
                       in if pt == kf then True else old
                     ) (afReset af) }
      in mgr { amFields = Map.insert name af' (amFields mgr) }

-- Internal: extract basic (runmean/runaccum) values.
extractBasic :: AccumField -> Int -> VU.Vector Double
extractBasic af npts = VU.generate npts $ \k -> afVal af VU.! k

-- Internal: extract timeavg values (SPVAL at non-boundary steps).
extractTimeAvg :: AccumField -> Int -> Int -> VU.Vector Double
extractTimeAvg af npts nstep = VU.generate npts $ \k ->
  let effStep = nstep - (afNdaysShift af VU.! k)
  in if effStep `mod` afPeriod af == 0
       then afVal af VU.! k
       else spval
  where
    spval = 1.0e36

-- Internal: update timeavg field.
updateTimeAvg :: AccumField -> VU.Vector Double -> Int -> AccumField
updateTimeAvg af field nstep =
  let npts = afEnd1d af - afBeg1d af + 1
      period = afPeriod af

      -- Phase 1 & 2: reset and accumulate
      go k (val, nst, rst, nds) =
        let active  = afActive af VU.! k
            oldVal  = val VU.! k
            oldNst  = nst VU.! k
            oldRst  = rst VU.! k
            oldNds  = nds VU.! k
            effStep = nstep - oldNds
            timeToReset = (effStep `mod` period == 1 || period == 1) && effStep /= 0
            (v1, n1, r1, d1) =
              if active && (timeToReset || oldRst)
                then let d' = if oldRst && not timeToReset
                                then oldNds + oldNst
                                else oldNds
                     in (afInitval af, 0, False, d')
                else (oldVal, oldNst, False, oldNds)
            -- Accumulate
            (v2, n2) = if active
                         then (v1 + field VU.! k, n1 + 1)
                         else (v1, n1)
            -- Normalize at end of period
            effStep2 = nstep - d1
            v3 = if active && n2 > 0 && effStep2 `mod` period == 0
                   then v2 / fromIntegral n2
                   else v2
        in (v3, n2, r1, d1)

      results = map (\k ->
        go k (afVal af, afNsteps af, afReset af, afNdaysShift af)) [0 .. npts - 1]

      vals  = VU.fromList $ map (\(v,_,_,_) -> v) results
      nsts  = VU.fromList $ map (\(_,n,_,_) -> n) results
      rsts  = VU.fromList $ map (\(_,_,r,_) -> r) results
      ndss  = VU.fromList $ map (\(_,_,_,d) -> d) results

  in af { afVal = vals, afNsteps = nsts, afReset = rsts, afNdaysShift = ndss }

-- Internal: update runmean field.
updateRunMean :: AccumField -> VU.Vector Double -> Int -> AccumField
updateRunMean af field _nstep =
  let npts = afEnd1d af - afBeg1d af + 1
      period = afPeriod af

      vals = VU.generate npts $ \k ->
        if afActive af VU.! k
          then if afReset af VU.! k
                 then field VU.! k  -- first value after reset
                 else let accper = min (afNsteps af VU.! k + 1) period
                      in (fromIntegral (accper - 1) * (afVal af VU.! k) + field VU.! k)
                         / fromIntegral accper
          else afVal af VU.! k

      nsts = VU.generate npts $ \k ->
        if afActive af VU.! k
          then if afReset af VU.! k
                 then 1
                 else min (afNsteps af VU.! k + 1) period
          else afNsteps af VU.! k

      rsts = VU.replicate npts False

  in af { afVal = vals, afNsteps = nsts, afReset = rsts }

-- Internal: update runaccum field.
updateRunAccum :: AccumField -> VU.Vector Double -> Int -> AccumField
updateRunAccum af field _nstep =
  let npts = afEnd1d af - afBeg1d af + 1

      vals = VU.generate npts $ \k ->
        if afActive af VU.! k
          then if afReset af VU.! k
                 then 0.0
                 else min 99999.0 (max 0.0 (afVal af VU.! k + field VU.! k))
          else afVal af VU.! k

      nsts = VU.generate npts $ \k ->
        if afActive af VU.! k
          then if afReset af VU.! k
                 then truncate (afInitval af)
                 else afNsteps af VU.! k + 1
          else afNsteps af VU.! k

      rsts = VU.replicate npts False

  in af { afVal = vals, afNsteps = nsts, afReset = rsts }
