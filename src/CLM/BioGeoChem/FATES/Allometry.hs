{-# LANGUAGE BangPatterns #-}
-- | FATES Allometry Equations
-- Ported from FatesAllometryMod.F90 (~3,512 lines of Fortran)
module CLM.BioGeoChem.FATES.Allometry
  ( -- * Height to/from Diameter Allometries
    d2hObrien
  , d2hPoorter2006
  , d2h2pwr
  , d2hMartcano
  , hAllom
  , h2dObrien
  , h2dPoorter2006
  , h2d2pwr
  , h2dMartcano
  , h2dAllom
  
  -- * Aboveground Woody Biomass Allometries
  , d2bagw2pwr
  , dh2bagwChave2014
  , bagwAllom
  
  -- * Leaf Biomass Allometries
  , d2blmax2pwr
  , blmaxAllom
  , bleaf
  
  -- * Storage Biomass Allometries
  , bstoreBlcushion
  , bstoreAllom
  
  -- * Coarse Root & Dead Biomass Allometries
  , bbgwConst
  , bbgwAllom
  , bfineroot
  , bdeadAllom
  
  -- * Crown Area Allometries
  , carea2pwr
  , careaAllom
  
  -- * Utility functions
  , decayCoeffVcmax
  , leafcFromTreelai
  ) where

import qualified Data.Vector.Unboxed as U
import CLM.BioGeoChem.FATES.Constants
import CLM.BioGeoChem.FATES.Globals

-- | IEEE-754 NaN constant
nan :: Double
nan = 0.0 / 0.0

-- | Base 10 logarithm
log10 :: Double -> Double
log10 !x = log x / log 10.0

-- ============================================================================
-- 1. Height to/from Diameter Allometries
-- ============================================================================

-- | O'Brien Height Allometry (hmode = 1)
d2hObrien :: Double -> Double -> Double -> Double -> (Double, Double)
d2hObrien !d !p1 !p2 !dbhMaxh =
  let !dVal = min d dbhMaxh
      !h = 10.0 ** (log10 dVal * p1 + p2)
      !dhdd = if d >= dbhMaxh
                then 0.0
                else p1 * (10.0 ** p2) * (d ** (p1 - 1.0))
  in (h, dhdd)

-- | Poorter 2006 Weibull Height Allometry (hmode = 2)
d2hPoorter2006 :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
d2hPoorter2006 !d !p1 !p2 !p3 !dbhMaxh =
  let !dVal = min d dbhMaxh
      !h = p1 * (1.0 - exp (p2 * (dVal ** p3)))
      !dhdd = if d >= dbhMaxh
                then 0.0
                else -p1 * exp (p2 * (d ** p3)) * p3 * p2 * (d ** (p3 - 1.0))
  in (h, dhdd)

-- | 2-Parameter Power Height Allometry (hmode = 3)
d2h2pwr :: Double -> Double -> Double -> Double -> (Double, Double)
d2h2pwr !d !p1 !p2 !dbhMaxh =
  let !dVal = min d dbhMaxh
      !h = p1 * (dVal ** p2)
      !dhdd = if d >= dbhMaxh
                then 0.0
                else p1 * p2 * (d ** (p2 - 1.0))
  in (h, dhdd)

-- | Martinez-Cano Michaelis-Menten Height Allometry (hmode = 5)
d2hMartcano :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
d2hMartcano !d !p1 !p2 !p3 !dbhMaxh =
  let !dVal = min d dbhMaxh
      !h = (p1 * (dVal ** p2)) / (p3 + (dVal ** p2))
      !dhdd = if d >= dbhMaxh
                then 0.0
                else let !num = (p2 * p1 * (d ** (p2 - 1.0))) * (p3 + (d ** p2)) - (p2 * (d ** (p2 - 1.0))) * (p1 * (d ** p2))
                         !den = (p3 + (d ** p2)) ** 2.0
                     in num / den
  in (h, dhdd)

-- | Generic diameter to height dispatcher wrapper
hAllom :: Int -> Double -> Double -> Double -> Double -> Double -> (Double, Double)
hAllom !hmode !d !p1 !p2 !p3 !dbhMaxh =
  case hmode of
    1 -> d2hObrien d p1 p2 dbhMaxh
    2 -> d2hPoorter2006 d p1 p2 p3 dbhMaxh
    3 -> d2h2pwr d p1 p2 dbhMaxh
    5 -> d2hMartcano d p1 p2 p3 dbhMaxh
    _ -> (nan, 0.0)

-- | Inverse of O'Brien height allometry
h2dObrien :: Double -> Double -> Double -> (Double, Double)
h2dObrien !h !p1 !p2 =
  let !d = 10.0 ** ((log10 h - p2) / p1)
      !dddh = d / (p1 * h * log 10.0)
  in (d, dddh)

-- | Inverse of Poorter Weibull height allometry
h2dPoorter2006 :: Double -> Double -> Double -> Double -> (Double, Double)
h2dPoorter2006 !h !p1 !p2 !p3 =
  let !val = 1.0 - h / p1
      !d = (log val / p2) ** (1.0 / p3)
      !dddh = d / (p1 * p2 * p3 * val * (d ** p3))
  in (d, dddh)

-- | Inverse of 2-parameter power height allometry
h2d2pwr :: Double -> Double -> Double -> (Double, Double)
h2d2pwr !h !p1 !p2 =
  let !d = (h / p1) ** (1.0 / p2)
      !dddh = d / (p2 * h)
  in (d, dddh)

-- | Inverse of Martinez-Cano Michaelis-Menten height allometry
h2dMartcano :: Double -> Double -> Double -> Double -> (Double, Double)
h2dMartcano !h !p1 !p2 !p3 =
  let !d = ((h * p3) / (p1 - h)) ** (1.0 / p2)
      !dddh = d * p1 / (p2 * h * (p1 - h))
  in (d, dddh)

-- | Generic height to diameter dispatcher wrapper
h2dAllom :: Int -> Double -> Double -> Double -> Double -> (Double, Double)
h2dAllom !hmode !h !p1 !p2 !p3 =
  case hmode of
    1 -> h2dObrien h p1 p2
    2 -> h2dPoorter2006 h p1 p2 p3
    3 -> h2d2pwr h p1 p2
    5 -> h2dMartcano h p1 p2 p3
    _ -> (nan, 0.0)

-- ============================================================================
-- 2. Aboveground Woody Biomass Allometries
-- ============================================================================

-- | 2-Parameter Power AGBW Allometry
d2bagw2pwr :: Double -> Double -> Double -> Double -> (Double, Double)
d2bagw2pwr !d !p1 !p2 !c2b =
  let !bagw = (p1 * (d ** p2)) / c2b
      !dbagwdd = (p2 * p1 * (d ** (p2 - 1.0))) / c2b
  in (bagw, dbagwdd)

-- | Chave 2014 AGBW Allometry
dh2bagwChave2014 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> (Double, Double)
dh2bagwChave2014 !d !h !dhdd !p1 !p2 !woodDensity !c2b =
  let !bagw = (p1 * ((woodDensity * (d ** 2.0) * h) ** p2)) / c2b
      !dbagwdd1 = (p1 * (woodDensity ** p2)) / c2b
      !dbagwdd2 = p2 * (d ** (2.0 * p2)) * (h ** (p2 - 1.0)) * dhdd
      !dbagwdd3 = (h ** p2) * 2.0 * p2 * (d ** (2.0 * p2 - 1.0))
      !dbagwdd = dbagwdd1 * (dbagwdd2 + dbagwdd3)
  in (bagw, dbagwdd)

-- | Generic Aboveground Woody Biomass dispatcher wrapper (mode 1=obrien, 2=2pwr, 3=chave)
bagwAllom
  :: Int          -- ^ allomAmode (1=obrien/salda, 2=2pwr, 3=chave)
  -> Double       -- ^ d [cm]
  -> Int          -- ^ hmode for height subroutine
  -> Double       -- ^ p1H (height param 1)
  -> Double       -- ^ p2H
  -> Double       -- ^ p3H
  -> Double       -- ^ dbhMaxh
  -> Double       -- ^ p1A (agbw param 1)
  -> Double       -- ^ p2A
  -> Double       -- ^ p3A
  -- wood params
  -> Double       -- ^ woodDensity
  -> Double       -- ^ c2b
  -> Double       -- ^ agbFrac
  -> Double       -- ^ branchFrac
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ elongfStem (stem phenology)
  -> (Double, Double)
bagwAllom !amode !d !hmode !p1H !p2H !p3H !dbhMaxh !p1A !p2A !p3A !woodDensity !c2b !agbFrac !branchFrac !crowndamage !damageBins !elongfStem =
  let (h, dhdd) = hAllom hmode d p1H p2H p3H dbhMaxh
      (bagwRaw, dbagwRaw) = case amode of
        2 -> d2bagw2pwr d p1A p2A c2b
        3 -> dh2bagwChave2014 d h dhdd p1A p2A woodDensity c2b
        _ -> (nan, 0.0) -- Saldariagga / obrien fallback or other defaults
      
      -- Apply crown damage reduction
      crownRed = if crowndamage > 1
                   then let !edges = damageBins
                            !idx = crowndamage - 1
                        in if idx >= 0 && idx < U.length edges
                             then (edges U.! idx) / 100.0
                             else 0.0
                   else 0.0
      
      bagw = if crowndamage > 1
               then elongfStem * (bagwRaw - (bagwRaw * branchFrac * crownRed))
               else elongfStem * bagwRaw
      dbagwdd = if crowndamage > 1
                  then elongfStem * (dbagwRaw - (dbagwRaw * branchFrac * crownRed))
                  else elongfStem * dbagwRaw
  in (bagw, dbagwdd)

-- ============================================================================
-- 3. Leaf Biomass Allometries
-- ============================================================================

-- | 2-Parameter Power Leaf Biomass Allometry
d2blmax2pwr :: Double -> Double -> Double -> Double -> (Double, Double)
d2blmax2pwr !d !p1 !p2 !c2b =
  let !blmax = (p1 * (d ** p2)) / c2b
      !dblmaxdd = (p1 * p2 * (d ** (p2 - 1.0))) / c2b
  in (blmax, dblmaxdd)

-- | Generic maximum leaf biomass dispatcher wrapper
blmaxAllom :: Int -> Double -> Double -> Double -> Double -> Double -> (Double, Double)
blmaxAllom !lmode !d !p1 !p2 !p3 !c2b =
  case lmode of
    2 -> d2blmax2pwr d p1 p2 c2b
    _ -> (nan, 0.0) -- Fallback modes not yet active or standard

-- | Actual leaf biomass with trimming, damage, phenology
bleaf
  :: Double       -- ^ d [cm]
  -> Int          -- ^ lmode
  -> Double       -- ^ p1L
  -> Double       -- ^ p2L
  -> Double       -- ^ p3L
  -> Double       -- ^ c2b
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ canopyTrim
  -> Double       -- ^ elongfLeaf (leaf phenology)
  -> (Double, Double)
bleaf !d !lmode !p1L !p2L !p3L !c2b !crowndamage !damageBins !canopyTrim !elongfLeaf =
  let (blmax, dblmaxdd) = blmaxAllom lmode d p1L p2L p3L c2b
      blRaw = blmax * canopyTrim
      dblRaw = dblmaxdd * canopyTrim
      
      crownRed = if crowndamage > 1
                   then let !idx = crowndamage - 1
                        in if idx >= 0 && idx < U.length damageBins
                             then (damageBins U.! idx) / 100.0
                             else 0.0
                   else 0.0
      
      bl = if crowndamage > 1
             then elongfLeaf * blRaw * (1.0 - crownRed)
             else elongfLeaf * blRaw
      dbldd = if crowndamage > 1
                then elongfLeaf * dblRaw * (1.0 - crownRed)
                else elongfLeaf * dblRaw
  in (bl, dbldd)

-- ============================================================================
-- 4. Storage Biomass Allometries
-- ============================================================================

-- | Storage cushion allometry
bstoreBlcushion :: Double -> Double -> Double -> (Double, Double)
bstoreBlcushion !bl !dbldd !cushion =
  (bl * cushion, dbldd * cushion)

-- | Generic target storage carbon dispatcher wrapper
bstoreAllom
  :: Int          -- ^ stmode (1=proportional to trimmed, 2=proportional to untrimmed)
  -> Double       -- ^ d [cm]
  -> Int          -- ^ lmode
  -> Double       -- ^ p1L
  -> Double       -- ^ p2L
  -> Double       -- ^ p3L
  -> Double       -- ^ c2b
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ canopyTrim
  -> Double       -- ^ cushion scaler
  -> (Double, Double)
bstoreAllom !stmode !d !lmode !p1L !p2L !p3L !c2b !crowndamage !damageBins !canopyTrim !cushion =
  case stmode of
    1 -> let (bl, dbldd) = bleaf d lmode p1L p2L p3L c2b crowndamage damageBins canopyTrim 1.0
         in bstoreBlcushion bl dbldd cushion
    2 -> let (blmax, dblmaxdd) = blmaxAllom lmode d p1L p2L p3L c2b
         in bstoreBlcushion blmax dblmaxdd cushion
    _ -> (nan, 0.0)

-- ============================================================================
-- 5. Coarse Root & Dead Biomass Allometries
-- ============================================================================

-- | Proportional coarse root biomass
bbgwConst :: Double -> Double -> Double -> Double -> (Double, Double)
bbgwConst !d !bagw !dbagwdd !agbFrac =
  let !bbgw = (1.0 / agbFrac - 1.0) * bagw
      !dbbgwdd = (1.0 / agbFrac - 1.0) * dbagwdd
  in (bbgw, dbbgwdd)

-- | Generic below ground coarse root wrapper
bbgwAllom
  :: Int          -- ^ cmode (1=constant ratio to agbw)
  -> Double       -- ^ d [cm]
  -> Int          -- ^ allomAmode (agb mode)
  -> Int          -- ^ hmode
  -> Double       -- ^ p1H
  -> Double       -- ^ p2H
  -> Double       -- ^ p3H
  -> Double       -- ^ dbhMaxh
  -> Double       -- ^ p1A
  -> Double       -- ^ p2A
  -> Double       -- ^ p3A
  -> Double       -- ^ woodDensity
  -> Double       -- ^ c2b
  -> Double       -- ^ agbFrac
  -> Double       -- ^ branchFrac
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ elongfStem
  -> (Double, Double)
bbgwAllom !cmode !d !amode !hmode !p1H !p2H !p3H !dbhMaxh !p1A !p2A !p3A !woodDensity !c2b !agbFrac !branchFrac !crowndamage !damageBins !elongfStem =
  case cmode of
    1 -> -- bbgw is not affected by damage/phenology directly, so we evaluate target AGBW with crowndamage=1 and elongfStem
         let (bagw, dbagwdd) = bagwAllom amode d hmode p1H p2H p3H dbhMaxh p1A p2A p3A woodDensity c2b agbFrac branchFrac 1 damageBins elongfStem
         in bbgwConst d bagw dbagwdd agbFrac
    _ -> (nan, 0.0)

-- | Fine root biomass wrapper
bfineroot
  :: Int          -- ^ fmode (1=proportional to trimmed leaf, 2=proportional to untrimmed leaf)
  -> Double       -- ^ d [cm]
  -- leaf params
  -> Int          -- ^ lmode
  -> Double       -- ^ p1L
  -> Double       -- ^ p2L
  -> Double       -- ^ p3L
  -> Double       -- ^ c2b
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ canopyTrim
  -> Double       -- ^ l2fr (leaf to fine root ratio)
  -> Double       -- ^ elongfFnrt (fine root phenology)
  -> (Double, Double)
bfineroot !fmode !d !lmode !p1L !p2L !p3L !c2b !crowndamage !damageBins !canopyTrim !l2fr !elongfFnrt =
  let (bfrRaw, dbfrRaw) = case fmode of
        1 -> let (blmax, dblmaxdd) = blmaxAllom lmode d p1L p2L p3L c2b
             in (blmax * l2fr * canopyTrim, dblmaxdd * l2fr * canopyTrim)
        2 -> let (blmax, dblmaxdd) = blmaxAllom lmode d p1L p2L p3L c2b
             in (blmax * l2fr, dblmaxdd * l2fr)
        _ -> (nan, 0.0)
      bfr = elongfFnrt * bfrRaw
      dbfrdd = elongfFnrt * dbfrRaw
  in (bfr, dbfrdd)

-- | Coarse wood dead structural biomass allometry
bdeadAllom
  :: Int          -- ^ amode
  -> Double       -- ^ bagw
  -> Double       -- ^ bbgw
  -> Double       -- ^ bsap
  -> Double       -- ^ agbFrac
  -> Double       -- ^ dbagwdd
  -> Double       -- ^ dbbgwdd
  -> Double       -- ^ dbsapdd
  -> (Double, Double)
bdeadAllom !amode !bagw !bbgw !bsap !agbFrac !dbagwdd !dbbgwdd !dbsapdd =
  case amode of
    1 -> (bagw / agbFrac, dbagwdd / agbFrac)
    _ -> (bagw + bbgw - bsap, dbagwdd + dbbgwdd - dbsapdd)

-- ============================================================================
-- 6. Crown Area Allometries
-- ============================================================================

-- | 2-Parameter Power Crown Area Allometry
carea2pwr
  :: Double       -- ^ dbh [cm]
  -> Double       -- ^ site_spread factor
  -> Double       -- ^ d2bl_p2 (exponent)
  -> Double       -- ^ d2bl_ediff (exponent difference)
  -> Double       -- ^ d2ca_min
  -> Double       -- ^ d2ca_max
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> (Double, Double)
carea2pwr !dbh !spread !p2 !ediff !caMin !caMax !crowndamage !damageBins =
  let !exponent_val = p2 + ediff
      !spreadterm = spread * caMax + (1.0 - spread) * caMin
      !c_area_raw = spreadterm * (dbh ** exponent_val)
      !dcdd_raw = spreadterm * exponent_val * (dbh ** (exponent_val - 1.0))
      
      !crownRed = if crowndamage > 1
                    then let !idx = crowndamage - 1
                         in if idx >= 0 && idx < U.length damageBins
                              then (damageBins U.! idx) / 100.0
                              else 0.0
                    else 0.0
      
      !c_area = if crowndamage > 1
                  then c_area_raw * (1.0 - crownRed)
                  else c_area_raw
      !dcdd = if crowndamage > 1
                then dcdd_raw * (1.0 - crownRed)
                else dcdd_raw
  in (c_area, dcdd)

-- | Generic crown area dispatcher wrapper
careaAllom
  :: Double       -- ^ dbh [cm]
  -> Double       -- ^ site_spread factor
  -> Double       -- ^ nplant (individuals count)
  -> Int          -- ^ lmode
  -> Double       -- ^ dbhMaxh
  -> Double       -- ^ d2bl_p2
  -> Double       -- ^ d2bl_ediff
  -> Double       -- ^ d2ca_min
  -> Double       -- ^ d2ca_max
  -> Int          -- ^ crowndamage class
  -> U.Vector Double -- ^ damage bin edges
  -> Double       -- ^ individual crown area (returns total crown area per cohort)
careaAllom !dbh !spread !nplant !lmode !dbhMaxh !p2 !ediff !caMin !caMax !crowndamage !damageBins =
  let !dbhEff = case lmode of
                  2 -> dbh
                  _ -> min dbh dbhMaxh
      (cAreaInd, _) = carea2pwr dbhEff spread p2 ediff caMin caMax crowndamage damageBins
  in cAreaInd * nplant

-- ============================================================================
-- 7. Utility functions
-- ============================================================================

-- | Vertical canopy decay rate scaled on vcmax
decayCoeffVcmax :: Double -> Double -> Double -> Double
decayCoeffVcmax !vcmax25top !slopeParam !interceptParam =
  exp (slopeParam * vcmax25top - interceptParam)

-- | Calculate target leaf carbon for a given treelai for SP mode
leafcFromTreelai
  :: Double       -- ^ treelai
  -> Double       -- ^ cArea
  -> Double       -- ^ nplant
  -> Double       -- ^ slatop
  -> Double       -- ^ slamax
  -> Double       -- ^ vcmax25top
  -> Double       -- ^ leafn_vert_scaler_coeff1
  -> Double       -- ^ leafn_vert_scaler_coeff2
  -> Double
leafcFromTreelai !treelai !cArea !nplant !slatop !slamax !vcmax25top !coeff1 !coeff2
  | treelai <= 0.0 = 0.0
  | otherwise =
      let !g_per_kg = 1000.0
          !slat = g_per_kg * slatop
          !sla_max = g_per_kg * slamax
          !kn = decayCoeffVcmax vcmax25top coeff1 coeff2
          
          -- Leafc_per_unitarea at which sla_max is reached due to exponential sla profile
          !leafc_slamax = max 0.0 ((slat - sla_max) / (-1.0 * kn * slat * sla_max))
          
          -- treelai at which we reach maximum sla
          !tree_lai_at_slamax = log (1.0 - kn * slat * leafc_slamax) / (-1.0 * kn)
          
          !leafc_per_unitarea = if treelai < tree_lai_at_slamax
                                  then (1.0 - exp (treelai * (-1.0 * kn))) / (kn * slat)
                                  else let !leafc_linear_phase = (treelai - tree_lai_at_slamax) / sla_max
                                       in leafc_slamax + leafc_linear_phase
      in leafc_per_unitarea * (cArea / nplant)
