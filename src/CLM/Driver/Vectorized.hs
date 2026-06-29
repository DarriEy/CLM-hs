-- | Array-vectorized (Structure-of-Arrays) model state and physics, FOUNDATION.
--
-- The single-column 'CLMState' stores one column's fields as scalars
-- (@t_grnd_col :: Double@, …). True CLM/CTSM (and the Julia port and any GPU
-- backend) instead hold N columns at once as Structure-of-Arrays: each
-- per-column scalar becomes a @VU.Vector Double@ indexed by column, and each
-- per-(column,layer) field is flattened row-major as @c*nlev + j@ (the same
-- layout 'CLM.Infrastructure.SubgridAverage' already assumes, so the @c2g@
-- roll-up interoperates for free).
--
-- This module is the migration-safe foundation for that rewrite. Rather than
-- mutate 'CLMState' in place (which would break every adapter at once), the
-- vectorized state ('CLMStateV') COEXISTS with the scalar one, bridged by
-- 'gather' (['CLMState'] -> SoA) and 'scatter' (overlay SoA back onto a
-- ['CLMState'] base). That bridge gives a free correctness ORACLE: for any
-- vectorized step @fV@ and its scalar counterpart @f@,
--
-- @
--   scatter sts (fV cfg ctx (gather sts))  ==  map (f cfg ctx) sts
-- @
--
-- must hold bit-for-bit. Vectorization therefore changes only STORAGE and
-- ITERATION (SoA arrays + 'VU.zipWith'/'VU.generate'), reusing the existing
-- per-column physics KERNELS unchanged — so each step is bit-identical by
-- construction and verifiable independently. 'CLMStateV' grows one field group
-- at a time as subsystems are migrated.
--
-- Migrated so far: water balance, hydrology drainage, driver init, soil-evap
-- resistance, surface-water fraction.
module CLM.Driver.Vectorized
  ( -- * Vectorized state
    CLMStateV(..)
  , gather
  , scatter
    -- * Patch-indexed (CSR) layout helpers for ragged per-patch data
  , patchOffsets
  , patchSliceD
  , patchSliceI
    -- * Vectorized physics steps (SoA counterparts of the scalar adapters)
  , waterBalanceStepV
  , hydrologyDrainageStepV
  , drvInitStepV
  , soilEvapResistanceStepV
  , fracH2oSfcStepV
  , preFluxCalcsStepV
  , snowCompactionStepV
  , snowPercolationStepV
  , waterTableStepV
  , surfaceHumidityStepV
  , soilHydrologyStepV
  , soilFluxesStepV
  , baregroundFluxesStepV
  , soilTemperatureFullStepV
  , snowLayerCombineStepV
  , snowLayerDivideStepV
  , snowAgingStepV
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Driver.CLMDriver        (CLMState(..), TimestepContext(..), defaultCLMState)
import CLM.Constants.ControlFlags  (CLMDriverConfig)
import CLM.Constants.PhysicalConstants (nlevsno, nlevgrnd, nlevsoi, tfrz, denice)
import CLM.Types.WaterFluxData          (WaterFluxData(..))
import CLM.Types.WaterBalanceData       (WaterBalanceData(..))
import CLM.Types.WaterStateData         (WaterStateData(..))
import CLM.Types.ColumnData             (ColumnData(..))
import CLM.Types.TemperatureData        (TemperatureData(..))
import CLM.Types.EnergyFluxData         (EnergyFluxData(..))
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..))
import CLM.Types.SoilStateData          (SoilStateData(..))
import CLM.Types.SoilHydrologyData      (SoilHydrologyData(..))
import CLM.Types.CanopyStateData        (CanopyStateData(..))
import CLM.Types.FrictionVelocityData   (FrictionVelocityData(..))
import CLM.Types.GridcellData           (GridcellData(..))
import CLM.BioGeoPhys.BalanceCheck
  ( waterBalanceCol, WaterBalanceColInput(..), WaterBalanceColOutput(..) )
import CLM.BioGeoPhys.HydrologyDrainage
  ( computeTotalRunoff, TotalRunoffInput(..), TotalRunoffResult(..) )
import CLM.BioGeoPhys.SurfaceResistance
  ( calcBetaLeePielke1992, BetaInput(..), BetaResult(..) )
import CLM.BioGeoPhys.SurfaceWater
  ( computeFracH2osfc, FracH2osfcInput(..), FracH2osfcResult(..) )
import CLM.BioGeoPhys.SnowHydrology
  ( snowPercolationBottomPacked, SnowPercResult(..), defaultSnowHydroParams )
-- Two adapters whose kernels are inherently per-column sequential are reused
-- directly per column (rebuild single-column state -> run -> re-flatten). No
-- import cycle: PhysicsAdapters does not import this module.
import CLM.Driver.PhysicsAdapters
  ( snowCompactionStep, soilHydrologyStep, soilFluxesStep, baregroundFluxesStep
  , soilTemperatureFullStep, snowLayerCombineStep, snowLayerDivideStep, snowAgingStep )
import CLM.BioGeoPhys.SoilHydrology
  ( waterTable, WaterTableResult(..), defaultSoilHydroParams )
import CLM.BioGeoPhys.SnowSNICAR (SnicarOptics)
import CLM.BioGeoPhys.SurfaceHumidity
  ( surfaceHumidity, SurfaceHumidityInput(..), SurfaceHumidityResult(..) )

-- | The combined snow+soil grid stride for per-(column,layer) fields.
gridNlev :: Int
gridNlev = nlevsno + nlevgrnd

-- | Pad (or truncate) a per-layer vector to a fixed stride, zero-filling beyond
-- its length. Reproduces the scalar adapters' @safeIdx _ j = 0.0@ out-of-range
-- semantics, so the flattened SoA reads are bit-identical to the scalar reads.
padTo :: Int -> VU.Vector Double -> VU.Vector Double
padTo n v = VU.generate n (\j -> if j < VU.length v then v VU.! j else 0.0)

-- ============================================================================
-- Patch-indexed (CSR) layout for ragged per-patch data
-- ============================================================================
-- Per-patch fields have a per-column patch count (the number of PFT patches on
-- that column), which varies between columns. They are stored flat, with a
-- compressed-sparse-row offset vector @vPatchOff@ of length @nCols+1@: column
-- @c@ owns flat indices @[vPatchOff!c .. vPatchOff!(c+1) - 1]@, so
-- @patchCount c = vPatchOff!(c+1) - vPatchOff!c@ (0 for a patch-less column, in
-- which case its slice is empty — reproducing the scalar adapters' @VU.null@
-- branch exactly). Within a column, every populated per-patch field must have
-- length @patchCount@ (the CLM invariant); a field that is empty for ALL columns
-- is stored as a single empty flat vector and reconstructed empty per column.

-- | CSR offset vector (length @nCols+1@, prefix sums) from per-column patch counts.
patchOffsets :: [Int] -> VU.Vector Int
patchOffsets counts = VU.fromList (scanl (+) 0 counts)

-- | Slice column @c@'s patch data out of a flat per-patch 'Double' vector.
patchSliceD :: VU.Vector Int -> VU.Vector Double -> Int -> VU.Vector Double
patchSliceD off arr c
  | VU.null arr = VU.empty                       -- field absent for all columns
  | otherwise   = VU.slice (off VU.! c) (off VU.! (c + 1) - off VU.! c) arr

-- | Slice column @c@'s patch data out of a flat per-patch 'Int' vector.
patchSliceI :: VU.Vector Int -> VU.Vector Int -> Int -> VU.Vector Int
patchSliceI off arr c
  | VU.null arr = VU.empty
  | otherwise   = VU.slice (off VU.! c) (off VU.! (c + 1) - off VU.! c) arr

-- | Structure-of-Arrays model state: one entry per column in every scalar
-- vector; per-(column,layer) fields are flattened @c*gridNlev + j@ (soil-only
-- @watsat@ is @c*nlevsoi + j@). Grows as subsystems are vectorized.
data CLMStateV = CLMStateV
  { vNumCols       :: !Int                 -- ^ Number of columns
  , vNlev          :: !Int                 -- ^ Snow+soil layer stride (= nlevsno+nlevgrnd)
    -- water balance / hydrology drainage --------------------------------------
  , vqflx_evap_tot :: !(VU.Vector Double)  -- ^ Total evapotranspiration [mm/s] (read)
  , vqflx_surf     :: !(VU.Vector Double)  -- ^ Surface runoff [mm/s] (read+write)
  , vqflx_drain    :: !(VU.Vector Double)  -- ^ Sub-surface drainage [mm/s] (read)
  , vwb_errh2o     :: !(VU.Vector Double)  -- ^ Water conservation error [mm] (write)
    -- driver init -------------------------------------------------------------
  , vt_soisno      :: !(VU.Vector Double)  -- ^ per-(col,layer) t_soisno_col (read)
  , vt_soisno_bef  :: !(VU.Vector Double)  -- ^ per-(col,layer) t_soisno_bef_col (write)
  , vt_h2osfc      :: !(VU.Vector Double)  -- ^ surface-water temperature (read)
  , vt_h2osfc_bef  :: !(VU.Vector Double)  -- ^ surface-water temperature snapshot (write)
  , veflx_soil_grnd :: !(VU.Vector Double) -- ^ ground heat flux (write, :=0)
    -- soil-evap resistance / surface-water fraction ---------------------------
  , vsnl           :: !(VU.Vector Int)     -- ^ snow-layer count clmSnl (read)
  , vh2osoi_liq    :: !(VU.Vector Double)  -- ^ per-(col,layer) liquid water (read)
  , vh2osoi_ice    :: !(VU.Vector Double)  -- ^ per-(col,layer) ice (read)
  , vcolDz         :: !(VU.Vector Double)  -- ^ per-(col,layer) layer thickness (read)
  , vwatsat        :: !(VU.Vector Double)  -- ^ per-(col,soil-layer) saturation (read, stride nlevsoi)
  , vh2osfc        :: !(VU.Vector Double)  -- ^ surface water [kg/m^2] (read)
  , vh2osno        :: !(VU.Vector Double)  -- ^ snow water equivalent [kg/m^2] (read)
  , vfrac_sno      :: !(VU.Vector Double)  -- ^ snow fraction (read)
  , vfrac_sno_eff  :: !(VU.Vector Double)  -- ^ effective snow fraction (read)
  , vfrac_h2osfc   :: !(VU.Vector Double)  -- ^ surface-water fraction (read+write)
  , vsoilbeta      :: !(VU.Vector Double)  -- ^ soil evap resistance beta (write)
    -- pre-flux calcs ----------------------------------------------------------
  , vt_grnd        :: !(VU.Vector Double)  -- ^ ground surface temperature [K] (read)
  , vfrac_iceold   :: !(VU.Vector Double)  -- ^ top-layer ice fraction (write)
    -- snow compaction ---------------------------------------------------------
  , vcolZ          :: !(VU.Vector Double)  -- ^ per-(col,layer) midpoint depth, stride vNlev (read+write)
  , vcolZi         :: !(VU.Vector Double)  -- ^ per-(col,interface) depth, stride vNlev+1 (read+write)
  , vsnow_depth    :: !(VU.Vector Double)  -- ^ snow depth of covered area [m] (write)
    -- water table -------------------------------------------------------------
  , vbsw              :: !(VU.Vector Double)  -- ^ per-(col,soil-layer) Clapp-Hornberger b (read, stride nlevsoi)
  , vsucsat           :: !(VU.Vector Double)  -- ^ per-(col,soil-layer) sat. matric potential (read, stride nlevsoi)
  , vwt_zwt_in        :: !(VU.Vector Double)  -- ^ prognostic water-table depth in [m] (read; fallback 2.0)
  , vwt_qcharge       :: !(VU.Vector Double)  -- ^ aquifer recharge [mm/s] (read; fallback 0.0)
  , vwt_frost_carried :: !(VU.Vector Double)  -- ^ carried frost table for snow-free columns (read)
  , vsh_zwt           :: !(VU.Vector Double)  -- ^ water-table depth zwt [m] (write)
  , vsh_zwts          :: !(VU.Vector Double)  -- ^ shallower water-table depth [m] (write)
  , vsh_zwt_perched   :: !(VU.Vector Double)  -- ^ perched water-table depth [m] (write)
  , vsh_frost_table   :: !(VU.Vector Double)  -- ^ frost-table depth [m] (write)
    -- surface humidity --------------------------------------------------------
  , vqg               :: !(VU.Vector Double)  -- ^ ground specific humidity [kg/kg] (write)
  , vqg_snow          :: !(VU.Vector Double)  -- ^ snow specific humidity (write)
  , vqg_soil          :: !(VU.Vector Double)  -- ^ soil specific humidity (write)
  , vqg_h2osfc        :: !(VU.Vector Double)  -- ^ surface-water specific humidity (write)
  , vdqgdT            :: !(VU.Vector Double)  -- ^ d(qg)/dT [kg/kg/K] (write)
    -- soil hydrology ----------------------------------------------------------
  , vhksat             :: !(VU.Vector Double) -- ^ per-(col,soil-layer) sat. hydraulic conductivity (read, stride nlevsoi)
  , vqflx_tran_veg     :: !(VU.Vector Double) -- ^ canopy transpiration [mm/s] (read)
  , vp_e_ice           :: !(VU.Vector Double) -- ^ calib: ice impedance factor (read)
  , vp_baseflow_scalar :: !(VU.Vector Double) -- ^ calib: baseflow scalar (read)
  , vp_fff             :: !(VU.Vector Double) -- ^ calib: TOPMODEL decay factor (read)
  , vp_fmax            :: !(VU.Vector Double) -- ^ calib: max fractional saturated area (read)
  , vp_n_baseflow      :: !(VU.Vector Double) -- ^ calib: baseflow exponent (read)
  , vp_n_melt_coef     :: !(VU.Vector Double) -- ^ calib: snowmelt coefficient (read)
    -- patch-indexed (CSR) layout ----------------------------------------------
  , vPatchOff          :: !(VU.Vector Int)    -- ^ CSR patch offsets (length nCols+1)
  , vp_wtgcell         :: !(VU.Vector Double) -- ^ per-patch weight rel. gridcell (cstate_patch_wtgcell), flat CSR
    -- soil fluxes (energy/water partition over PFT patches) -------------------
  , veflx_sh_tot       :: !(VU.Vector Double) -- ^ eflx_sh_tot_patch scalar (read fallback + write)
  , veflx_sh_grnd      :: !(VU.Vector Double) -- ^ eflx_sh_grnd_patch scalar (read fallback + write)
  , veflx_lh_tot       :: !(VU.Vector Double) -- ^ eflx_lh_tot_patch scalar (write)
  , veflx_lwrad_out    :: !(VU.Vector Double) -- ^ eflx_lwrad_out_patch scalar (write)
  , veflx_lwrad_net    :: !(VU.Vector Double) -- ^ eflx_lwrad_net_patch scalar (read fallback + write)
  , vsabg              :: !(VU.Vector Double) -- ^ sabg_patch scalar (read fallback)
  , vcgrnds            :: !(VU.Vector Double) -- ^ cgrnds_patch scalar (read fallback)
  , vcgrndl            :: !(VU.Vector Double) -- ^ cgrndl_patch scalar (read fallback)
  , vdlrad             :: !(VU.Vector Double) -- ^ dlrad_patch scalar (read fallback)
  , vulrad             :: !(VU.Vector Double) -- ^ ulrad_patch scalar (read fallback)
  , vqflx_evap_grnd    :: !(VU.Vector Double) -- ^ qflx_evap_grnd_col scalar (read fallback + write)
  , vp_eflx_sh_tot     :: !(VU.Vector Double) -- ^ eflx_sh_tot_patch_vec    CSR (read+write)
  , vp_eflx_sh_grnd    :: !(VU.Vector Double) -- ^ eflx_sh_grnd_patch_vec   CSR (read+write)
  , vp_eflx_lwrad_net  :: !(VU.Vector Double) -- ^ eflx_lwrad_net_patch_vec CSR (read+write)
  , vp_sabg            :: !(VU.Vector Double) -- ^ sabg_patch_vec   CSR (read)
  , vp_cgrnds          :: !(VU.Vector Double) -- ^ cgrnds_patch_vec CSR (read)
  , vp_cgrndl          :: !(VU.Vector Double) -- ^ cgrndl_patch_vec CSR (read)
  , vp_dlrad           :: !(VU.Vector Double) -- ^ dlrad_patch_vec  CSR (read)
  , vp_ulrad           :: !(VU.Vector Double) -- ^ ulrad_patch_vec  CSR (read)
  , vp_qflx_evap_tot   :: !(VU.Vector Double) -- ^ qflx_evap_tot_patch_vec  CSR (read+write)
  , vp_qflx_evap_grnd  :: !(VU.Vector Double) -- ^ qflx_evap_grnd_patch_vec CSR (read+write)
  , vp_qflx_tran_veg   :: !(VU.Vector Double) -- ^ qflx_tran_veg_patch_vec  CSR (read)
  , vp_eflx_lh_tot     :: !(VU.Vector Double) -- ^ eflx_lh_tot_patch_vec    CSR (write)
  , vp_eflx_lwrad_out  :: !(VU.Vector Double) -- ^ eflx_lwrad_out_patch_vec CSR (write)
  , vp_frac_veg_nosno     :: !(VU.Vector Int) -- ^ cstate_frac_veg_nosno_patch     CSR (read)
  , vp_frac_veg_nosno_alb :: !(VU.Vector Int) -- ^ cstate_frac_veg_nosno_alb_patch CSR (read)
    -- bareground fluxes -------------------------------------------------------
  , vzii             :: !(VU.Vector Double) -- ^ convective BL height zii (Column) (read)
  , vp_elai          :: !(VU.Vector Double) -- ^ cstate_elai_patch CSR (read)
  , vp_esai          :: !(VU.Vector Double) -- ^ cstate_esai_patch CSR (read)
  , vcgrnd           :: !(VU.Vector Double) -- ^ cgrnd_patch scalar (read+write)
  , vt_ref2m         :: !(VU.Vector Double) -- ^ t_ref2m_patch scalar (read+write)
  , vp_cgrnd         :: !(VU.Vector Double) -- ^ cgrnd_patch_vec   CSR (read+write)
  , vp_t_ref2m       :: !(VU.Vector Double) -- ^ t_ref2m_patch_vec CSR (read+write)
  , vp_fvel_ram1     :: !(VU.Vector Double) -- ^ fvel_ram1_patch   CSR (read+write)
  , vp_fvel_ustar    :: !(VU.Vector Double) -- ^ fvel_ustar_patch  CSR (read+write)
    -- soil temperature full solve ---------------------------------------------
  , vtkmg          :: !(VU.Vector Double)  -- ^ sstate_tkmg_col   stride nlevgrnd (read; empty-for-all ⇒ empty)
  , vtkdry         :: !(VU.Vector Double)  -- ^ sstate_tkdry_col  stride nlevgrnd (read)
  , vcsol          :: !(VU.Vector Double)  -- ^ sstate_csol_col   stride nlevgrnd (read)
  , vtksatu        :: !(VU.Vector Double)  -- ^ sstate_tksatu_col stride nlevgrnd (read)
  , vthk_override  :: !(VU.Vector Double)  -- ^ sstate_thk_override_col stride vNlev (read; empty ⇒ Nothing)
  , vcv_override   :: !(VU.Vector Double)  -- ^ sstate_cv_override_col  stride vNlev (read; empty ⇒ Nothing)
  , vgrc_nbedrock  :: !(VU.Vector Int)     -- ^ grc_nbedrock resolved (empty⇒nlevsoi), one per col (read)
  , vp_eflx_gnet   :: !(VU.Vector Double)  -- ^ eflx_gnet_patch_vec CSR (write)
  , vqflx_snomelt  :: !(VU.Vector Double)  -- ^ clmQflxSnomelt top-level scalar (write)
    -- full-grid (nlevgrnd) soil props — soilTemperature reads the whole soil
    -- column, unlike the top-/soil-layer adapters that use the nlevsoi copies
  , vwatsat_g      :: !(VU.Vector Double)  -- ^ watsat resolved, stride nlevgrnd (read)
  , vbsw_g         :: !(VU.Vector Double)  -- ^ bsw    resolved, stride nlevgrnd (read)
  , vsucsat_g      :: !(VU.Vector Double)  -- ^ sucsat resolved, stride nlevgrnd (read)
    -- snow aging --------------------------------------------------------------
  , vsnw_rds_top   :: !(VU.Vector Double)  -- ^ wdiag_snw_rds_top_col bulk top grain radius [um] (read+write)
  } deriving (Eq, Show)

-- | Project a list of single-column 'CLMState's (column order = list order)
-- into the SoA 'CLMStateV'. Pure; the overlay inverse is 'scatter'.
gather :: [CLMState] -> CLMStateV
gather sts = CLMStateV
  { vNumCols       = length sts
  , vNlev          = gridNlev
  , vqflx_evap_tot = VU.fromList [ qflx_evap_tot_patch (clmWaterFlux s) | s <- sts ]
  , vqflx_surf     = VU.fromList [ qflx_surf_col       (clmWaterFlux s) | s <- sts ]
  , vqflx_drain    = VU.fromList [ qflx_drain_col      (clmWaterFlux s) | s <- sts ]
  , vwb_errh2o     = VU.fromList [ scal0 (wb_errh2o_col (clmWaterBalance s)) | s <- sts ]
  , vt_soisno      = VU.concat   [ padTo gridNlev (t_soisno_col     (clmTemp s)) | s <- sts ]
  , vt_soisno_bef  = VU.concat   [ padTo gridNlev (t_soisno_bef_col (clmTemp s)) | s <- sts ]
  , vt_h2osfc      = VU.fromList [ t_h2osfc_col     (clmTemp s) | s <- sts ]
  , vt_h2osfc_bef  = VU.fromList [ t_h2osfc_bef_col (clmTemp s) | s <- sts ]
  , veflx_soil_grnd = VU.fromList [ eflx_soil_grnd_col (clmEnergyFlux s) | s <- sts ]
  , vsnl           = VU.fromList [ clmSnl s | s <- sts ]
  , vh2osoi_liq    = VU.concat   [ padTo gridNlev (h2osoi_liq_col (clmWaterState s)) | s <- sts ]
  , vh2osoi_ice    = VU.concat   [ padTo gridNlev (h2osoi_ice_col (clmWaterState s)) | s <- sts ]
  , vcolDz         = VU.concat   [ padTo gridNlev (colDz  (clmColumn s)) | s <- sts ]
  , vwatsat        = VU.concat   [ padTo nlevsoi  (watsat (clmColumn s)) | s <- sts ]
  , vh2osfc        = VU.fromList [ h2osfc_col (clmWaterState s) | s <- sts ]
  , vh2osno        = VU.fromList [ h2osno_col (clmWaterState s) | s <- sts ]
  , vfrac_sno      = VU.fromList [ scal0 (wdiag_frac_sno_col     (clmWaterDiagBulk s)) | s <- sts ]
  , vfrac_sno_eff  = VU.fromList [ scal0 (wdiag_frac_sno_eff_col (clmWaterDiagBulk s)) | s <- sts ]
  , vfrac_h2osfc   = VU.fromList [ scal0 (wdiag_frac_h2osfc_col  (clmWaterDiagBulk s)) | s <- sts ]
  , vsoilbeta      = VU.fromList [ scal0 (sstate_soilbeta_col    (clmSoilState s)) | s <- sts ]
  , vt_grnd        = VU.fromList [ t_grnd_col (clmTemp s) | s <- sts ]
  , vfrac_iceold   = VU.fromList [ scal0 (wdiag_frac_iceold_col (clmWaterDiagBulk s)) | s <- sts ]
  , vcolZ          = VU.concat   [ padTo gridNlev       (colZ  (clmColumn s)) | s <- sts ]
  , vcolZi         = VU.concat   [ padTo (gridNlev + 1) (colZi (clmColumn s)) | s <- sts ]
  , vsnow_depth    = VU.fromList [ scal0 (wdiag_snow_depth_col (clmWaterDiagBulk s)) | s <- sts ]
  , vbsw              = VU.concat   [ padTo nlevsoi (rslv (sstate_bsw_col    (clmSoilState s)) (bsw    (clmColumn s))) | s <- sts ]
  , vsucsat           = VU.concat   [ padTo nlevsoi (rslv (sstate_sucsat_col (clmSoilState s)) (sucsat (clmColumn s))) | s <- sts ]
  , vwt_zwt_in        = VU.fromList [ scalOr 2.0 (sh_zwt_col     (clmSoilHydro s)) | s <- sts ]
  , vwt_qcharge       = VU.fromList [ scalOr 0.0 (sh_qcharge_col (clmSoilHydro s)) | s <- sts ]
  , vwt_frost_carried = VU.fromList [ frostCarried s | s <- sts ]
  , vsh_zwt           = VU.fromList [ scal0 (sh_zwt_col         (clmSoilHydro s)) | s <- sts ]
  , vsh_zwts          = VU.fromList [ scal0 (sh_zwts_col        (clmSoilHydro s)) | s <- sts ]
  , vsh_zwt_perched   = VU.fromList [ scal0 (sh_zwt_perched_col (clmSoilHydro s)) | s <- sts ]
  , vsh_frost_table   = VU.fromList [ scal0 (sh_frost_table_col (clmSoilHydro s)) | s <- sts ]
  , vqg               = VU.fromList [ scal0 (wdiag_qg_col        (clmWaterDiagBulk s)) | s <- sts ]
  , vqg_snow          = VU.fromList [ scal0 (wdiag_qg_snow_col   (clmWaterDiagBulk s)) | s <- sts ]
  , vqg_soil          = VU.fromList [ scal0 (wdiag_qg_soil_col   (clmWaterDiagBulk s)) | s <- sts ]
  , vqg_h2osfc        = VU.fromList [ scal0 (wdiag_qg_h2osfc_col (clmWaterDiagBulk s)) | s <- sts ]
  , vdqgdT            = VU.fromList [ scal0 (wdiag_dqgdT_col     (clmWaterDiagBulk s)) | s <- sts ]
  , vhksat             = VU.concat   [ padTo nlevsoi (rslv (sstate_hksat_col (clmSoilState s)) (hksat (clmColumn s))) | s <- sts ]
  , vqflx_tran_veg     = VU.fromList [ qflx_tran_veg_patch (clmWaterFlux s) | s <- sts ]
  , vp_e_ice           = VU.fromList [ clmP_e_ice           s | s <- sts ]
  , vp_baseflow_scalar = VU.fromList [ clmP_baseflow_scalar s | s <- sts ]
  , vp_fff             = VU.fromList [ clmP_fff             s | s <- sts ]
  , vp_fmax            = VU.fromList [ clmP_fmax            s | s <- sts ]
  , vp_n_baseflow      = VU.fromList [ clmP_n_baseflow      s | s <- sts ]
  , vp_n_melt_coef     = VU.fromList [ clmP_n_melt_coef     s | s <- sts ]
  , vPatchOff          = patchOffsets [ VU.length (cstate_patch_wtgcell (clmCanopyState s)) | s <- sts ]
  , vp_wtgcell         = VU.concat [ cstate_patch_wtgcell (clmCanopyState s) | s <- sts ]
  , veflx_sh_tot       = VU.fromList [ eflx_sh_tot_patch    (clmEnergyFlux s) | s <- sts ]
  , veflx_sh_grnd      = VU.fromList [ eflx_sh_grnd_patch   (clmEnergyFlux s) | s <- sts ]
  , veflx_lh_tot       = VU.fromList [ eflx_lh_tot_patch    (clmEnergyFlux s) | s <- sts ]
  , veflx_lwrad_out    = VU.fromList [ eflx_lwrad_out_patch (clmEnergyFlux s) | s <- sts ]
  , veflx_lwrad_net    = VU.fromList [ eflx_lwrad_net_patch (clmEnergyFlux s) | s <- sts ]
  , vsabg              = VU.fromList [ sabg_patch   (clmEnergyFlux s) | s <- sts ]
  , vcgrnds            = VU.fromList [ cgrnds_patch (clmEnergyFlux s) | s <- sts ]
  , vcgrndl            = VU.fromList [ cgrndl_patch (clmEnergyFlux s) | s <- sts ]
  , vdlrad             = VU.fromList [ dlrad_patch  (clmEnergyFlux s) | s <- sts ]
  , vulrad             = VU.fromList [ ulrad_patch  (clmEnergyFlux s) | s <- sts ]
  , vqflx_evap_grnd    = VU.fromList [ qflx_evap_grnd_col (clmWaterFlux s) | s <- sts ]
  , vp_eflx_sh_tot     = gPd (eflx_sh_tot_patch_vec    . clmEnergyFlux)
  , vp_eflx_sh_grnd    = gPd (eflx_sh_grnd_patch_vec   . clmEnergyFlux)
  , vp_eflx_lwrad_net  = gPd (eflx_lwrad_net_patch_vec . clmEnergyFlux)
  , vp_sabg            = gPd (sabg_patch_vec   . clmEnergyFlux)
  , vp_cgrnds          = gPd (cgrnds_patch_vec . clmEnergyFlux)
  , vp_cgrndl          = gPd (cgrndl_patch_vec . clmEnergyFlux)
  , vp_dlrad           = gPd (dlrad_patch_vec  . clmEnergyFlux)
  , vp_ulrad           = gPd (ulrad_patch_vec  . clmEnergyFlux)
  , vp_qflx_evap_tot   = gPd (qflx_evap_tot_patch_vec  . clmWaterFlux)
  , vp_qflx_evap_grnd  = gPd (qflx_evap_grnd_patch_vec . clmWaterFlux)
  , vp_qflx_tran_veg   = gPd (qflx_tran_veg_patch_vec  . clmWaterFlux)
  , vp_eflx_lh_tot     = gPd (eflx_lh_tot_patch_vec    . clmEnergyFlux)
  , vp_eflx_lwrad_out  = gPd (eflx_lwrad_out_patch_vec . clmEnergyFlux)
  , vp_frac_veg_nosno     = gPi (cstate_frac_veg_nosno_patch     . clmCanopyState)
  , vp_frac_veg_nosno_alb = gPi (cstate_frac_veg_nosno_alb_patch . clmCanopyState)
  , vzii          = VU.fromList [ zii (clmColumn s) | s <- sts ]
  , vp_elai       = gPd (cstate_elai_patch . clmCanopyState)
  , vp_esai       = gPd (cstate_esai_patch . clmCanopyState)
  , vcgrnd        = VU.fromList [ cgrnd_patch   (clmEnergyFlux s) | s <- sts ]
  , vt_ref2m      = VU.fromList [ t_ref2m_patch (clmTemp s)       | s <- sts ]
  , vp_cgrnd      = gPd (cgrnd_patch_vec   . clmEnergyFlux)
  , vp_t_ref2m    = gPd (t_ref2m_patch_vec . clmTemp)
  , vp_fvel_ram1  = gPd (fvel_ram1_patch   . clmFrictionVel)
  , vp_fvel_ustar = gPd (fvel_ustar_patch  . clmFrictionVel)
  , vtkmg         = gSg sstate_tkmg_col
  , vtkdry        = gSg sstate_tkdry_col
  , vcsol         = gSg sstate_csol_col
  , vtksatu       = gSg sstate_tksatu_col
  , vthk_override = gOv sstate_thk_override_col
  , vcv_override  = gOv sstate_cv_override_col
  , vgrc_nbedrock = VU.fromList [ nbOf s | s <- sts ]
  , vp_eflx_gnet  = gPd (eflx_gnet_patch_vec . clmEnergyFlux)
  , vqflx_snomelt = VU.fromList [ clmQflxSnomelt s | s <- sts ]
  , vwatsat_g     = VU.concat [ padTo nlevgrnd (rslv (sstate_watsat_col (clmSoilState s)) (watsat (clmColumn s))) | s <- sts ]
  , vbsw_g        = VU.concat [ padTo nlevgrnd (rslv (sstate_bsw_col    (clmSoilState s)) (bsw    (clmColumn s))) | s <- sts ]
  , vsucsat_g     = VU.concat [ padTo nlevgrnd (rslv (sstate_sucsat_col (clmSoilState s)) (sucsat (clmColumn s))) | s <- sts ]
  , vsnw_rds_top  = VU.fromList [ scal0 (wdiag_snw_rds_top_col (clmWaterDiagBulk s)) | s <- sts ]
  }
  where
    scal0 v = if VU.null v then 0.0 else v VU.! 0
    gPd sel = if all (VU.null . sel) sts then VU.empty else VU.concat [ sel s | s <- sts ]
    gPi sel = if all (VU.null . sel) sts then VU.empty else VU.concat [ sel s | s <- sts ]
    gSg sel = if all (VU.null . sel . clmSoilState) sts then VU.empty
              else VU.concat [ padTo nlevgrnd (sel (clmSoilState s)) | s <- sts ]
    gOv sel = if all (VU.null . sel . clmSoilState) sts then VU.empty
              else VU.concat [ padTo gridNlev (sel (clmSoilState s)) | s <- sts ]
    nbOf s = let nb = grc_nbedrock (clmGridcell s)
             in if VU.null nb then nlevsoi else nb VU.! 0
    scalOr d v = if VU.null v then d else v VU.! 0
    rslv ss colv = if VU.null ss then colv else ss   -- sstate else column (mirrors adapter)
    frostCarried s =
      let ft  = sh_frost_table_col (clmSoilHydro s)
          zp  = sh_zwt_perched_col (clmSoilHydro s)
          ziS = VU.slice nlevsno (nlevgrnd + 1) (padTo (gridNlev + 1) (colZi (clmColumn s)))
          six vec i = if i >= 0 && i < VU.length vec then vec VU.! i else 0.0
      in if not (VU.null ft) then ft VU.! 0
         else if not (VU.null zp) then zp VU.! 0
         else six ziS nlevsoi

-- | Overlay the SoA state's WRITE-target fields back onto a per-column base list
-- (one base 'CLMState' per column, same order/length as the 'gather'ed list),
-- leaving every untracked field of each base untouched. For fields a given step
-- did not modify, the vectorized value equals the gathered base value, so the
-- overlay is the identity — making 'scatter' safe to apply after any single
-- vectorized step. Per-(col,layer) fields are sliced @VU.slice (i*nlev) nlev@.
scatter :: [CLMState] -> CLMStateV -> [CLMState]
scatter bases v =
  let nlev = vNlev v
  in [ s { clmSnl = vsnl v VU.! i   -- soilHydrology meltout can flip snl -> 0
         , clmQflxSnomelt = vqflx_snomelt v VU.! i
         , clmWaterFlux    = (clmWaterFlux s)
             { qflx_evap_tot_patch = vqflx_evap_tot v VU.! i
             , qflx_surf_col       = vqflx_surf     v VU.! i
             , qflx_drain_col      = vqflx_drain    v VU.! i
             , qflx_evap_grnd_col  = vqflx_evap_grnd v VU.! i
             , qflx_evap_tot_patch_vec  = patchSliceD (vPatchOff v) (vp_qflx_evap_tot  v) i
             , qflx_evap_grnd_patch_vec = patchSliceD (vPatchOff v) (vp_qflx_evap_grnd v) i
             , qflx_tran_veg_patch      = vqflx_tran_veg v VU.! i
             , qflx_tran_veg_patch_vec  = patchSliceD (vPatchOff v) (vp_qflx_tran_veg v) i }
         , clmWaterBalance = (clmWaterBalance s)
             { wb_errh2o_col = VU.singleton (vwb_errh2o v VU.! i) }
         , clmTemp = (clmTemp s)
             { t_soisno_bef_col = VU.slice (i * nlev) nlev (vt_soisno_bef v)
             , t_h2osfc_bef_col = vt_h2osfc_bef v VU.! i
             , t_soisno_col     = VU.slice (i * nlev) nlev (vt_soisno v)
             , t_ref2m_patch     = vt_ref2m v VU.! i
             , t_ref2m_patch_vec = patchSliceD (vPatchOff v) (vp_t_ref2m v) i
             , t_grnd_col        = vt_grnd   v VU.! i
             , t_h2osfc_col      = vt_h2osfc v VU.! i }
         , clmEnergyFlux = (clmEnergyFlux s)
             { eflx_soil_grnd_col       = veflx_soil_grnd v VU.! i
             , eflx_sh_tot_patch        = veflx_sh_tot    v VU.! i
             , eflx_sh_grnd_patch       = veflx_sh_grnd   v VU.! i
             , eflx_lh_tot_patch        = veflx_lh_tot    v VU.! i
             , eflx_lwrad_out_patch     = veflx_lwrad_out v VU.! i
             , eflx_lwrad_net_patch     = veflx_lwrad_net v VU.! i
             , eflx_sh_tot_patch_vec    = patchSliceD (vPatchOff v) (vp_eflx_sh_tot    v) i
             , eflx_sh_grnd_patch_vec   = patchSliceD (vPatchOff v) (vp_eflx_sh_grnd   v) i
             , eflx_lh_tot_patch_vec    = patchSliceD (vPatchOff v) (vp_eflx_lh_tot    v) i
             , eflx_lwrad_out_patch_vec = patchSliceD (vPatchOff v) (vp_eflx_lwrad_out v) i
             , eflx_lwrad_net_patch_vec = patchSliceD (vPatchOff v) (vp_eflx_lwrad_net v) i
             , cgrnds_patch     = vcgrnds v VU.! i
             , cgrndl_patch     = vcgrndl v VU.! i
             , cgrnd_patch      = vcgrnd  v VU.! i
             , dlrad_patch      = vdlrad  v VU.! i
             , ulrad_patch      = vulrad  v VU.! i
             , cgrnds_patch_vec = patchSliceD (vPatchOff v) (vp_cgrnds v) i
             , cgrndl_patch_vec = patchSliceD (vPatchOff v) (vp_cgrndl v) i
             , cgrnd_patch_vec  = patchSliceD (vPatchOff v) (vp_cgrnd  v) i
             , dlrad_patch_vec  = patchSliceD (vPatchOff v) (vp_dlrad  v) i
             , ulrad_patch_vec  = patchSliceD (vPatchOff v) (vp_ulrad  v) i
             , eflx_gnet_patch_vec = patchSliceD (vPatchOff v) (vp_eflx_gnet v) i }
         , clmWaterState = (clmWaterState s)
             { h2osoi_liq_col = VU.slice (i * nlev) nlev (vh2osoi_liq v)
             , h2osoi_ice_col = VU.slice (i * nlev) nlev (vh2osoi_ice v)
             , h2osno_col     = vh2osno v VU.! i
             , h2osfc_col     = vh2osfc v VU.! i }
         , clmColumn = (clmColumn s)
             { colDz = VU.slice (i * nlev)       nlev       (vcolDz v)
             , colZ  = VU.slice (i * nlev)       nlev       (vcolZ  v)
             , colZi = VU.slice (i * (nlev + 1)) (nlev + 1) (vcolZi v) }
         , clmWaterDiagBulk = (clmWaterDiagBulk s)
             { wdiag_frac_h2osfc_col = VU.singleton (vfrac_h2osfc v VU.! i)
             , wdiag_frac_iceold_col = VU.singleton (vfrac_iceold v VU.! i)
             , wdiag_snow_depth_col  = VU.singleton (vsnow_depth  v VU.! i)
             , wdiag_qg_col          = VU.singleton (vqg        v VU.! i)
             , wdiag_qg_snow_col     = VU.singleton (vqg_snow   v VU.! i)
             , wdiag_qg_soil_col     = VU.singleton (vqg_soil   v VU.! i)
             , wdiag_qg_h2osfc_col   = VU.singleton (vqg_h2osfc v VU.! i)
             , wdiag_dqgdT_col       = VU.singleton (vdqgdT     v VU.! i)
             , wdiag_frac_sno_col     = VU.singleton (vfrac_sno     v VU.! i)
             , wdiag_frac_sno_eff_col = VU.singleton (vfrac_sno_eff v VU.! i)
             , wdiag_snw_rds_top_col  = VU.singleton (vsnw_rds_top  v VU.! i) }
         , clmSoilState = (clmSoilState s)
             { sstate_soilbeta_col = VU.singleton (vsoilbeta v VU.! i) }
         , clmCanopyState = (clmCanopyState s)
             { cstate_patch_wtgcell = patchSliceD (vPatchOff v) (vp_wtgcell v) i }
         , clmSoilHydro = (clmSoilHydro s)
             { sh_zwt_col         = VU.singleton (vsh_zwt         v VU.! i)
             , sh_zwts_col        = VU.singleton (vsh_zwts        v VU.! i)
             , sh_zwt_perched_col = VU.singleton (vsh_zwt_perched v VU.! i)
             , sh_frost_table_col = VU.singleton (vsh_frost_table v VU.! i)
             , sh_qcharge_col     = VU.singleton (vwt_qcharge     v VU.! i) }
         , clmFrictionVel = (clmFrictionVel s)
             { fvel_ram1_patch  = patchSliceD (vPatchOff v) (vp_fvel_ram1  v) i
             , fvel_ustar_patch = patchSliceD (vPatchOff v) (vp_fvel_ustar v) i }
         }
     | (i, s) <- zip [0 ..] bases ]

-- | Vectorized 'CLM.Driver.PhysicsAdapters.waterBalanceStep': reuses the
-- 'waterBalanceCol' kernel across all columns via 'VU.zipWith3'.
waterBalanceStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
waterBalanceStepV _cfg ctx v =
  let dtime     = tcDtime ctx
      forc_rain = if VU.null (tcForcRain ctx) then 0.0 else tcForcRain ctx VU.! 0
      forc_snow = if VU.null (tcForcSnow ctx) then 0.0 else tcForcSnow ctx VU.! 0
      mk evap surf drain = WaterBalanceColInput
        { wbci_endwb               = 0.0
        , wbci_begwb               = 0.0
        , wbci_forc_rain           = forc_rain
        , wbci_forc_snow           = forc_snow
        , wbci_qflx_flood          = 0.0
        , wbci_qflx_sfc_irrig      = 0.0
        , wbci_qflx_glcice_dyn     = 0.0
        , wbci_qflx_evap_tot       = evap
        , wbci_qflx_surf           = surf
        , wbci_qflx_qrgwl          = 0.0
        , wbci_qflx_drain          = drain
        , wbci_qflx_drain_perch    = 0.0
        , wbci_qflx_ice_runoff     = 0.0
        , wbci_qflx_snwcp_disc_liq = 0.0
        , wbci_qflx_snwcp_disc_ice = 0.0
        , wbci_dtime               = dtime
        , wbci_is_active           = True
        }
      errh2o = VU.zipWith3
        (\evap surf drain -> wbco_errh2o (waterBalanceCol (mk evap surf drain)))
        (vqflx_evap_tot v) (vqflx_surf v) (vqflx_drain v)
  in v { vwb_errh2o = errh2o }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.hydrologyDrainageStep': total runoff
-- per column via 'computeTotalRunoff' (single soil column ⇒ istsoil, non-urban).
hydrologyDrainageStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
hydrologyDrainageStepV _cfg _ctx v =
  let surf' = VU.zipWith
        (\drain surf -> trr_qflx_runoff (computeTotalRunoff TotalRunoffInput
          { tri_qflx_drain         = drain
          , tri_qflx_surf          = surf
          , tri_qflx_qrgwl         = 0.0
          , tri_qflx_drain_perched = 0.0
          , tri_lun_itype          = 1
          , tri_urbpoi             = False
          }))
        (vqflx_drain v) (vqflx_surf v)
  in v { vqflx_surf = surf' }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.drvInitStep': snapshot the soil/snow
-- and surface-water temperatures into their @_bef@ buffers and zero the ground
-- heat flux. The flat copies are exactly the per-column copies of the scalar
-- step.
drvInitStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
drvInitStepV _cfg _ctx v =
  v { vt_soisno_bef   = vt_soisno v
    , vt_h2osfc_bef   = vt_h2osfc v
    , veflx_soil_grnd = VU.replicate (vNumCols v) 0.0
    }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.soilEvapResistanceStep': reuses the
-- 'calcBetaLeePielke1992' kernel per column over the SoA arrays. Top layer index
-- = @nlevsno + snl@; @watsat@ is read on the soil-only grid at @topIdx-nlevsno@.
soilEvapResistanceStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
soilEvapResistanceStepV _cfg _ctx v =
  let nlev = vNlev v
      sIdx vec k = if k >= 0 && k < VU.length vec then vec VU.! k else 0.0
      beta c =
        let snl    = vsnl v VU.! c
            topIdx = nlevsno + snl
            inp = BetaInput
              { bi_lunType        = 1
              , bi_colType        = 1
              , bi_dz_top         = sIdx (vcolDz v)      (c * nlev + topIdx)
              , bi_h2osoi_liq_top = sIdx (vh2osoi_liq v) (c * nlev + topIdx)
              , bi_h2osoi_ice_top = sIdx (vh2osoi_ice v) (c * nlev + topIdx)
              , bi_watsat_top     = if topIdx >= nlevsno
                                    then sIdx (vwatsat v) (c * nlevsoi + (topIdx - nlevsno))
                                    else 1.0
              , bi_watfc_top      = if topIdx >= nlevsno
                                    then sIdx (vwatsat v) (c * nlevsoi + (topIdx - nlevsno)) * 0.5
                                    else 0.5
              , bi_frac_sno       = vfrac_sno v VU.! c
              , bi_frac_h2osfc    = vfrac_h2osfc v VU.! c
              }
        in br_soilbeta (calcBetaLeePielke1992 inp)
  in v { vsoilbeta = VU.generate (vNumCols v) beta }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.fracH2oSfcStep': reuses the
-- 'computeFracH2osfc' kernel per column. The per-column snow-layer water sum
-- runs over @j ∈ [nlevsno+snl .. nlevsno-1]@ on the flattened h2osoi arrays,
-- mirroring the scalar adapter's bounded loop exactly. @dtime@ is global.
fracH2oSfcStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
fracH2oSfcStepV _cfg ctx v =
  let dtime = tcDtime ctx
      nlev  = vNlev v
      frac c =
        let snl  = vsnl v VU.! c
            base = c * nlev
            h2osno_total = vh2osno v VU.! c
              + sum [ (vh2osoi_ice v VU.! (base + j)) + (vh2osoi_liq v VU.! (base + j))
                    | j <- [nlevsno + snl .. nlevsno - 1] ]
            inp = FracH2osfcInput
              { fhi_dtime        = dtime
              , fhi_micro_sigma  = 0.4
              , fhi_h2osno_total = h2osno_total
              , fhi_h2osfc       = vh2osfc v VU.! c
              , fhi_frac_sno     = vfrac_sno v VU.! c
              , fhi_frac_sno_eff = vfrac_sno_eff v VU.! c
              }
        in fhr_frac_h2osfc (computeFracH2osfc inp)
  in v { vfrac_h2osfc = VU.generate (vNumCols v) frac }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.preFluxCalcsStep'. Only two outputs
-- are live: the top-layer ice fraction (@frac_iceold@, top index @nlevsno+snl@)
-- and the frozen-ground soil-evap factor (@soilbeta = if t_grnd < tfrz then
-- 0.01 else 1.0@). The scalar adapter's other intermediates are dead. snl/layer
-- structure is unchanged.
preFluxCalcsStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
preFluxCalcsStepV _cfg _ctx v =
  let nlev = vNlev v
      sIdx vec k = if k >= 0 && k < VU.length vec then vec VU.! k else 0.0
      fracIceold c =
        let topIdx = nlevsno + (vsnl v VU.! c)
            base   = c * nlev
            liqTop = sIdx (vh2osoi_liq v) (base + topIdx)
            iceTop = sIdx (vh2osoi_ice v) (base + topIdx)
            tot    = liqTop + iceTop
        in if tot > 0.0 then iceTop / tot else 0.0
      soilbeta c = if vt_grnd v VU.! c < tfrz then 0.01 else 1.0
  in v { vfrac_iceold = VU.generate (vNumCols v) fracIceold
       , vsoilbeta    = VU.generate (vNumCols v) soilbeta
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.snowCompactionStep' (Anderson 1976).
-- Compaction is per-layer sequential (the overburden accumulates downward), so
-- this reuses the scalar adapter per column: rebuild a single-column 'CLMState'
-- from the SoA slices, run 'snowCompactionStep', re-flatten the written geometry.
-- Bit-identical by construction; @snl@/layer count unchanged.
snowCompactionStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
snowCompactionStepV cfg ctx v =
  let nlev   = vNlev v
      nlevZi = nlev + 1
      runCol c =
        let st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz = VU.slice (c * nlev)   nlev   (vcolDz v)
                  , colZ  = VU.slice (c * nlev)   nlev   (vcolZ  v)
                  , colZi = VU.slice (c * nlevZi) nlevZi (vcolZi v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_ice_col = VU.slice (c * nlev) nlev (vh2osoi_ice v)
                  , h2osoi_liq_col = VU.slice (c * nlev) nlev (vh2osoi_liq v) }
              , clmTemp = (clmTemp defaultCLMState)
                  { t_soisno_col = VU.slice (c * nlev) nlev (vt_soisno v) }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_col = VU.singleton (vfrac_sno v VU.! c) }
              }
        in snowCompactionStep cfg ctx st0
      results  = map runCol [0 .. vNumCols v - 1]
      scal0' u = if VU.null u then 0.0 else u VU.! 0
  in v { vcolDz      = VU.concat   [ colDz (clmColumn r) | r <- results ]
       , vcolZ       = VU.concat   [ colZ  (clmColumn r) | r <- results ]
       , vcolZi      = VU.concat   [ colZi (clmColumn r) | r <- results ]
       , vsnow_depth = VU.fromList [ scal0' (wdiag_snow_depth_col (clmWaterDiagBulk r)) | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.snowPercolationStep': reuses the
-- 'snowPercolationBottomPacked' kernel per column, replicating the scalar gate
-- (@snl<0 && -snl>=1 && frac_sno_eff>0 && canResolve@) and routing the
-- pack-bottom drainage into the top soil layer's liquid (index @nlevsno@).
-- Inactive columns pass through; @snl@/layer count unchanged.
snowPercolationStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
snowPercolationStepV _cfg ctx v =
  let dtime   = tcDtime ctx
      nlev    = vNlev v
      topSoil = nlevsno
      sIdx vec k = if k >= 0 && k < VU.length vec then vec VU.! k else 0.0
      perCol c =
        let base  = c * nlev
            slc   = VU.slice base nlev
            liq_v = slc (vh2osoi_liq v)
            ice_v = slc (vh2osoi_ice v)
            dz_v  = slc (vcolDz v)
            t_v   = slc (vt_soisno v)
            snl          = vsnl v VU.! c
            frac_sno_eff = vfrac_sno_eff v VU.! c
            topSnow = nlevsno + snl
            canResolve =
                 VU.length liq_v >= nlevsno + nlevgrnd
              && VU.length ice_v >= nlevsno + nlevgrnd
              && VU.length dz_v  >= nlevsno + nlevgrnd
              && VU.length t_v   >= nlevsno + nlevgrnd
              && topSnow >= 0
            active = snl < 0 && negate snl >= 1 && frac_sno_eff > 0.0 && canResolve
        in if not active
           then (liq_v, ice_v, t_v)
           else
             let res     = snowPercolationBottomPacked
                             defaultSnowHydroParams dtime frac_sno_eff snl
                             dz_v ice_v liq_v t_v
                 liqPerc = sprLiq res
                 drainMm = sprSnowDrain res
                 liq'    = if topSoil < VU.length liqPerc
                           then liqPerc VU.// [(topSoil, sIdx liqPerc topSoil + drainMm)]
                           else liqPerc
             in (liq', sprIce res, sprTSoisno res)
      results = map perCol [0 .. vNumCols v - 1]
  in v { vh2osoi_liq = VU.concat [ l | (l, _, _) <- results ]
       , vh2osoi_ice = VU.concat [ i | (_, i, _) <- results ]
       , vt_soisno   = VU.concat [ t | (_, _, t) <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.waterTableStep': reuses the
-- 'waterTable' kernel per column and reproduces the adapter's per-column frost-
-- and perched-table logic (computed outside the kernel). Soil-grid reads
-- (watsat/bsw/sucsat) stride @nlevsoi@; combined-grid reads stride @vNlev@;
-- @colZi@ on its @vNlev+1@ interface stride. @sh_zwts@ mirrors the scalar
-- adapter (= @wtr_zwt@); @h2osoi_liq@ is left unchanged (the kernel returns it
-- unchanged — drainage modifies it elsewhere). Frost-table carried value is
-- resolved in 'gather' (@vwt_frost_carried@).
waterTableStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
waterTableStepV _cfg ctx v =
  let dtime  = tcDtime ctx
      nlev   = vNlev v
      nlevZi = nlev + 1
      six vec i = if i >= 0 && i < VU.length vec then vec VU.! i else 0.0
      colCompute c =
        let base   = c * nlev
            baseZi = c * nlevZi
            baseS  = c * nlevsoi
            watsat_c = VU.slice baseS nlevsoi (vwatsat v)
            bsw_c    = VU.slice baseS nlevsoi (vbsw    v)
            sucsat_c = VU.slice baseS nlevsoi (vsucsat v)
            liq_c    = VU.slice base  nlev (vh2osoi_liq v)
            ice_c    = VU.slice base  nlev (vh2osoi_ice v)
            tsno_c   = VU.slice base  nlev (vt_soisno   v)
            dz_full  = VU.slice base  nlev (vcolDz v)
            z_full   = VU.slice base  nlev (vcolZ  v)
            zi_full  = VU.slice baseZi nlevZi (vcolZi v)
            z_soil  = VU.slice nlevsno nlevgrnd       z_full
            zi_soil = VU.slice nlevsno (nlevgrnd + 1) zi_full
            dz_soil = VU.slice nlevsno nlevgrnd       dz_full
            eff_por = VU.generate nlevsoi $ \j ->
              let ws_j  = six watsat_c j
                  ice_j = six ice_c   (j + nlevsno)
                  dz_j  = six dz_full (j + nlevsno)
              in max 0.01 (ws_j - ice_j / (denice * dz_j))
            zwt_in  = vwt_zwt_in  v VU.! c
            qcharge = vwt_qcharge v VU.! c
            wa_in   = 5000.0
            result = waterTable defaultSoilHydroParams
                       dtime qcharge zwt_in wa_in
                       watsat_c bsw_c sucsat_c eff_por
                       z_soil zi_soil dz_soil
                       liq_c ice_c tsno_c
            t_top_soil = six tsno_c nlevsno
            soilFrozen = t_top_soil <= tfrz
            k_frz_idx  = let go k | k >= nlevsoi = nlevsoi
                                  | six tsno_c (nlevsno + k - 1) > tfrz
                                    && six tsno_c (nlevsno + k) <= tfrz = k
                                  | otherwise = go (k + 1)
                         in if soilFrozen then 1 else go 1
            frost_table_val = if soilFrozen then six zi_soil k_frz_idx
                              else vwt_frost_carried v VU.! c
        in (wtr_zwt result, frost_table_val)
      results = map colCompute [0 .. vNumCols v - 1]
  in v { vsh_zwt         = VU.fromList [ z | (z, _) <- results ]
       , vsh_zwts        = VU.fromList [ z | (z, _) <- results ]
       , vsh_zwt_perched = VU.fromList [ f | (_, f) <- results ]
       , vsh_frost_table = VU.fromList [ f | (_, f) <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.surfaceHumidityStep': reuses the
-- 'surfaceHumidity' kernel per column. Top layer index = @nlevsno+snl@;
-- watsat/sucsat/bsw read on the soil grid at @topIdx-nlevsno@. @forc_pbot@/
-- @forc_q@ are global (ctx) with the scalar adapter's exact defaults. Writes the
-- ground specific-humidity diagnostics; layer structure unchanged.
surfaceHumidityStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
surfaceHumidityStepV _cfg ctx v =
  let nlev = vNlev v
      sIdx vec k = if k >= 0 && k < VU.length vec then vec VU.! k else 0.0
      forc_pbot = if VU.null (tcForcPbot ctx) then 101325.0 else tcForcPbot ctx VU.! 0
      forc_q    = if VU.null (tcForcQ    ctx) then 0.005    else tcForcQ    ctx VU.! 0
      perCol c =
        let snl    = vsnl v VU.! c
            topIdx = nlevsno + snl
            base   = c * nlev
            t_grnd = vt_grnd v VU.! c
            inp = SurfaceHumidityInput
              { shi_lunType        = 1
              , shi_colType        = 1
              , shi_snl            = snl
              , shi_dz_top         = sIdx (vcolDz v)      (base + topIdx)
              , shi_h2osoi_liq_top = sIdx (vh2osoi_liq v) (base + topIdx)
              , shi_h2osoi_ice_top = sIdx (vh2osoi_ice v) (base + topIdx)
              , shi_watsat_top     = if topIdx >= nlevsno
                                     then sIdx (vwatsat v) (c * nlevsoi + (topIdx - nlevsno))
                                     else 1.0
              , shi_smpmin         = -1.0e8
              , shi_sucsat_top     = if topIdx >= nlevsno
                                     then sIdx (vsucsat v) (c * nlevsoi + (topIdx - nlevsno))
                                     else 0.0
              , shi_bsw_top        = if topIdx >= nlevsno
                                     then sIdx (vbsw v) (c * nlevsoi + (topIdx - nlevsno))
                                     else 1.0
              , shi_frac_sno_eff   = vfrac_sno_eff v VU.! c
              , shi_frac_h2osfc    = vfrac_h2osfc v VU.! c
              , shi_t_soisno_top   = sIdx (vt_soisno v) (base + topIdx)
              , shi_t_soisno_snow  = if snl < 0
                                     then sIdx (vt_soisno v) (base + nlevsno + snl)
                                     else t_grnd
              , shi_t_grnd         = t_grnd
              , shi_t_h2osfc       = vt_h2osfc v VU.! c
              , shi_forc_pbot      = forc_pbot
              , shi_forc_q         = forc_q
              }
        in surfaceHumidity inp
      results = map perCol [0 .. vNumCols v - 1]
  in v { vqg        = VU.fromList [ shr_qg        r | r <- results ]
       , vqg_snow   = VU.fromList [ shr_qg_snow   r | r <- results ]
       , vqg_soil   = VU.fromList [ shr_qg_soil   r | r <- results ]
       , vqg_h2osfc = VU.fromList [ shr_qg_h2osfc r | r <- results ]
       , vdqgdT     = VU.fromList [ shr_dqgdT     r | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.soilHydrologyStep'. The
-- infiltration-excess + Richards solve is an inherently per-column implicit
-- tridiagonal solve, so this reuses the scalar adapter per column: rebuild a
-- single-column 'CLMState' from the SoA slices (every read field carried;
-- soil hydraulics resolved via clmColumn with empty sstate_*), run
-- 'soilHydrologyStep', re-flatten the soil water / runoff / qcharge / snl.
-- Bit-identical by construction. NOTE: snowpack meltout can flip @snl -> 0@,
-- which is why 'scatter' writes @clmSnl@.
soilHydrologyStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
soilHydrologyStepV cfg ctx v =
  let nlev   = vNlev v
      nlevZi = nlev + 1
      runCol c =
        let base   = c * nlev
            slc    = VU.slice base nlev
            slcS   = VU.slice (c * nlevsoi) nlevsoi
            st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_liq_col = slc (vh2osoi_liq v)
                  , h2osoi_ice_col = slc (vh2osoi_ice v)
                  , h2osno_col     = vh2osno v VU.! c
                  , h2osfc_col     = vh2osfc v VU.! c }
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz  = slc (vcolDz v)
                  , colZ   = slc (vcolZ  v)
                  , colZi  = VU.slice (c * nlevZi) nlevZi (vcolZi v)
                  , watsat = slcS (vwatsat v)
                  , bsw    = slcS (vbsw    v)
                  , hksat  = slcS (vhksat  v)
                  , sucsat = slcS (vsucsat v) }
              , clmSoilState = (clmSoilState defaultCLMState)
                  { sstate_watsat_col = VU.empty
                  , sstate_bsw_col    = VU.empty
                  , sstate_hksat_col  = VU.empty
                  , sstate_sucsat_col = VU.empty }
              , clmTemp = (clmTemp defaultCLMState)
                  { t_soisno_col = slc (vt_soisno v) }
              , clmWaterFlux = (clmWaterFlux defaultCLMState)
                  { qflx_evap_tot_patch = vqflx_evap_tot v VU.! c
                  , qflx_tran_veg_patch = vqflx_tran_veg v VU.! c }
              , clmSoilHydro = (clmSoilHydro defaultCLMState)
                  { sh_zwt_col = VU.singleton (vwt_zwt_in v VU.! c) }
              , clmP_e_ice           = vp_e_ice           v VU.! c
              , clmP_baseflow_scalar = vp_baseflow_scalar v VU.! c
              , clmP_fff             = vp_fff             v VU.! c
              , clmP_fmax            = vp_fmax            v VU.! c
              , clmP_n_baseflow      = vp_n_baseflow      v VU.! c
              , clmP_n_melt_coef     = vp_n_melt_coef     v VU.! c
              }
        in soilHydrologyStep cfg ctx st0
      results  = map runCol [0 .. vNumCols v - 1]
      scal0' u = if VU.null u then 0.0 else u VU.! 0
  in v { vh2osoi_liq = VU.concat   [ padTo nlev (h2osoi_liq_col (clmWaterState r)) | r <- results ]
       , vh2osoi_ice = VU.concat   [ padTo nlev (h2osoi_ice_col (clmWaterState r)) | r <- results ]
       , vh2osno     = VU.fromList [ h2osno_col (clmWaterState r) | r <- results ]
       , vh2osfc     = VU.fromList [ h2osfc_col (clmWaterState r) | r <- results ]
       , vqflx_drain = VU.fromList [ qflx_drain_col (clmWaterFlux r) | r <- results ]
       , vqflx_surf  = VU.fromList [ qflx_surf_col  (clmWaterFlux r) | r <- results ]
       , vwt_qcharge = VU.fromList [ scal0' (sh_qcharge_col (clmSoilHydro r)) | r <- results ]
       , vsnl        = VU.fromList [ clmSnl r | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.soilFluxesStep'. The ground/soil
-- flux partition is area-weighted over a ragged set of PFT patches, so this
-- reuses the scalar adapter per column: rebuild a single-column 'CLMState' from
-- the SoA slices (per-patch fields via the CSR helpers, per-column scalars/
-- fallbacks direct, per-layer via 'VU.slice'), run 'soilFluxesStep', re-flatten
-- the written energy/water fluxes. Bit-identical by construction under the CLM
-- per-patch length invariant (each populated per-patch field has length =
-- patch count = the column's 'vPatchOff' span). @snl@/layer count unchanged.
soilFluxesStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
soilFluxesStepV cfg ctx v =
  let nlev = vNlev v
      off  = vPatchOff v
      runCol c =
        let base = c * nlev
            slc  = VU.slice base nlev
            pD f = patchSliceD off (f v) c
            pI f = patchSliceI off (f v) c
            st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmTemp = (clmTemp defaultCLMState)
                  { t_grnd_col       = vt_grnd v VU.! c
                  , t_soisno_col     = slc (vt_soisno v)
                  , t_soisno_bef_col = slc (vt_soisno_bef v)
                  , t_h2osfc_col     = vt_h2osfc     v VU.! c
                  , t_h2osfc_bef_col = vt_h2osfc_bef v VU.! c }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_liq_col = slc (vh2osoi_liq v)
                  , h2osoi_ice_col = slc (vh2osoi_ice v) }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_eff_col = VU.singleton (vfrac_sno_eff v VU.! c)
                  , wdiag_frac_h2osfc_col  = VU.singleton (vfrac_h2osfc  v VU.! c) }
              , clmCanopyState = (clmCanopyState defaultCLMState)
                  { cstate_patch_wtgcell            = pD vp_wtgcell
                  , cstate_frac_veg_nosno_patch     = pI vp_frac_veg_nosno
                  , cstate_frac_veg_nosno_alb_patch = pI vp_frac_veg_nosno_alb }
              , clmEnergyFlux = (clmEnergyFlux defaultCLMState)
                  { eflx_sh_tot_patch    = veflx_sh_tot    v VU.! c
                  , eflx_sh_grnd_patch   = veflx_sh_grnd   v VU.! c
                  , sabg_patch           = vsabg   v VU.! c
                  , cgrnds_patch         = vcgrnds v VU.! c
                  , cgrndl_patch         = vcgrndl v VU.! c
                  , dlrad_patch          = vdlrad  v VU.! c
                  , ulrad_patch          = vulrad  v VU.! c
                  , eflx_lwrad_net_patch = veflx_lwrad_net v VU.! c
                  , eflx_sh_tot_patch_vec    = pD vp_eflx_sh_tot
                  , eflx_sh_grnd_patch_vec   = pD vp_eflx_sh_grnd
                  , eflx_lwrad_net_patch_vec = pD vp_eflx_lwrad_net
                  , sabg_patch_vec           = pD vp_sabg
                  , cgrnds_patch_vec         = pD vp_cgrnds
                  , cgrndl_patch_vec         = pD vp_cgrndl
                  , dlrad_patch_vec          = pD vp_dlrad
                  , ulrad_patch_vec          = pD vp_ulrad }
              , clmWaterFlux = (clmWaterFlux defaultCLMState)
                  { qflx_evap_tot_patch = vqflx_evap_tot  v VU.! c
                  , qflx_evap_grnd_col  = vqflx_evap_grnd v VU.! c
                  , qflx_tran_veg_patch = vqflx_tran_veg  v VU.! c
                  , qflx_evap_tot_patch_vec  = pD vp_qflx_evap_tot
                  , qflx_evap_grnd_patch_vec = pD vp_qflx_evap_grnd
                  , qflx_tran_veg_patch_vec  = pD vp_qflx_tran_veg }
              }
        in soilFluxesStep cfg ctx st0
      results = map runCol [0 .. vNumCols v - 1]
  in v { veflx_sh_tot     = VU.fromList [ eflx_sh_tot_patch    (clmEnergyFlux r) | r <- results ]
       , veflx_sh_grnd    = VU.fromList [ eflx_sh_grnd_patch   (clmEnergyFlux r) | r <- results ]
       , veflx_lh_tot     = VU.fromList [ eflx_lh_tot_patch    (clmEnergyFlux r) | r <- results ]
       , veflx_soil_grnd  = VU.fromList [ eflx_soil_grnd_col   (clmEnergyFlux r) | r <- results ]
       , veflx_lwrad_out  = VU.fromList [ eflx_lwrad_out_patch (clmEnergyFlux r) | r <- results ]
       , veflx_lwrad_net  = VU.fromList [ eflx_lwrad_net_patch (clmEnergyFlux r) | r <- results ]
       , vp_eflx_sh_tot    = VU.concat [ eflx_sh_tot_patch_vec    (clmEnergyFlux r) | r <- results ]
       , vp_eflx_sh_grnd   = VU.concat [ eflx_sh_grnd_patch_vec   (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lh_tot    = VU.concat [ eflx_lh_tot_patch_vec    (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lwrad_out = VU.concat [ eflx_lwrad_out_patch_vec (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lwrad_net = VU.concat [ eflx_lwrad_net_patch_vec (clmEnergyFlux r) | r <- results ]
       , vqflx_evap_tot   = VU.fromList [ qflx_evap_tot_patch (clmWaterFlux r) | r <- results ]
       , vqflx_evap_grnd  = VU.fromList [ qflx_evap_grnd_col  (clmWaterFlux r) | r <- results ]
       , vp_qflx_evap_tot  = VU.concat [ qflx_evap_tot_patch_vec  (clmWaterFlux r) | r <- results ]
       , vp_qflx_evap_grnd = VU.concat [ qflx_evap_grnd_patch_vec (clmWaterFlux r) | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.baregroundFluxesStep'. The
-- Monin-Obukhov surface-flux solve is per-column iterative and partitioned over
-- a ragged PFT-patch set, so this reuses the scalar adapter per column (rebuild
-- single-col CLMState from SoA slices → run → re-flatten). Per-patch fields use
-- the CSR layout. Bit-identical by construction; @snl@/layer count unchanged.
baregroundFluxesStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
baregroundFluxesStepV cfg ctx v =
  let nlev = vNlev v
      off  = vPatchOff v
      runCol c =
        let base = c * nlev
            slc  = VU.slice base nlev
            slcS = VU.slice (c * nlevsoi) nlevsoi
            pD f = patchSliceD off (f v) c
            pI f = patchSliceI off (f v) c
            st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmTemp = (clmTemp defaultCLMState)
                  { t_grnd_col   = vt_grnd   v VU.! c
                  , t_h2osfc_col = vt_h2osfc v VU.! c
                  , t_soisno_col = slc (vt_soisno v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_liq_col = slc (vh2osoi_liq v)
                  , h2osoi_ice_col = slc (vh2osoi_ice v) }
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz  = slc (vcolDz v)
                  , watsat = slcS (vwatsat v)
                  , zii    = vzii v VU.! c }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_qg_col        = VU.singleton (vqg        v VU.! c)
                  , wdiag_qg_snow_col   = VU.singleton (vqg_snow   v VU.! c)
                  , wdiag_qg_soil_col   = VU.singleton (vqg_soil   v VU.! c)
                  , wdiag_qg_h2osfc_col = VU.singleton (vqg_h2osfc v VU.! c)
                  , wdiag_dqgdT_col     = VU.singleton (vdqgdT     v VU.! c) }
              , clmSoilState = (clmSoilState defaultCLMState)
                  { sstate_soilbeta_col = VU.singleton (vsoilbeta v VU.! c) }
              , clmCanopyState = (clmCanopyState defaultCLMState)
                  { cstate_patch_wtgcell            = pD vp_wtgcell
                  , cstate_elai_patch               = pD vp_elai
                  , cstate_esai_patch               = pD vp_esai
                  , cstate_frac_veg_nosno_patch     = pI vp_frac_veg_nosno
                  , cstate_frac_veg_nosno_alb_patch = pI vp_frac_veg_nosno_alb }
              }
        in baregroundFluxesStep cfg ctx st0
      results = map runCol [0 .. vNumCols v - 1]
  in v { veflx_sh_tot     = VU.fromList [ eflx_sh_tot_patch    (clmEnergyFlux r) | r <- results ]
       , veflx_sh_grnd    = VU.fromList [ eflx_sh_grnd_patch   (clmEnergyFlux r) | r <- results ]
       , veflx_lh_tot     = VU.fromList [ eflx_lh_tot_patch    (clmEnergyFlux r) | r <- results ]
       , vcgrnds          = VU.fromList [ cgrnds_patch (clmEnergyFlux r) | r <- results ]
       , vcgrndl          = VU.fromList [ cgrndl_patch (clmEnergyFlux r) | r <- results ]
       , vcgrnd           = VU.fromList [ cgrnd_patch  (clmEnergyFlux r) | r <- results ]
       , vdlrad           = VU.fromList [ dlrad_patch  (clmEnergyFlux r) | r <- results ]
       , vulrad           = VU.fromList [ ulrad_patch  (clmEnergyFlux r) | r <- results ]
       , veflx_lwrad_out  = VU.fromList [ eflx_lwrad_out_patch (clmEnergyFlux r) | r <- results ]
       , veflx_lwrad_net  = VU.fromList [ eflx_lwrad_net_patch (clmEnergyFlux r) | r <- results ]
       , vt_ref2m         = VU.fromList [ t_ref2m_patch (clmTemp r) | r <- results ]
       , vqflx_evap_tot   = VU.fromList [ qflx_evap_tot_patch (clmWaterFlux r) | r <- results ]
       , vqflx_evap_grnd  = VU.fromList [ qflx_evap_grnd_col  (clmWaterFlux r) | r <- results ]
       , vqflx_tran_veg   = VU.fromList [ qflx_tran_veg_patch (clmWaterFlux r) | r <- results ]
       , vp_eflx_sh_tot    = VU.concat [ eflx_sh_tot_patch_vec    (clmEnergyFlux r) | r <- results ]
       , vp_eflx_sh_grnd   = VU.concat [ eflx_sh_grnd_patch_vec   (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lh_tot    = VU.concat [ eflx_lh_tot_patch_vec    (clmEnergyFlux r) | r <- results ]
       , vp_cgrnds         = VU.concat [ cgrnds_patch_vec (clmEnergyFlux r) | r <- results ]
       , vp_cgrndl         = VU.concat [ cgrndl_patch_vec (clmEnergyFlux r) | r <- results ]
       , vp_cgrnd          = VU.concat [ cgrnd_patch_vec  (clmEnergyFlux r) | r <- results ]
       , vp_dlrad          = VU.concat [ dlrad_patch_vec  (clmEnergyFlux r) | r <- results ]
       , vp_ulrad          = VU.concat [ ulrad_patch_vec  (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lwrad_out = VU.concat [ eflx_lwrad_out_patch_vec (clmEnergyFlux r) | r <- results ]
       , vp_eflx_lwrad_net = VU.concat [ eflx_lwrad_net_patch_vec (clmEnergyFlux r) | r <- results ]
       , vp_t_ref2m        = VU.concat [ t_ref2m_patch_vec (clmTemp r) | r <- results ]
       , vp_fvel_ram1      = VU.concat [ fvel_ram1_patch  (clmFrictionVel r) | r <- results ]
       , vp_fvel_ustar     = VU.concat [ fvel_ustar_patch (clmFrictionVel r) | r <- results ]
       , vp_qflx_evap_tot  = VU.concat [ qflx_evap_tot_patch_vec  (clmWaterFlux r) | r <- results ]
       , vp_qflx_evap_grnd = VU.concat [ qflx_evap_grnd_patch_vec (clmWaterFlux r) | r <- results ]
       , vp_qflx_tran_veg  = VU.concat [ qflx_tran_veg_patch_vec  (clmWaterFlux r) | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.soilTemperatureFullStep'. The
-- implicit tridiagonal heat solve (snow+soil+surface-water) is per-column, so
-- this reuses the scalar adapter per column: rebuild a single-col CLMState
-- carrying every read field — sstate thermal props (stride nlevgrnd), optional
-- thk/cv overrides (empty ⇒ Nothing), nbedrock, and the ragged per-patch flux
-- inputs via CSR slices — run 'soilTemperatureFullStep', re-flatten temps/water,
-- per-patch EFLX_GNET, and the carried snowmelt flux. Bit-identical; snl unchanged.
soilTemperatureFullStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
soilTemperatureFullStepV cfg ctx v =
  let nlev   = vNlev v
      nlevZi = nlev + 1
      off    = vPatchOff v
      runCol c =
        let base = c * nlev
            slc  = VU.slice base nlev
            pD f = patchSliceD off (f v) c
            pI f = patchSliceI off (f v) c
            sliceG flat = if VU.null flat then VU.empty else VU.slice (c * nlevgrnd) nlevgrnd flat
            sliceTot flat = if VU.null flat then VU.empty else slc flat
            st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmTemp = (clmTemp defaultCLMState)
                  { t_grnd_col   = vt_grnd   v VU.! c
                  , t_h2osfc_col = vt_h2osfc v VU.! c
                  , t_soisno_col = slc (vt_soisno v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_liq_col = slc (vh2osoi_liq v)
                  , h2osoi_ice_col = slc (vh2osoi_ice v)
                  , h2osno_col     = vh2osno v VU.! c
                  , h2osfc_col     = vh2osfc v VU.! c }
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz  = slc (vcolDz v)
                  , colZ   = slc (vcolZ  v)
                  , colZi  = VU.slice (c * nlevZi) nlevZi (vcolZi v)
                  , watsat = VU.slice (c * nlevgrnd) nlevgrnd (vwatsat_g v)
                  , bsw    = VU.slice (c * nlevgrnd) nlevgrnd (vbsw_g    v)
                  , sucsat = VU.slice (c * nlevgrnd) nlevgrnd (vsucsat_g v) }
              , clmSoilState = (clmSoilState defaultCLMState)
                  { sstate_watsat_col       = VU.empty
                  , sstate_bsw_col          = VU.empty
                  , sstate_sucsat_col       = VU.empty
                  , sstate_tkmg_col         = sliceG (vtkmg   v)
                  , sstate_tkdry_col        = sliceG (vtkdry  v)
                  , sstate_csol_col         = sliceG (vcsol   v)
                  , sstate_tksatu_col       = sliceG (vtksatu v)
                  , sstate_thk_override_col = sliceTot (vthk_override v)
                  , sstate_cv_override_col  = sliceTot (vcv_override  v) }
              , clmGridcell = (clmGridcell defaultCLMState)
                  { grc_nbedrock = VU.singleton (vgrc_nbedrock v VU.! c) }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_eff_col = VU.singleton (vfrac_sno_eff v VU.! c)
                  , wdiag_frac_h2osfc_col  = VU.singleton (vfrac_h2osfc  v VU.! c)
                  , wdiag_snow_depth_col   = VU.singleton (vsnow_depth   v VU.! c) }
              , clmCanopyState = (clmCanopyState defaultCLMState)
                  { cstate_patch_wtgcell            = pD vp_wtgcell
                  , cstate_frac_veg_nosno_patch     = pI vp_frac_veg_nosno
                  , cstate_frac_veg_nosno_alb_patch = pI vp_frac_veg_nosno_alb }
              , clmEnergyFlux = (clmEnergyFlux defaultCLMState)
                  { sabg_patch           = vsabg         v VU.! c
                  , eflx_sh_grnd_patch   = veflx_sh_grnd v VU.! c
                  , dlrad_patch          = vdlrad        v VU.! c
                  , cgrnds_patch         = vcgrnds       v VU.! c
                  , cgrndl_patch         = vcgrndl       v VU.! c
                  , cgrnd_patch          = vcgrnd        v VU.! c
                  , sabg_patch_vec           = pD vp_sabg
                  , eflx_sh_grnd_patch_vec   = pD vp_eflx_sh_grnd
                  , dlrad_patch_vec          = pD vp_dlrad
                  , cgrnds_patch_vec         = pD vp_cgrnds
                  , cgrndl_patch_vec         = pD vp_cgrndl
                  , cgrnd_patch_vec          = pD vp_cgrnd }
              , clmWaterFlux = (clmWaterFlux defaultCLMState)
                  { qflx_evap_grnd_col       = vqflx_evap_grnd v VU.! c
                  , qflx_evap_grnd_patch_vec = pD vp_qflx_evap_grnd }
              }
        in soilTemperatureFullStep cfg ctx st0
      results = map runCol [0 .. vNumCols v - 1]
  in v { vt_soisno     = VU.concat   [ padTo nlev (t_soisno_col (clmTemp r)) | r <- results ]
       , vt_grnd       = VU.fromList [ t_grnd_col   (clmTemp r) | r <- results ]
       , vt_h2osfc     = VU.fromList [ t_h2osfc_col (clmTemp r) | r <- results ]
       , vh2osoi_liq   = VU.concat   [ padTo nlev (h2osoi_liq_col (clmWaterState r)) | r <- results ]
       , vh2osoi_ice   = VU.concat   [ padTo nlev (h2osoi_ice_col (clmWaterState r)) | r <- results ]
       , vp_eflx_gnet  = VU.concat   [ eflx_gnet_patch_vec (clmEnergyFlux r) | r <- results ]
       , vqflx_snomelt = VU.fromList [ clmQflxSnomelt r | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.snowLayerCombineStep'. STRUCTURAL
-- (can reduce @snl@); per-column sequential top-packing, so reuse the scalar
-- adapter per column and re-flatten snl + snow geometry/state. Bit-identical.
snowLayerCombineStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
snowLayerCombineStepV cfg ctx v =
  let nlev   = vNlev v
      nlevZi = nlev + 1
      runCol c =
        let st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz = VU.slice (c * nlev)   nlev   (vcolDz v)
                  , colZ  = VU.slice (c * nlev)   nlev   (vcolZ  v)
                  , colZi = VU.slice (c * nlevZi) nlevZi (vcolZi v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_ice_col = VU.slice (c * nlev) nlev (vh2osoi_ice v)
                  , h2osoi_liq_col = VU.slice (c * nlev) nlev (vh2osoi_liq v)
                  , h2osno_col     = vh2osno v VU.! c }
              , clmTemp = (clmTemp defaultCLMState)
                  { t_soisno_col = VU.slice (c * nlev) nlev (vt_soisno v) }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_col     = VU.singleton (vfrac_sno     v VU.! c)
                  , wdiag_frac_sno_eff_col = VU.singleton (vfrac_sno_eff v VU.! c)
                  , wdiag_snow_depth_col   = VU.singleton (vsnow_depth   v VU.! c) }
              }
        in snowLayerCombineStep cfg ctx st0
      results  = map runCol [0 .. vNumCols v - 1]
      scal0' u = if VU.null u then 0.0 else u VU.! 0
  in v { vsnl          = VU.fromList [ clmSnl r                            | r <- results ]
       , vcolDz        = VU.concat   [ colDz (clmColumn r)                 | r <- results ]
       , vcolZ         = VU.concat   [ colZ  (clmColumn r)                 | r <- results ]
       , vcolZi        = VU.concat   [ colZi (clmColumn r)                 | r <- results ]
       , vt_soisno     = VU.concat   [ t_soisno_col   (clmTemp r)          | r <- results ]
       , vh2osoi_liq   = VU.concat   [ h2osoi_liq_col (clmWaterState r)     | r <- results ]
       , vh2osoi_ice   = VU.concat   [ h2osoi_ice_col (clmWaterState r)     | r <- results ]
       , vh2osno       = VU.fromList [ h2osno_col (clmWaterState r)         | r <- results ]
       , vsnow_depth   = VU.fromList [ scal0' (wdiag_snow_depth_col   (clmWaterDiagBulk r)) | r <- results ]
       , vfrac_sno     = VU.fromList [ scal0' (wdiag_frac_sno_col     (clmWaterDiagBulk r)) | r <- results ]
       , vfrac_sno_eff = VU.fromList [ scal0' (wdiag_frac_sno_eff_col (clmWaterDiagBulk r)) | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.snowLayerDivideStep'. STRUCTURAL
-- (can increase @snl@ by splitting thick layers); per-column, reuse the scalar
-- adapter per column and re-flatten snl + snow geometry/state. Bit-identical.
snowLayerDivideStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
snowLayerDivideStepV cfg ctx v =
  let nlev   = vNlev v
      nlevZi = nlev + 1
      runCol c =
        let st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmColumn = (clmColumn defaultCLMState)
                  { colDz = VU.slice (c * nlev)   nlev   (vcolDz v)
                  , colZ  = VU.slice (c * nlev)   nlev   (vcolZ  v)
                  , colZi = VU.slice (c * nlevZi) nlevZi (vcolZi v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_ice_col = VU.slice (c * nlev) nlev (vh2osoi_ice v)
                  , h2osoi_liq_col = VU.slice (c * nlev) nlev (vh2osoi_liq v) }
              , clmTemp = (clmTemp defaultCLMState)
                  { t_soisno_col = VU.slice (c * nlev) nlev (vt_soisno v) }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_col = VU.singleton (vfrac_sno v VU.! c) }
              }
        in snowLayerDivideStep cfg ctx st0
      results = map runCol [0 .. vNumCols v - 1]
  in v { vsnl        = VU.fromList [ clmSnl r                        | r <- results ]
       , vcolDz      = VU.concat  [ colDz (clmColumn r)             | r <- results ]
       , vcolZ       = VU.concat  [ colZ  (clmColumn r)             | r <- results ]
       , vcolZi      = VU.concat  [ colZi (clmColumn r)             | r <- results ]
       , vt_soisno   = VU.concat  [ t_soisno_col (clmTemp r)        | r <- results ]
       , vh2osoi_liq = VU.concat  [ h2osoi_liq_col (clmWaterState r) | r <- results ]
       , vh2osoi_ice = VU.concat  [ h2osoi_ice_col (clmWaterState r) | r <- results ]
       }

-- | Vectorized 'CLM.Driver.PhysicsAdapters.snowAgingStep' (grain metamorphism).
-- Takes the same 'SnicarOptics' arg as the scalar step; reuses it per column,
-- re-flattening the one write field (bulk top-layer grain radius). Passthrough
-- when aging tables are absent. Bit-identical; snl/layer structure unchanged.
snowAgingStepV :: SnicarOptics -> CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
snowAgingStepV snicarOpt cfg ctx v =
  let nlev = vNlev v
      runCol c =
        let st0 = defaultCLMState
              { clmSnl = vsnl v VU.! c
              , clmTemp = (clmTemp defaultCLMState)
                  { t_soisno_col = VU.slice (c * nlev) nlev (vt_soisno v) }
              , clmWaterState = (clmWaterState defaultCLMState)
                  { h2osoi_ice_col = VU.slice (c * nlev) nlev (vh2osoi_ice v)
                  , h2osoi_liq_col = VU.slice (c * nlev) nlev (vh2osoi_liq v)
                  , h2osno_col     = vh2osno v VU.! c }
              , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
                  { wdiag_frac_sno_col    = VU.singleton (vfrac_sno   v VU.! c)
                  , wdiag_snow_depth_col  = VU.singleton (vsnow_depth v VU.! c)
                  , wdiag_snw_rds_top_col = VU.singleton (vsnw_rds_top v VU.! c) }
              }
        in snowAgingStep snicarOpt cfg ctx st0
      results  = map runCol [0 .. vNumCols v - 1]
      scal0' u = if VU.null u then 0.0 else u VU.! 0
  in v { vsnw_rds_top = VU.fromList
           [ scal0' (wdiag_snw_rds_top_col (clmWaterDiagBulk r)) | r <- results ] }
