-- | Restart I/O data structures and pure helpers.
-- Ported from Julia: src/infrastructure/restart_io.jl
-- Data structures for restart variable registry; IO for NetCDF is placeholder.
module CLM.Infrastructure.RestartIO
  ( -- * Restart variable definition
    RestartVarDef(..)
  , RestartDims(..)
  , RestartLevel(..)
    -- * Registry
  , restartRegistry
    -- * Pure helpers
  , recomputeH2osoiVol
  , replaceFillWithNaN
  , replaceNaNWithFill
    -- * IO placeholders
  , writeRestart
  , readRestart
  , readFortranRestart
    -- * Binary restart
  , writeRestartBinary
  , readRestartBinary
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (denh2o, denice)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

-- | Number of dimensions for a restart variable.
data RestartDims = Dims1D | Dims2D
  deriving (Show, Eq)

-- | Subgrid level for a restart variable.
data RestartLevel = ColumnRestart | PatchRestart
  deriving (Show, Eq)

-- | Definition of a single restart variable.
data RestartVarDef = RestartVarDef
  { rvd_name  :: !String
  , rvd_dims  :: !RestartDims
  , rvd_level :: !RestartLevel
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Registry
-- ---------------------------------------------------------------------------

-- | List of state variables saved/restored in restart files.
restartRegistry :: [RestartVarDef]
restartRegistry =
  -- Temperature state
  [ RestartVarDef "T_SOISNO"           Dims2D ColumnRestart
  , RestartVarDef "T_GRND"             Dims1D ColumnRestart
  , RestartVarDef "T_LAKE"             Dims2D ColumnRestart
  , RestartVarDef "T_H2OSFC"           Dims1D ColumnRestart
  , RestartVarDef "T_VEG"              Dims1D PatchRestart
  , RestartVarDef "T_REF2M"            Dims1D PatchRestart
  , RestartVarDef "T_STEM"             Dims1D PatchRestart
  , RestartVarDef "T_VEG24"            Dims1D PatchRestart
  , RestartVarDef "T_VEG240"           Dims1D PatchRestart
  , RestartVarDef "T_A10"              Dims1D PatchRestart
  -- Water state
  , RestartVarDef "H2OSOI_LIQ"         Dims2D ColumnRestart
  , RestartVarDef "H2OSOI_ICE"         Dims2D ColumnRestart
  , RestartVarDef "H2OSOI_VOL"         Dims2D ColumnRestart
  , RestartVarDef "H2OSNO"             Dims1D ColumnRestart
  , RestartVarDef "H2OSFC"             Dims1D ColumnRestart
  , RestartVarDef "WA"                 Dims1D ColumnRestart
  -- Soil hydrology
  , RestartVarDef "ZWT"                Dims1D ColumnRestart
  , RestartVarDef "ZWT_PERCHED"        Dims1D ColumnRestart
  -- Snow state
  , RestartVarDef "SNL"                Dims1D ColumnRestart
  , RestartVarDef "SNOW_DEPTH"         Dims1D ColumnRestart
  , RestartVarDef "FRAC_SNO"           Dims1D ColumnRestart
  , RestartVarDef "FRAC_SNO_EFF"       Dims1D ColumnRestart
  , RestartVarDef "INT_SNOW"           Dims1D ColumnRestart
  , RestartVarDef "SNW_RDS"            Dims2D ColumnRestart
  -- Snow/soil geometry
  , RestartVarDef "COL_DZ"             Dims2D ColumnRestart
  , RestartVarDef "COL_Z"              Dims2D ColumnRestart
  , RestartVarDef "COL_ZI"             Dims2D ColumnRestart
  -- Lake state
  , RestartVarDef "LAKE_ICEFRAC"       Dims2D ColumnRestart
  , RestartVarDef "SAVEDTKE1"          Dims1D ColumnRestart
  -- Canopy state
  , RestartVarDef "TLAI"               Dims1D PatchRestart
  , RestartVarDef "ELAI"               Dims1D PatchRestart
  , RestartVarDef "HTOP"               Dims1D PatchRestart
  , RestartVarDef "ESAI"               Dims1D PatchRestart
  , RestartVarDef "FRAC_VEG_NOSNO_ALB" Dims1D PatchRestart
  , RestartVarDef "FSUN24"             Dims1D PatchRestart
  , RestartVarDef "FSUN240"            Dims1D PatchRestart
  , RestartVarDef "ELAI240"            Dims1D PatchRestart
  -- Plant hydraulic stress state
  , RestartVarDef "VEGWP"              Dims2D PatchRestart
  , RestartVarDef "VEGWP_LN"           Dims2D PatchRestart
  , RestartVarDef "VEGWP_PD"           Dims2D PatchRestart
  -- Soil hydraulic state
  , RestartVarDef "SMP_L"              Dims2D ColumnRestart
  , RestartVarDef "HK_L"               Dims2D ColumnRestart
  -- Aerosol mass in snow layers
  , RestartVarDef "MSS_BCPHI"          Dims2D ColumnRestart
  , RestartVarDef "MSS_BCPHO"          Dims2D ColumnRestart
  , RestartVarDef "MSS_OCPHI"          Dims2D ColumnRestart
  , RestartVarDef "MSS_OCPHO"          Dims2D ColumnRestart
  , RestartVarDef "MSS_DST1"           Dims2D ColumnRestart
  , RestartVarDef "MSS_DST2"           Dims2D ColumnRestart
  , RestartVarDef "MSS_DST3"           Dims2D ColumnRestart
  , RestartVarDef "MSS_DST4"           Dims2D ColumnRestart
  ]

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | Recompute volumetric soil water from liquid + ice.
-- h2osoi_vol = (liq/denh2o + ice/denice) / dz
-- All vectors are for a single column (nlevgrnd elements).
-- Offset indexing: liq/ice/dz use snow+soil combined index.
recomputeH2osoiVol
  :: Int                  -- ^ nlevsno
  -> Int                  -- ^ nlevgrnd
  -> VU.Vector Double     -- ^ h2osoi_liq (combined snow+soil)
  -> VU.Vector Double     -- ^ h2osoi_ice (combined snow+soil)
  -> VU.Vector Double     -- ^ dz (combined snow+soil)
  -> VU.Vector Double     -- ^ h2osoi_vol (soil-only, length nlevgrnd)
recomputeH2osoiVol nlevsno nlevgrnd liq ice dz =
  VU.generate nlevgrnd $ \j ->
    let jj = j + nlevsno
        dzj = dz VU.! jj
    in if dzj > 0.0
       then ((liq VU.! jj) / denh2o + (ice VU.! jj) / denice) / dzj
       else 0.0

-- | Replace fill values (-9999.0) with NaN.
replaceFillWithNaN :: VU.Vector Double -> VU.Vector Double
replaceFillWithNaN = VU.map (\x -> if x == (-9999.0) then 0.0 / 0.0 else x)

-- | Replace NaN values with fill value (-9999.0).
replaceNaNWithFill :: VU.Vector Double -> VU.Vector Double
replaceNaNWithFill = VU.map (\x -> if isNaN x then (-9999.0) else x)

-- ---------------------------------------------------------------------------
-- IO placeholders
-- ---------------------------------------------------------------------------

-- | Write restart file with essential state vectors.
writeRestart :: FilePath -> IO ()
writeRestart _filepath = return ()

-- | Read restart file.
readRestart :: FilePath -> IO ()
readRestart _filepath = return ()

-- | Read Fortran CLM restart file.
readFortranRestart :: FilePath -> IO ()
readFortranRestart _filepath = return ()

-- | Write binary restart: key state as line-delimited text (simple format).
writeRestartBinary :: FilePath -> Int -> Int
                   -> VU.Vector Double -> Double -> Double
                   -> VU.Vector Double -> VU.Vector Double
                   -> Double -> Double
                   -> VU.Vector Double
                   -> IO ()
writeRestartBinary path nstep snl tSoisno tGrnd tH2osfc
                   h2oLiq h2oIce h2osno h2osfc dz = do
  let header = unlines
        [ "RESTART_VERSION 1"
        , "NSTEP " ++ show nstep
        , "SNL " ++ show snl
        , "NLEVTOT " ++ show (VU.length tSoisno)
        , "T_GRND " ++ show tGrnd
        , "T_H2OSFC " ++ show tH2osfc
        , "H2OSNO " ++ show h2osno
        , "H2OSFC " ++ show h2osfc
        ]
      vecLine name v = name ++ " " ++ unwords (map show (VU.toList v))
      body = unlines
        [ vecLine "T_SOISNO" tSoisno
        , vecLine "H2OSOI_LIQ" h2oLiq
        , vecLine "H2OSOI_ICE" h2oIce
        , vecLine "COL_DZ" dz
        ]
  writeFile path (header ++ body)

-- | Read binary restart and return key state.
readRestartBinary :: FilePath -> IO (Int, Int, VU.Vector Double, Double, Double,
                                     VU.Vector Double, VU.Vector Double,
                                     Double, Double, VU.Vector Double)
readRestartBinary path = do
  content <- readFile path
  let ls = lines content
      getVal key = read (drop (length key + 1) (head (filter (\l -> take (length key) l == key) ls)))
      getVec key = let line = head (filter (\l -> take (length key) l == key) ls)
                       vals = drop 1 (words line)
                   in VU.fromList (map read vals)
      nstep = getVal "NSTEP" :: Int
      snl = getVal "SNL" :: Int
      tGrnd = getVal "T_GRND" :: Double
      tH2osfc = getVal "T_H2OSFC" :: Double
      h2osno = getVal "H2OSNO" :: Double
      h2osfc = getVal "H2OSFC" :: Double
      tSoisno = getVec "T_SOISNO"
      h2oLiq = getVec "H2OSOI_LIQ"
      h2oIce = getVec "H2OSOI_ICE"
      dz = getVec "COL_DZ"
  return (nstep, snl, tSoisno, tGrnd, tH2osfc, h2oLiq, h2oIce, h2osno, h2osfc, dz)
