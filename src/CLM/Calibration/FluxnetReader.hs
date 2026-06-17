{-# LANGUAGE BangPatterns #-}
-- | FluxNET forcing and observation reader for CLM calibration.
--
-- Maps half-hourly FluxNET CSV data to CLM atmospheric forcing fields
-- and calibration targets (GPP, NEE, latent heat).
--
-- FluxNET variable naming convention:
--   TA_F    — Air temperature [°C]
--   PA_F    — Air pressure [kPa]
--   WS_F    — Wind speed [m/s]
--   LW_IN_F — Incoming longwave [W/m2]
--   SW_IN_F — Incoming shortwave [W/m2]
--   P_F     — Precipitation [mm/half-hour]
--   VPD_F   — Vapor pressure deficit [hPa]
--   GPP_NT_VUT_REF — GPP (nighttime partitioning) [umolCO2/m2/s]
--   NEE_VUT_REF    — NEE [umolCO2/m2/s]
--   LE_F_MDS       — Latent heat [W/m2]
module CLM.Calibration.FluxnetReader
  ( -- * Site info
    FluxnetSiteInfo(..)
    -- * Parsed data
  , FluxnetForcing(..)
  , FluxnetTarget(..)
  , FluxnetTimestep(..)
    -- * Reading
  , parseFluxnetCSV
  , parseFluxnetLine
    -- * Unit conversion
  , forcingToCLMUnits
  , gppUmolToGC
  , neeUmolToGC
    -- * Synthetic data for testing
  , generateSyntheticFluxnet
  ) where

-- | Site metadata.
data FluxnetSiteInfo = FluxnetSiteInfo
  { fsi_site_id :: !String
  , fsi_lat     :: !Double
  , fsi_lon     :: !Double
  , fsi_elev    :: !Double
  , fsi_pft     :: !Int
  } deriving (Show)

-- | Parsed forcing for one timestep (CLM-compatible units).
data FluxnetForcing = FluxnetForcing
  { ff_ta       :: !Double  -- ^ Air temperature [K]
  , ff_pa       :: !Double  -- ^ Air pressure [Pa]
  , ff_ws       :: !Double  -- ^ Wind speed [m/s]
  , ff_lw_in    :: !Double  -- ^ Incoming longwave [W/m2]
  , ff_sw_in    :: !Double  -- ^ Incoming shortwave [W/m2]
  , ff_precip   :: !Double  -- ^ Precipitation [mm/s]
  , ff_vpd      :: !Double  -- ^ Vapor pressure deficit [Pa]
  , ff_co2      :: !Double  -- ^ CO2 [ppm]
  , ff_rh       :: !Double  -- ^ Relative humidity [0-1]
  } deriving (Show)

-- | Observation targets for calibration.
data FluxnetTarget = FluxnetTarget
  { ft_gpp      :: !Double  -- ^ GPP [gC/m2/s] (NaN if missing)
  , ft_nee      :: !Double  -- ^ NEE [gC/m2/s]
  , ft_le       :: !Double  -- ^ Latent heat [W/m2]
  , ft_sh       :: !Double  -- ^ Sensible heat [W/m2]
  } deriving (Show)

-- | Combined forcing + target for one timestep.
data FluxnetTimestep = FluxnetTimestep
  { fts_forcing :: !FluxnetForcing
  , fts_target  :: !FluxnetTarget
  } deriving (Show)

-- | Convert umol CO2/m2/s to gC/m2/s.
gppUmolToGC :: Double -> Double
gppUmolToGC umol = umol * 12.011e-6

-- | Convert umol CO2/m2/s to gC/m2/s (NEE sign convention: positive = source).
neeUmolToGC :: Double -> Double
neeUmolToGC = gppUmolToGC

-- | Convert FluxNET raw values to CLM units.
forcingToCLMUnits :: Double -> Double -> Double -> Double -> Double -> Double -> Double
                  -> FluxnetForcing
forcingToCLMUnits ta_c pa_kpa ws lw_in sw_in precip_mm_hh vpd_hpa =
  FluxnetForcing
    { ff_ta = ta_c + 273.15
    , ff_pa = pa_kpa * 1000.0
    , ff_ws = max 0.1 ws
    , ff_lw_in = max 0.0 lw_in
    , ff_sw_in = max 0.0 sw_in
    , ff_precip = precip_mm_hh / 1800.0
    , ff_vpd = vpd_hpa * 100.0
    , ff_co2 = 400.0
    , ff_rh = 0.7
    }

-- | Parse a single CSV line (comma-separated, FluxNET FULLSET format).
-- Expects columns in standard FluxNET order. Returns Nothing for header/bad lines.
parseFluxnetLine :: String -> Maybe FluxnetTimestep
parseFluxnetLine line =
  let fields = splitOn ',' line
  in if length fields < 20 || head fields == "TIMESTAMP_START"
     then Nothing
     else case mapM readMaybe (take 20 fields) of
       Nothing -> Nothing
       Just vals ->
         let ta = vals !! 4
             sw_in = vals !! 8
             lw_in = vals !! 10
             vpd = vals !! 12
             pa = vals !! 14
             precip = vals !! 16
             ws = vals !! 6
             gpp = vals !! 18
             nee = vals !! 2
             le = if length vals > 19 then vals !! 19 else 0.0 / 0.0
         in if ta < -9990.0
            then Nothing
            else Just FluxnetTimestep
              { fts_forcing = forcingToCLMUnits ta pa ws lw_in sw_in precip vpd
              , fts_target = FluxnetTarget
                  { ft_gpp = if gpp > -9990.0 then gppUmolToGC gpp else 0.0 / 0.0
                  , ft_nee = if nee > -9990.0 then neeUmolToGC nee else 0.0 / 0.0
                  , ft_le  = if le > -9990.0 then le else 0.0 / 0.0
                  , ft_sh  = 0.0 / 0.0
                  }
              }

-- | Parse a full FluxNET CSV file.
parseFluxnetCSV :: String -> [FluxnetTimestep]
parseFluxnetCSV contents =
  let ls = lines contents
  in concatMap (\l -> case parseFluxnetLine l of
                        Just ts -> [ts]
                        Nothing -> []) ls

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim s =
  let (w, rest) = break (== delim) s
  in w : case rest of
           [] -> []
           (_:rs) -> splitOn delim rs

-- | Safe read for Double.
readMaybe :: String -> Maybe Double
readMaybe s = case reads s of
  [(v, "")] -> Just v
  _         -> Nothing

-- =========================================================================
-- Synthetic FluxNET data for testing
-- =========================================================================

-- | Generate synthetic FluxNET-like data for testing calibration.
-- Produces N timesteps with diurnal cycles in temperature and radiation.
generateSyntheticFluxnet :: Int     -- ^ number of timesteps (half-hourly)
                         -> Double  -- ^ mean temperature [K]
                         -> Double  -- ^ max PAR [W/m2]
                         -> [FluxnetTimestep]
generateSyntheticFluxnet nsteps meanT maxPar =
  map mkTimestep [0 .. nsteps - 1]
  where
    mkTimestep i =
      let !hour = fromIntegral (i `mod` 48) / 2.0
          !dayFrac = sin (pi * max 0.0 (hour - 6.0) / 12.0)
          !sw_in = if hour > 6.0 && hour < 18.0 then maxPar * dayFrac else 0.0
          !t_air = meanT + 5.0 * sin (2.0 * pi * (hour - 6.0) / 24.0)
          !lw_in = 0.96 * 5.67e-8 * t_air ** 4  -- approximate clear-sky LW
          -- Synthetic GPP: light-use efficiency model
          !gpp_umol = if sw_in > 1.0 then sw_in * 0.02 * min 1.0 (3.0 / 4.0) else 0.0
          !nee_umol = -(gpp_umol - 2.0)  -- respiration ~ 2 umol/m2/s
      in FluxnetTimestep
         { fts_forcing = FluxnetForcing
             { ff_ta = t_air
             , ff_pa = 101325.0
             , ff_ws = 3.0
             , ff_lw_in = lw_in
             , ff_sw_in = sw_in
             , ff_precip = 0.0
             , ff_vpd = 1000.0
             , ff_co2 = 400.0
             , ff_rh = 0.7
             }
         , fts_target = FluxnetTarget
             { ft_gpp = gppUmolToGC gpp_umol
             , ft_nee = neeUmolToGC nee_umol
             , ft_le = 0.0 / 0.0
             , ft_sh = 0.0 / 0.0
             }
         }
