-- | Model dimension parameters.
-- Ported from: src/main/clm_varpar.F90
-- Preserves Fortran variable names for traceability.
module CLM.Constants.VarPar
  ( -- * Fixed parameters (Fortran @parameter@)
    nlev_equalspace
  , toplev_equalspace
  , ngases
  , nlevcan
  , nvegwcs
  , numwat
  , numrad
  , ivis
  , inir
  , numsolar
  , ndst
  , dst_src_nbr
  , sz_nbr
  , mxpft
  , mxsowings
  , mxharvests
  , nrepr
  , nlayer
  , nvariants
    -- * Vegetation pool indices
  , nvegpool_natveg
  , nvegpool_crop
  , nveg_retransn
    -- * Leaf/organ pool indices
  , ileaf, ileaf_st, ileaf_xf
  , ifroot, ifroot_st, ifroot_xf
  , ilivestem, ilivestem_st, ilivestem_xf
  , ideadstem, ideadstem_st, ideadstem_xf
  , ilivecroot, ilivecroot_st, ilivecroot_xf
  , ideadcroot, ideadcroot_st, ideadcroot_xf
  , igrain, igrain_st, igrain_xf
    -- * Litter pool base index
  , i_litr1
    -- * Runtime-computed parameters
  , VarPar(..)
  , defaultVarPar
  , SoilLayerStruct(..)
  , varparInit
  ) where

-- | Fixed parameters (Fortran @parameter@ declarations)

-- | Number of equal-spaced levels
nlev_equalspace :: Int
nlev_equalspace = 15

-- | Top level for equal spacing
toplev_equalspace :: Int
toplev_equalspace = 6

-- | Number of gas species: CH4, O2, CO2
ngases :: Int
ngases = 3

-- | Number of canopy layers
nlevcan :: Int
nlevcan = 1

-- | Number of vegetation water conductance segments
nvegwcs :: Int
nvegwcs = 4

-- | Number of water types
numwat :: Int
numwat = 5

-- | Number of radiation bands (VIS, NIR)
numrad :: Int
numrad = 2

-- | Index for visible band
ivis :: Int
ivis = 1

-- | Index for near-infrared band
inir :: Int
inir = 2

-- | Number of solar type bands
numsolar :: Int
numsolar = 2

-- | Number of dust size classes
ndst :: Int
ndst = 4

-- | Number of source size distributions
dst_src_nbr :: Int
dst_src_nbr = 3

-- | Number of sub-bin bins
sz_nbr :: Int
sz_nbr = 200

-- | Maximum number of PFTs
mxpft :: Int
mxpft = 78

-- | Maximum number of crop sowings per year
mxsowings :: Int
mxsowings = 1

-- | Maximum number of crop harvests per year
mxharvests :: Int
mxharvests = mxsowings + 1

-- | Number of crop reproductive pools
nrepr :: Int
nrepr = 1

-- | Number of VIC layers
nlayer :: Int
nlayer = 3

-- | Number of soil moisture variants
nvariants :: Int
nvariants = 2

-- * Vegetation pool indices

nvegpool_natveg :: Int
nvegpool_natveg = 18

nvegpool_crop :: Int
nvegpool_crop = 3

nveg_retransn :: Int
nveg_retransn = 1

-- * Leaf/organ pool indices

ileaf :: Int
ileaf = 1

ileaf_st :: Int
ileaf_st = 2

ileaf_xf :: Int
ileaf_xf = 3

ifroot :: Int
ifroot = 4

ifroot_st :: Int
ifroot_st = 5

ifroot_xf :: Int
ifroot_xf = 6

ilivestem :: Int
ilivestem = 7

ilivestem_st :: Int
ilivestem_st = 8

ilivestem_xf :: Int
ilivestem_xf = 9

ideadstem :: Int
ideadstem = 10

ideadstem_st :: Int
ideadstem_st = 11

ideadstem_xf :: Int
ideadstem_xf = 12

ilivecroot :: Int
ilivecroot = 13

ilivecroot_st :: Int
ilivecroot_st = 14

ilivecroot_xf :: Int
ilivecroot_xf = 15

ideadcroot :: Int
ideadcroot = 16

ideadcroot_st :: Int
ideadcroot_st = 17

ideadcroot_xf :: Int
ideadcroot_xf = 18

igrain :: Int
igrain = 19

igrain_st :: Int
igrain_st = 20

igrain_xf :: Int
igrain_xf = 21

-- | Base litter index
i_litr1 :: Int
i_litr1 = 1

-- | Soil layer structure presets
data SoilLayerStruct
  = SL10_3_5m    -- ^ 10SL_3.5m
  | SL23_3_5m    -- ^ 23SL_3.5m
  | SL49_10m     -- ^ 49SL_10m
  | SL20_8_5m    -- ^ 20SL_8.5m (default)
  | SL4_2m       -- ^ 4SL_2m
  deriving (Show, Eq)

-- | Runtime-computed model parameters.
-- These depend on configuration and are set by 'varparInit'.
-- Mirrors variables computed in @clm_varpar_init@ in Fortran.
data VarPar = VarPar
  { vp_nlevsoi                       :: !Int  -- ^ Number of soil levels
  , vp_nlevsoifl                     :: !Int  -- ^ Number of soil levels (full)
  , vp_nlevgrnd                      :: !Int  -- ^ Number of ground levels (soil + bedrock)
  , vp_nlevurb                       :: !Int  -- ^ Number of urban levels
  , vp_nlevmaxurbgrnd                :: !Int  -- ^ Max urban ground levels
  , vp_nlevlak                       :: !Int  -- ^ Number of lake levels
  , vp_nlevdecomp                    :: !Int  -- ^ Number of decomposition levels
  , vp_nlevdecomp_full               :: !Int  -- ^ Full decomposition levels
  , vp_nlevsno                       :: !Int  -- ^ Number of snow levels
  , vp_nlayert                       :: !Int  -- ^ Total number of layers
  , vp_nvegcpool                     :: !Int  -- ^ Number of veg carbon pools
  , vp_nvegnpool                     :: !Int  -- ^ Number of veg nitrogen pools
  , vp_maxveg                        :: !Int  -- ^ Max vegetation types
  , vp_maxpatch_urb                  :: !Int  -- ^ Max urban patches
  , vp_maxsoil_patches               :: !Int  -- ^ Max soil patches
  , vp_maxpatch_glc                  :: !Int  -- ^ Max glacier patches
    -- Litter pool indices (runtime-dependent)
  , vp_i_litr2                       :: !Int
  , vp_i_litr3                       :: !Int
  , vp_i_litr_min                    :: !Int
  , vp_i_litr_max                    :: !Int
  , vp_i_met_lit                     :: !Int
  , vp_i_cop_mic                     :: !Int
  , vp_i_oli_mic                     :: !Int
  , vp_i_cwd                         :: !Int
  , vp_i_cwdl2                       :: !Int
    -- Decomposition pool counts
  , vp_ndecomp_pools_max             :: !Int
  , vp_ndecomp_pools                 :: !Int
  , vp_ndecomp_cascade_transitions   :: !Int
  , vp_ndecomp_cascade_outtransitions :: !Int
  , vp_ndecomp_pools_vr              :: !Int
    -- PFT/CFT ranges
  , vp_natpft_lb                     :: !Int
  , vp_natpft_ub                     :: !Int
  , vp_natpft_size                   :: !Int
  , vp_surfpft_lb                    :: !Int
  , vp_surfpft_ub                    :: !Int
  , vp_cft_lb                        :: !Int
  , vp_cft_ub                        :: !Int
  , vp_cft_size                      :: !Int
    -- Retranslocated N index
  , vp_iretransn                     :: !Int
  , vp_ioutc                         :: !Int
  , vp_ioutn                         :: !Int
  } deriving (Show, Eq)

-- | Default (uninitialized) VarPar values
defaultVarPar :: VarPar
defaultVarPar = VarPar
  { vp_nlevsoi                        = 0
  , vp_nlevsoifl                      = 0
  , vp_nlevgrnd                       = 0
  , vp_nlevurb                        = 0
  , vp_nlevmaxurbgrnd                 = 0
  , vp_nlevlak                        = 0
  , vp_nlevdecomp                     = 0
  , vp_nlevdecomp_full                = 0
  , vp_nlevsno                        = 12
  , vp_nlayert                        = 0
  , vp_nvegcpool                      = 0
  , vp_nvegnpool                      = 0
  , vp_maxveg                         = 0
  , vp_maxpatch_urb                   = 5
  , vp_maxsoil_patches                = 0
  , vp_maxpatch_glc                   = 0
  , vp_i_litr2                        = -9
  , vp_i_litr3                        = -9
  , vp_i_litr_min                     = -9
  , vp_i_litr_max                     = -9
  , vp_i_met_lit                      = -9
  , vp_i_cop_mic                      = -9
  , vp_i_oli_mic                      = -9
  , vp_i_cwd                          = -9
  , vp_i_cwdl2                        = -9
  , vp_ndecomp_pools_max              = 0
  , vp_ndecomp_pools                  = 0
  , vp_ndecomp_cascade_transitions    = 0
  , vp_ndecomp_cascade_outtransitions = 0
  , vp_ndecomp_pools_vr               = 0
  , vp_natpft_lb                      = 0
  , vp_natpft_ub                      = 0
  , vp_natpft_size                    = 0
  , vp_surfpft_lb                     = 0
  , vp_surfpft_ub                     = 0
  , vp_cft_lb                         = 0
  , vp_cft_ub                         = 0
  , vp_cft_size                       = 0
  , vp_iretransn                      = 0
  , vp_ioutc                          = 0
  , vp_ioutn                          = 0
  }

-- | Initialize runtime-dependent parameters (pure).
-- Ported from @clm_varpar_init()@ in @clm_varpar.F90@.
varparInit
  :: SoilLayerStruct  -- ^ Soil layer structure preset
  -> Bool             -- ^ use_extralakelayers
  -> Bool             -- ^ use_crop
  -> Int              -- ^ actual_maxsoil_patches
  -> Int              -- ^ surf_numpft
  -> Int              -- ^ surf_numcft
  -> Int              -- ^ actual_nlevurb
  -> VarPar
varparInit soilStruct useExtraLake useCrop maxSoilPat surfNumpft surfNumcft nlevurb =
  let (nlevsoi0, nlevgrnd0) = case soilStruct of
        SL10_3_5m -> (10, 15)
        SL23_3_5m -> (20, 25)
        SL49_10m  -> (49, 54)
        SL20_8_5m -> (20, 25)
        SL4_2m    -> (4,  4)
      nlevsoifl0     = nlevsoi0
      nlevmaxurbgrnd0 = max nlevurb nlevgrnd0
      nlevlak0       = if useExtraLake then 25 else 10
      nlevsno0       = 12
      nlevdecomp0    = 1
      nlevdecompF0   = nlevsoi0
      natpftLb       = 0
      natpftUb       = surfNumpft
      natpftSz       = natpftUb - natpftLb + 1
      (cftLb, cftUb, cftSz) =
        if useCrop
          then let lb = natpftUb + 1
                   ub = lb + surfNumcft - 1
               in (lb, ub, ub - lb + 1)
          else let lb = natpftUb + 1
               in (lb, lb, 0)
      surfpftLb      = natpftLb
      surfpftUb      = max natpftUb cftUb
      maxveg0        = surfpftUb
      (nvegc, nvegn) =
        if useCrop
          then (nvegpool_natveg + nvegpool_crop, nvegpool_natveg + nvegpool_crop)
          else (nvegpool_natveg, nvegpool_natveg)
      iretransn0     = nvegn + nveg_retransn
      ioutc0         = nvegc + 1
      ioutn0         = nvegn + nveg_retransn + 1
      nlayert0       = nlevsoi0 + nlevlak0 + nlayer
  in defaultVarPar
       { vp_nlevsoi          = nlevsoi0
       , vp_nlevsoifl         = nlevsoifl0
       , vp_nlevgrnd          = nlevgrnd0
       , vp_nlevurb           = nlevurb
       , vp_nlevmaxurbgrnd    = nlevmaxurbgrnd0
       , vp_nlevlak           = nlevlak0
       , vp_nlevdecomp        = nlevdecomp0
       , vp_nlevdecomp_full   = nlevdecompF0
       , vp_nlevsno           = nlevsno0
       , vp_nlayert           = nlayert0
       , vp_nvegcpool         = nvegc
       , vp_nvegnpool         = nvegn
       , vp_maxveg            = maxveg0
       , vp_maxsoil_patches   = maxSoilPat
       , vp_maxpatch_glc      = 10
       , vp_natpft_lb         = natpftLb
       , vp_natpft_ub         = natpftUb
       , vp_natpft_size       = natpftSz
       , vp_surfpft_lb        = surfpftLb
       , vp_surfpft_ub        = surfpftUb
       , vp_cft_lb            = cftLb
       , vp_cft_ub            = cftUb
       , vp_cft_size          = cftSz
       , vp_iretransn         = iretransn0
       , vp_ioutc             = ioutc0
       , vp_ioutn             = ioutn0
       }
