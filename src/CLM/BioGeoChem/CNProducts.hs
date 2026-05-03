{-# LANGUAGE BangPatterns #-}
-- | Wood and crop product pools.
-- Fortran: CNProductsMod.F90
-- Julia:   src/biogeochem/cn_products_mod.jl
--
-- Calculate loss fluxes from product pools and update state variables.
-- Pools: 1-year crop, 10-year wood, 100-year wood.
module CLM.BioGeoChem.CNProducts
  ( -- * Data types
    CNProductsState(..)
  , CNProductsFluxes(..)
    -- * Decay constants
  , kprod1
  , kprod10
  , kprod100
    -- * Functions
  , ProductUpdateInput(..)
  , ProductUpdateOutput(..)
  , productPoolUpdate
  , ProductSummary(..)
  , productSummary
  ) where

-- =========================================================================
-- Decay constants
-- =========================================================================

-- | Decay constant for 1-year crop product pool (1/s).
kprod1 :: Double
kprod1 = 7.2e-8

-- | Decay constant for 10-year wood product pool (1/s).
kprod10 :: Double
kprod10 = 7.2e-9

-- | Decay constant for 100-year wood product pool (1/s).
kprod100 :: Double
kprod100 = 7.2e-10

-- =========================================================================
-- State and flux types
-- =========================================================================

data CNProductsState = CNProductsState
  { cps_cropprod1  :: !Double  -- ^ (g/m2) crop product pool, 1-year lifespan
  , cps_prod10     :: !Double  -- ^ (g/m2) wood product pool, 10-year lifespan
  , cps_prod100    :: !Double  -- ^ (g/m2) wood product pool, 100-year lifespan
  } deriving (Show)

data CNProductsFluxes = CNProductsFluxes
  { cpf_dwt_cropprod1_gain  :: !Double  -- ^ gain from dynamic landcover [g/m2/s]
  , cpf_dwt_prod10_gain     :: !Double
  , cpf_dwt_prod100_gain    :: !Double
  , cpf_gru_prod10_gain     :: !Double  -- ^ gain from gross unrepresented [g/m2/s]
  , cpf_gru_prod100_gain    :: !Double
  , cpf_crop_harvest_gain   :: !Double  -- ^ gain from crop harvest [g/m2/s]
  , cpf_hrv_prod10_gain     :: !Double  -- ^ gain from wood harvest [g/m2/s]
  , cpf_hrv_prod100_gain    :: !Double
  } deriving (Show)

-- =========================================================================
-- Product pool update (single gridcell)
-- =========================================================================

data ProductUpdateInput = ProductUpdateInput
  { pui_state  :: !CNProductsState
  , pui_fluxes :: !CNProductsFluxes
  , pui_dt     :: !Double
  } deriving (Show)

data ProductUpdateOutput = ProductUpdateOutput
  { puo_state          :: !CNProductsState
  , puo_cropprod1_loss :: !Double  -- ^ (g/m2/s)
  , puo_prod10_loss    :: !Double
  , puo_prod100_loss   :: !Double
  } deriving (Show)

-- | Update product pools: compute losses, apply gains and losses.
productPoolUpdate :: ProductUpdateInput -> ProductUpdateOutput
productPoolUpdate inp =
  let !st = pui_state inp
      !fl = pui_fluxes inp
      !dt = pui_dt inp
      -- Loss fluxes
      !cl1   = cps_cropprod1 st * kprod1
      !cl10  = cps_prod10 st * kprod10
      !cl100 = cps_prod100 st * kprod100
      -- Updated states
      !cp1' = cps_cropprod1 st
            + cpf_dwt_cropprod1_gain fl * dt
            + cpf_crop_harvest_gain fl * dt
            - cl1 * dt
      !p10' = cps_prod10 st
            + cpf_dwt_prod10_gain fl * dt
            + cpf_gru_prod10_gain fl * dt
            + cpf_hrv_prod10_gain fl * dt
            - cl10 * dt
      !p100' = cps_prod100 st
             + cpf_dwt_prod100_gain fl * dt
             + cpf_gru_prod100_gain fl * dt
             + cpf_hrv_prod100_gain fl * dt
             - cl100 * dt
  in ProductUpdateOutput
    { puo_state = CNProductsState
        { cps_cropprod1 = cp1', cps_prod10 = p10', cps_prod100 = p100' }
    , puo_cropprod1_loss = cl1
    , puo_prod10_loss = cl10
    , puo_prod100_loss = cl100
    }

-- =========================================================================
-- Summary
-- =========================================================================

data ProductSummary = ProductSummary
  { ps_tot_woodprod      :: !Double  -- ^ total wood product pool [g/m2]
  , ps_tot_woodprod_loss :: !Double  -- ^ total wood product loss [g/m2/s]
  , ps_product_loss      :: !Double  -- ^ total product loss (all pools) [g/m2/s]
  } deriving (Show)

-- | Compute summary variables from product state and losses.
productSummary :: CNProductsState -> Double -> Double -> Double -> ProductSummary
productSummary st cl1 cl10 cl100 = ProductSummary
  { ps_tot_woodprod = cps_prod10 st + cps_prod100 st
  , ps_tot_woodprod_loss = cl10 + cl100
  , ps_product_loss = cl1 + cl10 + cl100
  }
