-- | Carbon and nitrogen product pool data structures.
-- Fortran: CNProductsMod.F90 — wood products, crop products at gridcell level.
module CLM.Types.CNProductsData
  ( CNProductsData(..)
  , defaultCNProductsData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN product pools (SoA layout).
-- Preserves Fortran variable names with @cnprod_@ prefix.
data CNProductsData = CNProductsData
  { cnprod_cropprod1_grc    :: !(VU.Vector Double)  -- ^ (g/m2) crop product pool (1-year)
  , cnprod_tot_woodprod_grc :: !(VU.Vector Double)  -- ^ (g/m2) total wood product pools
  , cnprod_product_loss_grc :: !(VU.Vector Double)  -- ^ (g/m2/s) losses from wood & crop products
  } deriving (Show)

defaultCNProductsData :: CNProductsData
defaultCNProductsData = CNProductsData
  { cnprod_cropprod1_grc = VU.empty
  , cnprod_tot_woodprod_grc = VU.empty
  , cnprod_product_loss_grc = VU.empty
  }
