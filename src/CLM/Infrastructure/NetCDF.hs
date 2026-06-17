{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
-- | Minimal NetCDF reader via FFI to libnetcdf.
--
-- Provides just enough to read CLM forcing and surfdata files:
--   - Open/close files
--   - Read 1D and 2D Double arrays by variable name
--   - Read dimension lengths
--   - Read scalar attributes
module CLM.Infrastructure.NetCDF
  ( -- * File operations
    NcFile
  , ncOpen
  , ncClose
  , withNcFile
    -- * Reading
  , ncReadDouble1D
  , ncReadDouble2D
  , ncReadDoubleScalar
  , ncDimLen
  , ncHasVar
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.Vector.Unboxed as VU
import Control.Exception (bracket)

-- =========================================================================
-- FFI bindings to libnetcdf
-- =========================================================================

foreign import ccall "nc_open"
  c_nc_open :: CString -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "nc_close"
  c_nc_close :: CInt -> IO CInt

foreign import ccall "nc_inq_varid"
  c_nc_inq_varid :: CInt -> CString -> Ptr CInt -> IO CInt

foreign import ccall "nc_inq_dimid"
  c_nc_inq_dimid :: CInt -> CString -> Ptr CInt -> IO CInt

foreign import ccall "nc_inq_dimlen"
  c_nc_inq_dimlen :: CInt -> CInt -> Ptr CSize -> IO CInt

foreign import ccall "nc_get_var_double"
  c_nc_get_var_double :: CInt -> CInt -> Ptr CDouble -> IO CInt

foreign import ccall "nc_inq_varndims"
  c_nc_inq_varndims :: CInt -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "nc_inq_vardimid"
  c_nc_inq_vardimid :: CInt -> CInt -> Ptr CInt -> IO CInt

-- NC_NOWRITE = 0
ncNoWrite :: CInt
ncNoWrite = 0

-- =========================================================================
-- Haskell API
-- =========================================================================

newtype NcFile = NcFile CInt

-- | Open a NetCDF file for reading.
ncOpen :: FilePath -> IO (Either String NcFile)
ncOpen path = withCString path $ \cpath ->
  alloca $ \ncidPtr -> do
    status <- c_nc_open cpath ncNoWrite ncidPtr
    if status /= 0
      then return (Left $ "nc_open failed for " ++ path ++ " (status=" ++ show status ++ ")")
      else do
        ncid <- peek ncidPtr
        return (Right (NcFile ncid))

-- | Close a NetCDF file.
ncClose :: NcFile -> IO ()
ncClose (NcFile ncid) = do
  _ <- c_nc_close ncid
  return ()

-- | Bracket pattern for safe file handling.
withNcFile :: FilePath -> (NcFile -> IO a) -> IO (Either String a)
withNcFile path action = do
  result <- ncOpen path
  case result of
    Left err -> return (Left err)
    Right nc -> do
      val <- bracket (return nc) ncClose action
      return (Right val)

-- | Check if a variable exists in the file.
ncHasVar :: NcFile -> String -> IO Bool
ncHasVar (NcFile ncid) varname = withCString varname $ \cname ->
  alloca $ \vidPtr -> do
    status <- c_nc_inq_varid ncid cname vidPtr
    return (status == 0)

-- | Get length of a named dimension.
ncDimLen :: NcFile -> String -> IO (Either String Int)
ncDimLen (NcFile ncid) dimname = withCString dimname $ \cname ->
  alloca $ \didPtr ->
    alloca $ \lenPtr -> do
      status1 <- c_nc_inq_dimid ncid cname didPtr
      if status1 /= 0
        then return (Left $ "dimension not found: " ++ dimname)
        else do
          did <- peek didPtr
          status2 <- c_nc_inq_dimlen ncid did lenPtr
          if status2 /= 0
            then return (Left $ "nc_inq_dimlen failed for " ++ dimname)
            else do
              len <- peek lenPtr
              return (Right (fromIntegral len))

-- | Read an entire variable as a flat Double vector.
ncReadDouble1D :: NcFile -> String -> IO (Either String (VU.Vector Double))
ncReadDouble1D (NcFile ncid) varname = withCString varname $ \cname ->
  alloca $ \vidPtr -> do
    status <- c_nc_inq_varid ncid cname vidPtr
    if status /= 0
      then return (Left $ "variable not found: " ++ varname)
      else do
        vid <- peek vidPtr
        -- Query total number of elements via ndims + dim sizes
        let querySize = do
              -- Get ndims
              ndPtr <- malloc :: IO (Ptr CInt)
              _ <- c_nc_inq_varndims ncid vid ndPtr
              nd <- peek ndPtr
              free ndPtr
              if nd <= 0 then return 1
              else do
                dimidsPtr <- mallocArray (fromIntegral nd) :: IO (Ptr CInt)
                _ <- c_nc_inq_vardimid ncid vid dimidsPtr
                dimids <- peekArray (fromIntegral nd) dimidsPtr
                free dimidsPtr
                sizes <- mapM (\did -> alloca $ \lenP -> do
                  _ <- c_nc_inq_dimlen ncid did lenP
                  fromIntegral <$> peek lenP) dimids
                return (product sizes)
        totalElems <- querySize
        buf <- mallocArray totalElems :: IO (Ptr CDouble)
        status2 <- c_nc_get_var_double ncid vid buf
        if status2 /= 0
          then do
            free buf
            return (Left $ "nc_get_var_double failed for " ++ varname)
          else do
            vals <- peekArray totalElems buf
            free buf
            let vec = VU.fromList (map realToFrac vals)
            return (Right vec)

-- | Read a variable and reshape as 2D (time x space).
-- Returns the raw flat vector; caller reshapes.
ncReadDouble2D :: NcFile -> String -> IO (Either String (VU.Vector Double))
ncReadDouble2D = ncReadDouble1D  -- same underlying read; caller interprets shape

-- | Read a scalar variable (first element).
ncReadDoubleScalar :: NcFile -> String -> IO (Either String Double)
ncReadDoubleScalar nc varname = do
  result <- ncReadDouble1D nc varname
  case result of
    Left err -> return (Left err)
    Right vec
      | VU.null vec -> return (Left $ "empty variable: " ++ varname)
      | otherwise -> return (Right (vec VU.! 0))
