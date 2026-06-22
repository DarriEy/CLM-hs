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
    -- * Writing
  , ncWriteTimeseries
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.Vector.Unboxed as VU
import Control.Exception (bracket)
import Control.Monad (foldM)

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

foreign import ccall "nc_create"
  c_nc_create :: CString -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "nc_def_dim"
  c_nc_def_dim :: CInt -> CString -> CSize -> Ptr CInt -> IO CInt

foreign import ccall "nc_def_var"
  c_nc_def_var :: CInt -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "nc_put_att_text"
  c_nc_put_att_text :: CInt -> CInt -> CString -> CSize -> CString -> IO CInt

foreign import ccall "nc_enddef"
  c_nc_enddef :: CInt -> IO CInt

foreign import ccall "nc_put_var_double"
  c_nc_put_var_double :: CInt -> CInt -> Ptr CDouble -> IO CInt

-- NC_NOWRITE = 0
ncNoWrite :: CInt
ncNoWrite = 0

-- NC_CLOBBER = 0 (overwrite), NC_DOUBLE = 6, NC_GLOBAL = -1
ncClobber :: CInt
ncClobber = 0

ncDouble :: CInt
ncDouble = 6

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

-- =========================================================================
-- Writing
-- =========================================================================

-- | Write a NetCDF file holding a set of time-series variables, each
-- dimensioned @(time)@ with length @n@ and a @long_name@ attribute. Overwrites
-- any existing file. Each variable's data vector must have length @n@.
--
-- This is a minimal CF-ish history writer for the single-column port: one
-- "time" dimension, scalar-per-step variables. Returns 'Left' on any NetCDF
-- error (file creation, definition, or write).
ncWriteTimeseries
  :: FilePath
  -> Int                                   -- ^ time length (n)
  -> [(String, String, VU.Vector Double)]  -- ^ (var name, long_name, data[n])
  -> IO (Either String ())
ncWriteTimeseries path n vars =
  withCString path $ \cpath ->
  alloca $ \ncidP -> do
    st0 <- c_nc_create cpath ncClobber ncidP
    if st0 /= 0 then return (Left ("nc_create failed: " ++ show st0)) else do
      ncid <- peek ncidP
      -- define the time dimension
      eDim <- withCString "time" $ \cdim -> alloca $ \didP -> do
        s <- c_nc_def_dim ncid cdim (fromIntegral n) didP
        if s /= 0 then return (Left ("nc_def_dim failed: " ++ show s))
                  else Right <$> peek didP
      case eDim of
        Left e -> c_nc_close ncid >> return (Left e)
        Right timeDim -> do
          -- define each variable + its long_name attribute
          eVars <- foldM (defOne ncid timeDim) (Right []) vars
          case eVars of
            Left e -> c_nc_close ncid >> return (Left e)
            Right varidsRev -> do
              sEnd <- c_nc_enddef ncid
              if sEnd /= 0 then c_nc_close ncid >> return (Left ("nc_enddef failed: " ++ show sEnd))
              else do
                -- varidsRev is in reverse definition order; pair with data in
                -- original variable order.
                let pairs = zip (reverse varidsRev) (map (\(_, _, d) -> d) vars)
                ePut <- foldM (putOne ncid) (Right ()) pairs
                _ <- c_nc_close ncid
                return ePut
  where
    defOne _ _ (Left e) _ = return (Left e)
    defOne ncid timeDim (Right acc) (name, longName, dat)
      | VU.length dat /= n =
          return (Left ("variable " ++ name ++ " has length "
                        ++ show (VU.length dat) ++ ", expected " ++ show n))
      | otherwise =
          withCString name $ \cname ->
          alloca $ \dimsP -> alloca $ \vidP -> do
            poke dimsP timeDim
            s <- c_nc_def_var ncid cname ncDouble 1 dimsP vidP
            if s /= 0 then return (Left ("nc_def_var failed for " ++ name ++ ": " ++ show s))
            else do
              vid <- peek vidP
              sa <- withCString "long_name" $ \catt ->
                    withCStringLen longName $ \(cval, vlen) ->
                      c_nc_put_att_text ncid vid catt (fromIntegral vlen) cval
              if sa /= 0 then return (Left ("nc_put_att_text failed for " ++ name ++ ": " ++ show sa))
                         else return (Right (vid : acc))

    putOne _ (Left e) _ = return (Left e)
    putOne ncid (Right ()) (vid, dat) =
      withArray (map realToFrac (VU.toList dat)) $ \buf -> do
        s <- c_nc_put_var_double ncid vid buf
        if s /= 0 then return (Left ("nc_put_var_double failed: " ++ show s))
                  else return (Right ())
