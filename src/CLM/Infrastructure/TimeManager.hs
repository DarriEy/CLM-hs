-- | Minimal time manager for CLM.
-- Fortran: clm_time_manager.F90
-- Tracks simulation time using a simple record with NO_LEAP calendar.
module CLM.Infrastructure.TimeManager
  ( TimeManager(..)
  , defaultTimeManager
  , DateTime(..)
  , advanceTimestep
  , getCurrDate
  , getCurrCalday
  , isBegCurrDay
  , isEndCurrDay
  , isBegCurrYear
  , getNstep
  , secondsPerDay
  , addSeconds
  , dayOfYear
  ) where

-- | Date-time in the NO_LEAP calendar: 365 days/year, no leap years.
data DateTime = DateTime
  { dtYear   :: !Int
  , dtMonth  :: !Int
  , dtDay    :: !Int
  , dtHour   :: !Int
  , dtMinute :: !Int
  , dtSecond :: !Int
  } deriving (Eq, Show)

-- | Seconds per day.
secondsPerDay :: Int
secondsPerDay = 86400

-- | Days per month in a NO_LEAP calendar.
daysInMonth :: Int -> Int
daysInMonth  1 = 31
daysInMonth  2 = 28
daysInMonth  3 = 31
daysInMonth  4 = 30
daysInMonth  5 = 31
daysInMonth  6 = 30
daysInMonth  7 = 31
daysInMonth  8 = 31
daysInMonth  9 = 30
daysInMonth 10 = 31
daysInMonth 11 = 30
daysInMonth 12 = 31
daysInMonth _  = 30

-- | Add seconds to a DateTime (NO_LEAP calendar).
addSeconds :: DateTime -> Int -> DateTime
addSeconds dt secs =
  let totalSec = dtHour dt * 3600 + dtMinute dt * 60 + dtSecond dt + secs
      (extraDays, remSec) = totalSec `divMod` secondsPerDay
      h = remSec `div` 3600
      m = (remSec `mod` 3600) `div` 60
      s = remSec `mod` 60
  in advanceDays (dt { dtHour = h, dtMinute = m, dtSecond = s }) extraDays

-- | Advance by a number of days (NO_LEAP).
advanceDays :: DateTime -> Int -> DateTime
advanceDays dt 0 = dt
advanceDays dt n
  | n < 0 = dt  -- not supported
  | otherwise =
      let daysLeft = daysInMonth (dtMonth dt) - dtDay dt
      in if n <= daysLeft
           then dt { dtDay = dtDay dt + n }
           else let dt' = if dtMonth dt == 12
                            then dt { dtYear = dtYear dt + 1, dtMonth = 1, dtDay = 1 }
                            else dt { dtMonth = dtMonth dt + 1, dtDay = 1 }
                    consumed = daysLeft + 1
                in advanceDays dt' (n - consumed)

-- | Day of year (1 = Jan 1).
dayOfYear :: DateTime -> Int
dayOfYear dt = sum (map daysInMonth [1 .. dtMonth dt - 1]) + dtDay dt

-- | Minimal time manager for CLM.
data TimeManager = TimeManager
  { tmStartDate   :: !DateTime   -- ^ Simulation start date
  , tmCurrentDate :: !DateTime   -- ^ Current simulation date
  , tmDtime       :: !Int        -- ^ Timestep in seconds
  , tmNstep       :: !Int        -- ^ Current timestep number
  } deriving (Show)

defaultTimeManager :: TimeManager
defaultTimeManager = TimeManager
  { tmStartDate   = DateTime 2000 1 1 0 0 0
  , tmCurrentDate = DateTime 2000 1 1 0 0 0
  , tmDtime       = 1800
  , tmNstep       = 0
  }

-- | Advance the time manager by one timestep. Returns updated manager.
advanceTimestep :: TimeManager -> TimeManager
advanceTimestep tm = tm
  { tmNstep       = tmNstep tm + 1
  , tmCurrentDate = addSeconds (tmCurrentDate tm) (tmDtime tm)
  }

-- | Return current date as (year, month, day, time-of-day in seconds).
getCurrDate :: TimeManager -> (Int, Int, Int, Int)
getCurrDate tm =
  let dt = tmCurrentDate tm
      tod = dtHour dt * 3600 + dtMinute dt * 60 + dtSecond dt
  in (dtYear dt, dtMonth dt, dtDay dt, tod)

-- | Return current calendar day (1.0 = Jan 1 00:00) for NO_LEAP calendar.
getCurrCalday :: TimeManager -> Double
getCurrCalday tm =
  let dt = tmCurrentDate tm
      doy = dayOfYear dt
      frac = fromIntegral (dtHour dt * 3600 + dtMinute dt * 60 + dtSecond dt)
             / fromIntegral secondsPerDay
  in fromIntegral doy + frac

-- | True if at the beginning of a day (time-of-day == 0).
isBegCurrDay :: TimeManager -> Bool
isBegCurrDay tm =
  let dt = tmCurrentDate tm
  in dtHour dt == 0 && dtMinute dt == 0 && dtSecond dt == 0

-- | True if the next timestep would start a new day.
isEndCurrDay :: TimeManager -> Bool
isEndCurrDay tm =
  let nextDt = addSeconds (tmCurrentDate tm) (tmDtime tm)
      currDt = tmCurrentDate tm
  in dtDay nextDt /= dtDay currDt || dtMonth nextDt /= dtMonth currDt

-- | True if at Jan 1 00:00.
isBegCurrYear :: TimeManager -> Bool
isBegCurrYear tm =
  let dt = tmCurrentDate tm
  in dtMonth dt == 1 && dtDay dt == 1 && isBegCurrDay tm

-- | Return current timestep number.
getNstep :: TimeManager -> Int
getNstep = tmNstep
