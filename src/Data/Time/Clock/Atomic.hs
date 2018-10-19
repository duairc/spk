{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Time.Clock.Atomic
    ( ET, TAI (TAI), TCB (TCB), TCG (TCG), TDB (TDB), TDT, TT (TT)
    , JD (JD), j2000, fromJD, toJD
    , Geocentric (fromTT, toTT)
    , Barycentric (fromTDB, toTDB)
    , Time (fromBarycentric, toBarycentric, fromGeocentric, toGeocentric)
    , tdbToTT, ttToTDB
    )
where

-- base ----------------------------------------------------------------------
import           Data.Fixed (divMod')
import           Data.Maybe (fromJust)
import           Data.Typeable (Typeable)
import           Foreign.C.Types (CTime (CTime))
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- leapseconds-announced -----------------------------------------------------
import           Data.Time.Clock.AnnouncedLeapSeconds (lst)


-- time ----------------------------------------------------------------------
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.TAI as T
import qualified Data.Time.LocalTime as T


------------------------------------------------------------------------------
type ET = TDB


------------------------------------------------------------------------------
newtype TAI = TAI Double
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
newtype TCB = TCB Double
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
newtype TCG = TCG Double
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
newtype TDB = TDB Double
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
type TDT = TT


------------------------------------------------------------------------------
newtype TT = TT Double
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
newtype JD a = JD a
  deriving
    ( Eq, Ord, Read, Show, Enum, Num, Real, Fractional, Floating, RealFrac
    , RealFloat, Generic, Typeable
    )


------------------------------------------------------------------------------
toJD :: Fractional a => a -> JD a
toJD t = JD $ 2451545.0 + t / 86400


------------------------------------------------------------------------------
fromJD :: Fractional a => JD a -> a
fromJD (JD d) = (d - 2451545.0) * 86400


------------------------------------------------------------------------------
class Barycentric a where
    fromTDB :: TDB -> a
    toTDB :: a -> TDB


------------------------------------------------------------------------------
instance Barycentric TCB where
    toTDB (TCB tcb) = TDB $ tcb - lb * (tcb - tt0) - p0
    fromTDB (TDB tdb) = TCB $ (tdb - lb * tt0 + p0) / (1 - lb)


------------------------------------------------------------------------------
instance Barycentric TDB where
    fromTDB = id
    toTDB = id


------------------------------------------------------------------------------
instance (Barycentric a, Fractional a) => Barycentric (JD a) where
    fromTDB = toJD . fromTDB
    toTDB = toTDB . fromJD


------------------------------------------------------------------------------
class Geocentric a where
    fromTT :: TT -> a
    toTT :: a -> TT


------------------------------------------------------------------------------
instance Geocentric T.AbsoluteTime where
    fromTT (TT s) = T.addAbsoluteTime d j2000AbsoluteTime
      where
        d = T.picosecondsToDiffTime (round ((s - taiDelta) * 1e12))

    toTT at =
        TT $ (fromInteger (T.diffTimeToPicoseconds d) / 1e12) + taiDelta
      where
        d = T.diffAbsoluteTime at j2000AbsoluteTime


------------------------------------------------------------------------------
instance Geocentric CTime where
    fromTT (TT s) = CTime $ round $ s - unix0
    toTT (CTime ut) = TT $ fromIntegral ut + unix0


------------------------------------------------------------------------------
instance Geocentric T.Day where
    fromTT = T.utctDay . fromTT
    toTT = toTT . flip T.UTCTime 0


------------------------------------------------------------------------------
instance Geocentric TAI where
    fromTT (TT s) = TAI (s - taiDelta)
    toTT (TAI s) = TT (s + taiDelta)


------------------------------------------------------------------------------
instance Geocentric TCG where
    fromTT (TT tt) = TCG $ tt0 + (tt - tt0) / (1 - lg)
    toTT (TCG tcg) = TT $ tt0 + (tcg - tt0) * (1 - lg)


------------------------------------------------------------------------------
instance Geocentric TT where
    fromTT = id
    toTT = id


------------------------------------------------------------------------------
instance Geocentric T.UniversalTime where
    fromTT = utcToUT . fromTT
    toTT = toTT . utToUTC


------------------------------------------------------------------------------
instance Geocentric T.UTCTime where
    fromTT = taiToUTC . fromTT
    toTT = toTT . utcToTAI


------------------------------------------------------------------------------
instance Geocentric T.ZonedTime where
    fromTT = T.utcToZonedTime T.utc . fromTT
    toTT = toTT . T.zonedTimeToUTC


------------------------------------------------------------------------------
instance (Geocentric a, Fractional a) => Geocentric (JD a) where
    fromTT = toJD . fromTT
    toTT = toTT . fromJD


------------------------------------------------------------------------------
class Time a where
    fromBarycentric :: Barycentric b => (TDB -> TT) -> b -> a
    toBarycentric :: Barycentric b => (TT -> TDB) -> a -> b
    fromGeocentric :: Geocentric b => (TT -> TDB) -> b -> a
    toGeocentric :: Geocentric b => (TDB -> TT) -> a -> b


------------------------------------------------------------------------------
instance Time T.AbsoluteTime where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time CTime where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time T.Day where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time TAI where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time TCB where
    fromBarycentric = barycentricFromBarycentric
    toBarycentric = barycentricToBarycentric
    fromGeocentric = barycentricFromGeocentric
    toGeocentric = barycentricToGeocentric


------------------------------------------------------------------------------
instance Time TCG where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time TDB where
    fromBarycentric = barycentricFromBarycentric
    toBarycentric = barycentricToBarycentric
    fromGeocentric = barycentricFromGeocentric
    toGeocentric = barycentricToGeocentric


------------------------------------------------------------------------------
instance Time TT where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time T.UniversalTime where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time T.UTCTime where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance Time T.ZonedTime where
    fromBarycentric = geocentricFromBarycentric
    toBarycentric = geocentricToBarycentric
    fromGeocentric = geocentricFromGeocentric
    toGeocentric = geocentricToGeocentric


------------------------------------------------------------------------------
instance (Time a, Fractional a) => Time (JD a) where
    fromBarycentric conv = toJD . fromBarycentric conv
    toBarycentric conv = toBarycentric conv . fromJD
    fromGeocentric conv = toJD . fromGeocentric conv
    toGeocentric conv = toGeocentric conv . fromJD


------------------------------------------------------------------------------
barycentricFromBarycentric :: (Barycentric a, Barycentric b)
    => (TDB -> TT) -> b -> a
barycentricFromBarycentric _ = fromTDB . toTDB


------------------------------------------------------------------------------
barycentricToBarycentric :: (Barycentric a, Barycentric b)
    => (TT -> TDB) -> a -> b
barycentricToBarycentric _ = fromTDB . toTDB


------------------------------------------------------------------------------
barycentricFromGeocentric :: (Barycentric a, Geocentric b)
    => (TT -> TDB) -> b -> a
barycentricFromGeocentric conv = fromTDB . conv . toTT


------------------------------------------------------------------------------
barycentricToGeocentric :: (Barycentric a, Geocentric b)
    => (TDB -> TT) -> a -> b
barycentricToGeocentric conv = fromTT . conv . toTDB


------------------------------------------------------------------------------
geocentricFromBarycentric :: (Geocentric a, Barycentric b)
    => (TDB -> TT) -> b -> a
geocentricFromBarycentric conv = fromTT . conv . toTDB


------------------------------------------------------------------------------
geocentricToBarycentric :: (Geocentric a, Barycentric b)
    => (TT -> TDB) -> a -> b
geocentricToBarycentric conv = fromTDB . conv . toTT


------------------------------------------------------------------------------
geocentricFromGeocentric :: (Geocentric a, Geocentric b)
    => (TT -> TDB) -> b -> a
geocentricFromGeocentric _ = fromTT . toTT


------------------------------------------------------------------------------
geocentricToGeocentric :: (Geocentric a, Geocentric b)
    => (TDB -> TT) -> a -> b
geocentricToGeocentric _ = fromTT . toTT


------------------------------------------------------------------------------
ttToTDB :: TT -> TDB
ttToTDB (TT tt) = TDB $ tt + tdbDelta tt


------------------------------------------------------------------------------
tdbToTT :: TDB -> TT
tdbToTT (TDB tdb) = TT $ invert (\tt -> tt + tdbDelta tt) tdb


------------------------------------------------------------------------------
tdbDelta :: Double -> Double
tdbDelta s = 0.001658 * sin (g + 0.01671 * sin g)
  where
    g = 2 * pi * (357.528 + 0.9856003 * s / 86400) / 360


------------------------------------------------------------------------------
invert :: (Double -> Double) -> Double -> Double
invert f i = delta i (\di -> go (i - di))
  where
    epsilon = 1e-15

    go o = delta o step
      where
        step do_
            | abs di < epsilon = o
            | otherwise = go (o + di)
          where
            i' = o + do_
            di = i - i'

    delta i' step = step $ f i' - i'


------------------------------------------------------------------------------
utcToUT :: T.UTCTime -> T.UniversalTime
utcToUT (T.UTCTime day@(T.ModifiedJulianDay mjd) time) =
    T.ModJulianDate $ fromInteger mjd + toRational (time / len)
  where
    len = maybe 86400 id (T.utcDayLength lst day)


------------------------------------------------------------------------------
utToUTC :: T.UniversalTime -> T.UTCTime
utToUTC (T.ModJulianDate jd) = T.UTCTime day time
  where
    (mjd, fraction) = divMod' jd 1
    day = T.ModifiedJulianDay mjd
    len = maybe 86400 id (T.utcDayLength lst day)
    time = fromRational fraction * len


------------------------------------------------------------------------------
utcToTAI :: T.UTCTime -> T.AbsoluteTime
utcToTAI = fromJust . T.utcToTAITime (Just . maybe 0 id . lst)


------------------------------------------------------------------------------
taiToUTC :: T.AbsoluteTime -> T.UTCTime
taiToUTC = fromJust . T.taiToUTCTime (Just . maybe 0 id . lst)


------------------------------------------------------------------------------
j2000 :: JD TT
j2000 = toJD 0


------------------------------------------------------------------------------
j2000mjd :: Integer
j2000mjd = 51544


------------------------------------------------------------------------------
tai0 :: Double
tai0 = -725803200.0


------------------------------------------------------------------------------
tt0 :: Double
tt0 = tai0 + taiDelta


------------------------------------------------------------------------------
p0 :: Double
p0 = 6.55e-5


------------------------------------------------------------------------------
unix0 :: Double
unix0 = -946727967.816


------------------------------------------------------------------------------
lb :: Double
lb = 1.550519768e-8


------------------------------------------------------------------------------
lg :: Double
lg = 6.969290134e-10


------------------------------------------------------------------------------
taiDelta :: Double
taiDelta = 32.184


------------------------------------------------------------------------------
j2000Day :: T.Day
j2000Day = T.ModifiedJulianDay j2000mjd


------------------------------------------------------------------------------
j2000Time :: T.DiffTime
j2000Time = 43200


------------------------------------------------------------------------------
j2000AbsoluteTime :: T.AbsoluteTime
j2000AbsoluteTime = T.addAbsoluteTime j2000Time day
  where
    day = T.taiNominalDayStart j2000Day
