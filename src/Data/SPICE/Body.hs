{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.SPICE.Body
    ( Body (..), locate
    )
where

-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- spk -----------------------------------------------------------------------
import           Data.Space.ICRS (ICRS)
import           Data.SPICE.Kernel (Kernel, lookup)
import           Data.SPICE.Time (ttToTDB)
import           Data.Time.Clock.Uniform (Time, TDB (TDB), toBarycentric)


------------------------------------------------------------------------------
data Body
    = SolarSystemBarycenter
    | MercuryBarycenter
    | VenusBarycenter
    | EarthMoonBarycenter
    | MarsBarycenter
    | JupiterBarycenter
    | SaturnBarycenter
    | UranusBarycenter
    | NeptuneBarycenter
    | PlutoBarycenter
    | Sun
    | Moon
    | Earth
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Generic, Typeable)


------------------------------------------------------------------------------
code :: Body -> Word32
code SolarSystemBarycenter = 0
code MercuryBarycenter = 1
code VenusBarycenter = 2
code EarthMoonBarycenter = 3
code MarsBarycenter = 4
code JupiterBarycenter = 5
code SaturnBarycenter = 6
code UranusBarycenter = 7
code NeptuneBarycenter = 8
code PlutoBarycenter = 9
code Sun = 10
code Moon = 301
code Earth = 399


------------------------------------------------------------------------------
locate :: Time t => Body -> t -> Kernel -> Maybe (ICRS, ICRS)
locate body_ time kernel = go (code body_)
  where
    barycenter = code SolarSystemBarycenter
    TDB tdb = toBarycentric (ttToTDB kernel) time
    go body
        | body == barycenter = Just (0, 0)
        | otherwise = do
            (target, position, velocity) <- lookup body tdb kernel
            (position', velocity') <- go target
            pure (position + position', velocity + velocity')
