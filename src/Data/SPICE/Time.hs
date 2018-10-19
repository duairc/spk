module Data.SPICE.Time
    ( tdbToTT, ttToTDB
    )
where

-- base ----------------------------------------------------------------------
import           Prelude hiding (lookup)


-- spk -----------------------------------------------------------------------
import           Data.Space.ICRS (ICRS (ICRS))
import           Data.SPICE.Kernel (Kernel, lookup)
import           Data.Time.Clock.Uniform (TT (TT), TDB (TDB))
import qualified Data.Time.Clock.Uniform as T


------------------------------------------------------------------------------
tdbToTT :: Kernel -> TDB -> TT
tdbToTT kernel (TDB s) = TT (dtdbToTT kernel s)


------------------------------------------------------------------------------
ttToTDB :: Kernel -> TT -> TDB
ttToTDB kernel (TT s) = TDB (dttToTDB kernel s)


------------------------------------------------------------------------------
dtdbToTT :: Kernel -> Double -> Double
dtdbToTT kernel s = case lookup 1000000001 s kernel of
    Just (center, ICRS d _ _, _) | center == 1000000000 -> s + d
    _ -> let TT tt = T.tdbToTT (TDB s) in tt


------------------------------------------------------------------------------
dttToTDB :: Kernel -> Double -> Double
dttToTDB = invert . dtdbToTT


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

    delta x step = step $ f x - x

