{-# LANGUAGE DeriveGeneric #-}

module Data.Space.ICRS
    ( ICRS (ICRS)
    )
where

-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
type KM = Double


------------------------------------------------------------------------------
data ICRS = ICRS {x :: !KM, y :: !KM, z :: !KM}
  deriving (Eq, Ord, Read, Show, Generic, Typeable)


------------------------------------------------------------------------------
instance Semigroup ICRS where
    (<>) = (+)


------------------------------------------------------------------------------
instance Monoid ICRS where
    mempty = 0
    mappend = (<>)


------------------------------------------------------------------------------
instance Num ICRS where
    ICRS a b c + ICRS a' b' c' =
        ICRS (a + a') (b + b') (c + c')
    ICRS a b c - ICRS a' b' c' =
        ICRS (a - a') (b - b') (c - c')
    ICRS a b c * ICRS a' b' c' =
        ICRS (a * a') (b * b') (c * c')
    negate (ICRS a b c) = ICRS (negate a) (negate b) (negate c)
    signum (ICRS a b c) = ICRS (signum a) (signum b) (signum c)
    abs (ICRS a b c) = ICRS (abs a) (abs b) (abs c)
    fromInteger = ICRS <$> fromInteger <*> fromInteger <*> fromInteger
