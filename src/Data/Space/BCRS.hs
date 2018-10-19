{-# LANGUAGE DeriveGeneric #-}

module Data.Space.BCRS
    ( BCRS (BCRS)
    )
where

-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
data BCRS = BCRS
    { x :: !Double
    , y :: !Double
    , z :: !Double
    }
  deriving (Eq, Ord, Read, Show, Generic, Typeable)


------------------------------------------------------------------------------
instance Semigroup BCRS where
    (<>) = (+)


------------------------------------------------------------------------------
instance Monoid BCRS where
    mempty = 0
    mappend = (<>)


------------------------------------------------------------------------------
instance Num BCRS where
    BCRS a b c + BCRS a' b' c' =
        BCRS (a + a') (b + b') (c + c')
    BCRS a b c - BCRS a' b' c' =
        BCRS (a - a') (b - b') (c - c')
    BCRS a b c * BCRS a' b' c' =
        BCRS (a * a') (b * b') (c * c')
    negate (BCRS a b c) = BCRS (negate a) (negate b) (negate c)
    signum (BCRS a b c) = BCRS (signum a) (signum b) (signum c)
    abs (BCRS a b c) = BCRS (abs a) (abs b) (abs c)
    fromInteger = BCRS <$> fromInteger <*> fromInteger <*> fromInteger
