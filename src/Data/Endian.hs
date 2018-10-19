{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Endian
    ( Endianness (LittleEndian, BigEndian)
    , Endian (Endian)
    , HasEndianness (big, little)
    , IsEndian (swap)
    , Endianed (Little, Big)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad.Fix (MonadFix, mfix)
import           Control.Monad.Zip (MonadZip, mzipWith, munzip)
import           Data.Bits (Bits (..), FiniteBits)
import           Data.Function (fix)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Ix (Ix)
import           Data.Proxy (Proxy (Proxy))
import           Data.String (IsString)
import           Data.Typeable (Typeable)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, sizeOf, peek, poke)
import           GHC.Float
                     ( castDoubleToWord64, castFloatToWord32
                     , castWord32ToFloat, castWord64ToDouble
                     )
import           GHC.Generics (Generic, Generic1)


-- cereal --------------------------------------------------------------------
import           Data.Serialize (Serialize, get, put)


-- cpu -----------------------------------------------------------------------
import           System.Endian
                     ( Endianness (LittleEndian, BigEndian)
                     , toLE16, toLE32, toLE64, toBE16, toBE32, toBE64
                     )


-- deriving-storable ---------------------------------------------------------
import           Foreign.Storable.Generic


------------------------------------------------------------------------------
newtype Endian (e :: Endianness) a = Endian a
  deriving
    ( Bits, Bounded, Enum, Eq, FiniteBits, Floating, Fractional, Generic
    , Integral, IsString, Ix, Semigroup, Monoid, Num, Ord, Real, RealFrac
    , RealFloat, Typeable, Functor, Foldable, Generic1, Traversable
    )
  deriving newtype (Read, Show)


------------------------------------------------------------------------------
instance Applicative (Endian e) where
    pure = Endian
    Endian f <*> Endian a = Endian $ f a


------------------------------------------------------------------------------
instance Monad (Endian e) where
    return = pure
    Endian a >>= f = f a


------------------------------------------------------------------------------
instance MonadFix (Endian e) where
    mfix f = Endian (fix (\a -> let Endian b = f a in b))


------------------------------------------------------------------------------
instance MonadZip (Endian e) where
    munzip (Endian (a, b)) = (Endian a, Endian b)
    mzipWith f (Endian a) (Endian b) = Endian (f a b)


------------------------------------------------------------------------------
class HasEndianness a where
    big, little :: a -> a
    big = id
    little = id


------------------------------------------------------------------------------
instance HasEndianness Word8


------------------------------------------------------------------------------
instance HasEndianness Word16 where
    big = toBE16
    little = toLE16


------------------------------------------------------------------------------
instance HasEndianness Word32 where
    big = toBE32
    little = toLE32


------------------------------------------------------------------------------
instance HasEndianness Word64 where
    big = toBE64
    little = toLE64


------------------------------------------------------------------------------
instance HasEndianness Int8


------------------------------------------------------------------------------
instance HasEndianness Int16 where
    big = w2i16 . toBE16 . i2w16
    little = w2i16 . toLE16 . i2w16


------------------------------------------------------------------------------
instance HasEndianness Int32 where
    big = w2i32 . toBE32 . i2w32
    little = w2i32 . toLE32 . i2w32


------------------------------------------------------------------------------
instance HasEndianness Int64 where
    big = w2i64 . toBE64 . i2w64
    little = w2i64 . toLE64 . i2w64


------------------------------------------------------------------------------
instance HasEndianness Float where
    big = w2f32 . toBE32 . f2w32
    little = w2f32 . toLE32 . f2w32


------------------------------------------------------------------------------
instance HasEndianness Double where
    big = w2f64 . toBE64 . f2w64
    little = w2f64 . toLE64 . f2w64


------------------------------------------------------------------------------
class IsEndian (e :: Endianness) where
    swap :: HasEndianness a => Proxy e -> a -> a


------------------------------------------------------------------------------
instance IsEndian 'BigEndian where
    swap _ = big


------------------------------------------------------------------------------
instance IsEndian 'LittleEndian where
    swap _ = little


------------------------------------------------------------------------------
instance (IsEndian e, HasEndianness a, Storable a) => Storable (Endian e a)
  where
    sizeOf _ = sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr = Endian . swap (Proxy :: Proxy e) <$> peek (castPtr ptr)
    poke ptr (Endian a) = poke (castPtr ptr) $ swap (Proxy :: Proxy e) a


------------------------------------------------------------------------------
instance (IsEndian e, HasEndianness a, Storable a) => GStorable (Endian e a)
  where
    gsizeOf = sizeOf
    galignment = alignment
    gpeekByteOff = peekByteOff
    gpokeByteOff = pokeByteOff


------------------------------------------------------------------------------
instance (IsEndian e, HasEndianness a, Serialize a) => Serialize (Endian e a)
  where
    get = Endian . big . swap (Proxy :: Proxy e) <$> get
    put (Endian a) = put $ swap (Proxy :: Proxy e) a


------------------------------------------------------------------------------
i2w16 :: Int16 -> Word16
i2w16 = fromIntegral


------------------------------------------------------------------------------
w2i16 :: Word16 -> Int16
w2i16 = fromIntegral


------------------------------------------------------------------------------
i2w32 :: Int32 -> Word32
i2w32 = fromIntegral


------------------------------------------------------------------------------
w2i32 :: Word32 -> Int32
w2i32 = fromIntegral


------------------------------------------------------------------------------
i2w64 :: Int64 -> Word64
i2w64 = fromIntegral


------------------------------------------------------------------------------
w2i64 :: Word64 -> Int64
w2i64 = fromIntegral


------------------------------------------------------------------------------
w2f32 :: Word32 -> Float
w2f32 = castWord32ToFloat


------------------------------------------------------------------------------
f2w32 :: Float -> Word32
f2w32 = castFloatToWord32


------------------------------------------------------------------------------
w2f64 :: Word64 -> Double
w2f64 = castWord64ToDouble


------------------------------------------------------------------------------
f2w64 :: Double -> Word64
f2w64 = castDoubleToWord64


------------------------------------------------------------------------------
data Endianed f = Little !(f 'LittleEndian) | Big !(f 'BigEndian)
deriving instance (Show (f 'LittleEndian), Show (f 'BigEndian))
    => Show (Endianed f)

