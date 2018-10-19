{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SPICE.Kernel
    ( Kernel, comment, load, lookup
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (liftA2)
import           Data.Char (isSpace)
import           Data.Fixed (divMod')
import           Data.List (find)
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           Foreign.ForeignPtr (castForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array (advancePtr)
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable (peekElemOff, sizeOf)
import           GHC.Float (castWord64ToDouble, double2Int)
import           GHC.Generics (Generic, Generic1)
import           Prelude hiding (lookup)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as C


-- cereal --------------------------------------------------------------------
import           Data.Serialize.Get
                     ( Get, runGet, getBytes, getWord32host, getWord64host
                     , bytesRead, isolate, lookAhead, remaining, skip
                     )


-- cpu -----------------------------------------------------------------------
import           System.Endian
                     ( Endianness (LittleEndian, BigEndian)
                     , getSystemEndianness
                     )


-- derive-storable -----------------------------------------------------------
import           Foreign.Storable.Generic (GStorable)


-- mmap ----------------------------------------------------------------------
import           System.IO.MMap (mmapFileByteString)


-- parallel ------------------------------------------------------------------
import           Control.Parallel.Strategies (runEval)


-- repa ----------------------------------------------------------------------
import           Data.Array.Repa
                     ( D
                     , (:.) ((:.)), Z (Z), All (All), Any (Any)
                     , DIM3, ix1, ix2, ix3
                     )
import qualified Data.Array.Repa as R
import           Data.Array.Repa.Repr.ForeignPtr (fromForeignPtr)


-- spk -----------------------------------------------------------------------
import           Data.Space.ICRS (ICRS (ICRS))


-- vector --------------------------------------------------------------------
import qualified Data.Vector as I
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Internal as V


------------------------------------------------------------------------------
data Kernel = Kernel
    { comment :: !ByteString
    , segments :: !(I.Vector (Segment Array))
    }


------------------------------------------------------------------------------
load :: FilePath -> IO Kernel
load path = do
    mmap <- mmapFileByteString path Nothing
    file <- either fail pure $ runGet getFile mmap
    comment_ <- either fail pure $ runGet (getComment file) mmap
    segments_ <- either fail pure $ runGet (getSegments file) mmap
    segments' <- I.fromListN (V.length segments_) <$>
         traverse (lookupSegment mmap) (V.toList segments_)
    pure $ Kernel comment_ segments'


------------------------------------------------------------------------------
lookup :: Word32 -> Double -> Kernel -> Maybe (Word32, ICRS, ICRS)
lookup body time = fmap locate . find go . segments
  where
    go segment = time >= startTime segment && time < endTime segment
        && body == center segment
    locate segment = (target segment, position, velocity)
      where
        (position, velocity) = getPositionAndVelocity (array segment) time


------------------------------------------------------------------------------
check :: Monad m => Bool -> String -> m ()
check True _ = pure ()
check False s = fail s


------------------------------------------------------------------------------
data File = File
    { _fward :: !Word32
    }
  deriving (Eq, Ord, Read, Show, Generic, Typeable)


------------------------------------------------------------------------------
trim :: ByteString -> ByteString
trim = fst . C.spanEnd (\c -> isSpace c || c == '\0')


------------------------------------------------------------------------------
getFile :: Get File
getFile = isolate 1024 $ do
    locidw <- trim <$> getBytes 8
    check (locidw == "DAF/SPK") "LOCIDW is not \"DAF/SPK\""
    locfmt <- lookAhead $ skip 80 *> (trim <$> getBytes 8)
    endianness <- case locfmt of
        "BIG-IEEE" -> pure BigEndian
        "LTL-IEEE" -> pure LittleEndian
        _ -> fail "LOCFMT not understood"
    check (endianness == getSystemEndianness) $ concat
        [ "SPK file is " ++ show endianness ++ " but system is "
        , show getSystemEndianness
        ]
    nd <- getWord32host
    check (nd == 2) "ND != 2"
    ni <- getWord32host
    check (ni == 6) "NI != 6"
    skip 60
    fward <- getWord32host
    skip 619
    ftpstr <- getBytes 28
    check (ftpstr == ftp) "SPK file is corrupt, bad FTPSR"
    skip 297
    pure $ File fward
      where
        ftp = "FTPSTR:\r:\n:\r\n:\r\x00:\x81:\x10\xce:ENDFTP"


------------------------------------------------------------------------------
record :: Word32 -> Get ()
record n = do
    p <- bytesRead
    skip $ 1024 * (fromIntegral n - 1) - p


------------------------------------------------------------------------------
data Indices = Indices
    { _start :: !Word32
    , _end :: !Word32
    }
  deriving (Eq, Ord, Read, Show, Generic, Typeable, GStorable)


------------------------------------------------------------------------------
data Segment a = Segment
    { startTime :: !Double
    , endTime :: !Double
    , center :: !Word32
    , target :: !Word32
    , frame :: !Word32
    , dataType :: !Word32
    , array :: !a
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable, GStorable
    , Foldable, Functor, Traversable, Generic1
    )


------------------------------------------------------------------------------
data Array = Array
    { _initTime :: !Double
    , _interval :: !Double
    , _elements :: !(R.Array D DIM3 Double)
    }


------------------------------------------------------------------------------
getChunk :: Get (Vector (Segment Indices), Word32)
getChunk = isolate 1024 $ skip 8 *> liftA2 (flip (,)) w32 (w32 >>= go)
  where
    w32 = fromIntegral . double2Int <$> getFloat64host
    go nsum = do
        (fp, offset, _) <- B.toForeignPtr <$> (remaining >>= getBytes)
        let fp' = castForeignPtr $ V.updPtr (flip plusPtr offset) fp
        pure $ V.unsafeFromForeignPtr0 fp' (fromIntegral nsum)


------------------------------------------------------------------------------
getSegments :: File -> Get (Vector (Segment Indices))
getSegments = fmap V.concat . go . _fward
  where
    go 0 = pure []
    go n = do
        record n
        (chunk, next) <- getChunk
        chunks <- go next
        pure $ chunk : chunks


------------------------------------------------------------------------------
getComment :: File -> Get ByteString
getComment = fmap cleanup . traverse go . enumFromTo 2 . subtract 1 . _fward
  where
    go n = record n *> getBytes 1000
    cleanup = C.map replace . C.takeWhile (/= '\4') . B.concat
      where
        replace '\0' = '\n'
        replace c = c


------------------------------------------------------------------------------
lookupSegment :: ByteString -> Segment Indices -> IO (Segment Array)
lookupSegment input segment = do
    array_ <- getArray input segment
    pure $ fmap (const array_) segment


------------------------------------------------------------------------------
getArray :: ByteString -> Segment Indices -> IO Array
getArray input segment
    | type_ /= 2 && type_ /= 3 = fail "bad segment, type != (2 || 3)"
    | nelem < 4 = fail "bad array, NELEM < 4"
    | end' * sizeOf (0.0 :: Double) > len = fail "bad array, out of range"
    | otherwise = do
        (init_, interval_, rsize, nrecords) <- withForeignPtr fp' $ \ptr -> do
            init_ <- peekElemOff ptr (nelem - 4)
            interval_ <- peekElemOff ptr (nelem - 3)
            rsize <- double2Int <$> peekElemOff ptr (nelem - 2)
            nrecords <- double2Int <$> peekElemOff ptr (nelem - 1)
            pure (init_, interval_, rsize, nrecords)
        let f = R.unsafeIndex $ fromForeignPtr (ix2 nrecords rsize) fp'
        let matrix = R.fromFunction (ix2 nrecords (rsize - 2))
             (\(Z :. m :. n) -> f (ix2 m (n + 2)))
        let ncoefficients = div (rsize - 2) ncomponents
        let matrix' = R.reshape (ix3 nrecords ncomponents ncoefficients) matrix
        let matrix'' = R.fromFunction (ix3 ncomponents nrecords ncoefficients)
             (\(Z :. m :. n :. o) -> R.unsafeIndex matrix' (ix3 n m o))
        pure $ Array init_ interval_ matrix''
  where
    type_ = dataType segment
    ncomponents
        | type_ == 2 = 3
        | type_ == 3 = 6
        | otherwise = error "bad segment, type != (2 || 3)"
    Indices start end = array segment
    start' = fromIntegral start - 1
    end' = fromIntegral end
    nelem = end' - start'
    (fp, offset, len) = B.toForeignPtr input
    fp' = V.updPtr (flip advancePtr start') $
        castForeignPtr $ V.updPtr (flip plusPtr offset) fp


------------------------------------------------------------------------------
getFloat64host :: Get Double
getFloat64host = castWord64ToDouble <$> getWord64host


------------------------------------------------------------------------------
getPositionAndVelocity :: Array -> Double -> (ICRS, ICRS)
getPositionAndVelocity (Array i v elems) time
    | index < 0 || index > n = error "getPosition: time index out of bounds"
    | ncomponents == 3 = runEval $ do
        (position, velocity) <- liftA2 (,) result differential
        let [px, py, pz] = R.toList position
        let [vx, vy, vz] = R.toList velocity
        pure (ICRS px py pz, ICRS vx vy vz)
    | ncomponents == 6 = runEval $ do
        [px, py, pz, vx, vy, vz] <- R.toList <$> result
        pure (ICRS px py pz, ICRS vx vy vz)
    | otherwise = error "getPosition: bad array, ncomponents != (3 || 6)"
  where
    Z :. ncomponents :. n :. ncoefficients = R.extent elems

    (index, offset)
        | j == n = (1, v)
        | otherwise = (j, o)
      where
        (j, o) = divMod' (time - i) v

    coefficients = R.slice elems (R.Any :. R.All :. index :. R.All)

    ov = offset / v
    t1 = 2 * ov - 1
    tt1 = 2 * t1

    ts = 1 : t1 : zipWith f (tail ts) ts
      where
        f x y = tt1 * x - y
    rts = R.extend (Any :. ncomponents :. All) $
        R.fromListUnboxed (ix1 ncoefficients) (take ncoefficients ts)
    result = R.sumP $ coefficients R.*^ rts

    dts = map (* tdv) $ 0 : idts
      where
        tdv = 2 / v
        idts = 1 : 2 * tt1 : zipWith3 f (tail idts) idts (tail (tail ts))
          where
            f dx dy x = tt1 * dx - dy + 2 * x
    rdts = R.extend (Any :. ncomponents :. All) $
        R.fromListUnboxed (ix1 ncoefficients) (take ncoefficients dts)
    differential = R.sumP $ coefficients R.*^ rdts
