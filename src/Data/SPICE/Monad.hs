{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SPICE.Monad
    ( Kernel, comment, load
    , Body (..), locate
    , Spice, runSpice, SpiceT, runSpiceT, loadSpiceT
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Alternative)
import           Control.Monad (MonadPlus)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Zip (MonadZip)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( Iso1, Codomain1
                     , LayerResult, DefaultLayerResult2
                     , LayerState, DefaultLayerState2
                     , MonadInner, liftI
                     , MonadTrans, lift, defaultLift2
                     , MonadTransControl
                     , suspend, defaultSuspend2
                     , resume, defaultResume2
                     , capture, defaultCapture2
                     , extract, defaultExtract2
                     , MInvariant, hoistiso, defaultHoistiso2
                     , MFunctor, hoist, defaultHoist2
                     )


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)


-- spk -----------------------------------------------------------------------
import           Data.Space.ICRS (ICRS)
import           Data.SPICE.Body (Body (..))
import qualified Data.SPICE.Body as B
import           Data.SPICE.Kernel (Kernel, comment, load)
import           Data.Time.Clock.Atomic (Time)


------------------------------------------------------------------------------
type Spice = SpiceT Identity


------------------------------------------------------------------------------
runSpice :: Spice a -> Kernel -> Maybe a
runSpice m = runIdentity . runSpiceT m


------------------------------------------------------------------------------
newtype SpiceT m a = SpiceT (ReaderT Kernel (MaybeT m) a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadIO
    , MonadFix, MonadZip, Generic, Generic1, Typeable
    )


------------------------------------------------------------------------------
type instance LayerResult SpiceT = DefaultLayerResult2 (ReaderT Kernel) MaybeT


------------------------------------------------------------------------------
type instance LayerState SpiceT = DefaultLayerState2 (ReaderT Kernel) MaybeT


------------------------------------------------------------------------------
instance Iso1 (SpiceT m) where
    type Codomain1 (SpiceT m) = ReaderT Kernel (MaybeT m)


------------------------------------------------------------------------------
instance MonadTrans SpiceT where
    lift = defaultLift2


------------------------------------------------------------------------------
instance MonadTransControl SpiceT where
    suspend = defaultSuspend2
    resume = defaultResume2
    capture = defaultCapture2
    extract = defaultExtract2


------------------------------------------------------------------------------
instance MInvariant SpiceT where
    hoistiso = defaultHoistiso2


------------------------------------------------------------------------------
instance MFunctor SpiceT where
    hoist = defaultHoist2


------------------------------------------------------------------------------
runSpiceT :: SpiceT m a -> Kernel -> m (Maybe a)
runSpiceT (SpiceT m) = runMaybeT . runReaderT m


------------------------------------------------------------------------------
loadSpiceT :: MonadInner IO m => SpiceT m a -> FilePath -> m (Maybe a)
loadSpiceT m path = liftI (load path) >>= runSpiceT m


------------------------------------------------------------------------------
locate :: (Applicative m, Time t) => Body -> t -> SpiceT m (ICRS, ICRS)
locate body_ time = SpiceT $ ReaderT $ MaybeT . pure . B.locate body_ time
