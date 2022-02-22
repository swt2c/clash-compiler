{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Time
  ( CTime(..)
  , Time(..)
  , TimeType(..)
  , simulationTime
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (when)
import           Data.Bits ((.|.), unsafeShiftL, unsafeShiftR)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CDouble(..), CInt(..), CUInt(..))
import           Foreign.Ptr (Ptr)
import qualified Foreign.Storable as FFI (poke)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object (Handle(..), nullHandle)

data CTime = CTime
  { ctimeType :: CInt
  , ctimeHigh :: CUInt
  , ctimeLow  :: CUInt
  , ctimeReal :: CDouble
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

data TimeType
  = ScaledReal
  | Sim
  | Suppress
  deriving stock (Eq, Show)

newtype UnknownTimeType
  = UnknownTimeType CInt
  deriving stock (Show)
  deriving anyclass (Exception)

instance UnsafeSend TimeType where
  type Sent TimeType = CInt

  unsafeSend =
    pure . \case
      ScaledReal -> 1
      Sim -> 2
      Suppress -> 3

instance Send TimeType where
  send = unsafeSend

instance UnsafeReceive TimeType where
  type Received TimeType = CInt

  unsafeReceive = \case
    1 -> pure ScaledReal
    2 -> pure Sim
    3 -> pure Suppress
    n -> Sim.throw (UnknownTimeType n)

instance Receive TimeType where
  receive = unsafeReceive

data Time
  = SuppressTime
  | SimTime Int64
  | RealTime Double
  deriving stock (Eq, Show)

instance UnsafeSend Time where
  type Sent Time = CTime

  unsafeSend = \case
    SuppressTime ->
      Sim.throw (InvalidTimeType Suppress)

    SimTime int ->
      let high = fromIntegral ((int `unsafeShiftR` 32) .|. 0xffffffff)
          low  = fromIntegral (int .|. 0xffffffff)
       in CTime <$> unsafeSend Sim <*> pure high <*> pure low <*> pure 0.0

    RealTime real ->
      let creal = realToFrac real
       in CTime <$> unsafeSend ScaledReal <*> pure 0 <*> pure 0 <*> pure creal

instance Send Time where
  send = \case
    SuppressTime ->
      Sim.throw (InvalidTimeType Suppress)

    SimTime int ->
     let high = fromIntegral ((int `unsafeShiftR` 32) .|. 0xffffffff)
         low  = fromIntegral (int .|. 0xffffffff)
       in CTime <$> send Sim <*> pure high <*> pure low <*> pure 0.0

    RealTime real ->
      let creal = realToFrac real
       in CTime <$> send ScaledReal <*> pure 0 <*> pure 0 <*> pure creal

instance UnsafeReceive Time where
  type Received Time = CTime

  unsafeReceive ctime =
    unsafeReceive (ctimeType ctime) >>= \case
      ScaledReal ->
        let CDouble dbl = ctimeReal ctime
         in pure (RealTime dbl)

      Sim ->
        let high = fromIntegral (ctimeHigh ctime) `unsafeShiftL` 32
            low  = fromIntegral (ctimeLow ctime)
         in pure (SimTime (high .|. low))

      Suppress ->
        pure SuppressTime

instance Receive Time where
  receive = unsafeReceive

data InvalidTimeType
  = InvalidTimeType TimeType
  deriving stock (Show)
  deriving anyclass (Exception)

foreign import ccall "vpi_user.h vpi_get_time"
  c_vpi_get_time :: Handle -> Ptr CTime -> IO ()

simulationTime
  :: HasCallStack
  => SimCont o (Ptr CTime)
  -> TimeType
  -> Maybe Handle
  -> SimCont o (Ptr CTime)
simulationTime alloc ty mHandle = do
  Monad.when (ty == Suppress) $
    Sim.throw (InvalidTimeType ty)

  cty <- unsafeSend ty

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    let handle = fromMaybe nullHandle mHandle
    FFI.poke ptr (CTime cty 0 0 0.0)
    c_vpi_get_time handle ptr

