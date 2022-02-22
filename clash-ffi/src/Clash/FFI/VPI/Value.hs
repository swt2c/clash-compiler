{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Value
  ( CValue(..)
  , Value(..)
  , getValue
  , unsafeReceiveValue
  , receiveValue
  , InvalidFormat(..)
  , InvalidValue(..)

  , module Clash.FFI.VPI.Value.Format
  , module Clash.FFI.VPI.Value.Scalar
  , module Clash.FFI.VPI.Value.Strength
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  , module Clash.FFI.VPI.Value.Vector
#endif
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           Foreign.C.String (CString)
import           Foreign.C.Types (CDouble, CInt(..))
import           Foreign.Ptr (Ptr)
import           Foreign.Storable as FFI (Storable(..))
import           GHC.TypeNats (KnownNat, type (<=), natVal, sameNat)

import           Clash.Sized.BitVector (Bit, BitVector)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr, throw, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object (Handle(..))
import           Clash.FFI.VPI.Time (CTime, Time)
-- import           Clash.FFI.VPI.Value.Delay
import           Clash.FFI.VPI.Value.Format
import           Clash.FFI.VPI.Value.Scalar
import           Clash.FFI.VPI.Value.Strength

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
import           Clash.FFI.VPI.Value.Vector
#endif

data CValue n where
  CBinStrVal :: CString -> CValue n
  COctStrVal :: CString -> CValue n
  CDecStrVal :: CString -> CValue n
  CHexStrVal :: CString -> CValue n
  CScalarVal :: CInt -> CValue 1
  CIntVal :: CInt -> CValue 4
#if defined(IVERILOG)
  CRealVal :: CDouble -> CValue 1
#else
  CRealVal :: CDouble -> CValue 8
#endif
  CStringVal :: CString -> CValue n
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  CVectorVal :: Ptr CVector -> CValue n
#endif
  CStrengthVal :: Ptr CStrength -> CValue n
  CTimeVal :: Ptr CTime -> CValue 24
  CMiscVal :: CString -> CValue n

deriving stock instance Show (CValue n)

data InvalidFormat where
  InvalidFormat :: ValueFormat n -> InvalidFormat

deriving stock instance Show InvalidFormat
deriving anyclass instance Exception InvalidFormat

data InvalidValue where
  InvalidValue :: CValue n -> InvalidValue

deriving stock instance Show InvalidValue
deriving anyclass instance Exception InvalidValue

instance (KnownNat n, 1 <= n) => Storable (CValue n) where
  sizeOf _ = 16
  alignment _ = 8

  peek ptr =
    FFI.peekByteOff @(ValueFormat n) ptr 0 >>= \case
      BinStrFmt ->
        CBinStrVal <$> FFI.peekByteOff ptr 8

      OctStrFmt ->
        COctStrVal <$> FFI.peekByteOff ptr 8

      DecStrFmt ->
        CDecStrVal <$> FFI.peekByteOff ptr 8

      HexStrFmt ->
        CHexStrVal <$> FFI.peekByteOff ptr 8

      ScalarFmt ->
        CScalarVal <$> FFI.peekByteOff ptr 8

      IntFmt ->
        CIntVal <$> FFI.peekByteOff ptr 8

      RealFmt ->
        CRealVal <$> FFI.peekByteOff ptr 8

      StringFmt ->
        CStringVal <$> FFI.peekByteOff ptr 8

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
      VectorFmt ->
        CVectorVal <$> FFI.peekByteOff ptr 8
#endif

      StrengthFmt ->
        CStrengthVal <$> FFI.peekByteOff ptr 8

      TimeFmt ->
        CTimeVal <$> FFI.peekByteOff ptr 8

      fmt ->
        IO.throwIO (InvalidFormat fmt)

  poke ptr = \case
    CBinStrVal bin ->
      FFI.pokeByteOff ptr 0 (BinStrFmt @n) *> FFI.pokeByteOff ptr 8 bin

    COctStrVal oct ->
      FFI.pokeByteOff ptr 0 (OctStrFmt @n) *> FFI.pokeByteOff ptr 8 oct

    CDecStrVal dec ->
      FFI.pokeByteOff ptr 0 (DecStrFmt @n) *> FFI.pokeByteOff ptr 8 dec

    CHexStrVal hex ->
      FFI.pokeByteOff ptr 0 (HexStrFmt @n) *> FFI.pokeByteOff ptr 8 hex

    CScalarVal scalar ->
      FFI.pokeByteOff ptr 0 ScalarFmt *> FFI.pokeByteOff ptr 8 scalar

    CIntVal int ->
      FFI.pokeByteOff ptr 0 IntFmt *> FFI.pokeByteOff ptr 8 int

    CRealVal real ->
      FFI.pokeByteOff ptr 0 RealFmt *> FFI.pokeByteOff ptr 8 real

    CStringVal str ->
      FFI.pokeByteOff ptr 0 (StringFmt @n) *> FFI.pokeByteOff ptr 8 str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec ->
      FFI.pokeByteOff ptr 0 (VectorFmt @n) *> FFI.pokeByteOff ptr 8 vec
#endif

    CStrengthVal strength ->
      FFI.pokeByteOff ptr 0 (StrengthFmt @n) *> FFI.pokeByteOff ptr 8 strength

    CTimeVal time ->
      FFI.pokeByteOff ptr 0 TimeFmt *> FFI.pokeByteOff ptr 8 time

    val ->
      IO.throwIO (InvalidValue val)

data Value n where
  BitVal :: Bit -> Value 1
  BitVectorVal :: BitVector n -> Value n
  IntVal :: Int -> Value 4
#if defined(IVERILOG)
  RealVal :: Double -> Value 1
#else
  RealVal :: Double -> Value 8
#endif
  StringVal :: ByteString -> Value n
-- StrengthVal
  TimeVal :: Time -> Value 24
  MiscVal :: ByteString -> Value n

deriving stock instance KnownNat n => Show (Value n)

instance (KnownNat n, 1 <= n) => UnsafeSend (Value n) where
  type Sent (Value n) = CValue n

  unsafeSend = \case
    BitVal bit ->
      CScalarVal <$> unsafeSend (bitToScalar bit)

    BitVectorVal bv ->
      let list = vectorToCVectorList (bitVectorToVector bv)
       in CVectorVal <$> unsafeSend list

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal str ->
      CStringVal <$> unsafeSend str

    TimeVal time -> do
      ctime <- unsafeSend @Time time
      ptr   <- fst <$> Sim.withNewPtr Sim.stackPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal bytes ->
      CMiscVal <$> unsafeSend bytes

instance (KnownNat n, 1 <= n) => Send (Value n) where
  send = \case
    BitVal bit ->
      CScalarVal <$> send (bitToScalar bit)

    BitVectorVal bv ->
      let list = vectorToCVectorList (bitVectorToVector bv)
       in CVectorVal <$> send list

    IntVal int ->
      pure (CIntVal (fromIntegral int))

    RealVal real ->
      pure (CRealVal (realToFrac real))

    StringVal str ->
      CStringVal <$> send str

    TimeVal time -> do
      ctime <- send time
      ptr   <- fst <$> Sim.withNewPtr Sim.heapPtr (`FFI.poke` ctime)

      pure (CTimeVal ptr)

    MiscVal bytes ->
      CMiscVal <$> send bytes

instance (KnownNat n, 1 <= n) => UnsafeReceive (Value n) where
  type Received (Value n) = CValue n

  unsafeReceive = \case
    CBinStrVal _bin ->
      undefined -- TODO parser

    COctStrVal _oct ->
      undefined -- TODO parser

    CDecStrVal _dec ->
      undefined -- TODO parser

    CHexStrVal _hex ->
      undefined -- TODO parser

    CScalarVal scalar ->
      case sameNat (Proxy @1) (Proxy @n) of
        Just Refl -> BitVal . scalarToBit <$> unsafeReceive scalar
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 1 size)

    CIntVal int ->
      case sameNat (Proxy @4) (Proxy @n) of
        Just Refl -> pure (IntVal (fromIntegral int))
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 4 size)

    CRealVal real ->
#if defined(IVERILOG)
      case sameNat (Proxy @1) (Proxy @n) of
#else
      case sameNat (Proxy @8) (Proxy @n) of
#endif
        Just Refl -> pure (RealVal (realToFrac real))
        Nothing   -> let size = natVal (Proxy @n)
#if defined(IVERILOG)
                      in Sim.throw (InvalidSize 1 size)
#else
                      in Sim.throw (InvalidSize 8 size)
#endif

    CStringVal str ->
      StringVal <$> unsafeReceive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec -> do
      BitVectorVal <$> unsafeReceive vec
#endif

    CStrengthVal _strength ->
      undefined -- TODO need to create type level size. Getting size needs handle

    CTimeVal time ->
      case sameNat (Proxy @24) (Proxy @n) of
        Just Refl -> TimeVal <$> unsafePeekReceive time
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 24 size)

    CMiscVal bytes ->
      MiscVal <$> unsafeReceive bytes

instance (KnownNat n, 1 <= n) => Receive (Value n) where
  receive = \case
    CBinStrVal _bin ->
      undefined -- TODO parser

    COctStrVal _oct ->
      undefined -- TODO parser

    CDecStrVal _dec ->
      undefined -- TODO parser

    CHexStrVal _hex ->
      undefined -- TODO parser

    CScalarVal scalar ->
      case sameNat (Proxy @1) (Proxy @n) of
        Just Refl -> BitVal . scalarToBit <$> receive scalar
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 1 size)

    CIntVal int ->
      case sameNat (Proxy @4) (Proxy @n) of
        Just Refl -> pure (IntVal (fromIntegral int))
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 4 size)

    CRealVal real ->
#if defined(IVERILOG)
      case sameNat (Proxy @1) (Proxy @n) of
#else
      case sameNat (Proxy @8) (Proxy @n) of
#endif
        Just Refl -> pure (RealVal (realToFrac real))
        Nothing   -> let size = natVal (Proxy @n)
#if defined(IVERILOG)
                      in Sim.throw (InvalidSize 1 size)
#else
                      in Sim.throw (InvalidSize 8 size)
#endif

    CStringVal str ->
      StringVal <$> receive str

#if defined(VERILOG_2005) && defined(VPI_VECVAL)
    CVectorVal vec ->
      BitVectorVal <$> receive vec
#endif

    CStrengthVal _strength ->
      undefined -- TODO need to create type level size. Getting size needs handle

    CTimeVal time ->
      case sameNat (Proxy @24) (Proxy @n) of
        Just Refl -> TimeVal <$> peekReceive time
        Nothing   -> let size = natVal (Proxy @n)
                      in Sim.throw (InvalidSize 24 size)

    CMiscVal bytes ->
      MiscVal <$> receive bytes

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Handle -> Ptr (CValue n) -> IO ()

getValue
  :: (KnownNat n, 1 <= n)
  => SimCont o (Ptr (CValue n))
  -> ValueFormat n
  -> Handle
  -> SimCont o (Ptr (CValue n))
getValue alloc fmt handle = do
  cfmt <- unsafeSend fmt

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value handle ptr

    pure ()

unsafeReceiveValue
  :: (KnownNat n, 1 <= n)
  => ValueFormat n
  -> Handle
  -> SimCont o (Value n)
unsafeReceiveValue fmt handle =
  getValue Sim.stackPtr fmt handle >>= unsafePeekReceive

receiveValue
  :: (KnownNat n, 1 <= n)
  => ValueFormat n
  -> Handle
  -> SimCont o (Value n)
receiveValue fmt handle =
  getValue Sim.heapPtr fmt handle >>= peekReceive

foreign import ccall "vpi_user.h vpi_put_value"
  c_vpi_put_value :: Handle -> Ptr (CValue n) -> Ptr CTime -> CInt -> IO Handle

-- TODO putValue

