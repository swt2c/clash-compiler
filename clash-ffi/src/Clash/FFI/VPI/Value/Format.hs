{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Value.Format
  ( ValueFormat(..)
  , UnknownFormat(..)
  , InvalidSize(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           Foreign.C.Types (CInt)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.Natural (Natural)
import           GHC.TypeNats (KnownNat, type (<=), natVal, sameNat)

import           Clash.FFI.View

data ValueFormat n where
  BinStrFmt :: 1 <= n => ValueFormat n
  OctStrFmt :: 1 <= n => ValueFormat n
  DecStrFmt :: 1 <= n => ValueFormat n
  HexStrFmt :: 1 <= n => ValueFormat n
  ScalarFmt :: ValueFormat 1
  IntFmt :: ValueFormat 4
#if defined(IVERILOG)
  RealFmt :: ValueFormat 1
#else
  RealFmt :: ValueFormat 8
#endif
  StringFmt :: 1 <= n => ValueFormat n
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  VectorFmt :: 1 <= n => ValueFormat n
#endif
  StrengthFmt :: 1 <= n => ValueFormat n
  TimeFmt :: ValueFormat 24
  ObjTypeFmt :: 1 <= n => ValueFormat n
  SuppressFmt :: ValueFormat 0

deriving stock instance Eq (ValueFormat n)
deriving stock instance Show (ValueFormat n)

newtype UnknownFormat
  = UnknownFormat CInt
  deriving stock (Show)
  deriving anyclass (Exception)


data InvalidSize
  = InvalidSize { expected :: Natural, actual :: Natural }
  deriving stock (Show)
  deriving anyclass (Exception)

cintToFormat :: forall n. (KnownNat n, 1 <= n) => CInt -> IO (ValueFormat n)
cintToFormat = \case
  1 -> pure BinStrFmt
  2 -> pure OctStrFmt
  3 -> pure DecStrFmt
  4 -> pure HexStrFmt
  5 ->
    case sameNat (Proxy @1) (Proxy @n) of
      Just Refl -> pure ScalarFmt
      Nothing   -> let size = natVal (Proxy @n)
                    in IO.throwIO (InvalidSize 1 size)

  6 ->
    case sameNat (Proxy @4) (Proxy @n) of
      Just Refl -> pure IntFmt
      Nothing   -> let size = natVal (Proxy @n)
                    in IO.throwIO (InvalidSize 4 size)

  7 ->
#if defined(IVERILOG)
    case sameNat (Proxy @1) (Proxy @n) of
#else
    case sameNat (Proxy @8) (Proxy @n) of
#endif
      Just Refl -> pure RealFmt
      Nothing   -> let size = natVal (Proxy @n)
#if defined(IVERILOG)
                    in IO.throwIO (InvalidSize 1 size)
#else
                    in IO.throwIO (InvalidSize 8 size)
#endif

  8 -> pure StringFmt
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  9 -> pure VectorFmt
#endif
  10 -> pure StrengthFmt
  11 ->
    case sameNat (Proxy @24) (Proxy @n) of
      Just Refl -> pure TimeFmt
      Nothing   -> let size = natVal (Proxy @n)
                    in IO.throwIO (InvalidSize 24 size)

  12 -> pure ObjTypeFmt
  n  -> IO.throwIO (UnknownFormat n)

formatToCInt :: ValueFormat n -> CInt
formatToCInt = \case
  BinStrFmt -> 1
  OctStrFmt -> 2
  DecStrFmt -> 3
  HexStrFmt -> 4
  ScalarFmt -> 5
  IntFmt -> 6
  RealFmt -> 7
  StringFmt -> 8
#if defined(VERILOG_2005) && defined(VPI_VECVAL)
  VectorFmt -> 9
#endif
  StrengthFmt -> 10
  TimeFmt -> 11
  ObjTypeFmt -> 12
  SuppressFmt -> 13

instance (KnownNat n, 1 <= n) => Storable (ValueFormat n) where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)

  peek ptr =
    peek (FFI.castPtr @_ @CInt ptr) >>= cintToFormat

  poke ptr =
    let cintPtr = FFI.castPtr @_ @CInt ptr
     in poke cintPtr . formatToCInt

instance UnsafeSend (ValueFormat n) where
  type Sent (ValueFormat n) = CInt

  unsafeSend =
    pure . formatToCInt

instance Send (ValueFormat n) where
  send = unsafeSend

instance (KnownNat n, 1 <= n) => UnsafeReceive (ValueFormat n) where
  type Received (ValueFormat n) = CInt

  unsafeReceive =
    IO.liftIO . cintToFormat

instance (KnownNat n, 1 <= n) => Receive (ValueFormat n) where
  receive = unsafeReceive

