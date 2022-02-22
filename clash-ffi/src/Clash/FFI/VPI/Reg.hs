{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Reg
  ( Reg(..)
  , regName
  , regFullName
  , regSize
  , regIsScalar
  , regIsVector
#if defined(VERILOG_2001)
  , regIsSigned
#endif
  , regValue
  ) where

import Data.ByteString (ByteString)
import GHC.TypeNats (KnownNat, type (<=))

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Property
import Clash.FFI.VPI.Value

newtype Reg = Reg { regHandle :: Handle }

regName :: Reg -> SimCont o ByteString
regName = receiveProperty VpiName . regHandle

regFullName :: Reg -> SimCont o ByteString
regFullName = receiveProperty VpiFullName . regHandle

regIsScalar :: Reg -> SimCont o Bool
regIsScalar = getProperty VpiScalar . regHandle

regIsVector :: Reg -> SimCont o Bool
regIsVector = getProperty VpiVector . regHandle

#if defined(VERILOG_2001)
regIsSigned :: Reg -> SimCont o Bool
regIsSigned = getProperty VpiSigned . regHandle
#endif

regSize :: Integral n => Reg -> SimCont o n
regSize = fmap fromIntegral . getProperty VpiSize . regHandle

regValue
  :: (KnownNat n, 1 <= n)
  => ValueFormat n
  -> Reg
  -> SimCont o (Value n)
regValue fmt =
  receiveValue fmt . regHandle

