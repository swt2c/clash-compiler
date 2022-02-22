{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Net
  ( Net(..)
  , netName
  , netFullName
  , netSize
  , netIsScalar
  , netIsVector
#if defined(VERILOG_2001)
  , netIsSigned
#endif
  , netValue
  ) where

import Data.ByteString (ByteString)
import GHC.TypeNats (KnownNat, type (<=))

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Property
import Clash.FFI.VPI.Value

newtype Net = Net { netHandle :: Handle }

netName :: Net -> SimCont o ByteString
netName = receiveProperty VpiName . netHandle

netFullName :: Net -> SimCont o ByteString
netFullName = receiveProperty VpiFullName . netHandle

netSize :: Integral n => Net -> SimCont o n
netSize = fmap fromIntegral . getProperty VpiSize . netHandle

netIsScalar :: Net -> SimCont o Bool
netIsScalar = getProperty VpiScalar . netHandle

netIsVector :: Net -> SimCont o Bool
netIsVector = getProperty VpiVector . netHandle

#if defined(VERILOG_2001)
netIsSigned :: Net -> SimCont o Bool
netIsSigned = getProperty VpiSigned . netHandle
#endif

netValue
  :: (KnownNat n, 1 <= n)
  => ValueFormat n
  -> Net
  -> SimCont o (Value n)
netValue fmt = receiveValue fmt . netHandle

