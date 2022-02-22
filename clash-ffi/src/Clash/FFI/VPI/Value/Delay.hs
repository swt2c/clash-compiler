{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Value.Delay
  ( DelayMode(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import           Foreign.C.Types (CInt)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable(..))

import           Clash.FFI.View (Send(..), UnsafeSend(..))

data DelayMode
  = NoDelay
  | InertialDelay
  | TransportDelay
  | PureTransportDelay
  | Force
  | Release
  | CancelEvent

newtype UnknownDelayMode
  = UnknownDelayMode CInt
  deriving stock (Show)
  deriving anyclass (Exception)

instance Storable DelayMode where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)

  peek ptr =
    peek (FFI.castPtr @_ @CInt ptr) >>= \case
      1 -> pure NoDelay
      2 -> pure InertialDelay
      3 -> pure TransportDelay
      4 -> pure PureTransportDelay
      5 -> pure Force
      6 -> pure Release
      7 -> pure CancelEvent
      n -> IO.throwIO (UnknownDelayMode n)

  poke ptr =
    let cintPtr = FFI.castPtr @_ @CInt ptr
     in poke cintPtr . \case
          NoDelay -> 1
          InertialDelay -> 2
          TransportDelay -> 3
          PureTransportDelay -> 4
          Force -> 5
          Release -> 6
          CancelEvent -> 7

instance UnsafeSend DelayMode where
  type Sent DelayMode = CInt

  unsafeSend =
    pure . \case
      NoDelay -> 1
      InertialDelay -> 2
      TransportDelay -> 3
      PureTransportDelay -> 4
      Force -> 5
      Release -> 6
      CancelEvent -> 7

instance Send DelayMode where
  send = unsafeSend

