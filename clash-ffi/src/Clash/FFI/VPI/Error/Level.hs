{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Error.Level
  ( ErrorLevel(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..))

data ErrorLevel
  = Success
  | Notice
  | Warning
  | Error
  | System
  | Internal
  deriving stock (Show)

newtype UnknownErrorLevel
  = UnknownErrorLevel CInt
  deriving stock (Show)
  deriving anyclass (Exception)

instance UnsafeReceive ErrorLevel where
  type Received ErrorLevel = CInt

  unsafeReceive = \case
    0 -> pure Success
    1 -> pure Notice
    2 -> pure Warning
    3 -> pure Error
    4 -> pure System
    5 -> pure Internal
    n -> Sim.throw (UnknownErrorLevel n)

instance Receive ErrorLevel where
  receive = unsafeReceive

