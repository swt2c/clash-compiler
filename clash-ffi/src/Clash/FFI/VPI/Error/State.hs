{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Error.State
  ( ErrorState(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..))

data ErrorState
  = CompileError
  | PliError
  | RunError
  deriving stock (Eq, Show)

newtype UnknownErrorState
  = UnknownErrorState CInt
  deriving stock (Show)
  deriving anyclass (Exception)

instance UnsafeReceive ErrorState where
  type Received ErrorState = CInt

  unsafeReceive = \case
    1 -> pure CompileError
    2 -> pure PliError
    3 -> pure RunError
    n -> Sim.throw (UnknownErrorState n)

instance Receive ErrorState where
  receive = unsafeReceive

