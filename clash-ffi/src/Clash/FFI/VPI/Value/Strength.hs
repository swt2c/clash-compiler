{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Value.Strength
  ( CStrength(..)
  , Strength(..)
  , StrengthLevel(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View
import           Clash.FFI.VPI.Value.Scalar (Scalar)

data StrengthLevel
  = SupplyDrive
  | StrongDrive
  | PullDrive
  | WeakDrive
  | LargeCharge
  | MediumCharge
  | SmallCharge
  | HiZ
  deriving stock (Show)

instance UnsafeSend StrengthLevel where
  type Sent StrengthLevel = CInt

  unsafeSend =
    pure . \case
      SupplyDrive -> 0x80
      StrongDrive -> 0x40
      PullDrive -> 0x20
      WeakDrive -> 0x08
      LargeCharge -> 0x10
      MediumCharge -> 0x04
      SmallCharge -> 0x02
      HiZ -> 0x01

instance Send StrengthLevel where
  send = unsafeSend

newtype UnknownStrengthLevel
  = UnknownStrengthLevel CInt
  deriving stock (Show)
  deriving anyclass (Exception)

instance UnsafeReceive StrengthLevel where
  type Received StrengthLevel = CInt

  unsafeReceive = \case
    0x80 -> pure SupplyDrive
    0x40 -> pure StrongDrive
    0x20 -> pure PullDrive
    0x08 -> pure WeakDrive
    0x10 -> pure LargeCharge
    0x04 -> pure MediumCharge
    0x02 -> pure SmallCharge
    0x01 -> pure HiZ
    n    -> Sim.throw (UnknownStrengthLevel n)

instance Receive StrengthLevel where
  receive = unsafeReceive

data CStrength = CStrength
  { cstrengthLogic :: CInt
  , cstrengthS0    :: CInt
  , cstrengthS1    :: CInt
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

-- TODO I think this is strength n and returns an array
data Strength = Strength
  { strengthLogic :: Scalar
  , strengthS0    :: StrengthLevel
  , strengthS1    :: StrengthLevel
  }
  deriving stock (Show)

instance UnsafeSend Strength where
  type Sent Strength = CStrength

  unsafeSend strength =
    CStrength
      <$> unsafeSend (strengthLogic strength)
      <*> unsafeSend (strengthS0 strength)
      <*> unsafeSend (strengthS1 strength)

instance Send Strength where
  send strength =
    CStrength
      <$> send (strengthLogic strength)
      <*> send (strengthS0 strength)
      <*> send (strengthS1 strength)

instance UnsafeReceive Strength where
  type Received Strength = CStrength

  unsafeReceive cstrength =
    Strength
      <$> unsafeReceive (cstrengthLogic cstrength)
      <*> unsafeReceive (cstrengthS0 cstrength)
      <*> unsafeReceive (cstrengthS1 cstrength)

instance Receive Strength where
  receive cstrength =
    Strength
      <$> receive (cstrengthLogic cstrength)
      <*> receive (cstrengthS0 cstrength)
      <*> receive (cstrengthS1 cstrength)

