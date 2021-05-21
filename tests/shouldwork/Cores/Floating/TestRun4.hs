{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TestRun4 where

import Clash.Prelude hiding ((^))
import Prelude ((^))
import qualified Prelude as P

import Numeric (showHex)

import Clash.Explicit.Testbench

import Clash.Cores.Floating.Xilinx
-- import Clash.Cores.Floating.Xilinx.Internal (xilinxNaN)

import Xilinx.Annotations
import Xilinx.TH

newtype FloatVerifier = FloatVerifier Float

instance Eq FloatVerifier where
  (FloatVerifier x) == (FloatVerifier y) = pack x == pack y

instance ShowX FloatVerifier where
  showsPrecX = showsPrecXWith showsPrec

instance Show FloatVerifier where
  showsPrec = floatVerifierShowsPrec#

floatVerifierShowsPrec#
  :: Int
  -> FloatVerifier
  -> ShowS
floatVerifierShowsPrec# _ (FloatVerifier x)
  | isNaN x = nanSign x . nanString x . showHex (payload x) . (')':)
  | otherwise = shows x
 where
  nanSign x | msb (pack x) == 0 = ('+':)
            | otherwise         = ('-':)
  nanString x
    | testBit (pack x) 22 = ("qNaN(0x" P.++)
    | otherwise           = ("sNaN(0x" P.++)
  payload x = truncateB (pack x) :: BitVector 22

topEntity
  :: HiddenClockResetEnable System
  => Signal System (Unsigned 8)
  -> Signal System (Bool, (Float, Float, Float))
topEntity =
  fmap $ unpack . asyncRomFile d256
    $(romDataFromFile "samplerom.bin" . P.map pack . addDone $ delayOutput 12 addFloatBasicSamples)
