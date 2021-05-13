{-# LANGUAGE NumericUnderscores #-}

module Xilinx where

import Clash.Prelude
import qualified Prelude as P

import Clash.Explicit.Testbench

import Clash.Cores.Floating.Xilinx

import Xilinx.Annotations
import Xilinx.TH

addFloatBasic
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 12 Float
addFloatBasic clk en x y
  = withClock clk $ withEnable en $ addFloat' x y
{-# NOINLINE addFloatBasic #-}
{-# ANN addFloatBasic (binTopAnn "addFloatBasic") #-}

basicTB
  :: forall n d
   . ( KnownNat n
     , KnownNat d
     )
  => (  Clock XilinxSystem
      -> Enable XilinxSystem
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem d Float
     )
  -> Vec n (Float, Float, Float)
  -> Signal XilinxSystem Bool
basicTB comp samples = done
  where
    (inputX, inputY, expectedOutput) = unzip3 samples
    testInputX = fromSignal $ stimuliGenerator clk rst inputX
    testInputY = fromSignal $ stimuliGenerator clk rst inputY
    expectOutput = outputVerifier' clk rst (repeat @d 0 ++ expectedOutput)
    done = expectOutput $ ignoreFor clk rst en (SNat @d) 0
      $ toSignal $ comp clk en testInputX testInputY
    clk = tbClockGen @XilinxSystem (not <$> done)
    rst = resetGen @XilinxSystem
    en = enableGen
{-# INLINE basicTB #-}


addFloatBasicTB :: Signal XilinxSystem Bool
addFloatBasicTB =
  basicTB
    addFloatBasic
    $(listToVecTH addFloatBasicSamples)
{-# ANN addFloatBasicTB (TestBench 'addFloatBasic) #-}
