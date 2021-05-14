{-# LANGUAGE NumericUnderscores #-}

module Xilinx where

import Clash.Prelude
import qualified Prelude as P

import Clash.Explicit.Testbench

import Clash.Cores.Floating.Xilinx

import Xilinx.Annotations
import Xilinx.TH

basicTB
  :: forall n d
   . ( KnownNat n
     , KnownNat d
     )
  => (   Clock XilinxSystem
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
      $ toSignal $ comp clk testInputX testInputY
    clk = tbClockGen @XilinxSystem (not <$> done)
    rst = resetGen @XilinxSystem
    en = enableGen
{-# INLINE basicTB #-}

enableTB
  :: forall d
   . KnownNat d
  => (   Clock XilinxSystem
      -> Enable XilinxSystem
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem d Float
     )
  -> (Float -> Float -> Float)
  -> Signal XilinxSystem Bool
enableTB comp f0 = done
  where
    f i = f0 i i
    testInput =
      fromSignal $ stimuliGenerator clk rst
          $(listToVecTH [1 :: Float .. 30])
    en =
      toEnable $ stimuliGenerator clk rst
          (   (repeat @d True ++ replicate d4 True ++ replicate d4 False)
           :< True)
    -- XXX: Whoops! Accidentally hardcoded delay 12 in expectedOutput. Compute
    -- based on @d instead!
    expectedOutput =
         repeat @d 0
      ++ map f $(listToVecTH [1 :: Float .. 4])
      ++ replicate d5 (f 5)
      ++ map f $(listToVecTH [6 :: Float .. 16])
      ++ map f $(listToVecTH [21 :: Float .. 30])
    expectOutput =
      outputVerifier' clk rst expectedOutput
    done = expectOutput $ ignoreFor clk rst enableGen (SNat @d) 0
      $ toSignal $ comp clk en testInput testInput
    clk = tbClockGen @XilinxSystem (not <$> done)
    rst = resetGen @XilinxSystem
{-# INLINE enableTB #-}

addFloatBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 12 Float
addFloatBasic clk x y
  = withClock clk $ withEnable enableGen $ addFloat' x y
{-# NOINLINE addFloatBasic #-}
{-# ANN addFloatBasic (binTopAnn "addFloatBasic") #-}

addFloatBasicTB :: Signal XilinxSystem Bool
addFloatBasicTB = basicTB addFloatBasic $(listToVecTH addFloatBasicSamples)
{-# ANN addFloatBasicTB (TestBench 'addFloatBasic) #-}

addFloatEnable
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 12 Float
addFloatEnable clk en x y
  = withClock clk $ withEnable en $ addFloat' x y
{-# NOINLINE addFloatEnable #-}
{-# ANN addFloatEnable (binEnTopAnn "addFloatEnable") #-}

addFloatEnableTB :: Signal XilinxSystem Bool
addFloatEnableTB = enableTB addFloatEnable (+)
{-# ANN addFloatEnableTB (TestBench 'addFloatEnable) #-}
