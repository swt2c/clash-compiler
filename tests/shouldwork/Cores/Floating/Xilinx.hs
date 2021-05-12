{-# LANGUAGE NumericUnderscores #-}

module Xilinx where

import Clash.Prelude

import Clash.Explicit.Testbench

import Clash.Cores.Floating.Xilinx

import Xilinx.Annotations

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
  -> Vec n (Float, Float)
  -> Vec n Float
  -> Signal XilinxSystem Bool
basicTB comp inputs expectedOutput = done
  where
    testInputX = fromSignal $ stimuliGenerator clk rst (map fst inputs)
    testInputY = fromSignal $ stimuliGenerator clk rst (map snd inputs)
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
    inputs
    (map (uncurry (+)) inputs)
 where
  inputs = $(listToVecTH
    [ (1, 1) :: (Float, Float)
    , (2, 1)
    , (3, 1)
    ])
{-# ANN addFloatBasicTB (TestBench 'addFloatBasic) #-}
