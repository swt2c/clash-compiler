{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Xilinx where

import Clash.Prelude
import qualified Prelude as P
import qualified Clash.Explicit.Prelude as CEP

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
  | isNaN x = nanSign . nanString . showHex payload . (')':)
  | otherwise = shows x
 where
  nanSign | msb (pack x) == 0 = ('+':)
          | otherwise         = ('-':)
  nanString
    | testBit (pack x) 22 = ("qNaN(0x" P.++)
    | otherwise           = ("sNaN(0x" P.++)
  payload = truncateB $ pack x :: BitVector 22

playSampleRom
  :: forall n a dom
   . ( KnownDomain dom
     , KnownNat n
     , BitPack a
     , 1 <= n
     )
  => Clock dom
  -> Reset dom
  -> SNat n
  -> FilePath
  -> (Signal dom Bool, Signal dom a)
playSampleRom clk rst n file = (done, out)
 where
  out = unpack . asyncRomFile n file <$> cnt
  done = CEP.register clk rst enableGen False $ (== maxBound) <$> cnt
  cnt :: Signal dom (Index n)
  cnt = CEP.register clk rst enableGen 0 $ satSucc SatBound <$> cnt

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

basicRomTB
  :: forall d n
   . ( KnownNat n
     , KnownNat d
     , 1 <= n
     )
  => (   Clock XilinxSystem
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem 0 Float
      -> DSignal XilinxSystem d Float
     )
  -> SNat n
  -> FilePath
  -> Signal XilinxSystem Bool
basicRomTB comp n sampleFile = done
  where
    (done0, samples) = playSampleRom clk rst n sampleFile
    (inputX, inputY, expectedOutput) = unbundle samples
    -- Only assert while not finished
    done = mux done0 done0
      $ assert clk rst "basicRomTB" out (fmap FloatVerifier expectedOutput) done0
    out = fmap FloatVerifier $ ignoreFor clk rst en (SNat @d) 0
      $ toSignal $ comp clk (fromSignal inputX) (fromSignal inputY)
    clk = tbClockGen @XilinxSystem (not <$> done)
    rst = resetGen @XilinxSystem
    en = enableGen
{-# INLINE basicRomTB #-}

addFloatBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem AddFloatDefDelay Float
addFloatBasic clk x y
  = withClock clk $ withEnable enableGen $ addFloat' x y
{-# NOINLINE addFloatBasic #-}
{-# ANN addFloatBasic (binTopAnn "addFloatBasic") #-}

addFloatBasicTB :: Signal XilinxSystem Bool
addFloatBasicTB =
  uncurry (basicRomTB addFloatBasic)
          $(romDataFromFile "samplerom.bin" addFloatBasicSamples)
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
addFloatEnableTB = done
 where
  testInput =
    fromSignal $ stimuliGenerator clk rst $ $(listToVecTH [1 :: Float .. 25])
  en =
    toEnable $ stimuliGenerator clk rst
        (   (replicate d12 True ++ replicate d4 True ++ replicate d4 False)
         :< True)
  expectedOutput =
       replicate d12 0
    ++ $(listToVecTH . P.map (\i -> i + i) $
                [1 :: Float .. 4]
           -- Stall for four cycles
           P.++ P.replicate 4 5
           -- Still in the pipeline (12 deep) from before the stall.
           P.++ P.take 12 [5 .. 30]
           -- We "lose" four samples of what remains due to not being enabled
           -- for those inputs.
           P.++ P.drop 4 (P.drop 12 [5 .. 25])
        )
  expectOutput =
    outputVerifier' clk rst expectedOutput
  done = expectOutput $ ignoreFor clk rst enableGen d12 0
    $ toSignal $ addFloatEnable clk en testInput testInput
  clk = tbClockGen @XilinxSystem (not <$> done)
  rst = resetGen @XilinxSystem
{-# ANN addFloatEnableTB (TestBench 'addFloatEnable) #-}

addFloatShortPL
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 6 Float
addFloatShortPL clk x y
  = withClock clk $ withEnable enableGen $ addFloat defFloatingC x y
{-# NOINLINE addFloatShortPL #-}
{-# ANN addFloatShortPL (binTopAnn "addFloatShortPL") #-}

addFloatShortPLTB :: Signal XilinxSystem Bool
addFloatShortPLTB =
  basicTB addFloatShortPL
    $(listToVecTH [ (1, 4, 5) :: (Float, Float, Float)
                  , (2, 5, 7)
                  , (3, 6, 9)
                  ])
{-# ANN addFloatShortPLTB (TestBench 'addFloatShortPL) #-}
