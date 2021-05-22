{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Xilinx where

import Clash.Prelude hiding ((^))
import Prelude ((^))
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
  -> (SNat n, FilePath)
  -> (Signal dom Bool, Signal dom a)
playSampleRom clk rst (n, file) = (done, out)
 where
  out = unpack . asyncRomFile n file <$> cnt
  done = CEP.register clk rst enableGen False $ (== maxBound) <$> cnt
  cnt :: Signal dom (Index n)
  cnt = CEP.register clk rst enableGen 0 $ satSucc SatBound <$> cnt

basicTB
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
  -> (SNat n, FilePath)
  -> Signal XilinxSystem Bool
basicTB comp (n, sampleFile) = done
  where
    (done0, samples) = playSampleRom clk rst (n, sampleFile)
    (inputX, inputY, expectedOutput) = unbundle samples
    -- Only assert while not finished
    done = mux done0 done0
      $ assert clk rst "basicTB" out (fmap FloatVerifier expectedOutput) done0
    out = fmap FloatVerifier $ ignoreFor clk rst en (SNat @d) 0
      $ toSignal $ comp clk (fromSignal inputX) (fromSignal inputY)
    clk = tbClockGen @XilinxSystem (not <$> done)
    rst = resetGen @XilinxSystem
    en = enableGen
{-# INLINE basicTB #-}

-- TODO: Determine whether it will be useful to run this test bench as anything
-- other than addFloat. If not, remove the genericity.
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
    d = natToNum @d
    testInput =
      fromSignal $ stimuliGenerator clk rst $ iterateI @(d + 14) succ 1
    en =
      toEnable $ stimuliGenerator clk rst
          (   (repeat @d True ++ replicate d4 True ++ replicate d4 False)
           :< True)
    -- XXX: Whoops! Accidentally hardcoded delay 12 in expectedOutput. Compute
    -- based on @d instead!
    expectedOutput =
         repeat @d 0
      ++ map f (iterate d4 succ 1)
      ++ replicate d5 (f 5)
      ++ map f (iterateI @(d + 5) succ 6)
      ++ map f (iterate d5 succ (d + 9))
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

{-
addFloatBasicSamples
  :: Vec _ (Float, Float, Float)
addFloatBasicSamples =
--      (1, 4, 5)
--   :> (2, 5, 7)
--   :> (3, 6, 9)
--      -- Subnormal positive number is conditioned to plus zero
--      --
--      -- The unconditioned result is the subnormal of largest magnitude
--   :> ( encodeFloat (-(2 ^ (digits - 1))) (minExp - digits)
--      , encodeFloat (2 ^ digits - 1) (minExp - digits)
--      , 0
--      )
--      -- The unconditioned result is the subnormal of smallest magnitude
--   :> ( encodeFloat (-(2 ^ (digits - 1))) (minExp - digits)
--      , encodeFloat (2 ^ (digits - 1) + 1) (minExp - digits)
--      , 0
--      )
--      -- Subnormal negative number is conditioned to minus zero
--      --
--      -- The unconditioned result is the subnormal of largest magnitude
--   :> ( encodeFloat (2 ^ (digits - 1)) (minExp - digits)
--      , encodeFloat (-(2 ^ digits - 1)) (minExp - digits)
--      , -0
--      )
--      -- The unconditioned result is the subnormal of smallest magnitude
--   :> ( encodeFloat (2 ^ (digits - 1)) (minExp - digits)
--      , encodeFloat (-(2 ^ (digits - 1) + 1)) (minExp - digits)
--      , -0
--      )
--      -- Round to nearest
--      --
--      -- For a datatype with 4 bits of precision, the significands align as:
--      -- 1000
--      --     1001
--      -- -------- +
--      -- 1001
--   :> ( 2 ^ (digits - 1)
--      , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
--      , 2 ^ (digits - 1) + 1
--      )
--      -- 1000
--      --     01111
--      -- --------- +
--      -- 1000
--   :> ( 2 ^ (digits - 1)
--      , encodeFloat (2 ^ digits - 1) (-digits - 1)
--      , 2 ^ (digits - 1)
--      )
--      -- Ties to even
--      --
--      -- 1000
--      --     1000
--      -- -------- +
--      -- 1000
--   :> ( 2 ^ (digits - 1)
--      , encodeFloat (2 ^ (digits - 1)) (-digits)
--      , 2 ^ (digits - 1)
--      )
--      -- Round to nearest
--      --
--      -- For a datatype with 4 bits of precision, the significands align as:
--      -- 1001
--      --     1001
--      -- -------- +
--      -- 1010
--   :> ( 2 ^ (digits - 1) + 1
--      , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
--      , 2 ^ (digits - 1) + 2
--      )
--      -- 1001
--      --     01111
--      -- --------- +
--      -- 1001
--   :> ( 2 ^ (digits - 1) + 1
--      , encodeFloat (2 ^ digits - 1) (-digits - 1)
--      , 2 ^ (digits - 1) + 1
--      )
--      -- Ties to even
--      --
--      -- 1001
--      --     1000
--      -- -------- +
--      -- 1010
--   :> ( 2 ^ (digits - 1) + 1
--      , encodeFloat (2 ^ (digits - 1)) (-digits)
--      , 2 ^ (digits - 1) + 2
--      )
--      -- Rounding at maximum exponent
--      --
--      -- 1111
--      --     1000
--      -- -------- +
--      -- infinity
--   :> ( encodeFloat (2 ^ digits - 1) (maxExp - digits)
--      , encodeFloat (2 ^ (digits - 1)) (maxExp - 2*digits)
--      , 1/0
--      )
--      -- 1111
--      --     01111
--      -- --------- +
--      -- 1111
--   :> ( encodeFloat (2 ^ digits - 1) (maxExp - digits)
--      , encodeFloat (2 ^ digits - 1) (maxExp - 2*digits - 1)
--      , encodeFloat (2 ^ digits - 1) (maxExp - digits)
--      )
--   :> (1/0, 1, 1/0)
--   :> (-1/0, 1, -1/0)
--   :> (1/0, -1/0, xilinxNaN)
--   :> (qNaN0PL, 1, xilinxNaN)
--   :> (1, qNaN0PL, xilinxNaN)
--   :> (qNaN0PL, qNaN0PL,xilinxNaN)
--   :> (negQNaN0PL, 1, xilinxNaN)
--   :> (1, negQNaN0PL, xilinxNaN)
--   :> (negQNaN0PL, negQNaN0PL,xilinxNaN)
--   :> (qNaN1, 1, xilinxNaN)
--   :> (1, qNaN1, xilinxNaN)
--   :> (qNaN1, qNaN1,xilinxNaN)
--   :> (negQNaN1, 1, xilinxNaN)
--   :> (1, negQNaN1, xilinxNaN)
--   :> (negQNaN1, negQNaN1,xilinxNaN)
    (sNaN1, 1, xilinxNaN)
  :> (1, sNaN1, xilinxNaN)
  :> (sNaN1, sNaN1,xilinxNaN)
  :> (negSNaN1, 1, xilinxNaN)
  :> (1, negSNaN1, xilinxNaN)
  :> (negSNaN1, negSNaN1,xilinxNaN)
  :> (qNaNMsb, 1, xilinxNaN)
  :> (1, qNaNMsb, xilinxNaN)
  :> (qNaNMsb, qNaNMsb,xilinxNaN)
  :> (negQNaNMsb, 1, xilinxNaN)
  :> (1, negQNaNMsb, xilinxNaN)
  :> (negQNaNMsb, negQNaNMsb,xilinxNaN)
  :> (sNaNMsb, 1, xilinxNaN)
  :> (1, sNaNMsb, xilinxNaN)
  :> (sNaNMsb, sNaNMsb,xilinxNaN)
  :> (negSNaNMsb, 1, xilinxNaN)
  :> (1, negSNaNMsb, xilinxNaN)
  :> (negSNaNMsb, negSNaNMsb,xilinxNaN)
--   :> (qNaNMax, 1, xilinxNaN)
--   :> (1, qNaNMax, xilinxNaN)
--   :> (qNaNMax, qNaNMax,xilinxNaN)
--   :> (negQNaNMax, 1, xilinxNaN)
--   :> (1, negQNaNMax, xilinxNaN)
--   :> (negQNaNMax, negQNaNMax,xilinxNaN)
--   :> (sNaNMax, 1, xilinxNaN)
--   :> (1, sNaNMax, xilinxNaN)
--   :> (sNaNMax, sNaNMax,xilinxNaN)
--   :> (negSNaNMax, 1, xilinxNaN)
--   :> (1, negSNaNMax, xilinxNaN)
--   :> (negSNaNMax, negSNaNMax,xilinxNaN)
--   :> (qNaNR1, 1, xilinxNaN)
--   :> (1, qNaNR1, xilinxNaN)
--   :> (qNaNR1, qNaNR1,xilinxNaN)
--   :> (negQNaNR1, 1, xilinxNaN)
--   :> (1, negQNaNR1, xilinxNaN)
--   :> (negQNaNR1, negQNaNR1,xilinxNaN)
--   :> (sNaNR1, 1, xilinxNaN)
--   :> (1, sNaNR1, xilinxNaN)
--   :> (sNaNR1, sNaNR1,xilinxNaN)
--   :> (negSNaNR1, 1, xilinxNaN)
--   :> (1, negSNaNR1, xilinxNaN)
--   :> (negSNaNR1, negSNaNR1,xilinxNaN)
--   :> (qNaNR2, 1, xilinxNaN)
--   :> (1, qNaNR2, xilinxNaN)
--   :> (qNaNR2, qNaNR2,xilinxNaN)
--   :> (negQNaNR2, 1, xilinxNaN)
--   :> (1, negQNaNR2, xilinxNaN)
--   :> (negQNaNR2, negQNaNR2,xilinxNaN)
--   :> (sNaNR2, 1, xilinxNaN)
--   :> (1, sNaNR2, xilinxNaN)
--   :> (sNaNR2, sNaNR2,xilinxNaN)
--   :> (negSNaNR2, 1, xilinxNaN)
--   :> (1, negSNaNR2, xilinxNaN)
--   :> (negSNaNR2, negSNaNR2,xilinxNaN)
  :> Nil
 where
   digits = floatDigits (undefined :: Float)
   (minExp, maxExp) = floatRange (undefined :: Float)
{-
++ concatMap testNaN
       (   qNaN0PL
--         :> negQNaN0PL
--         :> qNaN1
--         :> negQNaN1
--         :> sNaN1
--         :> negSNaN1
--         :> qNaNMsb
--         :> negQNaNMsb
--         :> sNaNMsb
--         :> negSNaNMsb
--         :> qNaNMax
--         :> negQNaNMax
--         :> sNaNMax
--         :> negSNaNMax
--         :> qNaNR1
--         :> negQNaNR1
--         :> sNaNR1
--         :> negSNaNR1
--         :> qNaNR2
--         :> negQNaNR2
--         :> sNaNR2
--         :> negSNaNR2
        :> Nil)
   testNaN :: Float -> Vec 3 (Float, Float, Float)
   testNaN nan =
        (nan, 1, xilinxNaN)
     :> (1, nan, xilinxNaN)
     :> (nan, nan, xilinxNaN)
     :> Nil
-}
-}

addFloatBasicTB :: Signal XilinxSystem Bool
addFloatBasicTB = basicTB addFloatBasic $(romDataFromFile "samplerom.bin" addFloatBasicSamples)
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

-- addFloatEnableTB :: Signal XilinxSystem Bool
-- addFloatEnableTB = enableTB addFloatEnable (+)
-- {-# ANN addFloatEnableTB (TestBench 'addFloatEnable) #-}

-- Quiet NaN with no payload
-- Actually, this is equal to xilinxNaN
qNaN0PL :: Float
qNaN0PL
  = unpack 0b0111_1111_1100_0000_0000_0000_0000_0000

-- Negative version
negQNaN0PL :: Float
negQNaN0PL
  = unpack 0b1111_1111_1100_0000_0000_0000_0000_0000

-- Quiet NaN with payload 1
qNaN1 :: Float
qNaN1
  = unpack 0b0111_1111_1100_0000_0000_0000_0000_0001

-- Negative version
negQNaN1 :: Float
negQNaN1
  = unpack 0b1111_1111_1100_0000_0000_0000_0000_0001

-- Signaling NaN with payload 1
sNaN1 :: Float
sNaN1
  = unpack 0b0111_1111_1000_0000_0000_0000_0000_0001

-- Negative version
negSNaN1 :: Float
negSNaN1
  = unpack 0b1111_1111_1000_0000_0000_0000_0000_0001

-- Quiet NaN with payload with only MSB set
qNaNMsb :: Float
qNaNMsb
  = unpack 0b0111_1111_1110_0000_0000_0000_0000_0000

-- Negative version
negQNaNMsb :: Float
negQNaNMsb
  = unpack 0b1111_1111_1110_0000_0000_0000_0000_0000

-- Signaling NaN with payload with only MSB set
sNaNMsb :: Float
sNaNMsb
  = unpack 0b0111_1111_1010_0000_0000_0000_0000_0000

-- Negative version
negSNaNMsb :: Float
negSNaNMsb
  = unpack 0b1111_1111_1010_0000_0000_0000_0000_0000

-- Quiet NaN with maximum-valued payload
qNaNMax :: Float
qNaNMax
  = unpack 0b0111_1111_1111_1111_1111_1111_1111_1111

-- Negative version
negQNaNMax :: Float
negQNaNMax
  = unpack 0b1111_1111_1111_1111_1111_1111_1111_1111

-- Signaling NaN with maximum-valued payload
sNaNMax :: Float
sNaNMax
  = unpack 0b0111_1111_1011_1111_1111_1111_1111_1111

-- Negative version
negSNaNMax :: Float
negSNaNMax
  = unpack 0b1111_1111_1011_1111_1111_1111_1111_1111

-- Quiet NaN with random payload
qNaNR1 :: Float
qNaNR1
  = unpack 0b0111_1111_1110_0000_1011_0001_0011_1100

-- Negative version
negQNaNR1 :: Float
negQNaNR1
  = unpack 0b1111_1111_1110_0000_1011_0001_0011_1100

-- Signaling NaN with random payload
sNaNR1 :: Float
sNaNR1
  = unpack 0b0111_1111_1010_0000_1011_0001_0011_1100

-- Negative version
negSNaNR1 :: Float
negSNaNR1
  = unpack 0b1111_1111_1010_0000_1011_0001_0011_1100

-- Quiet NaN with random payload
qNaNR2 :: Float
qNaNR2
  = unpack 0b0111_1111_1100_0010_0011_0000_1110_0101

-- Negative version
negQNaNR2 :: Float
negQNaNR2
  = unpack 0b1111_1111_1100_0010_0011_0000_1110_0101

-- Signaling NaN with random payload
sNaNR2 :: Float
sNaNR2
  = unpack 0b0111_1111_1000_0010_0011_0000_1110_0101

-- Negative version
negSNaNR2 :: Float
negSNaNR2
  = unpack 0b1111_1111_1000_0010_0011_0000_1110_0101
