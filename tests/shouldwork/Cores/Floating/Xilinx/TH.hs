module Xilinx.TH where

import Clash.Prelude (BitVector, KnownNat, unpack)
import Prelude

import Language.Haskell.TH (ExpQ, litE, stringL)
import Language.Haskell.TH.Syntax (qRunIO)

import Clash.Cores.Floating.Xilinx.Internal (xilinxNaN)

createRomFile
  :: KnownNat n
  => FilePath
  -> [BitVector n]
  -> IO ()
createRomFile file = writeFile file . unlines . map (drop 2 . filter (/= '_') . show)

romDataFromFile
  :: KnownNat n
  => FilePath
  -> [BitVector n]
  -> ExpQ
romDataFromFile file es = qRunIO (createRomFile file es) >> litE (stringL file)

addFloatBasicSamples
  :: [(Float, Float, Float)]

addFloatBasicSamples =
  [ (1, 4, 5)
  , (2, 5, 7)
  , (3, 6, 9)
    -- Subnormal positive number is conditioned to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( encodeFloat (-(2 ^ (digits - 1))) (minExp - digits)
    , encodeFloat (2 ^ digits - 1) (minExp - digits)
    , 0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( encodeFloat (-(2 ^ (digits - 1))) (minExp - digits)
    , encodeFloat (2 ^ (digits - 1) + 1) (minExp - digits)
    , 0
    )
    -- Subnormal negative number is conditioned to minus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
  , ( encodeFloat (2 ^ (digits - 1)) (minExp - digits)
    , encodeFloat (-(2 ^ digits - 1)) (minExp - digits)
    , -0
    )
    -- The unconditioned result is the subnormal of smallest magnitude
  , ( encodeFloat (2 ^ (digits - 1)) (minExp - digits)
    , encodeFloat (-(2 ^ (digits - 1) + 1)) (minExp - digits)
    , -0
    )
    -- Round to nearest
    --
    -- For a datatype with 4 bits of precision, the significands align as:
    -- 1000
    --     1001
    -- -------- +
    -- 1001
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
    , 2 ^ (digits - 1) + 1
    )
    -- 1000
    --     01111
    -- --------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ digits - 1) (-digits - 1)
    , 2 ^ (digits - 1)
    )
    -- Ties to even
    --
    -- 1000
    --     1000
    -- -------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1)
    )
    -- Round to nearest
    --
    -- For a datatype with 4 bits of precision, the significands align as:
    -- 1001
    --     1001
    -- -------- +
    -- 1010
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
    , 2 ^ (digits - 1) + 2
    )
    -- 1001
    --     01111
    -- --------- +
    -- 1001
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ digits - 1) (-digits - 1)
    , 2 ^ (digits - 1) + 1
    )
    -- Ties to even
    --
    -- 1001
    --     1000
    -- -------- +
    -- 1010
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1) + 2
    )
    -- Rounding at maximum exponent
    --
    -- 1111
    --     1000
    -- -------- +
    -- infinity
  , ( encodeFloat (2 ^ digits - 1) (maxExp - digits)
    , encodeFloat (2 ^ (digits - 1)) (maxExp - 2*digits)
    , 1/0
    )
    -- 1111
    --     01111
    -- --------- +
    -- 1111
  , ( encodeFloat (2 ^ digits - 1) (maxExp - digits)
    , encodeFloat (2 ^ digits - 1) (maxExp - 2*digits - 1)
    , encodeFloat (2 ^ digits - 1) (maxExp - digits)
    )
  , (1/0, 1, 1/0)
  , (-1/0, 1, -1/0)
  , (1/0, -1/0, xilinxNaN)
  ]
  ++ concatMap testNaN
       [   qNaN0PL
       , negQNaN0PL
       , qNaN1
       , negQNaN1
       , sNaN1
       , negSNaN1
       , qNaNMsb
       , negQNaNMsb
       , sNaNMsb
       , negSNaNMsb
       , qNaNMax
       , negQNaNMax
       , sNaNMax
       , negSNaNMax
       , qNaNR1
       , negQNaNR1
       , sNaNR1
       , negSNaNR1
       , qNaNR2
       , negQNaNR2
       , sNaNR2
       , negSNaNR2
       ]
 where
   digits = floatDigits (undefined :: Float)
   (minExp, maxExp) = floatRange (undefined :: Float)
   testNaN :: Float -> [(Float, Float, Float)]
   testNaN nan =
     [ (nan, 1, xilinxNaN)
     , (1, nan, xilinxNaN)
     , (nan, nan, xilinxNaN)
     ]

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
