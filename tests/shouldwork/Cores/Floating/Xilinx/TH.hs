module Xilinx.TH where

import Prelude

import Clash.Cores.Floating.Xilinx.Internal (xilinxNaN)

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
 where
   digits = floatDigits (undefined :: Float)
   (minExp, maxExp) = floatRange (undefined :: Float)
