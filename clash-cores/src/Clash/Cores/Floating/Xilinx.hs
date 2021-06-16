{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://www.xilinx.com/support/documentation/ip_documentation/floating_point/v7_1/pg060-floating-point.pdf).

The functions in this module make it possible to use the Xilinx IP in Clash.
Simulation produces bit-identical results to synthesis, and compilation will
instantiate the Xilinx IP. Clash will output a TCL script named
@floating_point@/.../@.tcl@ which needs to be executed in the Vivado project to
create the proper entity.

Most functions allow customization of the Xilinx IP. Valid combinations will
need to be gleaned from, e.g., the Vivado wizard (IP Catalog -> Floating-point).
All IP instantiated by this module always has the following properties:

* Single precision
* Non-blocking
* No reset input
* No optional output fields, no @TLAST@, no @TUSER@.
* @TVALID@ on the inputs is always asserted, @TVALID@ on the output is ignored.
Note that it would appear the Xilinx IP does not use these signals in non-blocking mode anyway.
* 1 cycle per operation (meaning there need not be any dummy cycles between
consecutive inputs)

The latency of the IP is set through the delay argument of the 'DSignal'. Other
customization is done through the 'FloatingConfig' argument of customizable
functions.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The Xilinx IP does not support calculations with subnormal numbers. All
subnormal numbers are rounded to zero on both input and output. Note that this
introduces a slight bias as the larger subnormal numbers are closer to the
smallest normal number, but they are rounded to zero nonetheless!

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from 'defFloatingC' and the maximum
delay for the Xilinx IP with that configuration. That delay is also defined as
a type variable for delay annotation in circuits.
-}

module Clash.Cores.Floating.Xilinx
  ( absFloat
  , addFloat
  , addFloat'
  , AddFloatDefDelay
  , copySignFloat
  , defFloatingC
  , divFloat
  , divFloat'
  , DivFloatDefDelay
  , expFloat
  , expFloat'
  , ExpFloatDefDelay
  , fixedToFloat
  , FloatingArchOpt(..)
  , FloatingConfig(..)
  , FloatingDspUsage(..)
  , floatToFixed
  , fmaFloat
  , mulFloat
  , mulFloat'
  , MulFloatDefDelay
  , negateFloat
  , subFloat
  , subFloat'
  , SubFloatDefDelay
  , xilinxIsNaN
  , xilinxIsInf
  ) where

import Clash.Prelude

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Floating.Xilinx.Internal

-- | Default customization options.
--
-- For those operations that support it, the default options are:
--
-- * Speed-optimized architecture ('SpeedArch').
-- * Full DSP slice usage ('FullDspUsage').
-- * Exponential operator does not use block memory.
defFloatingC :: FloatingConfig
defFloatingC = FloatingConfig
  { floatingArchOpt = SpeedArch
  , floatingDspUsage = FullDspUsage
  , floatingBMemUsage = False
  }

absFloat
  :: Float
  -> Float
absFloat x = unpack $ pack x .&. (-1) `shiftR` 1

negateFloat
  :: Float
  -> Float
negateFloat x = unpack $ pack x `xor` (1 `shiftL` 31)

copySignFloat
  :: Float
  -> Float
  -> Float
copySignFloat x y = unpack $     pack x .&. (-1) `shiftR` 1
                             .|. pack y .&. 1 `shiftL` 31

-- | Floating point addition.
addFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addFloat cfg = withFrozenCallStack $ hideEnable . hideClock $ addFloat# cfg
{-# INLINE addFloat #-}

-- | The default delay for floating point addition with default customization.
type AddFloatDefDelay = 11

-- | Floating point addition with default settings.
addFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + AddFloatDefDelay) Float
addFloat' = withFrozenCallStack $ addFloat defFloatingC
{-# INLINE addFloat' #-}

subFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subFloat cfg = hideEnable $ hideClock $ subFloat# cfg
{-# INLINE subFloat #-}

type SubFloatDefDelay = 11

subFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + SubFloatDefDelay) Float
subFloat' = subFloat defFloatingC
{-# INLINE subFloat' #-}

mulFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulFloat cfg = hideEnable $ hideClock $ mulFloat# cfg
{-# INLINE mulFloat #-}

type MulFloatDefDelay = 8

mulFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + MulFloatDefDelay) Float
mulFloat' = mulFloat defFloatingC
{-# INLINE mulFloat' #-}

divFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divFloat cfg = hideEnable $ hideClock $ divFloat# cfg
{-# INLINE divFloat #-}

type DivFloatDefDelay = 28

divFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + DivFloatDefDelay) Float
divFloat' = divFloat defFloatingC
{-# INLINE divFloat' #-}

expFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
expFloat cfg = hideEnable $ hideClock $ expFloat# cfg

type ExpFloatDefDelay = 20

expFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     )
  => DSignal dom n Float
  -> DSignal dom (n + ExpFloatDefDelay) Float
expFloat' = expFloat defFloatingC

fixedToFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , KnownNat int
     , KnownNat frac
     , 1 <= int
     , 4 <= (int + frac)
     , (int + frac) <= 64
     )
  => DSignal dom n (SFixed int frac)
  -> DSignal dom (n + d) Float
fixedToFloat = undefined

floatToFixed
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , KnownNat int
     , 1 <= int
     , int <= 64
     , KnownNat frac
     , frac <= 32
     )
  => DSignal dom n Float
  -> DSignal dom (n + d) (SFixed int frac)
floatToFixed = undefined

fmaFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
fmaFloat = undefined

-- | More efficient version to check a result for NaN.
--
-- Since Xilinx IP always generates a specific NaN, it can be tested more
-- cheaply than a generic operator that correctly distinguishes all valid
-- transfinite floating point encodings.
xilinxIsNaN
  :: Float
  -> Bool
xilinxIsNaN = undefined

-- | More efficient version to check a result for infinity.
--
-- Since Xilinx IP always generates a specific NaN, infinity can be tested more
-- cheaply than a generic operator that correctly distinguishes all valid
-- transfinite floating point encodings.
xilinxIsInf
  :: Float
  -> Bool
xilinxIsInf = undefined
