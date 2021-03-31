{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx
  ( module Clash.Cores.Floating.Xilinx
  , module Clash.Cores.Floating.Xilinx.Internal
  )
where

import           Clash.Prelude

import           Clash.Cores.Floating.Xilinx.Internal

defFloatingC :: FloatingConfig
defFloatingC = FloatingConfig
  { floatingArchOpt = SpeedArch
  , floatingDspUsage = NoDspUsage
  , floatingBMemUsage = False
  }

absFloat
  :: KnownDomain dom
  => Signal dom Float
  -> Signal dom Float
absFloat = undefined

negateFloat
  :: KnownDomain dom
  => Signal dom Float
  -> Signal dom Float
negateFloat = undefined

copySignFloat
  :: KnownDomain dom
  => Signal dom Float
  -> Signal dom Float
  -> Signal dom Float
copySignFloat = undefined

addFloat
  :: forall d dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addFloat cfg = hideEnable $ hideClock $ addFloat# cfg
{-# INLINE addFloat #-}

addFloat'
  :: ( HiddenClock dom
     , HiddenEnable dom
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + 12) Float
addFloat' = addFloat defFloatingC
{-# INLINE addFloat' #-}

divFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divFloat = undefined

expFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     )
  => FloatingConfig
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
expFloat = undefined

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
