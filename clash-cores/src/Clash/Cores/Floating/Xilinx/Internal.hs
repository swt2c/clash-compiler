{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx.Internal
where

import           Clash.Explicit.Prelude

import           Clash.Annotations.Primitive
import           Data.String.Interpolate.IsString (i)
import           Data.String.Interpolate.Util    (unindent)

data FloatingConfig = FloatingConfig
  { floatingArchOpt :: !FloatingArchOpt
  , floatingDspUsage :: !FloatingDspUsage
  , floatingBMemUsage :: !Bool
  } deriving (Eq, Show)

data FloatingArchOpt
  = SpeedArch
  | LatencyArch
  deriving (Eq, Show)

data FloatingDspUsage
  = NoDspUsage
  | MediumDspUsage
  | FullDspUsage
  deriving (Eq, Show)

addFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addFloat# !_ clk en x y = delayI undefined en clk $ x + y
{-# ANN addFloat# (InlinePrimitive [VHDL] $ unindent [i|
  [ { "BlackBox" :
      { "name"      : "Clash.Cores.Floating.Xilinx.Internal.addFloat#"
      , "type"      :
    "addFloat
       :: ( KnownDomain dom           --     ARG[0]
          , KnownNat d                --     ARG[1]
          )
       => FloatingConfig              --     ARG[2]
       -> Clock dom                   --     ARG[3]
       -> Enable dom                  --     ARG[4]
       -> DSignal dom n Float         -- a , ARG[5]
       -> DSignal dom n Float         -- b , ARG[6]
       -> DSignal dom (n + d) Float"
      , "kind"      : "Expression"
      , "template"  : "~DEVNULL[~LIT[2]]~ARG[5] + ~ARG[6]"
      , "includes"  : [ {"extension": "tcl"
                        ,"name": "floating_point"
                        ,"format": "Haskell"
                        ,"templateFunction": "Clash.Cores.Floating.Xilinx.BlackBoxes.addFloatTclTF"
                        }
                      ]
      }
    }
  ]
  |]) #-}
{-# NOINLINE addFloat# #-}
