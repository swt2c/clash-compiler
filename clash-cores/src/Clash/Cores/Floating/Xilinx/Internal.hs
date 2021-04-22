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
      , "kind"      : "Declaration"
      , "template"  :
    "-- addFloat begin
    ~DEVNULL[~LIT[2]]~GENSYM[addFloat][0] : block
      COMPONENT ~INCLUDENAME[0]
        PORT (
          aclk : IN STD_LOGIC;
          aclken : IN STD_LOGIC;
          s_axis_a_tvalid : IN STD_LOGIC;
          s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
          s_axis_b_tvalid : IN STD_LOGIC;
          s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
          m_axis_result_tvalid : OUT STD_LOGIC;
          m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
      END COMPONENT;
      signal ~GENSYM[clken_std][2]: std_logic;
      signal ~GENSYM[result][3]: std_logic_vector(31 downto 0);
    begin
      ~SYM[2] <= '1' when (~ARG[4]) else '0';
      ~RESULT <= ~FROMBV[~SYM[3]][~TYPO];
      ~GENSYM[addFloat][1] : ~INCLUDENAME[0]
        PORT MAP (
          aclk => ~ARG[3],
          aclken => ~SYM[2],
          s_axis_a_tvalid => '1',
          s_axis_a_tdata => ~TOBV[~ARG[5]][~TYP[5]],
          s_axis_b_tvalid => '1',
          s_axis_b_tdata => ~TOBV[~ARG[6]][~TYP[6]],
          m_axis_result_tvalid => open,
          m_axis_result_tdata => ~SYM[3]
        );

    end block;
    -- addFloat end"
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
