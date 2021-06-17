{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx.TH where

import Prelude

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)


-- | The content of an InlinePrimitive, for a binary function in VHDL.
--
-- Note: The BlackBox template includes ~DEVNULL[~LIT[3]] which will ensure the
-- template function (tclTFName argument) gets a fully evaluated FloatingConfig.
vhdlBinaryPrim
  :: String
  -> String
  -> String
  -> String
vhdlBinaryPrim primName funcName tclTFName = unindent [i|
  [ { "BlackBox" :
      { "name"      : "#{primName}"
      , "type"      :
    "#{primName}
       :: ( KnownDomain dom           --     ARG[0]
          , KnownNat d                --     ARG[1]
          , HasCallStack              --     ARG[2]
          )
       => FloatingConfig              --     ARG[3]
       -> Clock dom                   --     ARG[4]
       -> Enable dom                  --     ARG[5]
       -> DSignal dom n Float         -- x , ARG[6]
       -> DSignal dom n Float         -- y , ARG[7]
       -> DSignal dom (n + d) Float"
      , "kind"      : "Declaration"
      , "template"  :
    "-- #{funcName} begin
    ~DEVNULL[~LIT[3]]~GENSYM[#{funcName}][0] : block
      COMPONENT ~INCLUDENAME[0]
        PORT (
          aclk : IN STD_LOGIC;
    ~IF~ISACTIVEENABLE[5]~THEN      aclken : IN STD_LOGIC;
    ~ELSE~FI      s_axis_a_tvalid : IN STD_LOGIC;
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
      ~SYM[2] <= '1' when (~ARG[5]) else '0';
      ~RESULT <= ~FROMBV[~SYM[3]][~TYPO];
      ~GENSYM[addFloat][1] : ~INCLUDENAME[0]
        PORT MAP (
          aclk => ~ARG[4],
    ~IF~ISACTIVEENABLE[5]~THEN      aclken => ~SYM[2],
    ~ELSE~FI      s_axis_a_tvalid => '1',
          s_axis_a_tdata => ~TOBV[~ARG[6]][~TYP[6]],
          s_axis_b_tvalid => '1',
          s_axis_b_tdata => ~TOBV[~ARG[7]][~TYP[7]],
          m_axis_result_tvalid => open,
          m_axis_result_tdata => ~SYM[3]
        );

    end block;
    -- #{funcName} end"
      , "includes"  : [ {"extension": "tcl"
                        ,"name": "floating_point"
                        ,"format": "Haskell"
                        ,"templateFunction": "#{tclTFName}"
                        }
                      ]
      }
    }
  ]
  |]

-- | The content of an InlinePrimitive, for a unary function in VHDL.
--
-- Note: The BlackBox template includes ~DEVNULL[~LIT[3]] which will ensure the
-- template function (tclTFName argument) gets a fully evaluated FloatingConfig.
vhdlUnaryPrim
  :: String
  -> String
  -> String
  -> String
vhdlUnaryPrim primName funcName tclTFName = unindent [i|
  [ { "BlackBox" :
      { "name"      : "#{primName}"
      , "type"      :
    "#{primName}
       :: ( KnownDomain dom           --     ARG[0]
          , KnownNat d                --     ARG[1]
          , HasCallStack              --     ARG[2]
          )
       => FloatingConfig              --     ARG[3]
       -> Clock dom                   --     ARG[4]
       -> Enable dom                  --     ARG[5]
       -> DSignal dom n Float         -- x , ARG[6]
       -> DSignal dom (n + d) Float"
      , "kind"      : "Declaration"
      , "template"  :
    "-- #{funcName} begin
    ~DEVNULL[~LIT[3]]~GENSYM[#{funcName}][0] : block
      COMPONENT ~INCLUDENAME[0]
        PORT (
          aclk : IN STD_LOGIC;
    ~IF~ISACTIVEENABLE[5]~THEN      aclken : IN STD_LOGIC;
    ~ELSE~FI      s_axis_a_tvalid : IN STD_LOGIC;
          s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
          m_axis_result_tvalid : OUT STD_LOGIC;
          m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
      END COMPONENT;
      signal ~GENSYM[clken_std][2]: std_logic;
      signal ~GENSYM[result][3]: std_logic_vector(31 downto 0);
    begin
      ~SYM[2] <= '1' when (~ARG[5]) else '0';
      ~RESULT <= ~FROMBV[~SYM[3]][~TYPO];
      ~GENSYM[addFloat][1] : ~INCLUDENAME[0]
        PORT MAP (
          aclk => ~ARG[4],
    ~IF~ISACTIVEENABLE[5]~THEN      aclken => ~SYM[2],
    ~ELSE~FI      s_axis_a_tvalid => '1',
          s_axis_a_tdata => ~TOBV[~ARG[6]][~TYP[6]],
          m_axis_result_tvalid => open,
          m_axis_result_tdata => ~SYM[3]
        );

    end block;
    -- #{funcName} end"
      , "includes"  : [ {"extension": "tcl"
                        ,"name": "floating_point"
                        ,"format": "Haskell"
                        ,"templateFunction": "#{tclTFName}"
                        }
                      ]
      }
    }
  ]
  |]
