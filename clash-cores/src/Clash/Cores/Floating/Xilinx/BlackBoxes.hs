{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.Floating.Xilinx.BlackBoxes
  ( addFloatTclTF
  , divFloatTclTF
  , expFloatTclTF
  , mulFloatTclTF
  , subFloatTclTF
  ) where

import Prelude

import Control.Monad.State (State())
import Data.Maybe (isJust, fromJust)
-- import Data.Semigroup.Monad (getMon)
import Data.String (fromString)
import Data.String.Interpolate (i)
-- import Data.String.Interpolate.Util (unindent)
-- import Data.Text as TextS
import Data.Text.Prettyprint.Doc.Extra

import Clash.Backend
-- import Clash.Netlist.BlackBox.Util
-- import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
-- import Clash.Netlist.Util

data HasCustom = HasCustom
  { addSubVal ::  !(Maybe String)
  , hasArchOpt :: !Bool
  , hasDspUsage :: !Bool
  , hasBMemUsage :: !Bool
  }

defHasCustom :: HasCustom
defHasCustom = HasCustom
  { addSubVal = Nothing
  , hasArchOpt = True
  , hasDspUsage = True
  , hasBMemUsage = False
  }

hasNoCustom :: HasCustom
hasNoCustom = HasCustom
  { addSubVal = Nothing
  , hasArchOpt = False
  , hasDspUsage = False
  , hasBMemUsage = False
  }

addFloatTclTF :: TemplateFunction
addFloatTclTF =
  binaryFloatTclTF (defHasCustom { addSubVal = Just "Add" }) "Add_Subtract"

subFloatTclTF :: TemplateFunction
subFloatTclTF =
  binaryFloatTclTF (defHasCustom { addSubVal = Just "Subtract" }) "Add_Subtract"

mulFloatTclTF :: TemplateFunction
mulFloatTclTF = binaryFloatTclTF hasCustom "Multiply"
 where
  hasCustom = HasCustom { addSubVal = Nothing
                        , hasArchOpt = False
                        , hasDspUsage = True
                        , hasBMemUsage = False
                        }

divFloatTclTF :: TemplateFunction
divFloatTclTF = binaryFloatTclTF hasNoCustom "Divide"

expFloatTclTF :: TemplateFunction
expFloatTclTF = unaryFloatTclTF hasCustom "Exponential"
 where
  hasCustom = HasCustom { addSubVal = Nothing
                        , hasArchOpt = False
                        , hasDspUsage = True
                        , hasBMemUsage = True
                        }

binaryFloatTclTF
  :: HasCustom
  -> String
  -> TemplateFunction
binaryFloatTclTF hasCustom operType =
  TemplateFunction used valid (floatTclTemplate hasCustom operType)
 where
  used = [0..4]
  valid = const True

unaryFloatTclTF
  :: HasCustom
  -> String
  -> TemplateFunction
unaryFloatTclTF hasCustom operType =
  TemplateFunction used valid (floatTclTemplate hasCustom operType)
 where
  used = [0..3]
  valid = const True

floatTclTemplate
  :: Backend s
  => HasCustom
  -> String
  -> BlackBoxContext
  -> State s Doc
floatTclTemplate (HasCustom {..}) operType bbCtx = pure bbText
 where
  compName = bbQsysIncName bbCtx !! 0

  (Literal _ (NumLit latency), _, _) = bbInputs bbCtx !! 1
  (DataCon _ _ cfgExprs, _, _) = bbInputs bbCtx !! 3
  cfgArchOptExpr = cfgExprs !! 0
  cfgDspUsageExpr = cfgExprs !! 1
  cfgBMemUsageExpr = cfgExprs !! 2

  DataCon _ (DC (Sum _ cfgArchOptConstrs, cfgArchOptTag)) _ = cfgArchOptExpr
  cfgArchOpt = cfgArchOptConstrs !! cfgArchOptTag
  tclArchOpt :: String
  tclArchOpt =
    case cfgArchOpt of
      "Clash.Cores.Floating.Xilinx.Internal.SpeedArch" -> "Speed_Optimized"
      "Clash.Cores.Floating.Xilinx.Internal.LatencyArch" -> "Low_Latency"
      _ -> error "Unknown FloatingArchOpt constructor"

  DataCon _ (DC (Sum _ cfgDspUsageConstrs, cfgDspUsageTag)) _ = cfgDspUsageExpr
  cfgDspUsage = cfgDspUsageConstrs !! cfgDspUsageTag
  tclDspUsage :: String
  tclDspUsage =
    case cfgDspUsage of
      "Clash.Cores.Floating.Xilinx.Internal.NoDspUsage" -> "No_Usage"
      "Clash.Cores.Floating.Xilinx.Internal.MediumDspUsage" -> "Medium_Usage"
      "Clash.Cores.Floating.Xilinx.Internal.FullDspUsage" -> "Full_Usage"
      "Clash.Cores.Floating.Xilinx.Internal.MaxDspUsage" -> "Max_Usage"
      _ -> error "Unknown FloatingDspUsage constructor"

  Literal Nothing (BoolLit cfgBMemUsage) = cfgBMemUsageExpr
  tclBMemUsage :: String
  tclBMemUsage | cfgBMemUsage = "Full_Usage"
               | otherwise    = "No_Usage"

  tclClkEn :: String
  tclClkEn =
    case bbInputs bbCtx !! 5 of
      (DataCon _ _ [Literal Nothing (BoolLit True)], _, _) -> "false"
      _                                                    -> "true"

  props =
    foldr prop ""
      [ (True, "CONFIG.Operation_Type", operType)
      , (isJust addSubVal, "CONFIG.Add_Sub_Value", fromJust addSubVal)
      , (hasArchOpt, "CONFIG.C_Optimization", tclArchOpt)
      , (hasDspUsage, "CONFIG.C_Mult_Usage", tclDspUsage)
      , (hasBMemUsage, "CONFIG.C_BRAM_Usage", tclBMemUsage)
      , (True, "CONFIG.Flow_Control", "NonBlocking")
      , (True, "CONFIG.Has_ACLKEN", tclClkEn)
      , (True, "CONFIG.Has_RESULT_TREADY", "false")
      , (True, "CONFIG.Maximum_Latency", "false")
      , (True, "CONFIG.C_Latency", show latency)
      ]
  prop (False, _, _) s = s
  prop (True, pName, pValue) s =
    replicate 25 ' ' ++ pName ++ ' ': pValue ++ " \\\n" ++ s

  bbText =
    fromString [i|create_ip -name floating_point -vendor xilinx.com -library ip \\
          -version 7.1 -module_name {#{compName}}
set_property -dict [list \\
#{props}                   ] \\
                   [get_ips {#{compName}}]
generate_target {synthesis simulation} [get_ips {#{compName}}]
# set_property GENERATE_SYNTH_CHECKPOINT FALSE \\
#     [get_files [get_property IP_FILE [get_ips {#{compName}}]]]
|]
{-
    Hello world!

    Name:
    #{compName}

    bbInputs 0:
    #{bbInputs bbCtx !! 0}

    bbInputs 1:
    #{bbInputs bbCtx !! 1}

    bbInputs 2:
    #{bbInputs bbCtx !! 2}

    bbInputs 3:
    #{bbInputs bbCtx !! 3}
    Latency:
    #{latency}

    ArchOpt:
    #{cfgArchOpt}

    DspUsage:
    #{cfgDspUsage}

    BMemUsage:
    #{cfgBMemUsage}

    bbName:
    #{bbName bbCtx}

    bbResults:
    #{bbResults bbCtx}

    bbFunctions:
    #{bbFunctions bbCtx}

    bbQsysIncName:

    bbCompName:
    #{bbCompName bbCtx}

    bbCtxName:
    #{bbCtxName bbCtx}
-}
