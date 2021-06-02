{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx.BlackBoxes
where

import           Prelude

import Clash.Backend
-- import Clash.Netlist.BlackBox.Util
-- import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
-- import Clash.Netlist.Util
import Control.Monad.State (State())
-- import Data.Semigroup.Monad (getMon)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
-- import Data.Text as TextS
import Data.Text.Prettyprint.Doc.Extra

addFloatTclTF :: TemplateFunction
addFloatTclTF = binaryFloatTclTF "Add_Subtract" "Add"

subFloatTclTF :: TemplateFunction
subFloatTclTF = binaryFloatTclTF "Add_Subtract" "Subtract"

mulFloatTclTF :: TemplateFunction
mulFloatTclTF = binaryFloatTclTF "Multiply" "Both"

divFloatTclTF :: TemplateFunction
divFloatTclTF = binaryFloatTclTF "Divide" "Both"

expFloatTclTF :: TemplateFunction
expFloatTclTF = unaryFloatTclTF "Exponential"

binaryFloatTclTF
  :: String
  -> String
  -> TemplateFunction
binaryFloatTclTF operType addSubVal =
  TemplateFunction used valid (floatTclTemplate operType addSubVal)
 where
  used = [0..4]
  valid = const True

unaryFloatTclTF
  :: String
  -> TemplateFunction
unaryFloatTclTF operType =
  TemplateFunction used valid (floatTclTemplate operType "Both")
 where
  used = [0..3]
  valid = const True

floatTclTemplate
  :: Backend s
  => String
  -> String
  -> BlackBoxContext
  -> State s Doc
floatTclTemplate operType addSubVal bbCtx = pure bbText
 where
  compName = bbQsysIncName bbCtx !! 0

  (Literal _ (NumLit latency), _, _) = bbInputs bbCtx !! 1
  (DataCon _ _ cfgExprs, _, _) = bbInputs bbCtx !! 2
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
    case bbInputs bbCtx !! 4 of
      (DataCon _ _ [Literal Nothing (BoolLit True)], _, _) -> "false"
      _                                                    -> "true"

  bbText = fromString $ unindent [i|
    create_ip -name floating_point -vendor xilinx.com -library ip \\
              -version 7.1 -module_name {#{compName}}
    set_property -dict [list CONFIG.Operation_Type #{operType} \\
                             CONFIG.Add_Sub_Value #{addSubVal} \\
                             CONFIG.C_Optimization #{tclArchOpt} \\
                             CONFIG.C_Mult_Usage #{tclDspUsage} \\
                             CONFIG.C_BRAM_Usage #{tclBMemUsage} \\
                             CONFIG.Flow_Control NonBlocking \\
                             CONFIG.Has_ACLKEN #{tclClkEn} \\
                             CONFIG.Has_RESULT_TREADY false \\
                             CONFIG.Maximum_Latency false \\
                             CONFIG.C_Latency #{latency}] \\
                       [get_ips {#{compName}}]
    generate_target {synthesis simulation} [get_ips {#{compName}}]
    |]
{-
    Hello world!

    Name:
    #{compName}

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
    |]
-}
