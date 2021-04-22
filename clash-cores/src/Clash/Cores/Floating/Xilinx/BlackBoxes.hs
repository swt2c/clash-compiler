{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx.BlackBoxes
where

import           Prelude

import           Clash.Backend
-- import           Clash.Netlist.BlackBox.Util
-- import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
-- import           Clash.Netlist.Util
import           Control.Monad.State             (State())
-- import           Data.Semigroup.Monad            (getMon)
import           Data.String                     (fromString)
import           Data.String.Interpolate.IsString (i)
import           Data.String.Interpolate.Util    (unindent)
-- import           Data.Text                       as TextS
import           Data.Text.Prettyprint.Doc.Extra

addFloatTclTF :: TemplateFunction
addFloatTclTF = addSubFloatTclTF "Add"

subFloatTclTF :: TemplateFunction
subFloatTclTF = addSubFloatTclTF "Subtract"

addSubFloatTclTF
  :: String
  -> TemplateFunction
addSubFloatTclTF addSubVal =
  TemplateFunction used valid (addSubFloatTclTemplate addSubVal)
 where
  used = [0..4]
  valid bbCtx = cfgIsLit
    where
      (_, _, cfgIsLit) = bbInputs bbCtx !! 2

addSubFloatTclTemplate
  :: Backend s
  => String
  -> BlackBoxContext
  -> State s Doc
addSubFloatTclTemplate addSubVal bbCtx = pure bbText
 where
  primName = bbQsysIncName bbCtx !! 0

  (Literal _ (NumLit latency), _, _) = bbInputs bbCtx !! 1
  (DataCon _ _ cfgExprs, _, _) = bbInputs bbCtx !! 2
  cfgArchOptExpr = cfgExprs !! 0
  cfgDspUsageExpr = cfgExprs !! 1
  -- cfgBMemUsageExpr = cfgExprs !! 2

  DataCon _ cfgArchOptMod _ = cfgArchOptExpr
  DC (cfgArchOptHWType, cfgArchOptTag) = cfgArchOptMod
  Sum _ cfgArchOptConstrs = cfgArchOptHWType
  -- DataCon _ (DC (Sum _ cfgArchOptConstrs, cfgArchOptTag)) _ = cfgArchOptExpr
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
      "Clash.Cores.Floating.Xilinx.Internal.MediumDspUsage" -> undefined
      "Clash.Cores.Floating.Xilinx.Internal.FullDspUsage" -> undefined
      _ -> error "Unknown FloatingDspUsage constructor"

  -- Literal Nothing (BoolLit cfgBMemUsage) = cfgBMemUsageExpr

  bbText = fromString $ unindent [i|
    create_ip -name floating_point -vendor xilinx.com -library ip \\
              -version 7.1 -module_name {#{primName}}
    set_property -dict [list CONFIG.Add_Sub_Value {#{addSubVal}} \\
                             CONFIG.C_Optimization {#{tclArchOpt}} \\
                             CONFIG.C_Mult_Usage {#{tclDspUsage}} \\
                             CONFIG.Flow_Control {NonBlocking} \\
                             CONFIG.Has_ACLKEN {true} \\
                             CONFIG.Has_RESULT_TREADY {false} \\
                             CONFIG.C_Latency {#{latency}}] \\
                       [get_ips {#{primName}}]
    generate_target {synthesis simulation} [get_ips {#{primName}}]
    |]
{-
    Hello world!

    Name:
    #{primName}

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
