{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Cores.Floating.Xilinx.BlackBoxes
where

import           Prelude

import           Clash.Backend
import           Clash.Netlist.BlackBox.Util
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
import           Clash.Netlist.Util
import           Control.Monad.State             (State())
import           Data.Semigroup.Monad            -- (getMon)
import           Data.String                     (fromString)
import           Data.String.Interpolate.IsString (i)
import           Data.String.Interpolate.Util    (unindent)
import           Data.Text                       as TextS
import           Data.Text.Prettyprint.Doc.Extra

addFloatTclTF :: TemplateFunction
addFloatTclTF = TemplateFunction used valid addFloatTclTemplate
 where
  used = [0..4]
  valid bbCtx = cfgIsLit
    where
      (_, _, cfgIsLit) = bbInputs bbCtx !! 2

addFloatTclTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
addFloatTclTemplate bbCtx = pure bbText
 where
  (DataCon _ _ cfgExprs, _, _) = bbInputs bbCtx !! 2
  cfgArchOptExpr = cfgExprs !! 0
  cfgDspUsageExpr = cfgExprs !! 1
  cfgBMemUsageExpr = cfgExprs !! 2

  DataCon _ cfgArchOptMod _ = cfgArchOptExpr
  DC (cfgArchOptHWType, cfgArchOptTag) = cfgArchOptMod
  Sum _ cfgArchOptConstrs = cfgArchOptHWType
  -- DataCon _ (DC (Sum _ cfgArchOptConstrs, cfgArchOptTag)) _ = cfgArchOptExpr
  cfgArchOpt = cfgArchOptConstrs !! cfgArchOptTag

  DataCon _ (DC (Sum _ cfgDspUsageConstrs, cfgDspUsageTag)) _ = cfgDspUsageExpr
  cfgDspUsage = cfgDspUsageConstrs !! cfgDspUsageTag

  Literal Nothing (BoolLit cfgBMemUsage) = cfgBMemUsageExpr

  bbText = fromString $ unindent [i|
    Hello world!

    ArchOpt:
    #{cfgArchOpt}

    DspUsage:
    #{cfgDspUsage}

    BMemUsage:
    #{cfgBMemUsage}
    |]
