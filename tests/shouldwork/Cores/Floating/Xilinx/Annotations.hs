module Xilinx.Annotations where

import Clash.Prelude

binTopAnn :: String -> TopEntity
binTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "en"
          , PortName "x"
          , PortName "y"
          ]
    , t_output = PortName "result"
    }
