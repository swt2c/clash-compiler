{-# OPTIONS_GHC -Wdefault #-}
module Experiment where

import Clash.Prelude
import qualified Prelude as P

import Text.Show.Pretty (pPrint)

import Xilinx
-- import Xilinx.TH

samples = addFloatBasicSamples

asPack = map (\(a, b, c) -> (pack a, pack b, pack c)) samples
asDecode =
  map (\(a, b, c) -> (decodeFloat a, decodeFloat b, decodeFloat c)) samples

runTB
  :: KnownDomain dom
  => Signal dom Bool
  -> IO ()
runTB tb
  = let len = P.length . P.takeWhile not $ sample tb
    in len `seq` putStrLn $      "Testbench finished after " P.++ (show len)
                            P.++ " cycles."
