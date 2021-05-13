{-# OPTIONS_GHC -Wdefault #-}
module Experiment where

import Clash.Prelude
import qualified Prelude as P

import Text.Show.Pretty (pPrint)

import Xilinx
import Xilinx.TH

samples = addFloatBasicSamples

asPack = P.map (\(a, b, c) -> (pack a, pack b, pack c)) samples
asDecode =
  P.map (\(a, b, c) -> (decodeFloat a, decodeFloat b, decodeFloat c)) samples

runTB = P.head . P.dropWhile not $ sample addFloatBasicTB
