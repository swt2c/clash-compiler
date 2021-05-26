{-# LANGUAGE RankNTypes #-}
module Experiment
  ( module Experiment
  , module Xilinx
  , module Clash.Cores.Floating.Xilinx
  ) where

import Clash.Prelude
import qualified Prelude as P
import qualified Clash.Explicit.Prelude as CEP

import Clash.Cores.Floating.Xilinx
-- import Text.Show.Pretty (pPrint)

import Xilinx
-- import Xilinx.TH

runTB
  :: KnownDomain dom
  => Signal dom Bool
  -> IO ()
runTB tb
  = let len = P.length . P.takeWhile not $ sample tb
    in len `seq` putStrLn $      "Testbench finished after " P.++ (show len)
                            P.++ " cycles."
dsampleN
  :: forall d a
   . ( NFDataX a
     , KnownNat d
     )
  => Int
  -> (HiddenClockResetEnable XilinxSystem => DSignal XilinxSystem d a) -> [a]
dsampleN n f =
  CEP.sampleN n . toSignal
  $ withClockResetEnable clockGen resetGen enableGen f
