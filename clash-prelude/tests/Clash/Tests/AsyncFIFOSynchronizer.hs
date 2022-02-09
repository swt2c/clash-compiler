{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.AsyncFIFOSynchronizer where

import qualified Prelude as P

-- import Test.Tasty
-- import Test.Tasty.HUnit

import Clash.Explicit.Prelude

createDomain vSystem{vName = "Slow", vPeriod = 2 * vPeriod vSystem}

sync1Samples :: ( ( Signal Slow Bool
                  , Signal System (Maybe Int)
                  )
                , ( Signal Slow Int
                  , Signal Slow Bool
                  , Signal System Bool)
                )
sync1Samples = ((inputRInc, inputWDataM), out)
 where
  out = asyncFIFOSynchronizer d4 clockGen clockGen resetGen resetGen enableGen
          enableGen inputRInc inputWDataM
  inputRInc = fromList $ P.replicate 4 False <> P.replicate 4 True <> P.replicate 4 False <> P.replicate 4 True <> P.repeat False
  inputWDataM = fromList $ Nothing : P.map Just [1 .. 4] <> P.replicate 8 Nothing

sync1Slow :: Signal Slow (Bool, (Int, Bool))
sync1Slow = bundle (inputRInc, bundle (rdata, rempty))
 where
  ((inputRInc, _), (rdata, rempty, _)) = sync1Samples

sync1System :: Signal System (Maybe Int, Bool)
sync1System = bundle (inputWDataM, wfull)
 where
  ((_, inputWDataM), (_, _, wfull)) = sync1Samples
