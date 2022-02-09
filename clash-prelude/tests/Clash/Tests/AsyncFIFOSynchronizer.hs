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
  inputRInc = fromList $ P.replicate 5 False <> P.replicate 4 True <> P.replicate 4 False <> P.replicate 4 True <> P.repeat False
  inputWDataM = fromList $ Nothing : P.map Just [1 .. 4] <> P.repeat Nothing

sync1R :: Signal Slow (Bool, (Int, Bool))
sync1R = bundle (inputRInc, bundle (rdata, rempty))
 where
  ((inputRInc, _), (rdata, rempty, _)) = sync1Samples

sync1W :: Signal System (Maybe Int, Bool)
sync1W = bundle (inputWDataM, wfull)
 where
  ((_, inputWDataM), (_, _, wfull)) = sync1Samples

sync2Samples :: ( ( Signal System Bool
                  , Signal Slow (Maybe Int)
                  )
                , ( Signal System Int
                  , Signal System Bool
                  , Signal Slow Bool)
                )
sync2Samples = ((inputRInc, inputWDataM), out)
 where
  out = asyncFIFOSynchronizer d4 clockGen clockGen resetGen resetGen enableGen
          enableGen inputRInc inputWDataM
  inputRInc = fromList $ P.replicate 10 False <> P.replicate 4 True <> P.replicate 8 False <> P.replicate 4 True <> P.repeat False
  inputWDataM = fromList $ Nothing : P.map Just [1 .. 4] <> P.repeat Nothing

sync2R :: Signal System (Bool, (Int, Bool))
sync2R = bundle (inputRInc, bundle (rdata, rempty))
 where
  ((inputRInc, _), (rdata, rempty, _)) = sync2Samples

sync2W :: Signal Slow (Maybe Int, Bool)
sync2W = bundle (inputWDataM, wfull)
 where
  ((_, inputWDataM), (_, _, wfull)) = sync2Samples

printS1R :: IO ()
printS1R = P.mapM_ (putStrLn . showX) $ sampleN 30 sync1R

printS1W :: IO ()
printS1W = P.mapM_ (putStrLn . showX) $ sampleN 30 sync1W

printS2R :: IO ()
printS2R = P.mapM_ (putStrLn . showX) $ sampleN 30 sync2R

printS2W :: IO ()
printS2W = P.mapM_ (putStrLn . showX) $ sampleN 30 sync2W
