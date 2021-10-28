
module Clash.Tests.Constants where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Constants (version)

-- | Really weak tests on 'Clash.CPP.preludeVersion'. Mostly here to test whether
-- it crashes or not.
somePreludeVersion :: Assertion
somePreludeVersion = True @=? (major == 1 && ok minor && ok patch)
 where
  ok n = 0 <= n && n <= 100
  (major, minor, patch) = version

tests :: TestTree
tests =
  testGroup
    "CPP"
    [ testCase "preludeVersion" somePreludeVersion
    ]
