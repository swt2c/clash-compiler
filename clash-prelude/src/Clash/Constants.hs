{-|
Copyright  :  (C) 2021, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Constants
 ( maxTupleSize
 , version
 ) where

import Clash.CPP (maxTupleSize)
import Data.Typeable (Typeable, typeOf, tyConPackage, typeRepTyCon)
import Data.List.Extra (splitOn)
import GHC.Stack (HasCallStack)


data PreludeType = PreludeType

-- Version of clash-prelude. E.g., @(1, 6, 0)@.
version :: HasCallStack => (Int, Int, Int)
version = (read major, read minor, read patch)
 where
  [major, minor, patch] = splitOn "." version_
  [_clash, _prelude, version_, _digest] = splitOn "-" pkgId
  pkgId = pkgIdFromTypeable PreludeType

-- | Get the package id of the type of a value
--
-- >>> pkgIdFromTypeable (undefined :: PreludeType)
-- "clash-prelude-1.5.0-64904d90747cb49e17166bbc86fec8678918e4ead3847193a395b258e680373c"
--
pkgIdFromTypeable :: Typeable a => a -> String
pkgIdFromTypeable = tyConPackage . typeRepTyCon . typeOf
