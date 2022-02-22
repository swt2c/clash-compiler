module Clash.FFI.VPI.Module
  ( Module(..)
  , topModules
  , moduleName
  , moduleFullName
  , moduleNets
  , moduleParameters
  , modulePorts
  , moduleRegs
  ) where

import Data.ByteString (ByteString)

import Clash.FFI.Monad (SimCont)
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Net (Net(..))
import Clash.FFI.VPI.Parameter (Parameter(..))
import Clash.FFI.VPI.Port (Port(..))
import Clash.FFI.VPI.Property
import Clash.FFI.VPI.Reg (Reg(..))

newtype Module = Module { moduleHandle :: Handle }

topModules :: SimCont o [Module]
topModules =
  fmap Module <$> iterateHandle ObjModule Nothing

moduleName :: Module -> SimCont o ByteString
moduleName = receiveProperty VpiName . moduleHandle

moduleFullName :: Module -> SimCont o ByteString
moduleFullName = receiveProperty VpiFullName . moduleHandle

moduleNets :: Module -> SimCont o [Net]
moduleNets =
  fmap (fmap Net) . iterateHandle ObjNet . Just . moduleHandle

moduleParameters :: Module -> SimCont o [Parameter]
moduleParameters =
  fmap (fmap Parameter) . iterateHandle ObjParameter . Just . moduleHandle

modulePorts :: Module -> SimCont o [Port]
modulePorts =
  fmap (fmap Port) . iterateHandle ObjPort . Just . moduleHandle

moduleRegs :: Module -> SimCont o [Reg]
moduleRegs =
  fmap (fmap Reg) . iterateHandle ObjReg . Just . moduleHandle

