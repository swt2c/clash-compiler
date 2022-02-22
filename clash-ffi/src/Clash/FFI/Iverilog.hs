{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.FFI.Iverilog where

import           Data.Foldable (for_)
import           Data.Proxy (Proxy)
import           Data.String (fromString)
import           GHC.TypeNats (SomeNat(..), someNatVal)

import           Clash.Promoted.Nat

import qualified Clash.FFI.VPI.IO as VPI
import qualified Clash.FFI.VPI.Info as VPI
import qualified Clash.FFI.VPI.Module as VPI
import qualified Clash.FFI.VPI.Net as VPI
import qualified Clash.FFI.VPI.Parameter as VPI
import qualified Clash.FFI.VPI.Port as VPI
import qualified Clash.FFI.VPI.Reg as VPI
import qualified Clash.FFI.VPI.Time as VPI
import qualified Clash.FFI.VPI.Value as VPI
import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View

foreign export ccall "clash_ffi_main"
  clashMain :: IO ()

clashMain :: IO ()
clashMain =
  Sim.runSimAction $ do
    VPI.simPutStrLn "Hello from Haskell"

    cinfoPtr <- VPI.simulatorInfo Sim.stackPtr
    info     <- unsafePeekReceive @VPI.Info cinfoPtr
    VPI.simPutStrLn ("Found simulator: " <> fromString (show info))

    ctimePtr <- VPI.simulationTime Sim.stackPtr VPI.Sim Nothing
    time     <- unsafePeekReceive @VPI.Time ctimePtr
    VPI.simPutStrLn ("Current time: " <> fromString (show time))

    modules <- VPI.topModules

    for_ modules $ \m -> do
      mName <- VPI.moduleFullName m
      VPI.simPutStrLn ("Found module: " <> mName)

      params <- VPI.moduleParameters m

      for_ @_ @_ @_ @() params $ \p -> do
        pName <- VPI.parameterName p
        pSize <- VPI.parameterSize p

        case someNatVal pSize of
          SomeNat (proxy :: Proxy sz) ->
            case compareSNat (SNat @1) (snatProxy proxy) of
              SNatLE -> do
                pVal <- VPI.parameterValue (VPI.ObjTypeFmt @sz) p
                VPI.simPutStrLn ("Found parameter: " <> pName <> " = " <> fromString (show pVal))

              SNatGT ->
                error "Parameter is zero-width"

      ports <- VPI.modulePorts m

      for_ ports $ \p -> do
        pName <- VPI.portName p
        VPI.simPutStrLn ("Found port: " <> pName)

      nets <- VPI.moduleNets m

      for_ @_ @_ @_ @() nets $ \n -> do
        nName <- VPI.netName n
        nSize <- VPI.netSize n

        case someNatVal nSize of
          SomeNat (proxy :: Proxy sz) ->
            case compareSNat (SNat @1) (snatProxy proxy) of
              SNatLE -> do
                nVal <- VPI.netValue (VPI.ObjTypeFmt @sz) n
                VPI.simPutStrLn ("Found net: " <> nName <> " = " <> fromString (show nVal))

              SNatGT ->
                error "Net is zero-width"

      regs <- VPI.moduleRegs m

      for_ @_ @_ @_ @() regs $ \r -> do
        rName <- VPI.regName r
        rSize <- VPI.regSize r

        case someNatVal rSize of
          SomeNat (proxy :: Proxy sz) ->
            case compareSNat (SNat @1) (snatProxy proxy) of
              SNatLE -> do
                rVal <- VPI.regValue (VPI.ObjTypeFmt @sz) r
                VPI.simPutStrLn ("Found reg: " <> rName <> " = " <> fromString (show rVal))

              SNatGT ->
                error "Reg is zero-width"

