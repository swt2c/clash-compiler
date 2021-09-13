{-|
Copyright       : (C) 2020-2021, QBayLogic
License         : BSD2 (see the file LICENSE)
Maintainer      : QBayLogic B.V. <devops@qbaylogic.com>

Data types and rendering for Edalize Metadata files (EDAM).
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Edalize.Edam
  ( Edam(..)
  , EdamFile(..)
  , EdamFileType(..)
  , EdamTools(..)
  , GhdlOptions(..)
  , IcarusOptions(..)
  , ModelsimOptions(..)
  , QuartusOptions(..)
  , VivadoOptions(..)

  , pprEdam
  ) where

import Data.Default
import Data.Maybe
import Data.Text (Text)
import Prettyprinter

-- | EDAM data structure to be given to an Edalize backend. This contains all
-- information needed to generate a project scaffolding. Note that hooks and
-- VPI modules are currently not specified by clash.
--
data Edam = Edam
  { edamProjectName :: Text
  , edamTopEntity   :: Text
  , edamFiles       :: [EdamFile]
  , edamToolOptions :: EdamTools
  }

pprEdam :: Edam -> Doc ann
pprEdam (Edam n te fs ts) = vsep
  [ pyPre
  , hsep ["edam", equals, manifest]
  , pyPost
  ]
 where
  manifest = pyRecord
    [ pyField "name" $ squotes (pretty n)
    , pyField "toplevel" $ squotes (pretty te)
    , pyField "files" $ pyList (fmap pprFile fs)
    , pyField "tool_options" $ pprEdamTools ts
    ]

-- | Information about each file in the project. This does not include
-- is_include_file or include_path, as these are not currently used by Clash.
--
data EdamFile = EdamFile
  { efName        :: FilePath
  , efType        :: EdamFileType
  , efLogicalName :: Text
  }

pprFile :: EdamFile -> Doc ann
pprFile (EdamFile n ty ln) =
  pyRecord
    [ pyField "name" $ joinPath n
    , pyField "file_type" $ squotes (pprFileType ty)
    , pyField "logical_name" $ squotes (pretty ln)
    ]

-- | A subset of the file types recognized by Edalize. The supported formats
-- are largely from IP-XACT 2014 (IEEE 1685-2014), although Edalize extends
-- this with other types, e.g. QSYS.
--
-- Only file types which are generated by Clash are listed.
--
data EdamFileType
  = Unknown
    -- ^ Unknown file type.
  | VhdlSource
    -- ^ VHDL source.
  | VerilogSource
    -- ^ Verilog source.
  | SystemVerilogSource
    -- ^ SystemVerilog source.
  | TclSource
    -- ^ Tool Command Language source.
  | QSYS
    -- ^ QSys system source.
  | SDC
    -- ^ Synopsys Design Constraints source.
  deriving (Eq, Show)

pprFileType :: EdamFileType -> Doc ann
pprFileType = \case
  Unknown -> "unknown"
  VhdlSource -> "vhdlSource"
  VerilogSource -> "verilogSource"
  SystemVerilogSource -> "systemVerilogSource"
  TclSource -> "tclSource"
  QSYS -> "QSYS"
  SDC -> "SDC"

-- | Tool-specific configuration used by Edalize.
-- Currently only tools which are supported by Clash are provided.
--
data EdamTools = EdamTools
  { etGhdl      :: Maybe GhdlOptions
  , etIcarus    :: Maybe IcarusOptions
  , etModelsim  :: Maybe ModelsimOptions
  , etQuartus   :: Maybe QuartusOptions
  , etVivado    :: Maybe VivadoOptions
  }

instance Default EdamTools where
  def = EdamTools def def def def def

pprEdamTools :: EdamTools -> Doc ann
pprEdamTools tools =
  pyRecord
    [ pyField "ghdl" $ pprGhdlOptions ghdl
    , pyField "icarus" $ pprIcarusOptions icarus
    , pyField "modelsim" $ pprModelsimOptions modelsim
    , pyDocField
        "quartus"
        "TODO Specify options if using Quartus"
        (pprQuartusOptions quartus)

    , pyDocField
        "vivado"
        "TODO Specify options if using Vivado"
        (pprVivadoOptions vivado)
    ]
 where
  ghdl     = fromMaybe def (etGhdl tools)
  icarus   = fromMaybe def (etIcarus tools)
  modelsim = fromMaybe def (etModelsim tools)
  quartus  = fromMaybe def (etQuartus tools)
  vivado   = fromMaybe def (etVivado tools)

data GhdlOptions = GhdlOptions
  { ghdlAnalyseOpts :: [Text]
  , ghdlRunOpts     :: [Text]
  }

instance Default GhdlOptions where
  def = GhdlOptions [] []

pprGhdlOptions :: GhdlOptions -> Doc ann
pprGhdlOptions (GhdlOptions aOpts rOpts) =
  pyRecord
    [ pyDocField
        "analyze_options"
        "Command line arguments for analysis"
        (flagList (fmap pretty aOpts))

    , pyDocField
        "run_options"
        "Command line arguments for simulation"
        (flagList (fmap pretty rOpts))
    ]

data IcarusOptions = IcarusOptions
  { icarusOpts      :: [Text]
  , icarusTimeScale :: Text
  }

instance Default IcarusOptions where
  def = IcarusOptions [] "100fs/100fs"

pprIcarusOptions :: IcarusOptions -> Doc ann
pprIcarusOptions (IcarusOptions opts ts) =
  pyRecord
    [ pyDocField
        "iverilog_options"
        "Command line options for iverilog"
        (flagList (fmap pretty opts))

    , pyDocField
        "timescale"
        "Default timescale for simulation"
        (squotes (pretty ts))
    ]

data ModelsimOptions = ModelsimOptions
  { msVlogOpts :: [Text]
  , msVsimOpts :: [Text]
  }

instance Default ModelsimOptions where
  def = ModelsimOptions [] []

pprModelsimOptions :: ModelsimOptions -> Doc ann
pprModelsimOptions (ModelsimOptions vlog vsim) =
  pyRecord
    [ pyDocField
        "vlog_options"
        "Command line arguments for vlog"
        (flagList (fmap pretty vlog))

    , pyDocField
        "vsim_options"
        "Command line arguments for vsim"
        (flagList (fmap pretty vsim))
    ]

data QuartusOptions = QuartusOptions
  { quartusBoardDevIndex :: Int
  , quartusFamily        :: Text
  , quartusDevice        :: Text
  , quartusOpts          :: [Text]
  , quartusDseOpts       :: [Text]
  }

instance Default QuartusOptions where
  def = QuartusOptions 1 "" "" [] []

pprQuartusOptions :: QuartusOptions -> Doc ann
pprQuartusOptions (QuartusOptions bdi fam dev opts dse) =
  pyRecord
    [ pyDocField
        "board_device_index"
        "Specify the FPGA's device number in the JTAG chain"
        (squotes (pretty bdi))

    , pyDocField
        "family"
        "FPGA family, e.g. Cyclone IV E"
        (squotes (pretty fam))

    , pyDocField
        "device"
        "Device identifier, e.g. EP4CE55F23C8"
        (squotes (pretty dev))

    , pyDocField
        "quartus_options"
        "Command line arguments for Quartus"
        (flagList (fmap pretty opts))

    , pyDocField
        "dse_options"
        "Command line arguments for Design Space Explorer"
        (flagList (fmap pretty dse))
    ]

data VivadoOptions = VivadoOptions
  { vivadoPart :: Text
  }

instance Default VivadoOptions where
  def = VivadoOptions ""

pprVivadoOptions :: VivadoOptions -> Doc ann
pprVivadoOptions (VivadoOptions part) =
  pyRecord
    [ pyDocField
        "part"
        "Specify target part by ID, e.g. xc7z020-1clg400c"
        (squotes (pretty part))
    ]

-- Helpers; don't export

pyPre :: Doc ann
pyPre = vsep
  [ "import os"
  , ""
  , "edam_root = os.path.dirname(os.path.realpath(__file__))"
  , "work_root = 'build'"
  , ""
  , "# TODO Specify the EDA tool to use"
  , "tool = ''"
  , ""
  ]

pyPost :: Doc ann
pyPost = vsep
  [ ""
  , "if __name__ == '__main__':"
  , indent 4 $ vsep
      [ "from edalize import *"
      , ""
      , "tool = get_edatool(tool)(edam=edam, work_root=work_root)"
      , "os.makedirs(work_root)"
      , "tool.configure()"
      , "tool.build()"
      ]
  ]

pyList :: [Doc ann] -> Doc ann
pyList xs = vsep [lbracket, indent 4 (commaList xs), rbracket]

pyRecord :: [Doc ann] -> Doc ann
pyRecord xs = vsep [lbrace, indent 4 (commaList xs), rbrace]

pyComment :: Text -> Doc ann
pyComment x = hsep ["#", pretty x]

pyField :: Text -> Doc ann -> Doc ann
pyField n x = hcat [squotes (pretty n), colon, space, x]

pyDocField :: Text -> Text -> Doc ann -> Doc ann
pyDocField n d x = vsep [pyComment d, pyField n x]

joinPath :: FilePath -> Doc ann
joinPath x = hcat
  [ "os.path.join"
  , parens (hsep $ punctuate comma ["edam_root", squotes (pretty x)])
  ]

flagList :: [Doc ann] -> Doc ann
flagList = squotes . hsep

commaList :: [Doc ann] -> Doc ann
commaList = vsep . punctuate comma
