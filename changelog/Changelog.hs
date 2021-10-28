{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Main where

-- aeson
import "aeson" Data.Aeson (ToJSON, FromJSON)

-- base
import "base" GHC.Stack (HasCallStack)
import "base" GHC.Generics (Generic)
import "base" System.Environment (getArgs)

-- clash-prelude
import qualified "clash-prelude" Clash.Constants as Constants

-- directory
import "directory" System.Directory (canonicalizePath, doesFileExist)

-- deepseq
import Control.DeepSeq (force)

-- docopt
import "docopt" System.Console.Docopt (Docopt, docopt, parseArgsOrExit, isPresent, command, Arguments)

-- filepath
import "filepath" System.FilePath (takeDirectory, isDrive, (</>))

-- Glob
import "Glob" System.FilePath.Glob (glob)

-- regex-compat
import Text.Regex (mkRegex, subRegex)

type ClashVersion = (Int, Int, Int)

data Changelog = Changelog
  { pullRequests :: [Int]
  , issues :: [Int]
  , title :: String }
  deriving (Generic, Show, ToJSON, FromJSON)

patterns :: Docopt
patterns = [docopt|
changelog version 0.0.0

Usage:
  changelog create
  changelog check
  changelog release (major|minor|patch)

|]

versionToString :: ClashVersion -> String
versionToString (major, minor, patch) =
  show major <> "." <> show minor <> "." <> show patch

currentVersion :: ClashVersion
currentVersion = Constants.version

nextMajorVersion :: ClashVersion
nextMajorVersion = let (major, _minor, _patch) = currentVersion in (major+1, 0, 0)

nextMinorVersion :: ClashVersion
nextMinorVersion = let (major, minor, _patch) = currentVersion in (major, minor+1, 0)

nextPatchVersion :: ClashVersion
nextPatchVersion = let (major, minor, patch) = currentVersion in (major, minor, patch+1)

-- | Set all files referring to the current Clash version to a specific version.
setVersion :: HasCallStack => FilePath -> ClashVersion -> IO ()
setVersion projectRoot (versionToString -> newVersion) = do
  setPreludeVersion
  setLibVersion
  setGhcVersion
  setSnapVersion
  setDocsVersion
 where
  pkgPath pkg = projectRoot </> pkg </> (pkg <> ".cabal")

  replaceInFile path searchRe replacement = do
    !oldLines <- lines . force <$> readFile path
    let replace line = subRegex searchRe line replacement
    writeFile path (unlines (map replace oldLines))

  setCabalVersion pkg =
    let
      versionRegex = mkRegex "^(Version: *) [0-9.]+"
      versionReplace = "\\1 " <> newVersion
    in
      replaceInFile (pkgPath pkg) versionRegex versionReplace

  setCabalDependencyVersion pkg dep =
    let
      versionRegex = mkRegex ("(^ *" <> dep <> " *== *) [0-9.]+( *,)")
      versionReplace = "\\1 " <> newVersion <> "\\2"
    in
      replaceInFile (pkgPath pkg) versionRegex versionReplace

  setPreludeVersion = do
    setCabalVersion "clash-prelude"

  setLibVersion = do
    setCabalVersion "clash-prelude"
    setCabalDependencyVersion "clash-lib" "clash-prelude"

  setGhcVersion = do
    setCabalVersion "clash-ghc"
    setCabalDependencyVersion "clash-ghc" "clash-prelude"
    setCabalDependencyVersion "clash-ghc" "clash-lib"

  setSnapVersion =
    let
      snapDir = projectRoot </> ".ci" </> "bindist" </> "linux" </> "snap"
      snapPkg = snapDir </> "snap" </> "snapcraft.yaml"
      versionRegex = mkRegex "^(version: *)'[0-9.]+(')"
      versionReplace = "\\1'" <> newVersion <> "\\2"
    in
      replaceInFile snapPkg versionRegex versionReplace

  setDocsVersion =
    let
      docsConf = projectRoot </> "docs" </> "conf.py"
      versionRegex = mkRegex "^(version *= *)'[0-9.]+(')"
      versionReplace = "\\1'" <> newVersion <> "\\2"
    in
      replaceInFile docsConf versionRegex versionReplace

-- Searches for a file called @package.cabal@, where @package@ is given as an
-- argument. It will look for it in the current directory. If it can't find it
-- there, it will traverse up until it finds the file or a file called
-- @cabal.project@. In case of the latter, it will traverse down recursively
-- until it encounters a @package.cabal@.
--
-- The returned path points to the @package.cabal@. Errors if it could not
-- find @package.cabal@ anywhere, or when it found multiple.
--
findCabalPackage :: HasCallStack => String -> IO FilePath
findCabalPackage packageName = goUp =<< canonicalizePath packageName
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error ("Could not find '" <> packageFilename <> "'")
    | otherwise = do
      packageExists <- doesFileExist (path </> packageFilename)
      projectExists <- doesFileExist (path </> projectFilename)

      if | packageExists -> pure (path </> packageFilename)
         | projectExists -> goDown path
         | otherwise -> goUp (takeDirectory path)

  goDown :: FilePath -> IO FilePath
  goDown path = do
    candidates <- glob (path </> "**" </> packageFilename)
    case candidates of
      [] -> error ("Could not find " <> packageFilename <> " in project " <> path)
      (_:_:_) -> error ("Ambiguous packages in project " <> path <> ": " <> show candidates)
      [c] -> pure c

  packageFilename = packageName <> ".cabal"
  projectFilename = "cabal.project"

mainRelease :: Arguments -> FilePath -> IO ()
mainRelease args projectDir = do
  if
    | args `isPresent` command "major" -> setVersion projectDir nextMajorVersion
    | args `isPresent` command "minor" -> setVersion projectDir nextMinorVersion
    | args `isPresent` command "patch" -> setVersion projectDir nextPatchVersion
    | otherwise -> error "Internal error: should have seen major/minor/patch"

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  projectDir <- takeDirectory . takeDirectory <$> findCabalPackage "changelog"

  if
    | args `isPresent` command "create" -> pure ()
    | args `isPresent` command "check" -> pure ()
    | args `isPresent` command "release" -> mainRelease args projectDir
    | otherwise -> error "Internal error: should have seen create/check/release"
