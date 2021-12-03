{-# LANGUAGE TemplateHaskell #-}

module App.NewFossa.Config.Analyze (
  AnalyzeCliOpts,
  AnalyzeConfig (..),
  BinaryDiscovery (..),
  ExperimentalAnalyzeConfig (..),
  IncludeAll (..),
  JsonOutput (..),
  ScanDestination (..),
  UnpackArchives (..),
  VSIAnalysis (..),
  VSIModeOptions (..),
  subcommand,
) where

import App.Fossa.VSI.Types qualified as VSI
import App.NewFossa.Config.Common (
  GlobalOpts (GlobalOpts, optConfig),
  baseDirArg,
  filterOpt,
  globalOpts,
  metadataOpts,
  pathOpt,
  targetOpt,
 )
import App.NewFossa.ConfigFile (ConfigFile, resolveConfigFile)
import App.NewFossa.EnvironmentVars (EnvVars)
import App.NewFossa.Subcommand (SubCommand (SubCommand))
import App.Types (
  BaseDir (BaseDir),
  MonorepoAnalysisOpts (MonorepoAnalysisOpts),
  ProjectMetadata,
  ProjectRevision (ProjectRevision),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  fatalText,
  recover,
 )
import Control.Effect.Lift (Lift, sendIO)
import Data.Flag (Flag, flagOpt)
import Data.Set (Set)
import Data.String.Conversion (
  ToString (toString),
  ToText (toText),
 )
import Data.Text (Text)
import Discovery.Filters (AllFilters, noFilters)
import Effect.Exec (
  AllowErr (Never),
  Command (Command),
  Exec,
  exec,
 )
import Effect.Logger (Logger, Severity (SevInfo), logInfo, logError, logStdout)
import Effect.ReadFS (ReadFS, readContentsText, runReadFSIO, Has)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  Alternative (many),
  InfoMod,
  Parser,
  eitherReader,
  help,
  hidden,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  strOption,
  switch,
 )
import Path (Dir, Path, Rel, mkAbsDir, mkAbsFile)
import Path.IO (getCurrentDir)
import Types (TargetFilter)
import App.Fossa.VSI.Types (SkipResolution(SkipResolution))

-- CLI flags, for use with 'Data.Flag'
data BinaryDiscovery = BinaryDiscovery
data IncludeAll = IncludeAll
data JsonOutput = JsonOutput
data UnpackArchives = UnpackArchives
data VSIAnalysis = VSIAnalysis

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout

data VSIModeOptions = VSIModeOptions
  { vsiAnalysisEnabled :: Bool
  , vsiSkipSet :: VSI.SkipResolution
  , iatAssertionEnabled :: Bool
  , binaryDiscoveryEnabled :: Bool
  }

data AnalyzeCliOpts = AnalyzeCliOpts
  { globals :: GlobalOpts
  , analyzeOutput :: Bool
  , analyzeUnpackArchives :: Flag UnpackArchives
  , analyzeJsonOutput :: Flag JsonOutput
  , analyzeIncludeAllDeps :: Flag IncludeAll
  , analyzeBranch :: Maybe Text
  , analyzeMetadata :: ProjectMetadata
  , analyzeBuildTargetFilters :: [TargetFilter]
  , analyzeOnlyTargets :: [TargetFilter]
  , analyzeExcludeTargets :: [TargetFilter]
  , analyzeOnlyPaths :: [Path Rel Dir]
  , analyzeExcludePaths :: [Path Rel Dir]
  , analyzeVSIMode :: Flag VSIAnalysis
  , analyzeBinaryDiscoveryMode :: Flag BinaryDiscovery
  , analyzeAssertMode :: Maybe (FilePath)
  , analyzeSkipVSIGraphResolution :: [VSI.Locator]
  , monorepoAnalysisOpts :: MonorepoAnalysisOpts
  , analyzeBaseDir :: FilePath
  }

data AnalyzeConfig = AnalyzeConfig
  { baseDir :: BaseDir
  , severity :: Severity
  , scanDestination :: ScanDestination
  , projectRevision :: ProjectRevision
  , vsiOptions :: VSIModeOptions
  , filterSet :: AllFilters
  , experimental :: ExperimentalAnalyzeConfig
  }

newtype ExperimentalAnalyzeConfig = ExperimentalAnalyzeConfig
  { allowedGradleConfigs :: Maybe (Set Text)
  }

subcommand ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  SubCommand m AnalyzeCliOpts AnalyzeConfig
subcommand = SubCommand "analyze" analyzeInfo cliParser loadConfig mergeOpts doAnalyze

analyzeInfo :: InfoMod (IO ())
analyzeInfo = progDesc "Scan for projects and their dependencies"

cliParser :: Parser AnalyzeCliOpts
cliParser =
  AnalyzeCliOpts
    <$> globalOpts
    <*> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> flagOpt UnpackArchives (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
    <*> flagOpt JsonOutput (long "json" <> help "Output project metadata as json to the console. Useful for communicating with the FOSSA API")
    <*> flagOpt IncludeAll (long "include-unused-deps" <> help "Include all deps found, instead of filtering non-production deps.  Ignored by VSI.")
    <*> optional (strOption (long "branch" <> short 'b' <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> many filterOpt
    <*> many (option (eitherReader targetOpt) (long "only-target" <> help "Only scan these targets. See targets.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader targetOpt) (long "exclude-target" <> help "Exclude these targets from scanning. See targets.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "only-path" <> help "Only scan these paths. See paths.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "exclude-path" <> help "Exclude these paths from scanning. See paths.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> flagOpt VSIAnalysis (long "enable-vsi" <> hidden)
    <*> flagOpt BinaryDiscovery (long "experimental-enable-binary-discovery" <> hidden)
    <*> optional (strOption (long "experimental-link-project-binary" <> hidden))
    <*> many skipVSIGraphResolutionOpt
    <*> monorepoOpts
    <*> baseDirArg

skipVSIGraphResolutionOpt :: Parser VSI.Locator
skipVSIGraphResolutionOpt = (option (eitherReader parseLocator) (long "experimental-skip-vsi-graph" <> hidden))
  where
    parseLocator :: String -> Either String VSI.Locator
    parseLocator s = case VSI.parseLocator (toText s) of
      Left err -> Left $ toString (toText err)
      Right loc -> pure loc

monorepoOpts :: Parser MonorepoAnalysisOpts
monorepoOpts =
  MonorepoAnalysisOpts
    <$> optional (strOption (long "experimental-enable-monorepo" <> metavar "MODE" <> help "scan the project in the experimental monorepo mode. Supported modes: aosp"))

loadConfig ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  AnalyzeCliOpts ->
  m (Maybe ConfigFile)
loadConfig AnalyzeCliOpts{globals = GlobalOpts{optConfig}} =
  runReadFSIO $ do
    -- FIXME: We eventually want to use the basedir to inform the config file root
    configRelBase <- sendIO getCurrentDir
    resolveConfigFile configRelBase optConfig

mergeOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m AnalyzeConfig
mergeOpts _ _ _ = do
  useEffects
  pure dummyConfig

doAnalyze ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  AnalyzeConfig ->
  m ()
doAnalyze _ = do
  useEffects
  logError "Analyze not implmented"
  logStdout "Here's a result!"

-- only exists to prove effects are being run properly.
useEffects ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  m ()
useEffects = do
  _ <- recover $ fatalText "recovered fatal"
  _ <- exec $(mkAbsDir "/home/wes") $ Command "ls" ["-al"] Never
  _ <- readContentsText $(mkAbsFile "/home/wes/.gitconfig")
  logInfo "log test"

dummyConfig :: AnalyzeConfig
dummyConfig =
  AnalyzeConfig
    { baseDir = BaseDir $(mkAbsDir "/")
    , severity = SevInfo
    , scanDestination = OutputStdout
    , projectRevision = ProjectRevision "project" "revision" Nothing
    , vsiOptions =
        VSIModeOptions
          { vsiAnalysisEnabled = False
          , vsiSkipSet = SkipResolution mempty
          , iatAssertionEnabled = False
          , binaryDiscoveryEnabled = False
          }
    , filterSet = noFilters 
    , experimental = ExperimentalAnalyzeConfig Nothing
    }
