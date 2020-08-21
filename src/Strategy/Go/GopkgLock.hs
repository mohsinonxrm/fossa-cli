{-# language TemplateHaskell #-}

module Strategy.Go.GopkgLock
  ( discover
  , analyze

  , GoLock(..)
  , Project(..)

  , buildGraph
  , golockCodec
  )
  where

import Prologue hiding ((.=))

import Control.Effect.Diagnostics
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import qualified Toml
import Toml (TomlCodec, (.=))
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Gopkg.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "golang-gopkglock" GolangGroup $ analyze file

  pure $ WalkSkipSome ["vendor"]

golockCodec :: TomlCodec GoLock
golockCodec = GoLock
  <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec = Project
  <$> Toml.text "name" .= projectName
  <*> Toml.dioptional (Toml.text "source") .= projectSource
  <*> Toml.text "revision" .= projectRevision

newtype GoLock = GoLock
  { lockProjects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project
  { projectName     :: Text
  , projectSource   :: Maybe Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

analyze ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs File -> m ProjectClosureBody
analyze file = fmap (mkProjectClosure file) . graphingGolang $ do
  golock <- readContentsToml golockCodec file
  buildGraph (lockProjects golock)
  _ <- recover (fillInTransitive (parent file))
  pure ()

mkProjectClosure :: Path Abs File -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure file graph = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Has GolangGrapher sig m => [Project] -> m ()
buildGraph = void . traverse_ go
  where
  go :: Has GolangGrapher sig m => Project -> m ()
  go Project{..} = do
    let pkg = mkGolangPackage projectName

    direct pkg
    label pkg (mkGolangVersion projectRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) projectSource