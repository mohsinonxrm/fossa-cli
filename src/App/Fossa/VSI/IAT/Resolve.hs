{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Resolve (
  resolveUserDefined,
  resolveRevision,
  resolveGraph,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (..),
  UserDep,
 )
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, recover)
import Control.Effect.Lift (Lift)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Conversion (toText)
import Data.Text (Text, intercalate)
import Fossa.API.Types (ApiOpts)
import Graphing (Graphing, direct, edges, empty)
import Srclib.Types (
  SourceUserDefDep (..),
 )

resolveUserDefined :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [UserDep] -> m (Maybe [SourceUserDefDep])
resolveUserDefined apiOpts deps = context ("Resolving user defined dependencies " <> toText (show $ map IAT.renderUserDep deps)) $ do
  assertions <- traverse (Fossa.resolveUserDefinedBinary apiOpts) deps
  if null assertions
    then pure Nothing
    else pure . Just $ map toUserDefDep assertions

resolveRevision :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.Locator -> m (Maybe [VSI.Locator])
resolveRevision apiOpts locator =
  context ("Resolving dependencies for " <> VSI.renderLocator locator) $
    -- In the future we plan to support either returning a partial graph to the server for resolution there,
    -- or expanding this into a full fledged graph resolver for any dependency using dedicated endpoints.
    -- For the initial version of IAT however we're just resolving dependencies for top level projects.
    -- Since users may only register revision assertions as a byproduct of running CLI analysis,
    -- revision assertions are always for "custom" projects.
    if VSI.isTopLevelProject locator
      then recover $ Fossa.resolveProjectDependencies apiOpts locator
      else pure . Just $ []

resolveGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [VSI.Locator] -> VSI.SkipResolution -> m (Graphing VSI.Locator)
resolveGraph apiOpts locators skipResolving = context ("Resolving graph for " <> toText (show $ fmap VSI.renderLocator locators)) $ do
  -- If any of the resulting graphs are Nothing, we failed to fetch the graph from the server.
  -- This typically means that the user doesn't have access to the project, or the project doesn't exist.
  -- Collect failed locators and report them to the user, along with mitigation suggestions.
  subgraphs <- traverseZip (resolveSubgraph apiOpts skipResolving) locators
  if any resolutionFailed subgraphs
    then fatalText $ resolveGraphFailureBundle subgraphs
    else pure . mconcat $ fmap unwrap subgraphs
  where
    resolutionFailed (_, b) = isNothing b
    unwrap (_, b) = fromMaybe empty b

resolveGraphFailureBundle :: [(VSI.Locator, Maybe (Graphing VSI.Locator))] -> Text
resolveGraphFailureBundle subgraphs =
  "Failed to resolve dependencies for the following FOSSA projects:\n\t"
    <> intercalate "\n\t" (renderFailed subgraphs)
    <> "\n\n"
    <> "You may not have access to the projects, or they may not exist (see the warnings below for details).\n"
    <> "If desired you can use --experimental-skip-vsi-graph to skip resolving the dependencies of these projects."
  where
    renderFailed [] = []
    renderFailed ((a, b) : xs) = case b of
      Just _ -> renderFailed xs
      Nothing -> VSI.renderLocator a : renderFailed xs

-- | Given a traverseable list and a monadic function that resolves them to b, traverse and zip the list into a pair of (a, b)
traverseZip :: (Has (Lift IO) sig m, Traversable t) => (a -> m b) -> t a -> m (t (a, b))
traverseZip f = traverse (pairM f)
  where
    pairM :: Has (Lift IO) sig m => (a -> m b) -> a -> m (a, b)
    pairM transform a = transform a >>= (\b -> pure (a, b))

-- Pass through the list of skipped locators all the way here:
-- we want to still record the direct dependency, we just don't want to resolve it.
resolveSubgraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.SkipResolution -> VSI.Locator -> m (Maybe (Graphing VSI.Locator))
resolveSubgraph _ skip locator | VSI.shouldSkipResolving skip locator = pure . Just $ direct locator
resolveSubgraph apiOpts _ locator = do
  resolved <- resolveRevision apiOpts locator
  case resolved of
    Just deps -> pure . Just $ direct locator <> edges (map edge deps)
    Nothing -> pure Nothing
  where
    edge dep = (locator, dep)

toUserDefDep :: UserDefinedAssertionMeta -> SourceUserDefDep
toUserDefDep UserDefinedAssertionMeta{..} =
  SourceUserDefDep
    { srcUserDepName = assertedName
    , srcUserDepVersion = assertedVersion
    , srcUserDepLicense = assertedLicense
    , srcUserDepDescription = assertedDescription
    , srcUserDepHomepage = assertedUrl
    , srcUserDepOrigin = Nothing
    }
