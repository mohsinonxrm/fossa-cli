{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Subcommand (
  defaultRunSubCommand,
  runSubCommand,
  runEffs,
  SubCommand (..),
  EffStack,
) where

import App.NewFossa.ConfigFile (ConfigFile)
import App.NewFossa.EnvironmentVars (EnvVars, getEnvVars)
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Options.Applicative

data SubCommand m init prepared = SubCommand
  { commandName :: String
  , commandInfo :: InfoMod (IO ())
  , parser :: Parser init
  , configLoader :: init -> m (Maybe ConfigFile) -- CLI args can control where we look for config file
  , optMerge :: Maybe ConfigFile -> EnvVars -> init -> m prepared
  , perform :: prepared -> m ()
  }

type EffStack m = ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC m)))

defaultRunSubCommand :: SubCommand (EffStack IO) init prepared -> Parser (IO ())
defaultRunSubCommand = runSubCommand runEffs

runSubCommand ::
  forall init prepared sig m.
  Has (Lift IO) sig m =>
  (m () -> IO ()) ->
  SubCommand m init prepared ->
  Parser (IO ())
runSubCommand runEff SubCommand{..} = runEff . mergeAndRun <$> parser
  where
    mergeAndRun :: init -> m ()
    mergeAndRun cliOptions = do
      configFile <- configLoader cliOptions
      envvars <- getEnvVars
      prepared <- optMerge configFile envvars cliOptions
      perform prepared

runEffs :: EffStack IO () -> IO ()
runEffs = withDefaultLogger SevInfo . logWithExit_ . runReadFSIO . runExecIO
