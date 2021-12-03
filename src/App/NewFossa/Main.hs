{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Main (appMain) where

import App.NewFossa.Config.Analyze qualified as Analyze
import App.NewFossa.Subcommand (EffStack, SubCommand (..), defaultRunSubCommand)
import App.Version (fullVersionDescription)
import Control.Monad (join)
import Data.String.Conversion (toString)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  ParserPrefs,
  command,
  customExecParser,
  fullDesc,
  header,
  help,
  helpShowGlobals,
  helper,
  hsubparser,
  info,
  infoOption,
  long,
  prefs,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  subparserInline,
  (<**>),
 )

appMain :: IO ()
appMain = join $ customExecParser mainPrefs $ info (subcommands <**> helper <**> versionOpt) progData

versionOpt :: Parser (a -> a)
versionOpt = infoOption (toString fullVersionDescription) (long "version" <> short 'V' <> help "show version information")

progData :: InfoMod (IO ())
progData = fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"

subcommands :: Parser (IO ())
subcommands =
  hsubparser $
    mconcat
      [ decodeSubCommand Analyze.subcommand
      ]

decodeSubCommand :: SubCommand (EffStack IO) a b -> Mod CommandFields (IO ())
decodeSubCommand cmd@SubCommand{..} = command commandName $ info (defaultRunSubCommand cmd) commandInfo

mainPrefs :: ParserPrefs
mainPrefs =
  prefs $
    mconcat
      [ showHelpOnEmpty
      , showHelpOnError
      , subparserInline
      , helpShowGlobals
      ]
