module Xreferee.Lsp.Options where

import ClassyPrelude
import Options.Applicative qualified as Opt

data CliOptions = CliOptions
  { showVersionFlag :: Bool,
    logFilePath :: Maybe FilePath
  }

cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> Opt.switch
      ( Opt.long "version"
          <> Opt.help "Print version and exit"
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "log-file"
              <> Opt.metavar "FilePath"
              <> Opt.help "Write logs to the given file"
          )
      )

cliParserInfo :: Opt.ParserInfo CliOptions
cliParserInfo =
  Opt.info
    (cliOptionsParser Opt.<**> Opt.helper)
    Opt.fullDesc
