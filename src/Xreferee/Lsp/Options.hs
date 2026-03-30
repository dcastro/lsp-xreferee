module Xreferee.Lsp.Options where

import ClassyPrelude
import Options.Applicative qualified as Opt

data CliOptions = CliOptions
  { showVersionFlag :: Bool
  }

cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> Opt.switch
      ( Opt.long "version"
          <> Opt.help "Print version and exit"
      )

cliParserInfo :: Opt.ParserInfo CliOptions
cliParserInfo =
  Opt.info
    (cliOptionsParser Opt.<**> Opt.helper)
    Opt.fullDesc
