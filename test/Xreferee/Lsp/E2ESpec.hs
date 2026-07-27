module Xreferee.Lsp.E2ESpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Language.LSP.Test
import Text.Pretty.Simple (pShow)
import Xreferee.Lsp.TestPrelude

-- import Language.LSP.Test.Parsing

spec :: Spec
spec =
  describe "e2e" do
    it "deleteSymbolsForFileOrDirectory" do
      main

{-
  -- NOTE: `lsp-test` does NOT set the server process's working directory to `rootDir`;
  -- the server inherits the test runner's cwd. `xreferee` discovers the git repo
  -- (and thus all anchors/references) from its cwd, so we have to set it explicitly.
  let setCwd p = p {cwd = Just rootDir}

  runSessionWithConfigCustomProcess setCwd config "lsp-xreferee" fullLatestClientCaps rootDir $ do

 -}
main = do
  putStrLn "------------------------------------------ 1"
  let config =
        defaultConfig
          { logStdErr = False,
            logMessages = False,
            -- { logStdErr = True,
            --   logMessages = True,
            logColor = True,
            lspConfig = KM.fromList ["lsp-xreferee" .= object []]
          }

  let rootDir = "/home/dc/Dropbox/Projects/xreferee/example-xreferee"

  -- NOTE: `lsp-test` does NOT set the server process's working directory to `rootDir`;
  -- the server inherits the test runner's cwd.
  --
  -- We rely on the server `chdir`ing into the workspace directory it receives in the
  -- `initialize` request, see `Xreferee.Lsp.setWorkspaceDir`.
  runSessionWithConfig config "lsp-xreferee" fullLatestClientCaps rootDir $ do
    putStrLn "------------------------------------------ 2"
    _doc <- openDoc "didOpen.md" "markdown"
    putStrLn "------------------------------------------ 3"

    diags <- waitForDiagnostics
    putStrLn $ toStrict $ pShow diags
    diags <- waitForDiagnostics
    putStrLn $ toStrict $ pShow diags
    putStrLn "------------------------------------------ 4"

    -- print diags

    pure ()
