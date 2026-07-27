module Xreferee.Lsp.Capture where

{-
This module restores the "capture" functionality from the (now deprecated) `haskell-lsp` library.
The `haskell-lsp` was renamed to `lsp` when version 1.0.0.0 was released, and this is when the capture module was deleted.

See commit: 9e78cce4000dde4666cb04fa1fb59a38cc3d8a2a
https://github.com/haskell/lsp/commit/9e78cce4000dde4666cb04fa1fb59a38cc3d8a2a
-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics
import Language.LSP.Protocol.Message (FromClientMessage, FromServerMessage)
import System.IO
import Prelude

data Event
  = FromClient !UTCTime !FromClientMessage
  | FromServer !UTCTime !FromServerMessage
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CaptureContext = NoCapture | Capture (TChan Event)

noCapture :: CaptureContext
noCapture = NoCapture

captureToFile :: FilePath -> IO CaptureContext
captureToFile fname = do
  -- logs $ "haskell-lsp:Logging to " ++ fname
  chan <- newTChanIO
  _tid <- forkIO $ withFile fname WriteMode $ writeToHandle chan
  return $ Capture chan

captureFromServer :: FromServerMessage -> CaptureContext -> IO ()
captureFromServer _ NoCapture = return ()
captureFromServer msg (Capture chan) = do
  time <- getCurrentTime
  atomically $ writeTChan chan $ FromServer time msg

captureFromClient :: FromClientMessage -> CaptureContext -> IO ()
captureFromClient _ NoCapture = return ()
captureFromClient msg (Capture chan) = do
  time <- getCurrentTime
  atomically $ writeTChan chan $ FromClient time msg

writeToHandle :: TChan Event -> Handle -> IO ()
writeToHandle chan hdl = forever $ do
  ev <- atomically $ readTChan chan
  BSL.hPutStrLn hdl $ encode ev
