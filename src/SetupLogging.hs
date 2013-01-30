module SetupLogging
  ( setupLogging
  , str2logPriority
  ) where

import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

import Base

--
-- | Sets-up logging according to the provided configuration. PANE's logging
-- consists of console output and an optional file output. Each output can
-- have its log message level set independently.
--
setupLogging :: PaneConfig -> IO ()
setupLogging config = do
  let fmt = tfLogFormatter "%F %X.%q %Z" "$time - $prio - $loggername: $msg"
  s <- streamHandler stderr (logScreenPrio config)
  updateGlobalLogger rootLoggerName
        (setLevel DEBUG . setHandlers [ setFormatter s fmt ] )

  case logFilePath config of
      "" -> do return ()
      _  -> do f <- fileHandler (logFilePath config) (logFilePrio config)
               updateGlobalLogger rootLoggerName
                     (addHandler (setFormatter f fmt))

-- TODO(adf): str2upper ?
str2logPriority :: String -> Priority
str2logPriority str = case str of
    "DEBUG"     -> DEBUG
    "INFO"      -> INFO
    "NOTICE"    -> NOTICE
    "WARNING"   -> WARNING
    "ERROR"     -> ERROR
    "CRITICAL"  -> CRITICAL
    "ALERT"     -> ALERT
    "EMERGENCY" -> EMERGENCY
    _           -> DEBUG
