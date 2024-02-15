module Lib (
  logIter,
  logDir
) where

import System.Directory

-- | output this itteration as a log file with a unique name
logIter :: String -> Int -> IO ()
logIter s iteration = writeFile (".REL_log_files/REL_logfile_" ++ (show iteration)) s

-- | creates log directory
logDir :: IO ()
logDir = createDirectory ".REL_log_files"
