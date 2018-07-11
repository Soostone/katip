{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Katip.Compat
  ( ProcessID
  , getProcessID
  ) where

import System.Win32.Process

type ProcessID = ProcessId

getProcessID :: IO ProcessID
getProcessID = getCurrentProcessId
