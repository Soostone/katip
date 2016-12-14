{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Katip.Compat
  ( ProcessID
  , getProcessID
  ) where

import System.Win32.Process

type ProcessID = ProcessId

#if !MIN_VERSION_Win32(2,4,0)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO ProcessId

getProcessID :: IO ProcessID
getProcessID = c_GetCurrentProcessId
#else
getProcessID :: IO ProcessID
getProcessID = getCurrentProcessId
#endif
