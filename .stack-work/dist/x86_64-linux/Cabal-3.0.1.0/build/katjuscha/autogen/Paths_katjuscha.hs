{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_katjuscha (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/bin"
libdir     = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/lib/x86_64-linux-ghc-8.8.4/katjuscha-0.0.0-IppXWNAtFzC51EuPNbixWz-katjuscha"
dynlibdir  = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/share/x86_64-linux-ghc-8.8.4/katjuscha-0.0.0"
libexecdir = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/libexec/x86_64-linux-ghc-8.8.4/katjuscha-0.0.0"
sysconfdir = "/home/zypeh/Projects/Katjuscha/.stack-work/install/x86_64-linux/62339518f73cb40818f46df194829b1e4a214ff4bb2dd53dfca858c0d1ba8225/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "katjuscha_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "katjuscha_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "katjuscha_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "katjuscha_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "katjuscha_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "katjuscha_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
