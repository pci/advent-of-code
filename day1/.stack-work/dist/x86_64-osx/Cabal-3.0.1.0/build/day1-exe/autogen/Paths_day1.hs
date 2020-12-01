{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_day1 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/bin"
libdir     = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/lib/x86_64-osx-ghc-8.8.4/day1-0.1.0.0-CW24WmB1mx2LOKU0XbYn0h-day1-exe"
dynlibdir  = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/share/x86_64-osx-ghc-8.8.4/day1-0.1.0.0"
libexecdir = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/libexec/x86_64-osx-ghc-8.8.4/day1-0.1.0.0"
sysconfdir = "/Users/philip/go/src/github.com/pci/adventofcode2020/day1/.stack-work/install/x86_64-osx/c621179a5112bec23075cba83c2d8bfa3d1b33d26322e312d13969dcf134b699/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "day1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "day1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "day1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "day1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
