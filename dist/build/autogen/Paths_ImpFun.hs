{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ImpFun (
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

bindir     = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/bin"
libdir     = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/ImpFun-0.1.0.0"
dynlibdir  = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/lib/ghc-8.0.2/ImpFun-0.1.0.0"
datadir    = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/ImpFun-0.1.0.0"
libexecdir = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/libexec"
sysconfdir = "/home/hussam/git/inf222-programming-languages/ImpFun/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ImpFun_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ImpFun_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ImpFun_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ImpFun_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ImpFun_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ImpFun_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
