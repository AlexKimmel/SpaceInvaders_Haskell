{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SDLEvent (
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

bindir     = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/bin"
libdir     = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/lib/aarch64-osx-ghc-8.10.7/SDLEvent-0.1.0.0-Cd79g5aXUGuJSiBKcyjYpk"
dynlibdir  = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/share/aarch64-osx-ghc-8.10.7/SDLEvent-0.1.0.0"
libexecdir = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/libexec/aarch64-osx-ghc-8.10.7/SDLEvent-0.1.0.0"
sysconfdir = "/Users/alexkimmel/Uni/FP/Project/Project/project/SDL2FLight/.stack-work/install/aarch64-osx/cca880b06214c915afdbcfd9ce775e5baee8b1b4bc49128974df3d46f5e4badb/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SDLEvent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SDLEvent_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SDLEvent_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SDLEvent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SDLEvent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SDLEvent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
