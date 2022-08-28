{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Domain (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\cabal\\bin"
libdir     = "D:\\cabal\\x86_64-windows-ghc-9.0.2\\Domain-0.1.0.0-inplace"
dynlibdir  = "D:\\cabal\\x86_64-windows-ghc-9.0.2"
datadir    = "D:\\cabal\\x86_64-windows-ghc-9.0.2\\Domain-0.1.0.0"
libexecdir = "D:\\cabal\\Domain-0.1.0.0-inplace\\x86_64-windows-ghc-9.0.2\\Domain-0.1.0.0"
sysconfdir = "D:\\cabal\\etc"

getBinDir     = catchIO (getEnv "Domain_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Domain_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Domain_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Domain_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Domain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Domain_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
