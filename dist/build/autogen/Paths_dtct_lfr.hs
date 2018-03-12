module Paths_dtct_lfr (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/yuta/.cabal/bin"
libdir     = "/home/yuta/.cabal/lib/x86_64-linux-ghc-7.10.3/dtct-lfr-0.1.0.0-40U5tP7Rxk87xWB9KA41qH"
datadir    = "/home/yuta/.cabal/share/x86_64-linux-ghc-7.10.3/dtct-lfr-0.1.0.0"
libexecdir = "/home/yuta/.cabal/libexec"
sysconfdir = "/home/yuta/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dtct_lfr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dtct_lfr_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "dtct_lfr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dtct_lfr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dtct_lfr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
