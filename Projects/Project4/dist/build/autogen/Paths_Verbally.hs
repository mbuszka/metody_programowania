module Paths_Verbally (
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

bindir     = "/home/mbuszka/.cabal/bin"
libdir     = "/home/mbuszka/.cabal/lib/x86_64-linux-ghc-7.10.3/Verbally-0.1.0.0-A3lZK8lXvGvHjBW66XCk9s"
datadir    = "/home/mbuszka/.cabal/share/x86_64-linux-ghc-7.10.3/Verbally-0.1.0.0"
libexecdir = "/home/mbuszka/.cabal/libexec"
sysconfdir = "/home/mbuszka/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Verbally_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Verbally_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Verbally_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Verbally_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Verbally_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
