{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_exercises (
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
bindir     = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\bin"
libdir     = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\lib\\x86_64-windows-ghc-9.2.8\\exercises-0.1.0.0-6Cy5laklgf66rJotfjt2PA-main"
dynlibdir  = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\lib\\x86_64-windows-ghc-9.2.8"
datadir    = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\share\\x86_64-windows-ghc-9.2.8\\exercises-0.1.0.0"
libexecdir = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\libexec\\x86_64-windows-ghc-9.2.8\\exercises-0.1.0.0"
sysconfdir = "D:\\Code\\Monash\\Year 2\\FIT2102\\a2\\mwan0165 - Copy\\Haskell\\.stack-work\\install\\8eb5e8e5\\etc"

getBinDir     = catchIO (getEnv "exercises_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "exercises_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "exercises_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "exercises_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercises_sysconfdir") (\_ -> return sysconfdir)




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
