{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_final (
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
bindir     = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/bin"
libdir     = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/lib/x86_64-linux-ghc-9.2.5/final-0.1.0.0-Jy67A2u7ylD1mLZ58f1e04-final"
dynlibdir  = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/share/x86_64-linux-ghc-9.2.5/final-0.1.0.0"
libexecdir = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/libexec/x86_64-linux-ghc-9.2.5/final-0.1.0.0"
sysconfdir = "/home/chris/Documents/Homework/CSE_230/Project/final/.stack-work/install/x86_64-linux/f09561fa88e813ebc5d8d80175e184fd4d189213e679d2c3d917b731461400a1/9.2.5/etc"

getBinDir     = catchIO (getEnv "final_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "final_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "final_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "final_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "final_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "final_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
