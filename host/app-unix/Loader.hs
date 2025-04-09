module Loader (
  Library,
  loadLibrary,
  getFunction,
  freeLibrary,
) where

import Foreign.Ptr (FunPtr)
import System.Posix.DynamicLinker

newtype Library = MkLibrary DL

loadLibrary :: String -> IO Library
loadLibrary path = MkLibrary <$> dlopen path [RTLD_NOW, RTLD_LOCAL]

getFunction :: Library -> String -> IO (FunPtr a)
getFunction (MkLibrary lib) = dlsym lib

freeLibrary :: Library -> IO ()
freeLibrary (MkLibrary lib) = dlclose lib
