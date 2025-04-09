module Loader (
  Library,
  loadLibrary,
  getFunction,
  freeLibrary,
) where

import Foreign.Ptr (FunPtr, castPtrToFunPtr)
import System.Win32.DLL qualified as Win32

newtype Library = MkLibrary Win32.HMODULE

loadLibrary :: String -> IO Library
loadLibrary path = MkLibrary <$> Win32.loadLibrary path

getFunction :: Library -> String -> IO (FunPtr a)
getFunction (MkLibrary lib) name = castPtrToFunPtr <$> Win32.getProcAddress lib name

freeLibrary :: Library -> IO ()
freeLibrary (MkLibrary lib) = Win32.freeLibrary lib
