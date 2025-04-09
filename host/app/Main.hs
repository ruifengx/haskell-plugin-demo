module Main (main) where

import Foreign.Ptr (FunPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr)
import System.Environment (getArgs)
import System.Posix.DynamicLinker (RTLDFlags (..), dlclose, dlopen, dlsym)

import Host.Library

foreign import ccall "dynamic"
  runLoadPlugin :: FunPtr (IO (StablePtr Plugin)) -> IO (StablePtr Plugin)

main :: IO ()
main = do
  [pluginPath] <- getArgs
  pluginLib <- dlopen pluginPath [RTLD_NOW, RTLD_LOCAL]
  pLoadPlugin <- dlsym pluginLib "loadPlugin"
  pPlugin <- runLoadPlugin pLoadPlugin
  Plugin{parser, matcher} <- deRefStablePtr pPlugin
  case parse parser "te.*t" of
    Nothing -> error "cannot parse the regex"
    Just r -> do
      putStrLn ("parsed regex: " ++ show r)
      if match matcher r "test"
        then putStrLn "matching succeeded!"
        else error "matching failed!"
  freeStablePtr pPlugin
  dlclose pluginLib
