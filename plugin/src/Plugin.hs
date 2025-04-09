module Plugin (plugin) where

import Data.Functor (void)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Text.ParserCombinators.ReadP (ReadP, char, get)

import Host.Library

data AnyChar = AnyChar
  deriving stock (Show, Eq)

plugin :: Plugin
plugin = Plugin{parser = parseAnyChar, matcher = matchAnyChar}

parseAnyChar :: ReadP AnyChar
parseAnyChar = AnyChar <$ char '.'

matchAnyChar :: AnyChar -> ReadP ()
matchAnyChar _ = void get

foreign export ccall "loadPlugin" loadPlugin :: IO (StablePtr Plugin)

loadPlugin :: IO (StablePtr Plugin)
loadPlugin = newStablePtr plugin
