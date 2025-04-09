module Host.Library (
  Regex (..),
  parse,
  match,
  Plugin (..),
) where

import Control.Applicative ((<**>), (<|>))
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

data Regex a
  = Epsilon
  | Atom Char
  | Union (Regex a) (Regex a)
  | Concat (Regex a) (Regex a)
  | Star (Regex a)
  | Custom a
  deriving stock (Show, Eq)

parse :: ReadP a -> String -> Maybe (Regex a)
parse k = fmap fst . listToMaybe . readP_to_S (pRegex k <* eof)

pInfix :: (a -> b) -> (a -> b -> b) -> ReadP a -> ReadP op -> ReadP b
pInfix uni bin x op = chain
  where
    chain = x <**> (rest <++ pure uni)
    rest = (\_ r a -> bin a r) <$> op <*> chain

pRegex, pConcat, pStar, pAtom :: ReadP a -> ReadP (Regex a)
pRegex k = pInfix id Union (pConcat k) (char '|')
pConcat k = pInfix id Concat (pStar k) (pure ()) <++ pure Epsilon
pStar k = pAtom k <**> option id (Star <$ char '*')
pAtom k = Custom <$> k <|> Atom <$> satisfy (`notElem` "|*()") <|> char '(' *> pRegex k <* char ')'

match :: (a -> ReadP ()) -> Regex a -> String -> Bool
match k r = not . null . readP_to_S (pMatchRegex k r <* eof)

pMatchRegex :: (a -> ReadP ()) -> Regex a -> ReadP ()
pMatchRegex _ Epsilon = pure ()
pMatchRegex _ (Atom c) = void (char c)
pMatchRegex k (Union x y) = pMatchRegex k x <|> pMatchRegex k y
pMatchRegex k (Concat x y) = pMatchRegex k x *> pMatchRegex k y
pMatchRegex k (Star x) = skipMany (pMatchRegex k x)
pMatchRegex k (Custom p) = k p

data Plugin = forall a. (Show a, Eq a) => Plugin
  { parser :: ReadP a
  , matcher :: a -> ReadP ()
  }
