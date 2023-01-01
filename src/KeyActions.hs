module KeyActions
  ( KeyActions(..)
  , composeKeys
  , repeatKeys
  , identityKeys
  , mapGenerate
  , mapSave
  , mapLoad
  , gen
  , intActions ) where

import Data.Foldable
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.IO
import System.Random
import Parse

data KeyActions g s l k =
  KeyActions { generate :: g k
             , save :: k -> s ()
             , load :: l k }

composeKeys :: (Applicative g, Applicative s, Applicative l) =>
               KeyActions g s l k1 ->
               KeyActions g s l k2 ->
               KeyActions g s l (k1,k2)
composeKeys ka1 ka2 =
    KeyActions
      ((,) <$> generate ka1 <*> generate ka2)
      (\(k1,k2) -> save ka1 k1 *> save ka2 k2)
      ((,) <$> load ka1 <*> load ka2)

repeatKeys :: (Applicative g, Applicative s, Applicative l) =>
              Int -> KeyActions g s l k ->
                    KeyActions g s l [k]
repeatKeys n ka =
  KeyActions
    (replicateM n (generate ka))
    (traverse_ (save ka))
    (replicateM n (load ka))

identityKeys :: (Applicative g, Applicative s, Applicative l) =>
                KeyActions g s l ()
identityKeys =
    KeyActions
      (pure ())
      (const $ pure ())
      (pure ())

mapGenerate :: (g k -> g' k) -> KeyActions g s l k->
               KeyActions g' s l k
mapGenerate fromGen keyActions =
  KeyActions (fromGen . generate $ keyActions)
             (save keyActions)
             (load keyActions)

mapSave :: (s () -> s' ()) -> KeyActions g s l k ->
           KeyActions g s' l k
mapSave fromSave keyActions =
  KeyActions (generate keyActions)
             (fromSave . save keyActions)
             (load keyActions)

mapLoad :: (l k -> l' k) -> KeyActions g s l k ->
           KeyActions g s l' k
mapLoad fromLoad keyActions =
  KeyActions (generate keyActions)
             (save keyActions)
             (fromLoad . load $ keyActions)

gen :: (Random a, RandomGen g) => State g a
gen = state random

intActions :: KeyActions
                (State StdGen)
                (ReaderT Handle IO)
                (StateT String (Either String))
                Int
intActions =
  KeyActions
    gen
    (\k -> ReaderT (flip hPrint k))
    (many parseWhiteSpace *> parseInt <* many parseWhiteSpace)

