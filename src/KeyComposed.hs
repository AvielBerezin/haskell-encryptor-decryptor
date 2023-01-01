module KeyComposed (KeyComposed(..)) where

class KeyComposed alg where
  strongCompose :: alg k1 b c -> alg k2 a b -> alg (k1,k2) a c
  compose :: alg k b c -> alg k a b -> alg k a c
  identity :: alg () a a
  strongPower :: Int -> alg k a a -> alg [k] a a
  power :: Int -> alg k a a -> alg k a a


