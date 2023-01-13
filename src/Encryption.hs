module Encryption
  ( Encryption(..)
  , keyActionMap
  ) where

import KeyActions
import Encryptor
import KeyComposed

data Encryption g s l k dec enc =
  Encryption
    (KeyActions g s l k)
    (Encryptor k dec enc)

instance (Applicative g, Monoid s, Applicative l) =>
         KeyComposed (Encryption g s l) where
  strongCompose (Encryption ka1 enc1)
                (Encryption ka2 enc2) =
    Encryption (composeKeys ka1 ka2)
               (strongCompose enc1 enc2)
    
  compose (Encryption ka1 enc1)
          (Encryption ka2 enc2) =
    Encryption (ka1)
               (compose enc1 enc2)
    
  identity = Encryption identityKeys identity
  
  strongPower n (Encryption ka enc) =
    Encryption (repeatKeys n ka)
               (strongPower n enc)
    
  power n (Encryption ka enc) =
    Encryption ka (power n enc)


keyActionMap :: (KeyActions g s l k -> KeyActions g' s' l' k) ->
                Encryption g s l k enc dec -> Encryption g' s' l' k enc dec
keyActionMap keyActionsTrans (Encryption keyActions encryptor) =
  Encryption (keyActionsTrans keyActions) encryptor
