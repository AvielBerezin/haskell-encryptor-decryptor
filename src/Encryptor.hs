module Encryptor
  ( Encryptor(..)
  , liftKey
  , shift
  , operationAndKeyReverseEncryptor
  , intCharOpFrom
  , addEncrpytor
  , multEncryptor
  , shiftUp
  , shiftMult
  , shiftXor ) where

import KeyComposed
import Data.Char
import Data.Bits hiding (shift)

data Encryptor k dec enc =
  Encryptor { encrypt :: k -> dec -> enc
            , decrypt :: k -> enc -> dec }

instance KeyComposed Encryptor where
  strongCompose encryptor1 encryptor2 =
    Encryptor (\(k1,k2) -> encrypt encryptor1 k1 . encrypt encryptor2 k2)
              (\(k1,k2) -> decrypt encryptor2 k2 . decrypt encryptor1 k1)
    
  compose encryptor1 encryptor2 =
    Encryptor (\k -> encrypt encryptor1 k . encrypt encryptor2 k)
              (\k -> decrypt encryptor2 k . decrypt encryptor1 k)
    
  identity = Encryptor (const id) (const id)
  
  strongPower n encryptor =
    foldr strCompose (liftKey (const ()) identity) $ replicate n encryptor
    where
      strCompose encryptor1 encryptor2 =
        liftKey (\(x:xs) -> (x,xs)) $ strongCompose encryptor1 encryptor2
        
  power n encryptor =
    foldr compose (liftKey (const ()) identity) (replicate n encryptor)

liftKey :: (k1 -> k2) -> Encryptor k2 a b -> Encryptor k1 a b
liftKey k1ToK2 encryptor =
  Encryptor (encrypt encryptor . k1ToK2)
            (decrypt encryptor . k1ToK2)

operationAndKeyReverseEncryptor :: (k -> a -> a) -> (k -> k) -> Encryptor k a a
operationAndKeyReverseEncryptor op kRev =
  Encryptor op (op . kRev)

intCharOpFrom :: (Int -> Int -> Int) -> Int -> Char -> Char
intCharOpFrom op n c = chr $ mod (n `op` ord c) (ord maxBound + 1)

addEncrpytor :: Encryptor Int Char Char
addEncrpytor =
  operationAndKeyReverseEncryptor (intCharOpFrom (+)) negate

multEncryptor :: Encryptor Int Char Char
multEncryptor =
  operationAndKeyReverseEncryptor (intCharOpFrom (\k a -> rangedMult a k)) modReverse
  where
    bigPrime = 1114117
    modMult a b = mod (a * b) bigPrime
    rangedMult a b =
      if modMult a b <= maxBound
      then modMult a b
      else rangedMult (modMult a b) b
    modReverse = pow (bigPrime-2)
    pow 0 _ = 1
    pow 1 a = a
    pow n a | mod n 2 == 0 = pow (n `div` 2) a `modMult` pow (n `div` 2) a
            | otherwise = a `modMult` pow (n - 1) a

xorEncryptor :: Encryptor Int Char Char
xorEncryptor =
  operationAndKeyReverseEncryptor (intCharOpFrom xor) id

shift :: Functor f => Encryptor k a b -> Encryptor k (f a) (f b)
shift encryptor =
  Encryptor { encrypt = \k -> fmap $ encrypt encryptor k
            , decrypt = \k -> fmap $ decrypt encryptor k }

shiftUp :: Encryptor Int String String
shiftUp = shift addEncrpytor

shiftMult :: Encryptor Int String String
shiftMult = shift multEncryptor

shiftXor :: Encryptor Int String String
shiftXor = shift xorEncryptor
