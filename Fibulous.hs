{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Semigroup
import Control.Applicative
import Data.Ratio
import Data.Complex
import Data.List

import Data.FiniteField.PrimeField

import Data.Containers.ListUtils
import Data.Proxy
import Data.Singletons.TypeLits
import Numeric.Natural
import qualified Data.Set as Set
import Iso.Deriving
import Control.Alternative.Free
import Linear
import Linear.Affine
import Data.Maybe

newtype Fib a = F (Complex a) deriving (Eq, Functor, Applicative, Additive)

deriving via (V2 a `As` Fib a) instance Ord a => Ord (Fib a)

instance Inject (V2 a) (Fib a) where
  inj (V2 a b) = Fib a b

instance Project (V2 a) (Fib a) where
  prj (Fib a b) = V2 a b

pattern Fib a b = F (a :+ b)

instance Show a => Show (Fib a) where
  show (Fib p n) = show p <> " + (" <> show n <> ")f"

extractReal, extractFib :: Fib a -> a
extractReal (Fib real _) = real
extractFib (Fib _ f) = f

instance Num a => Semigroup (Fib a) where
  (Fib a1 a2) <> (Fib b1 b2) = Fib (a1*b1 + a2*b2) (a1*b2 + a2*b1 + a2*b2)

(*#) :: Num a => Complex a -> Complex a -> Complex a
(a1 :+ a2) *# (b1 :+ b2) = (a1*b1 + a2*b2) :+ (a1*b2 + a2*b1 + a2*b2)

inverse :: (Fractional a, Eq a) => Fib a -> Maybe (Fib a)
inverse (Fib a b)
  | a == 0 && b /= 0 = Just $ Fib (1/(-b)) (1/b)
  | a /=0 && b^2 /= a*(a + b) = Just $
      Fib ((a + b)/ (a^2 + a*b -b^2)) (-b/(a^2 + a*b - b^2))
  | otherwise = Nothing

binet :: Floating a => a -> a
binet n = ((5 + sqrt 5)**n - (5 - sqrt 5)**n)/(sqrt 5 * 2**n)

fibinet :: Floating a => a -> Fib a
fibinet n = Fib (binet (n-1)) (binet n)


instance Num a => Monoid (Fib a) where
  mempty = Fib 1 0

instance Num a => Num (Fib a) where
  (+)  = liftA2 (+)
  (*) = (<>)
  fromInteger n = Fib (fromInteger n) 0
  negate      = (<> Fib (fromInteger $ -1) 0)
  --    signum      = fmap signum

instance (Eq a, Fractional a) => Fractional (Fib a) where
  recip f = case inverse f of
    Just inverted -> inverted
    Nothing -> error "ahhh"
  fromRational nd = fromInteger (numerator nd) * recip (fromInteger (denominator nd))

f :: Num a => Fib a
f = Fib 0 1 -- 0 + f

fi :: (Eq a, Fractional a) => Fib a
fi = fromJust $ inverse f

logFib :: (Integral b, Num a) => b -> a
logFib n = extractFib $ f^n

flatten :: Num a => Fib (Fib a) -> Fib a
flatten (Fib (Fib rr rf) (Fib fr ff)) = Fib rr 0 + (Fib rf 0 + Fib fr 0)*f + (Fib ff 0)*f^2

expFib :: Natural -> Natural
expFib 0 = 1
expFib 1 = 1
expFib n = expFib (n-1) + expFib (n-2)

linearFib :: Natural -> Natural
linearFib = helper 0 1
  where helper _ b 1 = b
        helper a b n = helper b (a + b) (n - 1) 

fibsUnder :: forall n. KnownNat n => [Fib (PrimeField n)]
fibsUnder = Fib <$> numbers <*> numbers
  where numbers = fmap fromIntegral [0 .. natVal (Proxy @n) - 1]

invertibleFibs :: forall n. KnownNat n => [Fib (PrimeField n)]
invertibleFibs = filter ((/= Nothing) . inverse) $ fibsUnder @n

getGenerators :: forall n. KnownNat n => [Fib (PrimeField n)]
getGenerators = filter (\g -> generates g == fibsSet) invertibleFibs
  where
    generates g = Set.map (g^) $ Set.fromList [0 .. Set.size fibsSet]
    fibsSet = Set.fromList invertibleFibs

findInverses :: KnownNat n => Fib (PrimeField n) -> [Fib (PrimeField n)]
findInverses n = filter (\n2 -> n*n2 == Fib 1 0) fibsUnder

areNotUniquelyInvertible :: KnownNat n => [Fib (PrimeField n)]
areNotUniquelyInvertible = filter (\n -> length (findInverses n) /= 1) fibsUnder

areNotInvertible :: KnownNat n => [Fib (PrimeField n)]
areNotInvertible = filter (\n -> length (findInverses n) == 0) fibsUnder


any2Closed :: (Ord a, Semigroup a) => [a] -> Bool
any2Closed l = Set.fromList l == Set.fromList ((<>) <$> l <*> l)
-- isPrime :: (Integral a, Enum a) => Fib a -> Bool
-- isPrime f = filter (/= Fib 0 1) $ Fib <$> [0..max] <*> [1..max]
--   where max = maximum f


power :: RealFloat a => Fib (Complex a) -> Complex a -> Fib (Complex a)
power (Fib a b) n =
  Fib
    (
      ( a*(phi**n - (-phi)**(-n))
      + b*(phi**(n-1) + phi*(-phi)**(-n))
      )/sqrt 5
    )
    (
      ( a*(phi**(n+1) + (-phi)**n/phi)
      + b*(phi**n) - (-phi)**(-n)
      )/sqrt 5
    )
  where phi = (1 + sqrt 5)/2 :+ 0


--(a,b)

--(b, a + b)

--(a+b, a + 2b)

--(a + 2b, 2a + 3b)

--(2a + 3b, 3a + 5b)

--(3a + 5b, 5a + 8b)

--(5a + 8b, 8a + 13b)

--(8a + 13b, 13a + 21b)


main :: IO ()
main = print (areNotInvertible @37)


