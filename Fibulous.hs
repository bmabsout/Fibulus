{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
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

data Fib a = Fib a a deriving (Eq, Ord, Functor, Foldable)

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

-- invertFiniteFib :: (Integral a, Eq a) => Fib a -> Fib a
-- invertFiniteFib (Fib a b) 
--  | a == 0 && b /= 0 = Fib (1 `div` (-b)) (1 `div` b)
--  | a /=0 && b^2 /= a*(a + b)  = Fib ((a + b) `div` (a^2 + a*b -b^2)) (-b `div` (a^2 + a*b - b^2))
--  | otherwise = error "ahhh"
      

--a*ia + b*ib = 1
--(a*ib + b*ia + b*ib) = 0

binet :: Floating a => a -> a
binet n = ((5 + sqrt 5)**n - (5 - sqrt 5)**n)/(sqrt 5 * 2**n)

fibinet :: Floating a => a -> Fib a
fibinet n = Fib (binet (n-1)) (binet n)


--  Fib (a*ia + b*ib) (a*ib + b*ia + b*ib) = Fib 1 0
--  Fib a b * Fib ia ib = Fib 1 0

instance Applicative Fib where
  pure a = Fib a a
  liftA2 f (Fib a1 a2) (Fib b1 b2) = Fib (f a1 b1) (f a2 b2)

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

logFib :: (Integral b, Num a) => b -> a
logFib n = extractFib $ (Fib 0 1)^n

flatten :: Num a => Fib (Fib a) -> Fib a
flatten (Fib (Fib rr rf) (Fib fr ff)) = Fib rr 0 + (Fib rf 0 + Fib fr 0)*(Fib 0 1) + (Fib ff 0)*(Fib 0 1)^2

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
findInverses f = filter (\f2 -> f*f2 == Fib 0 1) fibsUnder

areNotInvertible :: KnownNat n => [Fib (PrimeField n)]
areNotInvertible = filter (\f -> length (findInverses f) /= 1) fibsUnder


any2Closed :: (Ord a, Semigroup a) => [a] -> Bool
any2Closed l = Set.fromList l == Set.fromList ((<>) <$> l <*> l)
-- isPrime :: (Integral a, Enum a) => Fib a -> Bool
-- isPrime f = filter (/= Fib 0 1) $ Fib <$> [0..max] <*> [1..max]
--   where max = maximum f



--(a,b)

--(b, a + b)

--(a+b, a + 2b)

--(a + 2b, 2a + 3b)

--(2a + 3b, 3a + 5b)

--(3a + 5b, 5a + 8b)

--(5a + 8b, 8a + 13b)

--(8a + 13b, 13a + 21b)


main :: IO ()
main = print (logFib 100000)


