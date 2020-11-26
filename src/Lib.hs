module Lib where

import Data.Char
import System.Random (randomIO)
import Control.Monad
import Control.Monad.Loops
import Data.Maybe (fromJust)

sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : filter (\y -> y `mod` x /= 0) (sieve xs)

-- https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.6
expmod b 0 m = 1
expmod b e m
  | even e = (expmod b (e `div` 2) m) ^ 2 `mod` m
  | otherwise = (b * expmod b (e - 1) m) `mod` m

expmod' b 0 m = Just 1
expmod' b e m
  | even e = do
      s <- expmod' b (e `div` 2) m
      if nonTrivialSqrtOf1 s m
        then Nothing
        else return $ s ^ 2 `mod` m
  | otherwise = do
    x <- expmod' b (e - 1) m
    return $ (b * x) `mod` m
  where
    nonTrivialSqrtOf1 s m = s /= 1 && s /= m - 1 && s ^ 2 `mod` m == 1

-- random integer in [lo,hi)
randomInteger lo hi = (+ lo) . (`mod` (hi - lo)) <$> randomIO

millerRabinTest n = do
  -- a <- (+1) . (`mod` (n - 1)) <$> randomIO -- random number between 1 and n-1
  a <- randomInteger 1 n -- random number between 1 and n-1 (inclusive)
  case expmod' a (n-1) n of
    Nothing -> return False -- nontrivial sqrt of 1 mod n
    Just k -> return $ k == 1

fastPrimeTest _ 1 = return False
fastPrimeTest t n = do
  tests <- replicateM t (millerRabinTest n)
  return $ and tests

bitLength n = floor $ logBase 2 n' + 1
  where n' = fromIntegral n

primes = sieve [2..]

-- probablePrimes = filterM (fastPrimeTest 10) [2..]

primesUpTo n = takeWhile (<= n) primes

primesWithBitLength b =
  takeWhile (\p -> bitLength p == b) $ dropWhile (\p -> bitLength p < b) primes

-- moduli b = filter (\(p,q,n) -> bitLength n == b) candidates
--   where candidates = [(p,q,p*q) | p <- primes', q <- primes']
--         primes' = takeWhile (\p -> bitLength p <= b) $ dropWhile (\p -> bitLength p < b `div` 2) primes

testTimes = 5

randomPrime n = do
  let n' = n + n `mod` 2 -- make n even
  x0 <- randomInteger 1 n
  let p0 = x0 + 1 - x0 `mod` 2  -- start with odd p
      ks = iterate (\x -> (x + 2) `mod` n') p0
  fromJust <$> firstM (fastPrimeTest testTimes) ks -- if it terminates, guaranteed to be Just p

-- randomPrime n = do
--   let n' = n + n `mod` 2 -- make n even
--   p0 <- randomInteger 0 n
--   let ks = iterate (\x -> (x + 2) `mod` n') p0
--   ts <- mapM (fastPrimeTest testTimes) ks
--   let ps = zip ks ts
--   return $ (fst . head) $ dropWhile (not . snd) ps

-- randomPrime n = do
--   p <- randomInteger 0 n
--   let n' = n + n `mod` 2    -- make n even
--   go (p + 1 - p `mod` 2) n  -- start with odd p
--     where
--       go p n = do
--         t <- fastPrimeTest testTimes p
--         if t then return p else go ((p + 2) `mod` n) n -- go to next odd number mod n

moduli b = do
  let minB = b `div` 2
  -- primes <- filterM (fastPrimeTest 10) $ takeWhile (\p -> bitLength p <= minB) [2..]
  primes <- filterM (fastPrimeTest testTimes) [2 ^ minB .. 2 ^ b - 1]
  -- indices <-
  let candidates = [(p,q,p*q) | p <- primes, q <- primes]
  return $ filter (\(p,q,n) -> bitLength n == b) candidates

encryptionKeys p q =
  let n = p * q
      phi = (p - 1) * (q - 1)
  in filter (\x -> gcd n x == 1 && gcd phi x == 1) [2 .. (phi-1)]

decryptionKeys p q e =
  let n = p * q
      phi = (p - 1) * (q - 1)
  in filter (\dc -> (dc * e) `mod` phi == 1) [1..]

rsa p q =
  let n = p * q
      phi = (p - 1) * (q - 1)
      es = encryptionKeys p q
      ds = map (\e -> (e, decryptionKeys e p q)) es
   in (n, ds)

sample p q k =
  let (n, ds) = rsa p q
   in (n, map (\(e, d) -> (e, take k d)) ds)

encrypt e n p = expmod p e n

decrypt d n c = expmod c d n

encryptString e n = map (encrypt e n . char2num)

decryptString d n = map (num2char . decrypt d n)

-- the following are only valid for characters in ['A'..'Z']
-- 'A' is mapped to 1, 'B' to 2, and so on
char2num :: Integral a => Char -> a
char2num c = fromIntegral $ ord c - ord 'A' + 1

num2char :: Integral a => a -> Char
num2char n = chr (fromIntegral n + fromIntegral (ord 'A') - 1)
