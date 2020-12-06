module RSA where

import Data.Char
import System.Random
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust)

-- https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.6
expmod :: (Integral a1, Integral a2) => a2 -> a1 -> a2 -> a2
expmod b 0 m = 1
expmod b e m
  | even e = (expmod b (e `div` 2) m) ^ 2 `mod` m
  | otherwise = (b * expmod b (e - 1) m) `mod` m

expmod' :: (Integral a1, Integral a2) => a2 -> a1 -> a2 -> Maybe a2
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

randomIO' :: (MonadIO m, Random a) => m a
randomIO' = liftIO randomIO

-- random integer in [lo,hi)
randomInteger :: (MonadIO m, Integral a, Random a) => a -> a -> m a
randomInteger lo hi = (+ lo) . (`mod` (hi - lo)) <$> randomIO'

testTimes = 5

millerRabinTest :: (MonadIO m, Integral a, Random a) => a -> m Bool
millerRabinTest n = do
  -- a <- (+1) . (`mod` (n - 1)) <$> randomIO -- random number between 1 and n-1
  a <- randomInteger 1 n -- random number between 1 and n-1 (inclusive)
  case expmod' a (n-1) n of
    Nothing -> return False -- nontrivial sqrt of 1 mod n
    Just k -> return $ k == 1

fastPrimeTest :: (MonadIO m, Integral a, Random a) => Int -> a -> m Bool
fastPrimeTest _ 1 = return False
fastPrimeTest t n = do
  tests <- replicateM t (millerRabinTest n)
  return $ and tests
bitLength :: (Integral b, Integral a) => a -> b

bitLength n = floor $ logBase 2 n' + 1
  where n' = fromIntegral n

-- return a random prime between 2 and n - 1
randomPrime :: (MonadIO m, Random b, Integral b) => b -> m b
randomPrime n = do
  let n' = n + n `mod` 2 -- make n even
  x0 <- randomInteger 1 n
  let p0 = x0 + 1 - x0 `mod` 2  -- start with odd p
      ks = iterate (\x -> (x + 2) `mod` n') p0
  fromJust <$> firstM (fastPrimeTest testTimes) ks -- if it terminates, guaranteed to be Just p

-- generate e coprime with phi s.t. 1 < e < n
-- n > 2
randomExp :: (MonadPlus m, MonadIO m, Integral a, Random a) => a -> a -> m a
randomExp n phi = do
  e0 <- randomInteger 2 n
  let ks = iterate next e0
  first test ks
  -- firstM (return <$> test) ks
  where
    next e = let e' = (e + 1) `mod` n
              in if e' <= 2 then 3 else e'
    test e = gcd phi e == 1

first :: MonadPlus m => (a -> Bool) -> [a] -> m a
first test [] = mzero
first test (x:xs)
  | test x    = return x
  | otherwise = first test xs

-- compute the inverse of a mod n
-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
inverse :: (MonadPlus m, Integral a) => a -> a -> m a
inverse a n = go 0 n 1 a
  where
    go t r newt 0
      | r > 1     = mzero  -- a is not invertible mod n
      | t < 0     = return $ t + n
      | otherwise = return t
    go t r newt newr = go t' r' newt' newr'
      where t' = newt
            r' = newr
            newt' = t - quot * newt
            newr' = r - quot * newr
            quot = r `div` newr

generate :: (MonadIO m, Random c, Integral c, MonadPlus m, Integral b) => b -> m (c, c, c)
generate b = do
  let maxPrime = 2 ^ (b `div` 2)
  p <- randomPrime maxPrime
  q <- randomPrime maxPrime
  let n = p * q
      phi = (p - 1) * (q - 1)
  e <- liftIO $ randomExp n phi
  d <- inverse e phi
  return (n, e, d)

encrypt n e p = expmod p e n

decrypt n d c = expmod c d n

encryptString n e = map (encrypt n e . char2num)

decryptString n d = map (num2char . decrypt n d)

-- the following are only valid for characters in ['A'..'Z']
-- 'A' is mapped to 1, 'B' to 2, and so on
char2num :: Integral a => Char -> a
char2num c = fromIntegral $ ord c - ord 'A' + 1

num2char :: Integral a => a -> Char
num2char n = chr (fromIntegral n + fromIntegral (ord 'A') - 1)
