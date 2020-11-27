module Main where

import RSA

import System.Environment
import Data.Char
import Control.Monad.Trans.Maybe

main :: IO ()
main = getArgs >>= parse

parse ["-k", bits] = do
    let b = read bits :: Integer
    genM <- runMaybeT $ generate b
    case genM of
        Nothing -> putStrLn "Error generating keys"
        Just (n, e, d) -> do
          putStrLn "Modulus"
          print (n :: Integer)
          putStrLn "Public Key"
          print (e :: Integer)
          putStrLn "Private Key"
          print (d :: Integer)
parse ["-e", modulus, encryptionKey] = do
    message <- getLine
    let normalized = map toUpper $ filter isAlpha message
        e = read encryptionKey :: Integer
        n = read modulus :: Integer
        ciphertext = encryptString n e normalized
    print ciphertext

parse ["-d", modulus, decryptionKey] = do
    ciphertext <- getLine
    let d = read decryptionKey :: Integer
        n = read modulus :: Integer
        normalized = read ciphertext :: [Integer]
        plaintext = decryptString n d normalized
    print plaintext