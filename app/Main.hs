{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import RSA

import Prelude hiding (putStr, putStrLn, getContents, lines, unlines)
import System.Environment
import Data.Char
import Control.Monad.Trans.Maybe
import Data.String (fromString)
-- import Data.String.ToString
import Crypto.Random.DRBG

import Data.ByteString.Lazy.Internal (unpackBytes)

import qualified Codec.Crypto.RSA.Pure as RSA
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Binary as Bin

main :: IO ()
main = getArgs >>= parse

privateKeyFile = "private.key"
publicKeyFile  = "public.key"

parse ["-k", bits] = do
    let b = read bits :: Int
    g <- newGenIO :: IO CtrDRBG
    case RSA.generateKeyPair g b of
        Left error -> undefined -- TODO handle error
        Right (publicKey, privateKey, g') -> do
            Bin.encodeFile privateKeyFile privateKey
            Bin.encodeFile publicKeyFile publicKey
parse ["-s"] = do  -- sign message
    message <- B.getContents
    privateKeyE <- Bin.decodeFileOrFail privateKeyFile
    case privateKeyE of
        Left (offset, err) -> B.putStrLn "Error reading private key file"
        Right privateKey -> do
          case RSA.sign privateKey message of -- TODO use sha-3
              Left error -> undefined -- TODO handle error
              Right signature -> B.putStr $ B.concat [(B64.encode signature), "\n\n", message]
               

parse ["-v"] = do  -- validate message
    publicKeyE <- Bin.decodeFileOrFail publicKeyFile
    case publicKeyE of
        Left (offset, err) -> B.putStrLn "Error reading public key file"
        Right publicKey -> do
          input <- B.getContents
          let signature : "" : message' = lines input
              message = unlines message'
          case RSA.verify publicKey message (B64.decodeLenient signature) of
              Left error -> undefined -- TODO handle error
              Right True -> B.putStrLn "OK"
              Right False -> B.putStrLn "FAIL"

lines :: B.ByteString -> [B.ByteString]
lines = B.split (head $ unpackBytes "\n")

unlines :: [B.ByteString] -> B.ByteString
unlines = B.intercalate "\n"

-- keyGen :: Integer -> IO ()
-- keyGen b = do
--     genM <- runMaybeT $ generate b
--     case genM of
--         Nothing -> putStrLn "Error generating keys"
--         Just (n, e, d)
--             | bitLength n < b -> putStrLn ("got bitlength " ++ show (bitLength n)) >> keyGen b
--             | otherwise -> do
--                 putStrLn ("got bitlength " ++ show (bitLength n)) 
--                 writeFile publicKeyFile (show (n :: Integer) ++ "\n" ++ show e)
--                 writeFile privateKeyFile (show n ++ "\n" ++ show d)

-- readKeys :: FilePath -> IO (Integer, Integer)
-- readKeys path = do
--     contents <- readFile path
--     let [modulusStr, keyStr] = B.lines contents
--         modulus = read modulusStr :: Integer
--         key     = read keyStr :: Integer
--     return (modulus, key)
-- 
-- encryptBS n k b = undefined