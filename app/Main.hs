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
import qualified Data.ByteString.Lazy.Char8 as B
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
        Left err -> undefined -- TODO handle error
        Right (publicKey, privateKey, g') -> do
            -- Bin.encodeFile privateKeyFile privateKey
            -- Bin.encodeFile publicKeyFile publicKey
            writePublicKey publicKeyFile publicKey
            writePrivateKey privateKeyFile privateKey

parse ["-s", filename, signatureFile] = do  -- sign message
    message <- B.readFile filename
    privateKeyE <- readPrivateKey privateKeyFile
    case privateKeyE of
        Left err -> B.putStrLn $ B.pack err
        Right privateKey ->
          case RSA.sign privateKey message of
              Left err -> B.putStrLn . B.pack $ show err
              Right signature -> Bin.encodeFile signatureFile signature

parse ["-v", filename, signatureFile] = do  -- validate message
    message <- B.readFile filename
    signature <- Bin.decodeFile signatureFile
    publicKeyE <- readPublicKey publicKeyFile
    case publicKeyE of
        Left err -> B.putStrLn $ B.pack err
        Right publicKey ->
          case RSA.verify publicKey message signature of
              Left err -> B.putStrLn . B.pack $ show err
              Right True -> B.putStrLn "Verified OK"
              Right False -> B.putStrLn "Verification Failure"

writePublicKey :: FilePath -> RSA.PublicKey -> IO ()
writePublicKey filename pk =
  B.writeFile filename bstr
    where bstr =  unlines ["-----BEGIN PUBLIC KEY-----", key, "-----END PUBLIC KEY-----"]
          key = B64.encode $ Bin.encode pk

writePrivateKey :: FilePath -> RSA.PrivateKey -> IO ()
writePrivateKey filename sk =
  B.writeFile filename bstr
    where bstr =  unlines ["-----BEGIN PRIVATE KEY-----", key, "-----END PRIVATE KEY-----"]
          key = B64.encode $ Bin.encode sk

readPublicKey :: FilePath -> IO (Either String RSA.PublicKey)
readPublicKey filename = do
  contents <- B.readFile filename
  case lines contents of
    ["-----BEGIN PUBLIC KEY-----", key, "-----END PUBLIC KEY-----"] ->
      case B64.decode key of
        Left err -> return $ Left err
        Right key' -> return . Right $ Bin.decode key'
    _ -> return $ Left "Error reading key file"

readPrivateKey :: FilePath -> IO (Either String RSA.PrivateKey)
readPrivateKey filename = do
  contents <- B.readFile filename
  case lines contents of
    ["-----BEGIN PRIVATE KEY-----", key, "-----END PRIVATE KEY-----"] ->
      case B64.decode key of
        Left err -> return $ Left err
        Right key' -> return . Right $ Bin.decode key'
    _ -> return $ Left "Error reading key file"

lines :: B.ByteString -> [B.ByteString]
lines = B.split '\n'

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
