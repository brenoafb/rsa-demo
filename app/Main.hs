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
        Left err -> B.putStrLn $ B.pack . show $ err
        Right (publicKey, privateKey, g') -> do
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

parse ["-h"] = B.putStr usage
parse _      = B.putStr usage


writePublicKey :: FilePath -> RSA.PublicKey -> IO ()
writePublicKey filename pk =
  B.writeFile filename bstr
    where bstr =  unlines ["-----BEGIN PUBLIC KEY-----", key, "-----END PUBLIC KEY-----"]
          key = breakLines width . B64.encode $ Bin.encode pk
          width = 64

writePrivateKey :: FilePath -> RSA.PrivateKey -> IO ()
writePrivateKey filename sk =
  B.writeFile filename bstr
    where bstr =  unlines ["-----BEGIN PRIVATE KEY-----", key, "-----END PRIVATE KEY-----"]
          key = breakLines width . B64.encode $ Bin.encode sk
          width = 64

readPublicKey :: FilePath -> IO (Either String RSA.PublicKey)
readPublicKey filename = do
  contents <- B.readFile filename
  case lines contents of
    "-----BEGIN PUBLIC KEY-----":xs
      | last xs == "-----END PUBLIC KEY-----"->
        let keyLines = takeWhile (/= "-----END PUBLIC KEY-----") xs
            key = B.concat keyLines
         in case B64.decode key of
              Left err -> return $ Left err
              Right key' -> return . Right $ Bin.decode key'


readPrivateKey :: FilePath -> IO (Either String RSA.PrivateKey)
readPrivateKey filename = do
  contents <- B.readFile filename
  case lines contents of
    "-----BEGIN PRIVATE KEY-----":xs
      | last xs == "-----END PRIVATE KEY-----"->
        let keyLines = takeWhile (/= "-----END PRIVATE KEY-----") xs
            key = B.concat keyLines
         in case B64.decode key of
              Left err -> return $ Left err
              Right key' -> return . Right $ Bin.decode key'

lines :: B.ByteString -> [B.ByteString]
lines = B.split '\n'

unlines :: [B.ByteString] -> B.ByteString
unlines = B.intercalate "\n"

-- breakLines :: Int -> B.ByteString -> B.ByteString
breakLines 0 s = s
breakLines n s
  | B.length s <= n = s
  | otherwise = B.take n s <> "\n" <> breakLines n (B.drop n s)


usage = unlines
  ["RSA signature verifier"
  , "Options"
  , "  -h: show this dialog"
  , "  -k <bit length>: generate public and private keys"
  , "  -s <input file> <signature file>: generate signature"
  , "  -v <input file> <signature file>: verify file with given signature file"
  ]
