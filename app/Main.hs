module Main where

import Data.Binary ( decode, encode )
import Data.Word (Word, Word32)
import Types

--fileData = decodeFile "D:\\Haskell\\h64\\m64 files\\SMOv5_TAS.m64"

m64 = M64 (_m64Signature :: Word32) 3 3 10 1

main :: IO ()
main = do
  print m64
  let m = (decode . encode) m64 :: M64
  print m
  putStrLn "Done"
