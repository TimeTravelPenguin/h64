{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Binary (Binary (get, put), Get)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word, Word32)

_m64Signature :: Word32
_m64Signature = 0x4D36341A

data M64 = M64
  { m64Signature :: Word32,
    m64Version :: Word,
    m64Uid :: Int,
    m64ViCount :: Word,
    m64ControllerCount :: Word
  }
  deriving (Show, Eq)

instance Binary M64 where
  put m64 = do
    put $ m64Signature m64
    put $ m64Version m64
    put $ m64Uid m64
    put $ m64ViCount m64
    put $ m64ControllerCount m64

  get =
    M64 <$> get
      <*> get
      <*> get
      <*> get
      <*> get

data Button
  = CRight
  | CLeft
  | CDown
  | CUp
  | R
  | L
  | Reserved01
  | Reserved02
  | DPadRight
  | DPadLeft
  | DPadDown
  | DPadUp
  | Start
  | Z
  | B
  | A
  deriving (Show, Eq)

instance Binary Button where
  put CRight = put (0x0001 :: Word32)
  put CLeft = put (0x0002 :: Word32)
  put CDown = put (0x0004 :: Word32)
  put CUp = put (0x0008 :: Word32)
  put R = put (0x0010 :: Word32)
  put L = put (0x0020 :: Word32)
  put Reserved01 = put (0x0040 :: Word32)
  put Reserved02 = put (0x0080 :: Word32)
  put DPadRight = put (0x0100 :: Word32)
  put DPadLeft = put (0x0200 :: Word32)
  put DPadDown = put (0x0400 :: Word32)
  put DPadUp = put (0x0800 :: Word32)
  put Start = put (0x1000 :: Word32)
  put Z = put (0x2000 :: Word32)
  put B = put (0x4000 :: Word32)
  put A = put (0x8000 :: Word32)

  get = do
    v <- get :: Get Word32
    case v of
      0x0001 -> return CRight
      0x0002 -> return CLeft
      0x0004 -> return CDown
      0x0008 -> return CUp
      0x0010 -> return R
      0x0020 -> return L
      0x0040 -> return Reserved01
      0x0080 -> return Reserved02
      0x0100 -> return DPadRight
      0x0200 -> return DPadLeft
      0x0400 -> return DPadDown
      0x0800 -> return DPadUp
      0x1000 -> return Start
      0x2000 -> return Z
      0x4000 -> return B
      0x8000 -> return A
      _ -> error "Invalid conversion"
