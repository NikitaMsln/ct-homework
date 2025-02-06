{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  , HiMonad(..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString as B
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert deriving (Eq, Ord, Show, Generic)

instance Serialise HiFun where

instance Pretty HiFun where
  pretty HiFunDiv            = pretty "div"
  pretty HiFunMul            = pretty "mul"
  pretty HiFunAdd            = pretty "add"
  pretty HiFunSub            = pretty "sub"
  pretty HiFunNot            = pretty "not"
  pretty HiFunAnd            = pretty "and"
  pretty HiFunOr             = pretty "or"
  pretty HiFunLessThan       = pretty "less-than"
  pretty HiFunGreaterThan    = pretty "greater-than"
  pretty HiFunEquals         = pretty "equals"
  pretty HiFunNotLessThan    = pretty "not-less-than"
  pretty HiFunNotGreaterThan = pretty "not-greater-than"
  pretty HiFunNotEquals      = pretty "not-equals"
  pretty HiFunIf             = pretty "if"
  pretty HiFunLength         = pretty "length"
  pretty HiFunToUpper        = pretty "to-upper"
  pretty HiFunToLower        = pretty "to-lower"
  pretty HiFunReverse        = pretty "reverse"
  pretty HiFunTrim           = pretty "trim"
  pretty HiFunList           = pretty "list"
  pretty HiFunRange          = pretty "range"
  pretty HiFunFold           = pretty "fold"
  pretty HiFunPackBytes      = pretty "pack-bytes"
  pretty HiFunUnpackBytes    = pretty "unpack-bytes"
  pretty HiFunEncodeUtf8     = pretty "encode-utf8"
  pretty HiFunDecodeUtf8     = pretty "decode-utf8"
  pretty HiFunZip            = pretty "zip"
  pretty HiFunUnzip          = pretty "unzip"
  pretty HiFunSerialise      = pretty "serialise"
  pretty HiFunDeserialise    = pretty "deserialise"
  pretty HiFunRead           = pretty "read"
  pretty HiFunWrite          = pretty "write"
  pretty HiFunMkDir          = pretty "mkdir"
  pretty HiFunChDir          = pretty "cd"
  pretty HiFunParseTime      = pretty "parse-time"
  pretty HiFunRand           = pretty "rand"
  pretty HiFunEcho           = pretty "echo"
  pretty HiFunCount          = pretty "count"
  pretty HiFunKeys           = pretty "keys"
  pretty HiFunValues         = pretty "values"
  pretty HiFunInvert         = pretty "invert"

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text deriving (Eq, Ord, Show, Generic)

instance Serialise HiAction

data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueTime UTCTime
  | HiValueNumber Rational
  | HiValueBytes B.ByteString
  | HiValueString T.Text
  | HiValueList (Seq HiValue)
  | HiValueDict (M.Map HiValue HiValue)
  | HiValueFunction HiFun
  | HiValueAction HiAction deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)] deriving (Eq, Show)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Eq, Show)
