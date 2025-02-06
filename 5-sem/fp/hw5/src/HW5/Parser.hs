module HW5.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Word (Word8)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer (charLiteral, scientific, signed)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Read (readMaybe)

import HW5.Base

type Parser = M.Parsec Void String

pTerm :: Parser HiExpr
pApply :: Parser HiExpr
pExprInner :: Parser HiExpr

pNone :: Parser ()
pNone = () <$ M.string ""

pFunc :: Parser HiFun
pFunc = HiFunDiv <$ M.string "div" M.<|>
        HiFunMul <$ M.string "mul" M.<|>
        HiFunAdd <$ M.string "add" M.<|>
        HiFunSub <$ M.string "sub" M.<|>
        HiFunAnd <$ M.string "and" M.<|>
        HiFunOr <$ M.string "or" M.<|>
        HiFunLessThan <$ M.string "less-than" M.<|>
        HiFunGreaterThan <$ M.string "greater-than" M.<|>
        HiFunEquals <$ M.string "equals" M.<|>
        HiFunNotLessThan <$ M.string "not-less-than" M.<|>
        HiFunNotGreaterThan <$ M.string "not-greater-than" M.<|>
        HiFunNotEquals <$ M.string "not-equals" M.<|>
        HiFunNot <$ M.string "not" M.<|>
        HiFunIf <$ M.string "if" M.<|>
        HiFunLength <$ M.string "length" M.<|>
        HiFunToUpper <$ M.string "to-upper" M.<|>
        HiFunToLower <$ M.string "to-lower" M.<|>
        HiFunReverse <$ M.string "reverse" M.<|>
        HiFunTrim <$ M.string "trim" M.<|>
        HiFunList <$ M.string "list" M.<|>
        HiFunRange <$ M.string "range" M.<|>
        HiFunFold <$ M.string "fold" M.<|>
        HiFunPackBytes <$ M.string "pack-bytes" M.<|>
        HiFunUnpackBytes <$ M.string "unpack-bytes" M.<|>
        HiFunEncodeUtf8 <$ M.string "encode-utf8" M.<|>
        HiFunDecodeUtf8 <$ M.string "decode-utf8" M.<|>
        HiFunZip <$ M.string "zip" M.<|>
        HiFunUnzip <$ M.string "unzip" M.<|>
        HiFunSerialise <$ M.string "serialise" M.<|>
        HiFunDeserialise <$ M.string "deserialise" M.<|>
        HiFunRead <$ M.string "read" M.<|>
        HiFunWrite <$ M.string "write" M.<|>
        HiFunMkDir <$ M.string "mkdir" M.<|>
        HiFunChDir <$ M.string "cd" M.<|>
        HiFunParseTime <$ M.string "parse-time" M.<|>
        HiFunRand <$ M.string "rand" M.<|>
        HiFunEcho <$ M.string "echo" M.<|>
        HiFunCount <$ M.string "count" M.<|>
        HiFunKeys <$ M.string "keys" M.<|>
        HiFunValues <$ M.string "values" M.<|>
        HiFunInvert <$ M.string "invert"

pText :: Parser Text
pText = fmap (pack) $ M.char '"' *> M.manyTill charLiteral (M.char '"')

pBool :: Parser Bool
pBool = True <$ M.string "true" M.<|> False <$ M.string "false"

pNull :: Parser HiValue
pNull = HiValueNull <$ M.string "null"

pNumber :: Parser Rational
pNumber = toRational <$> signed pNone scientific

pList :: Parser HiExpr
pList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> M.between (M.char '[' *> M.space) (M.char ']') (M.sepBy pExprInner (M.char ','))

readMaybeParser :: Read a => NonEmpty Char -> Parser a
readMaybeParser s = maybe (M.unexpected $ M.Tokens s) return $ readMaybe $ toList s

pByte :: Parser Word8
pByte = readMaybeParser =<< ((:|) '0' . (:) 'x') <$> M.between M.space M.space (M.count 2 M.hexDigitChar)

pBytes :: Parser BS.ByteString
pBytes = BS.pack <$> (M.between (M.string "[#" *> M.space) (M.string "#]") $ M.many pByte)

pDict :: Parser [(HiExpr, HiExpr)]
pDict = M.between (M.char '{') (M.char '}') $ M.sepBy ((,) <$> pExprInner <* M.char ':' <*> pExprInner) (M.char ',')

pValue :: Parser HiExpr
pValue =  HiExprValue <$> (
            HiValueNumber <$> pNumber M.<|>
            HiValueString <$> pText M.<|>
            HiValueFunction <$> pFunc M.<|>
            HiValueBool <$> pBool M.<|>
            HiValueBytes <$> pBytes M.<|>
            (HiValueAction HiActionCwd) <$ M.string "cwd" M.<|>
            (HiValueAction HiActionNow) <$ M.string "now" M.<|>
            pNull
          ) M.<|>
          pList M.<|>
          HiExprDict <$> pDict

buildApply :: HiExpr -> [Maybe [HiExpr]] -> HiExpr
buildApply expr (Just args : t) = buildApply (HiExprApply expr args) t
buildApply expr (Nothing : t)   = buildApply (HiExprRun expr) t
buildApply expr []              = expr

pTerm = M.between M.space M.space $ pValue M.<|> M.between (M.char '(') (M.char ')') pExprInner

pField :: Parser String
pField = intercalate "-" <$> M.sepBy1 ((:) <$> M.satisfy isAlpha <*> M.many (M.satisfy isAlphaNum)) (M.try $ (<*) (M.char '-') $ M.notFollowedBy $ M.satisfy $ not . isAlpha)

pApply = buildApply <$> pTerm <*> M.many (M.between M.space M.space $
    Just <$> M.between (M.char '(') (M.char ')') (M.sepBy pExprInner (M.char ',')) M.<|>
    Just . (:[]) . HiExprValue . HiValueString . pack <$> (M.char '.' *> pField) M.<|>
    Nothing <$ M.char '!'
  )

pExprInner = makeExprParser pApply [
    [
      InfixL ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [x, y]) <$ M.char '*'),
      InfixL ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [x, y]) <$ M.try (M.char '/' <* M.notFollowedBy (M.char '=')))
    ],
    [
      InfixL ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [x, y]) <$ M.char '+'),
      InfixL ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [x, y]) <$ M.char '-')
    ],
    [
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [x, y]) <$ M.string "=="),
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) [x, y]) <$ M.string "/="),
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) [x, y]) <$ M.string ">="),
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) [x, y]) <$ M.string "<="),
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) [x, y]) <$ M.string "<"),
      InfixN ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) [x, y]) <$ M.string ">")
    ],
    [
      InfixR ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [x, y]) <$ M.string "&&")
    ],
    [
      InfixR ((\x y -> HiExprApply (HiExprValue (HiValueFunction HiFunOr)) [x, y]) <$ M.string "||")
    ]
  ]

pExpr :: Parser HiExpr
pExpr = pExprInner <* M.eof

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = M.parse pExpr ""
