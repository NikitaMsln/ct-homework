{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad (MonadPlus (..))
import Data.Char (isDigit, isSpace)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import HW4.T1
import HW4.Types

data ParseError = ErrorAtPos Natural


    deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) s = fmap (\(x :# _) -> x) $ runES es (0, s)

parseError :: Parser a
parseError = P . ES $ \(i, _) -> Error $ ErrorAtPos i

instance Alternative Parser where
  empty = parseError
  (<|>) (P (ES f)) (P (ES g)) = P . ES $ (\t -> foldExcept Success (const $ g t) $ f t)

instance MonadPlus Parser

pChar :: (Char -> Bool) -> Parser Char
pChar p = P $ ES $ \(pos, s) ->
  case s of
    (c:cs) -> if (p c) then Success (c :# (pos + 1, cs)) else Error (ErrorAtPos pos)
    []     -> Error (ErrorAtPos pos)

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success $ () :# (pos, [])
    _  -> Error (ErrorAtPos pos)

pNone :: Parser ()
pNone = return ()

pWord :: String -> Parser String
pWord []      = return []
pWord (h : t) = liftA2 (:) (pChar $ (==) h) $ pWord t

pInner :: Parser a -> Parser b -> Parser c -> Parser b
pInner pa pb pc = liftA2 seq pa $ liftA2 const pb pc

pHandleNothing :: Parser (Maybe a) -> Parser a
pHandleNothing p = do
  x <- p
  case x of
    Just t  -> return t
    Nothing -> parseError

pDouble :: Parser Double
pDouble = pHandleNothing . fmap readMaybe $ liftA2 (++) (fmap (maybe "" pure) . optional $ pChar (== '-')) $
                            liftA2 (++) (some $ pChar isDigit) $
                            fmap (maybe "" id) . optional . liftA2 (:) (pChar (== '.')) $
                            some $ pChar isDigit

pWS :: Parser String
pWS = many $ pChar isSpace

data AddVariance = VarPlus | VarMinus
data ProdVariance = VarProd | VarDiv

pAdd :: Parser Expr
pAddList :: Parser [(AddVariance, Expr)]
pProd :: Parser Expr
pProdList :: Parser [(ProdVariance, Expr)]
pUnary :: Parser Expr

pAddList = many . liftA2 (,) (fmap (const VarPlus) (pChar ('+' ==)) <|> fmap (const VarMinus) (pChar ('-' ==))) $ pInner pWS pProd pWS

pAddConcat :: Expr -> [(AddVariance, Expr)] -> Expr
pAddConcat e ((VarPlus, n) : t)  = pAddConcat (e + n) t
pAddConcat e ((VarMinus, n) : t) = pAddConcat (e - n) t
pAddConcat e []                  = e

pAdd = liftA2 pAddConcat pProd $ pInner pWS pAddList pWS

pProdList = many . liftA2 (,) (fmap (const VarProd) (pChar ('*' ==)) <|> fmap (const VarDiv) (pChar ('/' ==))) $ pInner pWS pUnary pWS

pProdConcat :: Expr -> [(ProdVariance, Expr)] -> Expr
pProdConcat e ((VarProd, n) : t) = pProdConcat (e * n) t
pProdConcat e ((VarDiv, n) : t)  = pProdConcat (e / n) t
pProdConcat e []                 = e

pProd = liftA2 pProdConcat pUnary $ pInner pWS pProdList pWS

pBrackets :: Parser Expr
pBrackets = pInner (pChar ('(' ==)) pAdd (pChar (')' ==))

pSignum :: Parser Expr
pSignum = fmap signum $ pInner (liftA2 (,) (pWord "signum") pWS) pUnary pNone

pAbs :: Parser Expr
pAbs = fmap abs $ pInner (liftA2 (,) (pWord "abs") pWS) pUnary pNone

pUnary = pInner pWS (pBrackets <|> pSignum <|> pAbs <|> fmap Val pDouble) pWS

pExpr :: Parser Expr
pExpr = pInner pWS pAdd pEof

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
