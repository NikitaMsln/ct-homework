module HW5.Evaluator
  ( eval
  ) where

import qualified Codec.Compression.Zlib as Z
import qualified Codec.Compression.Zlib.Internal as ZI
import qualified Codec.Serialise as SZ
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW5.Base
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

applyFunc1 :: Monad m => (Either HiError a -> Either HiError b) -> [m (Either HiError a)] -> m (Either HiError b)
applyFunc1 f [x] = f <$> x
applyFunc1 _ _   = return $ Left HiErrorArityMismatch

applyFunc2' :: Monad m => (m (Either HiError a) -> m (Either HiError a) -> m (Either HiError b)) -> [m (Either HiError a)] -> m (Either HiError b)
applyFunc2' f [x, y] = f x y
applyFunc2' _ _      = return $ Left HiErrorArityMismatch

applyFunc2 :: Monad m => (Either HiError a -> Either HiError a -> Either HiError b) -> [m (Either HiError a)] -> m (Either HiError b)
applyFunc2 f = applyFunc2' (\x y -> f <$> x <*> y)

applyFunc3' :: Monad m => (m (Either HiError a) -> m (Either HiError a) -> m (Either HiError a) -> m (Either HiError b)) -> [m (Either HiError a)] -> m (Either HiError b)
applyFunc3' f [x, y, z] = f x y z
applyFunc3' _ _         = return $ Left HiErrorArityMismatch

toRationalV :: HiValue -> Either HiError Rational
toRationalV (HiValueNumber x) = Right x
toRationalV _                 = Left HiErrorInvalidArgument

toBoolV :: HiValue -> Either HiError Bool
toBoolV (HiValueBool x) = Right x
toBoolV _               = Left HiErrorInvalidArgument

toIntV :: HiValue -> Either HiError Int
toIntV x = do
  t <- toRationalV x
  if (denominator t == 1) then return (fromInteger $ numerator t) else Left HiErrorInvalidArgument

toTextV :: HiValue -> Either HiError T.Text
toTextV (HiValueString x) = Right x
toTextV _                 = Left HiErrorInvalidArgument

toListV :: HiValue -> Either HiError (S.Seq HiValue)
toListV (HiValueList x) = Right x
toListV _               = Left HiErrorInvalidArgument

toByteV :: HiValue -> Either HiError Word8
toByteV x = toIntV x >>= (\t -> if (t >= 0 && t <= 255) then (return . fromInteger . toInteger $ t) else Left HiErrorInvalidArgument)

toBytesV :: HiValue -> Either HiError B.ByteString
toBytesV (HiValueBytes x) = Right x
toBytesV _                = Left HiErrorInvalidArgument

toMapV :: HiValue -> Either HiError (M.Map HiValue HiValue)
toMapV (HiValueDict x) = Right x
toMapV _               = Left HiErrorInvalidArgument

toWriteableV :: HiValue -> Either HiError B.ByteString
toWriteableV (HiValueBytes x)  = Right x
toWriteableV (HiValueString x) = Right $ encodeUtf8 x
toWriteableV _                 = Left HiErrorInvalidArgument

data HiValueSeq = HiValueCharSeq T.Text | HiValueValueSeq (S.Seq HiValue) | HiValueByteSeq B.ByteString

seqIndex :: HiValueSeq -> Int -> HiValue
seqIndex (HiValueCharSeq x)  = HiValueString . T.singleton . T.index x
seqIndex (HiValueValueSeq x) = S.index x
seqIndex (HiValueByteSeq x)  = HiValueNumber . fromInteger . toInteger . B.index x

seqSubseq :: HiValueSeq -> Int -> Int -> HiValue
seqSubseq (HiValueCharSeq x) i j  = HiValueString $ T.drop i $ T.take j x
seqSubseq (HiValueValueSeq x) i j = HiValueList $ S.drop i $ S.take j x
seqSubseq (HiValueByteSeq x) i j  = HiValueBytes $ B.drop i $ B.take j x

seqLength :: HiValueSeq -> Int
seqLength (HiValueCharSeq x)  = T.length x
seqLength (HiValueValueSeq x) = S.length x
seqLength (HiValueByteSeq x)  = B.length x

seqReverse :: HiValueSeq -> HiValue
seqReverse (HiValueCharSeq x)  = HiValueString $ T.reverse x
seqReverse (HiValueValueSeq x) = HiValueList $ S.reverse x
seqReverse (HiValueByteSeq x)  = HiValueBytes $ B.reverse x

seqConcat :: HiValueSeq -> HiValueSeq -> Either HiError HiValue
seqConcat (HiValueCharSeq x) (HiValueCharSeq y)   = Right . HiValueString $ x <> y
seqConcat (HiValueValueSeq x) (HiValueValueSeq y) = Right . HiValueList $ x <> y
seqConcat (HiValueByteSeq x) (HiValueByteSeq y)   = Right . HiValueBytes $ x <> y
seqConcat _ _                                     = Left HiErrorInvalidArgument

seqCount :: HiValueSeq -> M.Map HiValue HiValue
seqCount (HiValueCharSeq x) = fmap HiValueNumber . M.fromListWith (+) $ fmap (\c -> (HiValueString $ T.singleton c, 1)) $ T.unpack x
seqCount (HiValueValueSeq x) = fmap HiValueNumber . M.fromListWith (+) $ fmap (\c -> (c, 1)) $ toList x
seqCount (HiValueByteSeq x) = fmap HiValueNumber . M.fromListWith (+) $ map (\c -> (HiValueNumber $ fromInteger $ toInteger c, 1)) $ B.unpack x

seqTimes :: HiValueSeq -> Int -> HiValue
seqTimes (HiValueCharSeq x) i  = HiValueString $ T.replicate i x
seqTimes (HiValueValueSeq x) i = HiValueList $ foldr (\_ b -> x <> b) S.empty [1..i]
seqTimes (HiValueByteSeq x) i  = HiValueBytes $ foldr (\_ b -> x <> b) B.empty [1..i]

seqFoldUnsafe :: HiMonad m => HiValueSeq -> ([m (Either HiError HiValue)] -> m (Either HiError HiValue)) -> m (Either HiError HiValue)
seqFoldUnsafe (HiValueCharSeq x) f = foldl1 (\p q -> f [p, q]) . fmap (pure . Right . HiValueString) $ (T.chunksOf 1 x)
seqFoldUnsafe (HiValueValueSeq x) f = foldl1 (\p q -> f [p, q]) . fmap (pure . Right) $ x
seqFoldUnsafe (HiValueByteSeq x) f = foldl1 (\p q -> f [p, q]) . fmap (pure . Right . HiValueNumber . fromInteger . toInteger) $ B.unpack x

seqFold :: HiMonad m => HiValueSeq -> ([m (Either HiError HiValue)] -> m (Either HiError HiValue)) -> m (Either HiError HiValue)
seqFold x f = if seqLength x == 0 then return $ Left HiErrorInvalidArgument else seqFoldUnsafe x f

toSeqV :: HiValue -> Either HiError HiValueSeq
toSeqV (HiValueBytes x)  = Right $ HiValueByteSeq x
toSeqV (HiValueString x) = Right $ HiValueCharSeq x
toSeqV (HiValueList x)   = Right $ HiValueValueSeq x
toSeqV _                 = Left HiErrorInvalidArgument

data HiValueAdditive = HiValueNumberAdditive Rational Rational | HiValueSeqAdditive HiValueSeq HiValueSeq | HiValueTimeAdditive UTCTime NominalDiffTime

additive :: HiValueAdditive -> Either HiError HiValue
additive (HiValueNumberAdditive x y) = Right . HiValueNumber $ x + y
additive (HiValueTimeAdditive x y)   = Right . HiValueTime $ addUTCTime y x
additive (HiValueSeqAdditive x y)    = seqConcat x y

toAdditiveV :: HiValue -> HiValue -> Either HiError HiValueAdditive
toAdditiveV (HiValueNumber x) (HiValueNumber y) = Right $ HiValueNumberAdditive x y
toAdditiveV (HiValueTime x) (HiValueNumber y)   = Right $ HiValueTimeAdditive x (fromRational y)
toAdditiveV (HiValueNumber x) (HiValueTime y)   = Right $ HiValueTimeAdditive y (fromRational x)
toAdditiveV x y                                 = HiValueSeqAdditive <$> toSeqV x <*> toSeqV y

data HiValueSubtractive = HiValueNumberSubtractive Rational Rational | HiValueTimeSubtractive UTCTime UTCTime | HiValueTimeNumSubtractive UTCTime NominalDiffTime

subtractive :: HiValueSubtractive -> Either HiError HiValue
subtractive (HiValueNumberSubtractive x y)  = Right . HiValueNumber $ x - y
subtractive (HiValueTimeSubtractive x y)    = Right . HiValueNumber . toRational $ diffUTCTime x y
subtractive (HiValueTimeNumSubtractive x y) = Right . HiValueTime $ addUTCTime (negate y) x

toSubtractiveV :: HiValue -> HiValue -> Either HiError HiValueSubtractive
toSubtractiveV (HiValueNumber x) (HiValueNumber y) = Right $ HiValueNumberSubtractive x y
toSubtractiveV (HiValueTime x) (HiValueTime y)     = Right $ HiValueTimeSubtractive x y
toSubtractiveV (HiValueTime x) (HiValueNumber y)   = Right $ HiValueTimeNumSubtractive x (fromRational y)
toSubtractiveV _ _                                 = Left HiErrorInvalidArgument

data HiValueMultiplicative = HiValueNumberMultiplicative Rational Rational | HiValueSeqMultiplicative HiValueSeq Int

multiplicative :: HiValueMultiplicative -> HiValue
multiplicative (HiValueNumberMultiplicative x y) = HiValueNumber $ x * y
multiplicative (HiValueSeqMultiplicative x y)    = seqTimes x y

toMultiplicativeV :: HiValue -> HiValue -> Either HiError HiValueMultiplicative
toMultiplicativeV (HiValueNumber x) (HiValueNumber y) = Right $ HiValueNumberMultiplicative x y
toMultiplicativeV x y = case HiValueSeqMultiplicative <$> toSeqV x <*> toIntV y of
  Left _ -> HiValueSeqMultiplicative <$> toSeqV y <*> toIntV x
  r      -> r

data HiValueDivisive = HiValueNumberDivisive Rational Rational | HiValueStringDivisive T.Text T.Text

divisive :: HiValueDivisive -> Either HiError HiValue
divisive (HiValueNumberDivisive _ 0) = Left HiErrorDivideByZero
divisive (HiValueNumberDivisive x y) = Right . HiValueNumber $ x / y
divisive (HiValueStringDivisive x y) = Right . HiValueString $ x <> (T.pack "/") <> y

toDivisiveV :: HiValue -> HiValue -> Either HiError HiValueDivisive
toDivisiveV (HiValueNumber x) (HiValueNumber y) = Right $ HiValueNumberDivisive x y
toDivisiveV (HiValueString x) (HiValueString y) = Right $ HiValueStringDivisive x y
toDivisiveV _ _                                 = Left HiErrorInvalidArgument

isBadValue :: HiValue -> Bool
isBadValue (HiValueBool x) = not x
isBadValue HiValueNull     = True
isBadValue _               = False

iterateWhile :: a -> (a -> Bool) -> (a -> a) -> [a]
iterateWhile x f g = if (f x) then x : iterateWhile (g x) f g else []

forceDecompress :: B.ByteString -> BL.ByteString
forceDecompress bs = let result = Z.decompress (BL.fromStrict bs) in rnf result `seq` result

maybeDecompress :: B.ByteString -> Maybe B.ByteString
maybeDecompress b =
    unsafePerformIO $
        (try (evaluate (forceDecompress b)) :: IO (Either SomeException BL.ByteString))
            >>= return . either (const Nothing) (Just . BL.toStrict)

maybeDecode :: B.ByteString -> Maybe T.Text
maybeDecode b =
    unsafePerformIO $
        (try (evaluate (decodeUtf8 b)) :: IO (Either SomeException T.Text))
            >>= return . either (const Nothing) Just

invertMap :: M.Map HiValue HiValue -> M.Map HiValue HiValue
invertMap m = fmap (HiValueList . S.fromList) $ M.fromListWith (++) $ fmap (\(k, v) -> (v, [k])) (M.toList m)

applyHiVal :: HiMonad m => HiValue -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
applyHi :: HiMonad m => Either HiError HiValue -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)

applyHiFunc :: HiMonad m => HiFun -> [m (Either HiError HiValue)] -> m (Either HiError HiValue)
applyHiFunc HiFunAdd = applyFunc2 $ \x y -> ((,) <$> x <*> y) >>= uncurry toAdditiveV >>= additive
applyHiFunc HiFunMul = applyFunc2 $ \x y -> multiplicative <$> (((,) <$> x <*> y) >>= uncurry toMultiplicativeV)
applyHiFunc HiFunSub = applyFunc2 $ \x y -> ((,) <$> x <*> y) >>= uncurry toSubtractiveV >>= subtractive
applyHiFunc HiFunDiv = applyFunc2 $ \x y -> ((,) <$> x <*> y) >>= uncurry toDivisiveV >>= divisive
applyHiFunc HiFunNot = applyFunc1 $ \x -> HiValueBool . not <$> (x >>= toBoolV)
applyHiFunc HiFunAnd = applyFunc2' $ \x y -> do
  x' <- x
  case isBadValue <$> x' of
    Left e      -> return (Left e)
    Right True  -> return x'
    Right False -> y
applyHiFunc HiFunOr = applyFunc2' $ \x y -> do
  x' <- x
  case isBadValue <$> x' of
    Left e      -> return (Left e)
    Right True  -> y
    Right False -> return x'
applyHiFunc HiFunLessThan = applyFunc2 $ \x y -> fmap HiValueBool $ (<) <$> x <*> y
applyHiFunc HiFunGreaterThan = applyFunc2 $ \x y -> fmap HiValueBool $ (>) <$> x <*> y
applyHiFunc HiFunEquals = applyFunc2 $ \x y -> fmap HiValueBool $ (==) <$> x <*> y
applyHiFunc HiFunNotEquals = applyFunc2 $ \x y -> fmap HiValueBool $ (/=) <$> x <*> y
applyHiFunc HiFunNotLessThan = applyFunc2 $ \x y -> fmap HiValueBool $ (>=) <$> x <*> y
applyHiFunc HiFunNotGreaterThan = applyFunc2 $ \x y -> fmap HiValueBool $ (<=) <$> x <*> y
applyHiFunc HiFunIf = applyFunc3' $ \x y z -> do
  x' <- ((=<<) toBoolV <$> x)
  case x' of
    Left e      -> return (Left e)
    Right True  -> y
    Right False -> z
applyHiFunc HiFunLength = applyFunc1 $ \x -> HiValueNumber . fromIntegral . seqLength <$> (x >>= toSeqV)
applyHiFunc HiFunReverse = applyFunc1 $ \x -> seqReverse <$> (x >>= toSeqV)
applyHiFunc HiFunToUpper = applyFunc1 $ \x -> HiValueString . T.toUpper <$> (x >>= toTextV)
applyHiFunc HiFunToLower = applyFunc1 $ \x -> HiValueString . T.toLower <$> (x >>= toTextV)
applyHiFunc HiFunTrim = applyFunc1 $ \x -> HiValueString . T.strip <$> (x >>= toTextV)
applyHiFunc HiFunList = \l -> fmap (HiValueList . S.fromList) . sequenceA <$> sequenceA l
applyHiFunc HiFunRange = applyFunc2 $ \x y -> do
  x' <- x >>= toRationalV
  y' <- y >>= toRationalV
  return . HiValueList . S.fromList . fmap HiValueNumber $ iterateWhile x' ((>=) y') ((+) 1)
applyHiFunc HiFunFold = applyFunc2' $ \x y -> do
  x' <- x
  y' <- y
  case flip seqFold (applyHi x') <$> (y' >>= toSeqV) of
    Left _  -> return (Left HiErrorInvalidArgument)
    Right v -> v
applyHiFunc HiFunPackBytes = applyFunc1 $ \x -> HiValueBytes . B.pack . toList <$> (x >>= toListV >>= sequenceA . fmap toByteV)
applyHiFunc HiFunUnpackBytes = applyFunc1 $ \x -> HiValueList . fmap (HiValueNumber . toRational) . S.fromList . B.unpack <$> (x >>= toBytesV)
applyHiFunc HiFunEncodeUtf8 = applyFunc1 $ \x -> HiValueBytes . encodeUtf8 <$> (x >>= toTextV)
applyHiFunc HiFunDecodeUtf8 = applyFunc1 $ \x -> maybe HiValueNull HiValueString . maybeDecode <$> (x >>= toBytesV)
applyHiFunc HiFunZip = applyFunc1 $ \x -> HiValueBytes . BL.toStrict . Z.compressWith ZI.defaultCompressParams { ZI.compressLevel = ZI.bestCompression } . BL.fromStrict <$> (x >>= toBytesV)
applyHiFunc HiFunUnzip = applyFunc1 $ \x -> maybe HiValueNull HiValueBytes . maybeDecompress <$> (x >>= toBytesV)
applyHiFunc HiFunSerialise = applyFunc1 $ (<$>) $ HiValueBytes . BL.toStrict . SZ.serialise
applyHiFunc HiFunDeserialise = applyFunc1 $ \x -> either (const HiValueNull) (id) . SZ.deserialiseOrFail . BL.fromStrict <$> (x >>= toBytesV)
applyHiFunc HiFunRead = applyFunc1 $ \x -> HiValueAction . HiActionRead . T.unpack <$> (x >>= toTextV)
applyHiFunc HiFunWrite = applyFunc2 $ \x y -> HiValueAction <$> (HiActionWrite <$> (T.unpack <$> (x >>= toTextV))  <*> (y >>= toWriteableV))
applyHiFunc HiFunMkDir = applyFunc1 $ \x -> HiValueAction . HiActionMkDir . T.unpack <$> (x >>= toTextV)
applyHiFunc HiFunChDir = applyFunc1 $ \x -> HiValueAction . HiActionChDir . T.unpack <$> (x >>= toTextV)
applyHiFunc HiFunParseTime = applyFunc1 $ \x -> maybe HiValueNull HiValueTime . readMaybe . T.unpack <$> (x >>= toTextV)
applyHiFunc HiFunRand = applyFunc2 $ \x y -> HiValueAction <$> (HiActionRand <$> (x >>= toIntV) <*> (y >>= toIntV))
applyHiFunc HiFunEcho = applyFunc1 $ \x -> HiValueAction . HiActionEcho <$> (x >>= toTextV)
applyHiFunc HiFunKeys = applyFunc1 $ \x -> HiValueList . S.fromList . M.keys <$> (x >>= toMapV)
applyHiFunc HiFunValues = applyFunc1 $ \x -> HiValueList . S.fromList . M.elems <$> (x >>= toMapV)
applyHiFunc HiFunCount = applyFunc1 $ \x -> HiValueDict . seqCount <$> (x >>= toSeqV)
applyHiFunc HiFunInvert = applyFunc1 $ \x -> HiValueDict . invertMap <$> (x >>= toMapV)

toIndexV :: Int -> HiValue -> Either HiError Int
toIndexV i HiValueNull = Right i
toIndexV _ x           = toIntV x

calcRangeIndex :: HiValueSeq -> Int -> Int
calcRangeIndex x i | i < negate (seqLength x) = 0
                   | i < 0 = seqLength x + i
                   | i < seqLength x = i
                   | otherwise = seqLength x

applyHiSeq :: HiValueSeq -> [Either HiError HiValue] -> Either HiError HiValue
applyHiSeq s [i] = (\t -> if t < 0 || t >= seqLength s then HiValueNull else seqIndex s t) <$> (i >>= toIntV)
applyHiSeq s [i, j] = seqSubseq s <$> (calcRangeIndex s <$> (i >>= toIndexV 0)) <*> (calcRangeIndex s <$> (j >>= toIndexV (seqLength s)))
applyHiSeq _ _ = Left HiErrorArityMismatch

applyHiVal (HiValueFunction f) args = applyHiFunc f args
applyHiVal (HiValueDict d) args = flip applyFunc1 args $ \x -> maybe HiValueNull id . (M.!?) d <$> x
applyHiVal x args = (\t -> (either (const $ Left HiErrorInvalidFunction) Right $ toSeqV x) >>= flip applyHiSeq t) <$> sequenceA args

applyHi (Right f) args = applyHiVal f args
applyHi (Left e) _     = return (Left e)

runHi :: HiMonad m => Either HiError HiValue -> m (Either HiError HiValue)
runHi (Right (HiValueAction a)) = Right <$> runAction a
runHi _                         = return $ Left HiErrorInvalidArgument

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue v) = return (Right v)
eval (HiExprRun e) = eval e >>= runHi
eval (HiExprApply f args) = eval f >>= flip applyHi (fmap eval args)
eval (HiExprDict l) = do
  t <- sequenceA $ fmap (\(k, v) -> (,) <$> eval k <*> eval v) l
  return $ HiValueDict . M.fromList <$> (sequenceA . fmap (\(k, v) -> (,) <$> k <*> v) $ t)
