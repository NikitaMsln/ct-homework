module HW5.Pretty
  ( prettyValue
  ) where

import Numeric (showHex)
import Prettyprinter (Doc, annotate, pretty, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color)

import qualified Data.ByteString as B
import Data.Map (toList)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text, pack)
import Data.Word (Word8)
import HW5.Base

isCorrectDenumerator :: Integer -> Bool
isCorrectDenumerator 1 = True
isCorrectDenumerator n =
  if (mod n 2) == 0 then isCorrectDenumerator (div n 2)
  else if (mod n 5) == 0 then isCorrectDenumerator (div n 5)
  else False

printRatio :: Rational -> String
printRatio r = (show $ numerator r) ++ "/" ++ (show $ denominator r)

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun = annotate (color Yellow) . pretty

prettyText :: Text -> Doc AnsiStyle
prettyText = annotate (color Magenta) . viaShow

prettyPrimitive :: String -> Doc AnsiStyle
prettyPrimitive = annotate (color Blue) . pretty

prettySeparator :: String -> Doc AnsiStyle
prettySeparator = annotate (color Cyan) . pretty

prettyListInner :: Foldable f => f (Doc AnsiStyle) -> Doc AnsiStyle
prettyListInner l = if null l then mempty else foldr1 (\x y -> x <> prettySeparator "," <+> y) l

prettyBetween :: String -> String -> Doc AnsiStyle -> Doc AnsiStyle
prettyBetween l r m = (annotate (color Green <> bold) . pretty $ l) <> m <> (annotate (color Green <> bold) . pretty $ r)

prettyApply :: HiFun -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyApply f v = prettyFun f <> prettyBetween "(" ")" (prettyListInner v)

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte b = let s = showHex b "" in prettyPrimitive $  if (length s < 2) then '0' : s else s

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueAction (HiActionRead a)) = prettyApply HiFunRead [prettyText $ pack a]
prettyValue (HiValueAction (HiActionWrite a b)) = prettyApply HiFunWrite [prettyText $ pack a, prettyValue (HiValueBytes b)]
prettyValue (HiValueAction (HiActionMkDir a)) = prettyApply HiFunMkDir [prettyText $ pack a]
prettyValue (HiValueAction (HiActionChDir a)) = prettyApply HiFunChDir [prettyText $ pack a]
prettyValue (HiValueAction HiActionCwd) = annotate (color Yellow) . pretty $ "cwd"
prettyValue (HiValueAction HiActionNow) = annotate (color Yellow) . pretty $ "now"
prettyValue (HiValueAction (HiActionRand a b)) = prettyApply HiFunRand [prettyPrimitive $ show a, prettyPrimitive $ show b]
prettyValue (HiValueAction (HiActionEcho a)) = prettyApply HiFunEcho [prettyText a]
prettyValue (HiValueDict d) = prettyBetween "{" "}" $ prettyListInner . fmap (\(x, y) -> prettyValue x <> prettySeparator ":" <+> prettyValue y) $ toList d
prettyValue (HiValueTime t) = prettyApply HiFunParseTime [prettyText $ pack $ show t]
prettyValue (HiValueFunction f) = prettyFun f
prettyValue (HiValueList l) = prettyBetween "[" "]" $ prettyListInner . fmap prettyValue $ l
prettyValue (HiValueBytes b) = prettyBetween "[#" "#]" $ if B.null b then mempty else mempty <+> (foldr1 (\x y -> x <+> y) . fmap prettyByte $ B.unpack b) <+> mempty
prettyValue (HiValueBool True) = prettyPrimitive "true"
prettyValue (HiValueBool False) = prettyPrimitive "false"
prettyValue HiValueNull = prettyPrimitive "null"
prettyValue (HiValueString s) = prettyText s
prettyValue (HiValueNumber p) = let (f, t) = quotRem (numerator p) (denominator p) in
  if t == 0 then prettyPrimitive $ show f
  else if isCorrectDenumerator (denominator p) then prettyPrimitive $ show (fromRational p :: Double)
  else if f == 0 then prettyPrimitive $ printRatio $ t % denominator p
  else if t < 0 then prettyPrimitive $ (show f) <> " - " <> (printRatio $ (negate t) % denominator p)
  else prettyPrimitive $ (show f) <> " + " <> (printRatio $ t % denominator p)
