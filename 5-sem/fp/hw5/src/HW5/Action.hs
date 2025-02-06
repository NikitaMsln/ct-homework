module HW5.Action (
    HiPermission(..),
    PermissionException(..),
    HIO(..)
    ) where

import HW5.Base

import Control.Exception (Exception, throwIO)
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as B
import Data.Sequence (fromList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as R
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (newStdGen, randomR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Eq, Ord, Show)

data PermissionException =
  PermissionRequired HiPermission deriving (Eq, Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: S.Set HiPermission -> IO a }

instance Functor HIO where
    fmap = liftM

instance Monad HIO where
    (HIO h) >>= f = HIO $ \permissions -> do
        a <- h permissions
        runHIO (f a) permissions

instance Applicative HIO where
    pure x = HIO $ \_ -> pure x
    (<*>) = ap

instance MonadIO HIO where
    liftIO ioAction = HIO $ \_ -> ioAction

readHIO :: IO a -> HIO a
readHIO op = HIO $ \permissions -> do
  if S.member AllowRead permissions then op else throwIO (PermissionRequired AllowRead)

writeHIO :: IO a -> HIO a
writeHIO op = HIO $ \permissions -> do
  if S.member AllowWrite permissions then op else throwIO (PermissionRequired AllowWrite)

timeHIO :: IO a -> HIO a
timeHIO op = HIO $ \permissions -> do
  if S.member AllowTime permissions then op else throwIO (PermissionRequired AllowTime)

instance HiMonad HIO where
  runAction HiActionCwd = readHIO $ getCurrentDirectory >>= return . HiValueString . T.pack
  runAction (HiActionRead path) = readHIO $ doesFileExist path >>= \t ->
    if (t) then R.readFile path >>= return . HiValueString
    else listDirectory path >>= return . HiValueList . fromList . fmap (HiValueString . T.pack)
  runAction (HiActionWrite path bytes) = writeHIO $ B.writeFile path bytes >> return HiValueNull
  runAction (HiActionMkDir path) = writeHIO $ createDirectoryIfMissing True path >> return HiValueNull
  runAction (HiActionChDir path) = readHIO $ setCurrentDirectory path >> return HiValueNull
  runAction HiActionNow = timeHIO $ getCurrentTime >>= return . HiValueTime
  runAction (HiActionRand x y) = HiValueNumber . fromInteger . toInteger . fst . randomR (x, y) <$> newStdGen
  runAction (HiActionEcho text) = writeHIO $ R.putStrLn text >> return HiValueNull
