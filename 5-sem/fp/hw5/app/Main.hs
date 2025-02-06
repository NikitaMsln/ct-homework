module Main (main) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Data.Set (Set, delete, empty, insert, member)
import HW5.Action
import HW5.Base
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import Prettyprinter as P
import Prettyprinter.Render.Terminal as P
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

helpMessage :: Doc AnsiStyle
helpMessage = P.annotate P.bold (P.pretty ":q") <> P.pretty " - exit form terminal" <> P.line <>
              P.annotate P.bold (P.pretty ":d") <> P.pretty " - enable/disable debug mode" <> P.line <>
              P.annotate P.bold (P.pretty ":w") <> P.pretty " - enable/disable write permission" <> P.line <>
              P.annotate P.bold (P.pretty ":r") <> P.pretty " - enable/disable read permission" <> P.line <>
              P.annotate P.bold (P.pretty ":t") <> P.pretty " - enable/disable permission to time" <> P.line <>
              P.annotate P.bold (P.pretty ":p") <> P.pretty " - permissions list" <> P.line <>
              P.annotate P.bold (P.pretty ":h") <> P.pretty " - help" <> P.line

evaluate :: MonadIO m => Bool -> Set HiPermission -> String -> m ()
evaluate debug permissions input = case parse input of
    Left err -> liftIO $ P.putDoc $ P.annotate (P.color P.Red) $ pretty (errorBundlePretty err) <> P.line
    Right expr -> do
        if debug then liftIO $ P.putDoc $ P.annotate P.italicized $ P.viaShow expr <> P.line else return ()
        result <- liftIO $ (try $ runHIO (eval expr) permissions :: IO (Either PermissionException (Either HiError HiValue)))
        case result of
            Left err -> liftIO $ P.putDoc $ P.annotate (P.color P.Red) $ P.viaShow err <> P.line
            Right (Left err) -> liftIO $ P.putDoc $ P.annotate (P.color P.Red) $ P.viaShow err <> P.line
            Right (Right value) -> liftIO $ P.putDoc $ prettyValue value <> P.line

changeMember :: Ord a => a -> Set a -> Set a
changeMember x s = if member x s then delete x s else insert x s

permissionMessage :: String -> Bool -> InputT IO ()
permissionMessage permission False = liftIO $ P.putDoc $ P.annotate (P.italicized <> P.color P.Green) $ P.pretty ("Permission to " <> permission <> " is enabled") <> P.line
permissionMessage permission True = liftIO $ P.putDoc $ P.annotate (P.italicized <> P.color P.Red) $ P.pretty ("Permission to " <> permission <> " is disabled") <> P.line

main :: IO ()
main = runInputT defaultSettings (loop False empty)
   where
       loop :: Bool -> Set HiPermission -> InputT IO ()
       loop debug permissions = do
           liftIO $ P.putDoc $ P.annotate (P.color P.Green) $ P.pretty "hi> "
           minput <- getInputLine ""
           case minput of
                Nothing -> return ()
                Just (':':'q':_) -> return ()
                Just (':':'d':_) -> do
                    liftIO $ P.putDoc $ P.annotate P.italicized $ P.pretty "Debug mode is " <> P.pretty (if debug then "disabled" else "enabled") <> P.line
                    loop (not debug) permissions
                Just (':':'w':_) -> do
                    permissionMessage "write" (member AllowWrite permissions)
                    loop debug (changeMember AllowWrite permissions)
                Just (':':'r':_) -> do
                    permissionMessage "read" (member AllowRead permissions)
                    loop debug (changeMember AllowRead permissions)
                Just (':':'t':_) -> do
                    permissionMessage "time" (member AllowTime permissions)
                    loop debug (changeMember AllowTime permissions)
                Just (':':'h':_) -> do
                    liftIO $ P.putDoc $ helpMessage
                    loop debug permissions
                Just (':':'p':_) -> do
                    liftIO $ P.putDoc $ P.annotate P.italicized $ P.pretty "Permissions: " <> P.viaShow permissions <> P.line
                    loop debug permissions
                Just input -> do
                    evaluate debug permissions input
                    loop debug permissions
