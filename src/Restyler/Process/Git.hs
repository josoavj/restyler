module Restyler.Process.Git
    ( GitExitCodeException(..)
    , run
    , readStdout
    ) where

import Restyler.Prelude

import System.Process.Typed

data GitExitCodeException = GitExitCodeException
    { geceSubcommand :: String
    , geceArguments :: [String]
    , geceException :: ExitCodeException
    }
    deriving stock Show
    deriving anyclass Exception

run :: MonadUnliftIO m => String -> [String] -> m ()
run = git runProcess_

readStdout :: MonadUnliftIO m => String -> [String] -> m Text
readStdout cmd = fmap (decodeUtf8 . toStrict) . git readProcessStdout_ cmd

git
    :: MonadUnliftIO m
    => (ProcessConfig () () () -> m a)
    -> String
    -> [String]
    -> m a
git f cmd args =
    f (proc "git" $ cmd : args)
        `catch` (throwIO . GitExitCodeException cmd args)
