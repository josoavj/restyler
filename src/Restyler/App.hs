module Restyler.App
    ( AppT
    , runAppT
    , App(..)
    , runApp
    ) where

import Restyler.Prelude

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Restyler.Options
import Restyler.Statsd (HasStatsClient(..), StatsClient, withStatsClient)

newtype AppT app m a = AppT
    { unAppT :: ReaderT app (LoggingT m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadUnliftIO
        , MonadLogger
        , MonadReader app
        )

runAppT :: MonadIO m => HasLogger app => app -> AppT app m a -> m a
runAppT app f = runLoggerLoggingT app $ runReaderT (unAppT f) app

data App = App
    { appOptions :: Options
    , appLogger :: Logger
    , appStatsClient :: StatsClient
    }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasStatsClient App where
    statsClientL = lens appStatsClient $ \x y -> x { appStatsClient = y }

runApp :: MonadUnliftIO m => Options -> AppT App m a -> m a
runApp appOptions@Options {..} f = do
    appLogger <- newLogger oLogSettings
    withStatsClient oStatsdHost oStatsdPort statsdTags $ \appStatsClient -> do
        runAppT App { .. } f
  where
    statsdTags :: [(Text, Text)]
    statsdTags = [] -- TODO
