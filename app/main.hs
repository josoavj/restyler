module Main
    ( main
    ) where

import Restyler.Prelude

import Data.Time (getCurrentTime)
import Relude.Lifted.Exit (exitSuccess)
import Restyler.App
import Restyler.Clone
import Restyler.GitHub.PullRequest
import Restyler.Options
import Restyler.Patch
import Restyler.PullRequest (pullRequestLocalHeadRef)
import Restyler.Statsd (HasStatsClient)
import qualified Restyler.Statsd as Statsd

newtype Restyled = Restyled Patch

newtype NotRestyled = NotRestyled Text
    deriving stock Show
    deriving anyclass Exception

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options@Options {..} <- parseOptions

    runApp options $ do
        start <- liftIO getCurrentTime
        mPatch <- handleErrors $ do
            pr <- fetchPullRequest oOwner oRepo oPullRequest

            withPullRequestClone oAccessToken pr $ do
                -- config@Config {..} <- Config.load =<< Config.locate

                -- traverse_ (throwIO . NotRestyledSkipped) $ shouldSkipRestyle config pr

                -- proc "restyle" [".", "--commit"] callProcess_
                --     `catch` throwIO . NotRestyledError

                fromMaybeM
                    (throwIO $ NotRestyled "No differences found")
                    (readPatch $ unpack $ pullRequestLocalHeadRef pr)

                -- patch <- fromMaybeM
                --     (throwIO $ NotRestyled "No differences found")
                --     (readPatch $ unpack $ pullRequestLocalHeadRef pr)
                -- when (shouldCreateRestyledPR config pr) $ updateRestyledPR pr
                -- pure patch

        Statsd.increment "restyler.finished" []
        Statsd.histogramSince "restyler.duration" [] start

        for_ mPatch $ \patch -> do
            Statsd.increment "restyler.success" []
            Statsd.increment "restyler.differences" []
            Statsd.gauge "restyler.patch_size" (patchSize patch) []
            -- Log patch lines
            -- Red status

handleErrors
    :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasStatsClient env)
    => m a
    -> m (Maybe a)
handleErrors f = (Just <$> f) `catches` errorHandlers

-- TODO: stanzas for all app-specific error types
--
-- - PullRequestNotFound
-- - GitExitCodeException
--
errorHandlers
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasStatsClient env)
    => [Handler m (Maybe a)]
errorHandlers =
    [ Handler $ \(NotRestyled reason) -> do
        Statsd.increment "restyler.success" []
        logInfo $ "Pull Request was not restyled" :# ["reason" .= reason]
        -- Log reason
        -- Green status
        pure Nothing
    , Handler $ \(ex :: SomeException) -> do
        -- TODO: tags
        Statsd.increment "restyler.error" []
        logError $ pack (displayException ex) :# []
        -- Red status
        exitFailure
    ]
