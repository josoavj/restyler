module Restyler.Clone
    ( withPullRequestClone
    ) where

import Restyler.Prelude hiding (withSystemTempDirectory)

import qualified Data.Text as T
import GitHub.Data.Definitions (unIssueNumber)
import Restyler.GitHub.PullRequest (PullRequest)
import qualified Restyler.Process.Git as Git
import Restyler.PullRequest
    ( pullRequestCloneUrlToken
    , pullRequestLocalHeadRef
    , pullRequestNumber
    , pullRequestOwnerName
    , pullRequestRemoteHeadRef
    , pullRequestRepoName
    )
import UnliftIO.Directory (withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

withPullRequestClone :: MonadUnliftIO m => Text -> PullRequest -> m a -> m a
withPullRequestClone token pr inner = do
    withSystemTempDirectory tmpPrefix $ \tmp -> do
        Git.run "init" ["--quiet", tmp]
        withCurrentDirectory tmp $ do
            Git.run "remote" ["add", "origin", cloneUrl]
            Git.run "fetch" ["--quiet", "--depth", "1", "origin", refSpec]
            Git.run "checkout" ["--no-progress", branch]
            inner

  where
    tmpPrefix = unpack $ T.intercalate
        "-"
        [ "restyler"
        , untagName $ pullRequestOwnerName pr
        , untagName $ pullRequestRepoName pr
        , pack $ show $ unIssueNumber $ pullRequestNumber pr
        , ""
        ]

    cloneUrl = unpack $ pullRequestCloneUrlToken token pr
    ref = unpack $ pullRequestRemoteHeadRef pr
    branch = unpack $ pullRequestLocalHeadRef pr
    refSpec = ref <> ":" <> branch
