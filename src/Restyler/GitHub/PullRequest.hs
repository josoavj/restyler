module Restyler.GitHub.PullRequest
    ( PullRequest
    , fetchPullRequest
    ) where

import Restyler.Prelude

import GitHub.Data.Definitions (IssueNumber, Owner)
import GitHub.Data.PullRequests (PullRequest)
import GitHub.Data.Repos (Repo)

data PullRequestNotFound = PullRequestNotFound
    deriving stock Show
    deriving anyclass Exception

fetchPullRequest
    :: MonadIO m => Name Owner -> Name Repo -> IssueNumber -> m PullRequest
fetchPullRequest _owner _repo _number = throwIO PullRequestNotFound
