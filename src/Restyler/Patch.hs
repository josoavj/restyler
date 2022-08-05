module Restyler.Patch
    ( Patch(..)
    , patchSize
    , readPatch
    ) where

import Restyler.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Restyler.Process.Git as Git

newtype Patch = Patch
    { getPatchLines :: NonEmpty Text
    }

patchSize :: Patch -> Int
patchSize = length . getPatchLines

readPatch :: MonadUnliftIO m => String -> m (Maybe Patch)
readPatch ref = do
    lns <- T.lines <$> Git.readStdout "git" ["format-patch", ref]
    pure $ Patch <$> NE.nonEmpty lns
