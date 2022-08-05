module Restyler.Error
    ( RestylerError(..)
    , AsRestylerError(..)

    -- * Re-exports
    , MonadError(..)
    ) where

import Restyler.Prelude

import Control.Monad.Except

data RestylerError = RestylerError

class AsRestylerError e where
    toRestylerError :: e -> RestylerError

instance AsRestylerError RestylerError where
    toRestylerError = id
