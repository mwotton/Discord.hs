{-# LANGUAGE RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides types and encoding/decoding code. Types should be identical to those provided
--   in the Discord API documentation.
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Network.Discord.Types.Prelude
  , module Network.Discord.Types.Channel
  , module Network.Discord.Types.Events
  , module Network.Discord.Types.Gateway
  , module Network.Discord.Types.Guild
  ) where

    import Network.Discord.Types.Channel
    import Network.Discord.Types.Events
    import Network.Discord.Types.Gateway
    import Network.Discord.Types.Guild
    import Network.Discord.Types.Prelude

    import Control.Monad.IO.Class

    class MonadIO m => DiscordAuth m where
      auth :: m Auth
      version :: m String