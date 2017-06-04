-- | Provides a convenience framework for writing Discord bots without dealing with Pipes
{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, FlexibleContexts #-}
module Network.Discord.Framework where
  import Data.Proxy
  import GHC.TypeLits hiding ((:<>:))
  import Control.Applicative
  import Control.Concurrent
  import System.IO.Unsafe (unsafePerformIO)

  import Network.Discord.Rest
  import Network.Discord.Gateway
  import Network.Discord.Types

  import Control.Monad.Reader
  import Network.WebSockets (Connection)
  
  newtype DiscordApp m a = DiscordApp 
    { runApp :: DiscordRest m => Event -> ReaderT Connection m a }

  instance Alternative (DiscordApp m) where
    empty = DiscordApp (\_ -> empty)
    DiscordApp f <|> DiscordApp g = DiscordApp (\e -> f e <|> g e)

  instance Applicative (DiscordApp m) where
    pure a = DiscordApp (\_ -> return a)
    DiscordApp f <*> DiscordApp a =
      DiscordApp (\e -> (f e) <*> (a e))

  instance DiscordAuth (DiscordApp m) where
    auth    = DiscordApp (\_ -> lift auth)
    version = DiscordApp (\_ -> lift version)
    runIO   = fail "DiscordApp cannot be lifted to IO"

  instance DiscordRest m => DiscordGate (DiscordApp m) where
    type Vault (DiscordApp m) = MVar

    data VaultKey (DiscordApp m) a = Store (MVar a)
    get = liftIO . readMVar
    put s v = liftIO $ putMVar s v

    sequenceKey = Store $ unsafePerformIO newEmptyMVar
    {-# NOINLINE sequenceKey #-}
    storeFor (Store var) = return var

    connection = DiscordApp (\_ -> ask)
    feed m event = do
      c <- connection
      _ <- liftIO . forkIO . runIO $ runReaderT ((runApp m) event) c
      return ()

    run m conn =
      runIO $ runReaderT ((runApp $ eventStream Create m) Nil) conn
    fork m = do
      c <- connection
      _ <- DiscordApp $ \e -> liftIO . forkIO . runIO $ runReaderT ((runApp m) e) c
      return ()

  instance Functor (DiscordApp m) where
    f `fmap` DiscordApp a = DiscordApp (\e -> f `fmap` a e)

  instance Monad (DiscordApp m) where
    m >>= k = DiscordApp $ \e -> do
      a <- runApp m e
      runApp (k a) e

  instance MonadIO (DiscordApp m) where
    liftIO f = DiscordApp (\_ -> liftIO f)

  instance MonadPlus (DiscordApp m)

  class (Monad m, HasEvent api Event, Step (EventHandler' api m) Event (m ()))
    => EventHandler api app m

  data Context types where
    EmptyContext :: Context '[]
    (:.) :: x -> Context xs -> Context (x ': xs)

  data DiscordApp' a where
    Leaf   :: DiscordApp' a
    Map    :: (a -> b)       -> DiscordApp' b -> DiscordApp' a
    Reduce :: (a -> Maybe b) -> DiscordApp' b -> DiscordApp' a
    Filter :: (a -> Bool)    -> DiscordApp' a -> DiscordApp' a
    Choose :: DiscordApp' a   -> DiscordApp' a -> DiscordApp' a

  class HasEvent api event where
    type EventHandler' api ( m :: * -> * )
    makeApp :: Proxy api -> Context context -> DiscordApp' event

  class Step f pre image where
    go :: f -> pre -> image

  instance Step (a -> b) a b where
    go = ($)

  instance (DiscordRest m, Step f a (m ()), Step g a (m ())) => Step (f :<>: g) a (m ()) where
    go (f :<>: g) pre = seq' (go f pre) (go g pre)
      where
        seq' :: DiscordRest m => m () -> m () -> m ()
        seq' = (>>)

  data a :> b
  infixr 5 :> 

  data a :<>: b = a :<>: b
  infixl 3 :<>:

  instance (HasEvent a e, HasEvent b e) => HasEvent (a :<>: b) e where
    type EventHandler' (a :<>: b) m = EventHandler' a m :<>: EventHandler' b m
    makeApp p c = Choose (makeApp a c) (makeApp b c)
      where
        (a, b) = split p
        split :: Proxy (a :<>: b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

  data EventHandle a

  instance HasEvent (EventHandle a) a where
    type EventHandler' (EventHandle a) m = a -> m ()
    makeApp p _ = leaf p
      where
        leaf :: Proxy (EventHandle a) -> DiscordApp' a
        leaf _ = Leaf

  class EventFilter a where
    type Filtered a
    reduce :: Proxy a -> Event -> Maybe (Filtered a)

  instance (EventFilter a, HasEvent b (Filtered a)) => HasEvent (a :> b) Event where
    type EventHandler' (a :> b) m = EventHandler' b m
    makeApp p c = Reduce (reduce a) $ makeApp b c
      where
        (a, b) = split p
        split :: Proxy (a :> b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

  data ReadyEvent
  
  instance EventFilter ReadyEvent where
    type Filtered ReadyEvent = Init
    reduce _ (Ready e) = Just e
    reduce _ _ = Nothing

  data ResumedEvent
  
  instance EventFilter ResumedEvent where
    type Filtered ResumedEvent = Object
    reduce _ (Resumed o) = Just o
    reduce _ _ = Nothing

  data ChannelCreateEvent

  instance EventFilter ChannelCreateEvent where
    type Filtered ChannelCreateEvent = Channel
    reduce _ (ChannelCreate c) = Just c
    reduce _ _ = Nothing

  data ChannelUpdateEvent

  instance EventFilter ChannelUpdateEvent where
    type Filtered ChannelUpdateEvent = Channel
    reduce _ (ChannelUpdate c) = Just c
    reduce _ _ = Nothing

  data ChannelDeleteEvent

  instance EventFilter ChannelDeleteEvent where
    type Filtered ChannelDeleteEvent = Channel
    reduce _ (ChannelDelete c) = Just c
    reduce _ _ = Nothing

  data GuildCreateEvent

  instance EventFilter GuildCreateEvent where
    type Filtered GuildCreateEvent = Guild
    reduce _ (GuildCreate c) = Just c
    reduce _ _ = Nothing

  data GuildUpdateEvent

  instance EventFilter GuildUpdateEvent where
    type Filtered GuildUpdateEvent = Guild
    reduce _ (GuildUpdate c) = Just c
    reduce _ _ = Nothing

  data GuildDeleteEvent

  instance EventFilter GuildDeleteEvent where
    type Filtered GuildDeleteEvent = Guild
    reduce _ (GuildDelete c) = Just c
    reduce _ _ = Nothing

  data GuildBanAddEvent

  instance EventFilter GuildBanAddEvent where
    type Filtered GuildBanAddEvent = Member
    reduce _ (GuildBanAdd c) = Just c
    reduce _ _ = Nothing

  data GuildBanRemoveEvent

  instance EventFilter GuildBanRemoveEvent where
    type Filtered GuildBanRemoveEvent = Member
    reduce _ (GuildBanRemove c) = Just c
    reduce _ _ = Nothing

  data GuildEmojiUpdateEvent

  instance EventFilter GuildEmojiUpdateEvent where
    type Filtered GuildEmojiUpdateEvent = Object
    reduce _ (GuildEmojiUpdate c) = Just c
    reduce _ _ = Nothing

  data GuildIntegrationsUpdateEvent

  instance EventFilter GuildIntegrationsUpdateEvent where
    type Filtered GuildIntegrationsUpdateEvent = Object
    reduce _ (GuildIntegrationsUpdate c) = Just c
    reduce _ _ = Nothing

  data GuildMemberAddEvent

  instance EventFilter GuildMemberAddEvent where
    type Filtered GuildMemberAddEvent = Member
    reduce _ (GuildMemberAdd c) = Just c
    reduce _ _ = Nothing

  data GuildMemberRemoveEvent

  instance EventFilter GuildMemberRemoveEvent where
    type Filtered GuildMemberRemoveEvent = Member
    reduce _ (GuildMemberRemove c) = Just c
    reduce _ _ = Nothing

  data GuildMemberUpdateEvent

  instance EventFilter GuildMemberUpdateEvent where
    type Filtered GuildMemberUpdateEvent = Member
    reduce _ (GuildMemberUpdate c) = Just c
    reduce _ _ = Nothing

  data GuildMemberChunkEvent

  instance EventFilter GuildMemberChunkEvent where
    type Filtered GuildMemberChunkEvent = Object
    reduce _ (GuildMemberChunk c) = Just c
    reduce _ _ = Nothing

  data GuildRoleCreateEvent

  instance EventFilter GuildRoleCreateEvent where
    type Filtered GuildRoleCreateEvent = Object
    reduce _ (GuildRoleCreate c) = Just c
    reduce _ _ = Nothing

  data GuildRoleUpdateEvent

  instance EventFilter GuildRoleUpdateEvent where
    type Filtered GuildRoleUpdateEvent = Object
    reduce _ (GuildRoleUpdate c) = Just c
    reduce _ _ = Nothing

  data GuildRoleDeleteEvent

  instance EventFilter GuildRoleDeleteEvent where
    type Filtered GuildRoleDeleteEvent = Object
    reduce _ (GuildRoleDelete c) = Just c
    reduce _ _ = Nothing

  data MessageCreateEvent

  instance EventFilter MessageCreateEvent where
    type Filtered MessageCreateEvent = Message
    reduce _ (MessageCreate c) = Just c
    reduce _ _ = Nothing

  data MessageUpdateEvent

  instance EventFilter MessageUpdateEvent where
    type Filtered MessageUpdateEvent = Message
    reduce _ (MessageUpdate c) = Just c
    reduce _ _ = Nothing

  data MessageDeleteEvent

  instance EventFilter MessageDeleteEvent where
    type Filtered MessageDeleteEvent = Object
    reduce _ (MessageDelete c) = Just c
    reduce _ _ = Nothing

  data MessageDeleteBulkEvent

  instance EventFilter MessageDeleteBulkEvent where
    type Filtered MessageDeleteBulkEvent = Object
    reduce _ (MessageDeleteBulk c) = Just c
    reduce _ _ = Nothing

  data PresenceUpdateEvent

  instance EventFilter PresenceUpdateEvent where
    type Filtered PresenceUpdateEvent = Object
    reduce _ (PresenceUpdate c) = Just c
    reduce _ _ = Nothing

  data TypingStartEvent

  instance EventFilter TypingStartEvent where
    type Filtered TypingStartEvent = Object
    reduce _ (TypingStart c) = Just c
    reduce _ _ = Nothing

  data UserSettingsUpdateEvent
  
  instance EventFilter UserSettingsUpdateEvent where
    type Filtered UserSettingsUpdateEvent = Object
    reduce _ (UserSettingsUpdate c) = Just c
    reduce _ _ = Nothing

  data UserUpdateEvent

  instance EventFilter UserUpdateEvent where
    type Filtered UserUpdateEvent = Object
    reduce _ (UserUpdate c) = Just c
    reduce _ _ = Nothing

  data VoiceStateUpdateEvent

  instance EventFilter VoiceStateUpdateEvent where
    type Filtered VoiceStateUpdateEvent = Object
    reduce _ (VoiceStateUpdate c) = Just c
    reduce _ _ = Nothing

  data VoiceServerUpdateEvent

  instance EventFilter VoiceServerUpdateEvent where
    type Filtered VoiceServerUpdateEvent = Object
    reduce _ (VoiceServerUpdate c) = Just c
    reduce _ _ = Nothing

  data SomeEvent (a :: Symbol)

  instance KnownSymbol a => EventFilter (SomeEvent a) where
    type Filtered (SomeEvent a) = Object
    reduce p (UnknownEvent s e)
      | s == eventName = Just e
      | otherwise    = Nothing
      where
        eventName = symbolVal $ event p
        event :: Proxy (SomeEvent a) -> Proxy a
        event _ = Proxy
    reduce _ _ = Nothing