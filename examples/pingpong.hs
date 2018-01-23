{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord
import qualified Token as Secret

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

main :: IO ()
main = runBot (Bot Secret.token) $ do
  with ReadyEvent $ \(Init v u _ _ _) -> do
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u
    fetch' (CreateMessage Secret.channelTest "Hello, World!" Nothing)

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("Ping" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
      liftIO $ putStrLn "got a trigger"
      reply msg "Pong!"
