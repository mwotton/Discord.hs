{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord
import qualified Secret as Secret

send :: Text -> Effect DiscordM ()
send mes = fetch' (CreateMessage Secret.channel mes Nothing)

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

main :: IO ()
main = runBot (Bot Secret.token) $ do
  with ReadyEvent $ \(Init v u _ _ _) -> do
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u
    send "Hello, World! I'm back."

  with MessageCreateEvent $ \msg@Message{..} -> do
    when (not (userIsBot messageAuthor) && messageChannel == Secret.channel) $ do
      if | "Ping" `isPrefixOf` messageContent -> do
             liftIO $ putStrLn "got a trigger"
             reply msg "Pong!"
         | "ピン" `isPrefixOf` messageContent -> reply msg "ポン!"
         | otherwise -> return ()
