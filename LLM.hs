{-# LANGUAGE OverloadedStrings #-}
module LLM
  ( llmExplainSteps 
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Aeson            as A
import           Data.Aeson            ((.=), (.:), (.:?))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Monoid           ((<>))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment    (lookupEnv)

newtype ChatResponse = ChatResponse { choices :: [Choice] }
instance A.FromJSON ChatResponse where
  parseJSON = A.withObject "ChatResponse" $ \o ->
    ChatResponse <$> o .: "choices"

newtype Choice = Choice { message :: Message }
instance A.FromJSON Choice where
  parseJSON = A.withObject "Choice" $ \o ->
    Choice <$> o .: "message"

data Message = Message { role :: Text, content :: Text }
instance A.FromJSON Message where
  parseJSON = A.withObject "Message" $ \o -> do
    r  <- o .: "role"
    mc <- o .:? "content"
    mr <- o .:? "reasoning"
    let txt = case (mc, mr) of
                (Just c, _)          -> c
                (Nothing, Just rtxt) -> rtxt
                _                    -> ""
    pure (Message r txt)



systemPrompt :: Text
systemPrompt =
  "You are a friendly calculus tutor talking to a first-year student.\n\n" <>
  "The user will describe what a program has ALREADY computed about a function:\n" <>
  "- the function f(x),\n" <>
  "- its derivative f'(x),\n" <>
  "- a symbolic antiderivative F(x), and\n" <>
  "- a numerical value for a definite integral.\n\n" <>
  "ASSUME all these results are correct. Do NOT recompute or correct them. " <>
  "Do NOT show derivation steps.\n\n" <>
  "STYLE RULES (MUST OBEY):\n" <>
  "- Answer in ENGLISH only.\n" <>
  "- Use AT MOST 4 short sentences.\n" <>
  "- Do NOT use bullet points or numbered lists.\n" <>
  "- Do NOT talk about yourself, the user, the program, your task, or your reasoning.\n" <>
  "- Do NOT say things like \"Okay\" or \"let's tackle\" or \"deconstruct the request\".\n" <>
  "- Just explain in simple terms what the function, its derivative, the antiderivative " <>
  "  and the definite integral mean in this situation."

buildUserPrompt :: String -> Maybe (Double,Double) -> Text
buildUserPrompt desc mb =
  case mb of
    Nothing ->
      T.pack $
        desc ++
        "\n\nExplain these results to a first-year calculus student in at most 4 short sentences."
    Just (a,b) ->
      T.pack $
        desc ++
        "\n\nExplain to a first-year calculus student, in at most 4 short sentences, " ++
        "what this tells them about f(x) on the interval [" ++ show a ++ "," ++ show b ++ "]."

llmExplainSteps :: String -> Maybe (Double,Double) -> IO (Either String String)
llmExplainSteps desc mBounds = do
  mKey <- lookupEnv "CEREBRAS_API_KEY"
  case mKey of
    Nothing   -> pure (Left "CEREBRAS_API_KEY is not set")
    Just key  -> do
      mgr     <- newManager tlsManagerSettings
      initReq <- parseRequest "https://api.cerebras.ai/v1/chat/completions"

      let modelName = "qwen-3-235b-a22b-instruct-2507"
          userMsg   = buildUserPrompt desc mBounds

          body      = A.object
            [ "model"       .= modelName
            , "messages"    .=
                [ A.object ["role" .= ("system" :: Text), "content" .= systemPrompt]
                , A.object ["role" .= ("user"   :: Text), "content" .= userMsg]
                ]
            , "temperature" .= (0.1 :: Double) 
            , "max_tokens"  .= (200  :: Int)     
            ]

          req = initReq
            { method          = "POST"
            , requestHeaders  =
                [ ("Content-Type", "application/json")
                , ("Authorization", BS.pack ("Bearer " <> key))
                , ("User-Agent", "integral-calculator/1.0")
                ]
            , requestBody     = RequestBodyLBS (A.encode body)
            , responseTimeout = responseTimeoutMicro (45 * 1000000)
            }

      resp <- httpLbs req mgr
      case A.eitherDecode (responseBody resp) of
        Left e  -> do
          let raw = decodeUtf8With lenientDecode (LBS.toStrict (responseBody resp))
          pure (Left ("LLM decode error: " <> e <> "\nRaw: " <> T.unpack raw))
        Right (ChatResponse cs) ->
          case cs of
            (Choice (Message _ txt):_) -> pure (Right (T.unpack txt))
            _                          -> pure (Left "LLM returned no choices")
