{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- todaybot
-- ben clifford benc@hawaga.org.uk

-- used https://www.fpcomplete.com/school/to-infinity-and-beyond/competition-winners/interfacing-with-restful-json-apis
-- as a tutorial

import Control.Applicative ( (<$>) )
import Control.Lens
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Aeson ( Value(..) )
import Data.Aeson.Lens (key, _Bool, _String, _Array)
import Data.Yaml (decodeFile)
import System.IO (hPutStrLn, stderr)
import Network.Wreq
import Data.Monoid ( (<>) )
import qualified Control.Lens.Getter as Getter
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.Traversable as TR
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSS8

data Configuration = Configuration {
  username :: T.Text,
  password :: T.Text,
  app_id :: BSS.ByteString,
  app_secret :: BSS.ByteString
}

type BearerToken = T.Text

main = do
  putStrLn "todaybot"

  configuration <- readConfiguration   
  bearerToken <- authenticate configuration

  T.putStrLn $ "Bearer token is " <> bearerToken

  hotPosts <- getHotPosts bearerToken

  let (d :: V.Vector Value) = hotPosts ^. key "data" . key "children" . _Array

  TR.mapM (processPost bearerToken) d

userAgentHeader = header "User-Agent" .~ ["lsc-todaybot by u/benclifford"]
authorizationHeader bearerToken = header "Authorization" .~ ["bearer " <> (TE.encodeUtf8 bearerToken)]

authenticate :: Configuration -> IO BearerToken
authenticate configuration = do
  progress "Authenticating"

  let opts = defaults
           & userAgentHeader
           & param "grant_type" .~ ["password"]
           & param "username" .~ [username configuration]
           & param "password" .~ [password configuration]
           & auth ?~ basicAuth (app_id configuration) (app_secret configuration)

  resp <- postWith opts ("https://www.reddit.com/api/v1/access_token") ([] :: [Part])

  return $ resp ^. responseBody . key "access_token" . _String

getHotPosts :: BearerToken -> IO BSL.ByteString
getHotPosts bearerToken = do
  progress "Getting hot posts"

  let opts = defaults
           & authorizationHeader bearerToken
           & userAgentHeader
  resp <- getWith opts ("https://oauth.reddit.com/r/LondonSocialClub/hot") 
  return $ resp ^. responseBody


readConfiguration = do
  (Just (configYaml :: Value)) <- decodeFile "secrets.yaml"
  return $ Configuration {
    username = configYaml ^. key "username" . _String,
    password = configYaml ^. key "password" . _String,
    app_id = configYaml ^. key "app_id" . _ByteString,
    app_secret = configYaml ^. key "app_secret" . _ByteString
  }

_ByteString = _String . Getter.to (T.unpack) . Getter.to (BSS8.pack)

processPost bearerToken post = do
  putStrLn $ "=== Processing post ==="
  let kind = post ^. key "kind" . _String
  let i = post ^. key "data" . key "id" . _String
  let kindid = kind <> "_" <> i
  let flair_text = post ^. key "data" . key "link_flair_text" . _String
  let flair_css = post ^. key "data" . key "link_flair_css_class" . _String
  let title = post ^. key "data" . key "title" . _String
  let stickied = fromMaybe False $ post ^? key "data" . key "stickied" . _Bool
  T.putStrLn $ " Title (id): " <> title <> " (" <> kindid <> ")"
  T.putStrLn $ "Flair (css): " <> flair_text <> " (" <> flair_css <> ")"
  when stickied $ T.putStrLn "[Stickied]"

  -- if flair has been modified (other than to Today) then
  -- stay away...
  let safeToChange = flair_text == "Today" || flair_text == ""

  putStrLn $ "Safe to change? " ++ (show safeToChange)

  when safeToChange $ do
  -- today?
  -- can we parse the date out of the subject line?
  -- let's use parsec...
    let parsedDate = P.parse datedSubjectLine "Post title" title

    case parsedDate of
      Right postDate -> do
        putStrLn $ "Post date is " ++ (show postDate)
        now <- localDay <$> getCurrentLocalTime

        putStrLn $ "Current date is " ++ (show now)

        -- posts move through a sequence of no flair, then today,
        -- then archived, except we do not archive stickied posts.
        if | postDate > now -> return () -- no flair change
           | postDate == now -> forceFlair bearerToken post "Today" "today"
           | postDate < now && not stickied -> forceFlair bearerToken post "Archived" "archived"

      Left e -> putStrLn $ "Date did not parse: " ++ (show e)

    let interestCheck = (T.toCaseFold "[Interest") `T.isPrefixOf` (T.toCaseFold title)
    putStrLn $ "Interest check? " ++ (show interestCheck)

    when interestCheck $ forceFlair bearerToken post "Interest Check" "interestcheck"

  -- because we love the royal george
  {-
  when (   kind == "t3" 
        && "Royal George" `T.isInfixOf` title ) $ do
    putStrLn "Royal george matched!"
    forceFlair bearerToken post "ROYAL GEORGE" ""
  -}

-- parser for subject line dates...
-- expecting (from automod config)
--   e (regex): "\\[([0-9]{1,2}[/.-][0-9]{1,2}[/.-]([0-9]{2}|[0-9]{4})|interest( check)?)\\].*"
datedSubjectLine = do
  P.char '['
  day <- dateComponent
  P.char '/'
  month <- dateComponent
  P.char '/'
  year <- yearComponent
  P.char ']'
  -- ignore the rest of the line...

  return $ fromGregorian year month day

dateComponent = read <$> (P.many $ P.oneOf "0123456789")

yearComponent = do
  year <- read <$> (P.many $ P.oneOf "0123456789")
  let normalisedYear = case () of
        _ | year > 2000 -> year
        _ | year > 0 && year < 100 -> 2000 + year -- hello, 2100!
  return normalisedYear

getCurrentLocalTime = do
  nowUTC <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz nowUTC

forceFlair bearerToken post forced_flair forced_flair_css = do
  let kind = post ^. key "kind" . _String
  let i = post ^. key "data" . key "id" . _String
  let kindid = kind <> "_" <> i
  T.putStrLn $ "Forcing flair for " <> kindid <> " to " <> forced_flair <> " if necessary"
  let flair_text = post ^. key "data" . key "link_flair_text" . _String
  let flair_css = post ^. key "data" . key "link_flair_css_class" . _String
  if flair_text == forced_flair && flair_css == forced_flair_css
    then putStrLn "No flair change necessary"
    else do putStrLn "Flair update necessary"
            let opts = defaults
                     & authorizationHeader bearerToken
                     & param "api_type" .~ ["json"]
                     & param "link" .~ [kindid]
                     & param "text" .~ [forced_flair]
                     & param "css_class" .~ [forced_flair_css]

            postWith opts "https://oauth.reddit.com/r/LondonSocialClub/api/flair" ([] :: [Part])
            -- TODO check if successful
            return ()


progress s = hPutStrLn stderr s

