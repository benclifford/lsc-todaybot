{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- todaybot
-- ben clifford benc@hawaga.org.uk

-- used https://www.fpcomplete.com/school/to-infinity-and-beyond/competition-winners/interfacing-with-restful-json-apis
-- as a tutorial

import Prelude hiding (mapM_)
import Control.Applicative ( (<$>), (<|>), many )
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException (..) )
import Control.Lens
import Control.Monad hiding (mapM_)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Maybe (fromMaybe)
import qualified Data.Proxy as Proxy
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Foldable (mapM_)
import Data.Aeson ( Value(..) )
import Data.Aeson.Lens (key, _Bool, _String, _Array)
import Data.Yaml (decodeFile)
import System.IO (hPutStrLn, stderr)
import Network.Wreq (auth,
                     basicAuth,
                     defaults,
                     getWith,
                     header,
                     param,
                     postWith,
                     Part (..) )
import qualified Network.Wreq as Wreq
import Data.Monoid ( (<>) )
import qualified Control.Lens.Getter as Getter
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSS8
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Servant.Client
import Servant.API

data Configuration = Configuration {
  username :: T.Text,
  password :: T.Text,
  app_id :: BSS.ByteString,
  app_secret :: BSS.ByteString
}

type BearerToken = T.Text

main = do
  progress "todaybot"

  configuration <- readConfiguration   

  forever $ do
    skipExceptions $ mainLoop configuration
    sleep 13

mainLoop configuration = do

  bearerToken <- authenticate configuration
  posts <- getHotPosts bearerToken
  mapM_ (skipExceptions . (processPost bearerToken)) posts
  progress "Pass completed."

skipExceptions a = a `catch` \(e :: SomeException) -> progress $ "Exception: " <> (show e)

userAgentHeader = header "User-Agent" .~ ["lsc-todaybot by u/benclifford"]
authorizationHeader bearerToken = header "Authorization" .~ ["bearer " <> (TE.encodeUtf8 bearerToken)]

authenticate :: Configuration -> IO BearerToken
authenticate configuration = do
  progress "Authenticating using servant"
  manager <- newManager tlsManagerSettings
  let authdata = BasicAuthData (app_id configuration) (app_secret configuration)
  (Right res) <- runExceptT (authcall authdata (Just "password") (Just $ username configuration) (Just $ password configuration) manager (BaseUrl Https "www.reddit.com" 443 "")) 
  print res
  let token = res ^. key "access_token" . _String
  print token
  return token

type AuthenticationAPI = BasicAuth "reddit" T.Text :> "api" :> "v1" :> "access_token" :> QueryParam "grant_type" T.Text :> QueryParam "username" T.Text :> QueryParam "password" T.Text :> Post '[JSON] Value
api :: Proxy.Proxy AuthenticationAPI
api = Proxy.Proxy
authcall = client api

hotPostsUrl = "https://oauth.reddit.com/r/LondonSocialClub/hot?limit=100"

getHotPosts :: BearerToken -> IO (V.Vector Value)
getHotPosts bearerToken = do
  progress "Getting hot posts"

  let opts = defaults
           & authorizationHeader bearerToken
           & userAgentHeader
  resp <- getWith opts hotPostsUrl
  return $ resp ^. (Wreq.responseBody) . key "data" . key "children" . _Array


readConfiguration = do
  configYaml :: Value <- fromMaybe (error "Cannot parse config file")  <$> decodeFile "secrets.yaml"
  return $ Configuration {
    username = configYaml ^. key "username" . _String,
    password = configYaml ^. key "password" . _String,
    app_id = configYaml ^. key "app_id" . _ByteString,
    app_secret = configYaml ^. key "app_secret" . _ByteString
  }

_ByteString = _String . Getter.to (T.unpack) . Getter.to (BSS8.pack)

processPost bearerToken post = do
  let kind = post ^. postKind
  let i = post ^. postId
  let fullname = kind <> "_" <> i
  let flair_text = post ^. postFlairText
  let flair_css = post ^. postFlairCss
  let title = post ^. postTitle
  let stickied = fromMaybe False $ post ^? key "data" . key "stickied" . _Bool
  T.putStr $ fullname <> ": " <> title <> " [" <> flair_text <> "/" <> flair_css <> "]"
  when stickied $ T.putStr " [Stickied]"
  T.putStrLn ""

  -- if flair has been modified (other than to Today) then
  -- stay away...
  let changeableFlair = flair_text == "Today" || flair_text == ""

  progress $ "    Changeable flair? " <> (show changeableFlair)

  when changeableFlair $ do
  -- today?
  -- can we parse the date out of the subject line?
  -- let's use parsec...
    let parsedDate = P.parse datedSubjectLine "Post title" title

    case parsedDate of
      Right postDate -> do
        progress $ "    Post date is " <> (show postDate)
        now <- localDay <$> getCurrentLocalTime

        -- posts move through a sequence of no flair, then today,
        -- then archived, except we do not archive stickied posts
        -- because that looks weird with a greyed out post being stickied.
        -- I'm unsure if the right thing to do is unsticky and archive or
        -- to leave stickied, with the today flair substituted back to
        -- nothing - then if someone unstickies, it will get archived flair
        -- in a future run.
        if | postDate > now -> progress $ "    Skipping: Post is in future"
           | postDate == now -> forceFlair bearerToken post "Today" "today"
           | postDate < now && not stickied -> forceFlair bearerToken post "Archived" "archived"
           | postDate < now && stickied -> forceFlair bearerToken post "" ""

      Left e -> progress $ "    Skipping: Date did not parse: " <> (show e)

    let interestCheck = (T.toCaseFold "[Interest") `T.isPrefixOf` (T.toCaseFold title)
    progress $ "    Interest check? " <> (show interestCheck)

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

datedSubjectLine = prefixDatedSubjectLine
               <|> postfixDatedSubjectLine

prefixDatedSubjectLine = dateBlock
  -- ignore the rest of the line...

postfixDatedSubjectLine = do
  many $ P.noneOf "["
  d <- dateBlock
  P.eof
  return d

dateBlock = do
  P.char '['
  day <- dateComponent
  dateSeparator
  month <- dateComponent
  dateSeparator
  year <- yearComponent
  P.char ']'
  return $ fromGregorian year month day

dateSeparator = P.oneOf "/-."

dateComponent = read <$> digits

yearComponent = (normaliseYear . read) <$> digits

digits = (P.many $ P.oneOf "0123456789")

normaliseYear year =
  case () of
    _ | year > 2000 -> year
    _ | year >= 0 && year < 100 -> 2000 + year -- hello, 2100!

getCurrentLocalTime = do
  nowUTC <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz nowUTC

postKind = key "kind" . _String
postId = key "data" . key "id" . _String
postFlairText = key "data" . key "link_flair_text" . _String
postFlairCss = key "data" . key "link_flair_css_class" . _String
postTitle = key "data" . key "title" . _String

forceFlair bearerToken post forced_flair forced_flair_css = do
  let kind = post ^. postKind
  let i = post ^. postId
  let fullname = kind <> "_" <> i
  T.putStrLn $ "    Setting flair for " <> fullname <> " to " <> forced_flair <> " if necessary"
  let flair_text = post ^. postFlairText
  let flair_css = post ^. postFlairCss
  if flair_text == forced_flair && flair_css == forced_flair_css
    then progress "    No flair change necessary"
    else do progress "    Updating flair"
            let opts = defaults
                     & authorizationHeader bearerToken
                     & param "api_type" .~ ["json"]
                     & param "link" .~ [fullname]
                     & param "text" .~ [forced_flair]
                     & param "css_class" .~ [forced_flair_css]

            postWith opts "https://oauth.reddit.com/r/LondonSocialClub/api/flair" ([] :: [Part])
            -- TODO check if successful
            return ()


progress s = hPutStrLn stderr s

-- | sleeps for specified number of minutes
sleep mins = threadDelay (mins * 60 * 1000000)

