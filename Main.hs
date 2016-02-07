{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE GADTs #-} -- needed for extensible-effects lift IO to work?

-- all of these come from the top of extensible-effects lift module
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- todaybot
-- ben clifford benc@hawaga.org.uk

-- used https://www.fpcomplete.com/school/to-infinity-and-beyond/competition-winners/interfacing-with-restful-json-apis
-- as a tutorial

import Prelude hiding (mapM_)
import Control.Applicative ( (<$>), (<|>), many )
import Control.Concurrent (threadDelay)
import Control.Eff -- TODO: tighten
import Control.Eff.Lift (lift, runLift, Lift (..) )
import Control.Exception (catch, SomeException (..) )
import Control.Lens
import Control.Monad hiding (mapM_)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Foldable (mapM_)
import Data.Aeson ( Value(..) )
import Data.Aeson.Lens (key, _Bool, _String, _Array)
import Data.Void
import Data.Yaml (decodeFile)
import System.IO (hPutStrLn, stderr)
import Network.Wreq (auth,
                     basicAuth,
                     defaults,
                     getWith,
                     header,
                     param,
                     postWith,
                     responseBody,
                     Part (..) )
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

data Configuration = Configuration {
  username :: T.Text,
  password :: T.Text,
  app_id :: BSS.ByteString,
  app_secret :: BSS.ByteString
}

type BearerToken = T.Text

main = runLift $ do
  progress "todaybot"
  configuration <- readConfiguration   

  forever $ do
    {- skipExceptions $ -} 
    mainLoop configuration
    sleep 13

-- more type signatures that don't pin right. using the more flexible signature,
-- I get:
{-
Main.hs:85:34:
    Couldn't match type ‘r0’ with ‘r’
      because type variable ‘r’ would escape its scope
    This (rigid, skolem) type variable is bound by
      the type signature for
        mainLoop :: SetMember Lift (Lift IO) r => Configuration -> Eff r ()
      at Main.hs:79:13-67
    Expected type: Value -> Free (Union r) ()
      Actual type: Value -> Free (Union r0) ()
    Relevant bindings include
      mainLoop :: Configuration -> Eff r () (bound at Main.hs:81:1)
    In the first argument of ‘mapM_’, namely
      ‘((processPost bearerToken))’
    In a stmt of a 'do' block: mapM_ ((processPost bearerToken)) posts

Main.hs:154:3:
    Couldn't match type ‘extensible-effects-1.11.0.3:Data.OpenUnion.Internal.OpenUnion2.Member'
                           (Lift IO) r0’
                   with ‘'True’
    The type variable ‘r0’ is ambiguous
    Relevant bindings include
      processPost :: T.Text -> Value -> Free (Union r0) ()
        (bound at Main.hs:146:1)
    In the expression: lift
    In a stmt of a 'do' block:
      lift
      $ T.putStr
        $ fullname
          <> ": " <> title <> " [" <> flair_text <> "/" <> flair_css <> "]"
    In the expression:
      do { let kind = post ^. postKind;
           let i = post ^. postId;
           let fullname = kind <> "_" <> i;
           let flair_text = post ^. postFlairText;
           .... }

-}
mainLoop :: SetMember Lift (Lift IO) r => Configuration -> Eff r ()
-- mainLoop :: Configuration -> Eff (Lift IO :> Data.Void.Void) ()
mainLoop configuration = do

  bearerToken <- authenticate configuration
  posts <- getHotPosts bearerToken
  mapM_ ({- skipExceptions . -} (processPost bearerToken)) posts
  progress "Pass completed."

-- skipExceptions a = a `catch` \(e :: SomeException) -> progress $ "Exception: " <> (show e)
skipExceptions = id
-- no implementation of exception handling in extensible effects branch...
-- I don't understand how IO exceptions work in this model, so leave
-- it 'till later?

userAgentHeader = header "User-Agent" .~ ["lsc-todaybot by u/benclifford"]
authorizationHeader bearerToken = header "Authorization" .~ ["bearer " <> (TE.encodeUtf8 bearerToken)]

authenticate :: SetMember Lift (Lift IO) r => Configuration -> Eff r BearerToken
-- authenticate :: _
authenticate configuration = do
  progress "Authenticating"

  let opts = defaults
           & userAgentHeader
           & param "grant_type" .~ ["password"]
           & param "username" .~ [username configuration]
           & param "password" .~ [password configuration]
           & auth ?~ basicAuth (app_id configuration) (app_secret configuration)

  resp <- lift $ postWith opts ("https://www.reddit.com/api/v1/access_token") ([] :: [Part])

  return $ resp ^. responseBody . key "access_token" . _String

hotPostsUrl = "https://oauth.reddit.com/r/LondonSocialClub/hot?limit=100"

getHotPosts :: SetMember Lift (Lift IO) r => T.Text -> Eff r (V.Vector Value)
getHotPosts bearerToken = do
  progress "Getting hot posts"

  let opts = defaults
           & authorizationHeader bearerToken
           & userAgentHeader
  resp <- lift $ getWith opts hotPostsUrl
  return $ resp ^. responseBody . key "data" . key "children" . _Array


-- Why doesn't this signature work?
-- readConfiguration :: SetMember Lift (Lift IO) r => Eff r Configuration
readConfiguration = do
  configYaml :: Value <- fromMaybe (error "Cannot parse config file")  <$> (lift $ decodeFile "secrets.yaml")
  return $ Configuration {
    username = configYaml ^. key "username" . _String,
    password = configYaml ^. key "password" . _String,
    app_id = configYaml ^. key "app_id" . _ByteString,
    app_secret = configYaml ^. key "app_secret" . _ByteString
  }

_ByteString = _String . Getter.to (T.unpack) . Getter.to (BSS8.pack)

-- This one gives a type-checking error too
-- (see readConfiguration error)
-- It seems to be something to do with lenses, and ParsecT, so
-- perhaps stuff was being inferred previously about Value or
-- something like that, that is now not being inferred because
-- extensible-effects are decoupling things more? If I figure that
-- out it is possibly interesting to write about?
-- This one is one of the weirdest errors...
-- processPost :: SetMember Lift (Lift IO) r => T.Text -> Value -> Eff r ()

-- This is what the hole-based inferer outputs:
{-
processPost :: (Data.OpenUnion.Internal.Base.MemberUImpl
                                  Data.OpenUnion.Internal.OpenUnion2.OU2
                                  Lift
                                  (Lift IO)
                                  r1,
                                Data.OpenUnion.Internal.OpenUnion1.Member'
                                  (Lift IO) r1
                                ~ 'True) =>
                               T.Text -> Value -> Free (Union r1) ()
-}
processPost :: SetMember Lift (Lift IO) r1 => T.Text -> Value -> Free (Union r1) ()
processPost bearerToken post = do
  let kind = post ^. postKind
  let i = post ^. postId
  let fullname = kind <> "_" <> i
  let flair_text = post ^. postFlairText
  let flair_css = post ^. postFlairCss
  let title = post ^. postTitle
  let stickied = fromMaybe False $ post ^? key "data" . key "stickied" . _Bool
  lift $ T.putStr $ fullname <> ": " <> title <> " [" <> flair_text <> "/" <> flair_css <> "]"
  when stickied $ lift $ T.putStr " [Stickied]"
  lift $ T.putStrLn ""

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

-- This signature is needed to allow processPost to have an
-- explicit type signature, in the same was that the reddit-specific
-- lenses also need them. It is unclear to me why that is the case.
-- TODO: replace with a Parsec signature...
datedSubjectLine :: P.ParsecT T.Text () Identity Day
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
  P.char '/'
  month <- dateComponent
  P.char '/'
  year <- yearComponent
  P.char ']'
  return $ fromGregorian year month day

dateComponent = read <$> digits

yearComponent = (normaliseYear . read) <$> digits

digits = (P.many $ P.oneOf "0123456789")

normaliseYear year =
  case () of
    _ | year > 2000 -> year
    _ | year >= 0 && year < 100 -> 2000 + year -- hello, 2100!

getCurrentLocalTime :: SetMember Lift (Lift IO) r => Eff r LocalTime
getCurrentLocalTime = do
  nowUTC <- lift $ getCurrentTime
  tz <- lift $ getCurrentTimeZone
  return $ utcToLocalTime tz nowUTC

-- TODO: i think this signature can probably be written
-- down as a lens signature.
postKind :: (T.Text -> Const T.Text T.Text)
                 -> Value -> Const T.Text Value
postKind = key "kind" . _String

postId :: (T.Text -> Const T.Text T.Text)
                 -> Value -> Const T.Text Value
postId = key "data" . key "id" . _String

postFlairText :: (T.Text -> Const T.Text T.Text)
                 -> Value -> Const T.Text Value
postFlairText = key "data" . key "link_flair_text" . _String

postFlairCss :: (T.Text -> Const T.Text T.Text)
                 -> Value -> Const T.Text Value
postFlairCss = key "data" . key "link_flair_css_class" . _String

postTitle :: (T.Text -> Const T.Text T.Text)
                 -> Value -> Const T.Text Value
postTitle = key "data" . key "title" . _String

forceFlair :: SetMember Lift (Lift IO) r => T.Text -> Value -> T.Text -> T.Text -> Eff r ()
forceFlair bearerToken post forced_flair forced_flair_css = do
  let kind = post ^. postKind
  let i = post ^. postId
  let fullname = kind <> "_" <> i
  lift $ T.putStrLn $ "    Setting flair for " <> fullname <> " to " <> forced_flair <> " if necessary"
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

            lift $ postWith opts "https://oauth.reddit.com/r/LondonSocialClub/api/flair" ([] :: [Part])
            -- TODO check if successful
            return ()

{-
Using :: _  we get this message
Main.hs:257:13:
    Found hole ‘_’ with type: String -> Eff r1 ()
    Where: ‘r1’ is a rigid type variable bound by
                the inferred type of
                progress :: (extensible-effects-1.11.0.3:Data.OpenUnion.Internal.Base.MemberUImpl
                               extensible-effects-1.11.0.3:Data.OpenUnion.Internal.OpenUnion2.OU2
                               Lift
                               (Lift IO)
                               r1,
                             extensible-effects-1.11.0.3:Data.OpenUnion.Internal.OpenUnion2.Member'
                               (Lift IO) r1
                             ~ 'True) =>
                            String -> Eff r1 ()
                at Main.hs:258:1
    To use the inferred type, enable PartialTypeSignatures
    In the type signature for ‘progress’: _
-}

progress :: SetMember Lift (Lift IO) r => String -> Eff r ()
progress s = lift $ hPutStrLn stderr s

-- | sleeps for specified number of minutes

sleep :: SetMember Lift (Lift IO) r => Int -> Eff r ()
sleep mins = lift $ threadDelay (mins * 60 * 1000000)

