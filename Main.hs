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
import Control.Eff.Exception (catchExc, runExc, throwExc, Exc (..) )
import Control.Eff.Lift (lift, runLift, Lift (..) )
import Control.Eff.Reader.Lazy (ask, runReader, Reader (..))
import Control.Eff.Writer.Strict (runMonoidWriter, tell, Writer (..) )
import Control.Exception (catch, SomeException (..) )
import Control.Lens
import Control.Monad hiding (mapM_)
import Data.Maybe (fromMaybe)
import Data.String.ToString 
import Data.Typeable (Typeable ())
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Foldable (mapM_)
import Data.Aeson ( Value(..) )
import Data.Aeson.Lens (key, _Bool, _String, _Array)
import Data.Void
import Data.Yaml (decodeFile)
import System.IO (hPutStr, hFlush, stderr, stdout)
import System.IO.Error (tryIOError)
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

-- this runExc will silently die if we get an
-- IO exception that has been handed up here...
-- should probably catch and log it. TODO
main = void $ runLift $ skipExceptionsTop $ handleWriter $ do
  progress "todaybot"

  withConfiguration $ forever $ do
    skipExceptions $ mainLoop
    sleep 1

-- BUG
-- IOError is too tight a bound here - I want to get
-- *all* exceptions. An example in practice is
-- StatusCodeException not being caught here, a
-- constructor of type HttpException.
skipExceptionsTop :: Eff (Exc IOError :> (Lift IO :> Void)) a0
                    -> Eff (Lift IO :> Void) ()
skipExceptionsTop act = do
  r <- runExc act
  case r of Right v -> return ()
            Left e -> lift $ putStrLn $ "Skipping at top level because of exception: " <> (show e)
                                        -- can't use "progress" here
                                        -- because it wants to be able
                                        -- to throw exceptions

-- | skipExceptions implements a deliberate "log, abandon this bit, and
-- carry on" policy. Problems this has dealt with in the past:
-- swedish letters in post titles, causing the logging system to
-- throw an exception if running in a $LANG that cannot output those;
-- network problems problems causing an entire run to be abandoned.
-- skipExceptions a = a `catch` \(e :: SomeException) -> progress $ "Exception: " <> (show e)
-- no implementation of exception handling in extensible effects branch...
-- I don't understand how IO exceptions work in this model, so leave
-- it 'till later?

skipExceptions :: (Member (Writer String) r, SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r ()
                    -> Eff r ()
skipExceptions act =
  catchExc act $ \(e :: IOError) -> progress $ "Skipping because of exception: " <> (show e)

-- mainLoop :: (Member (Reader Configuration) r, Member (Writer String) r, SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r ()
-- no signature here, so that a more concrete
-- type signature is inferred. This seems to be
-- necessary to make withAuthentication type check
-- because withAuthentication requires (Reader BearerToken)
-- to be at the top of the stack.
-- but what about if we use a different but similarly behaved
-- function to wrap some other effect, also in mainLoop?
-- withAuthentication will requirer (Reader BearerToken) at
-- the top of the stack and the other one will require the
-- other effect at the top of the stack.
-- (try this out in practice to see if it really is a problem).
-- Maybe there is a different way to express withAuthentication,
-- eg as an effect itself?
-- or maybe withAuthentication an outer handler, with two
-- effects: one to get the current bearer token, and one to force
-- a refresh. (although potentially we have refresh tokens and
-- so can handle that "transparently"). There wouldn't be a
-- "withAuthentication" wrapper to label stuff as using/not using
-- authentication -- that would be labelled as a set constraint.
-- perhaps that is more idiomatic?
-- more generally, there's a comment about running stuff that
-- removes effects inside the effectful computation, rather than
-- in the interpreter stack? (i.e. don't do it...)
-- so we lose some (type safety? type labelling?) of effects.
-- what does this look like for using the type system to label
-- where exceptions can occur? that's the big place I can
-- imagine wanting to do this kind of type safety?

mainLoop = do
  withAuthentication $ do
    posts <- getHotPosts
    mapM_ (skipExceptions . processPost) posts -- this could be a traversible rather than a monad?
  progress "Pass completed."

lift' :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => IO a -> Eff r a
lift' a = do
  r <- lift $ tryIOError a
  case r of
    (Right v) -> return v
    (Left ex) -> throwExc ex

userAgentHeader = header "User-Agent" .~ ["lsc-todaybot by u/benclifford"]
authorizationHeader bearerToken = header "Authorization" .~ ["bearer " <> (TE.encodeUtf8 bearerToken)]

-- can't put Reader BearerToken as :> on the parameter, because the caller
-- of this, mainLoop, does not have a concrete type representation of the
-- effect stack?
-- what am I trying to express here? that the supplied action has 
-- effects of bearer token reader, config reader, logger, and
-- the IO effects.
withAuthentication :: (Member (Reader Configuration) e, Member (Writer String) e, SetMember Lift (Lift IO) e, Member (Exc IOError) e
                      ) => Eff ((Reader BearerToken) :> e) v -> Eff e v
withAuthentication act = do
  progress "Authenticating"
  configuration <- getConfiguration

  let opts = defaults
           & userAgentHeader
           & param "grant_type" .~ ["password"]
           & param "username" .~ [username configuration]
           & param "password" .~ [password configuration]
           & auth ?~ basicAuth (app_id configuration) (app_secret configuration)

  resp <- lift' $ postWith opts ("https://www.reddit.com/api/v1/access_token") ([] :: [Part])

  let bearerToken = resp ^. responseBody . key "access_token" . _String
  runReader act bearerToken

getBearerToken :: (Member (Reader BearerToken) r) => Eff r BearerToken
getBearerToken = ask

hotPostsUrl = "https://oauth.reddit.com/r/LondonSocialClub/hot?limit=100"

getHotPosts :: (Member (Reader BearerToken) r, Member (Writer String) r, SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r (V.Vector Value)
getHotPosts = do
  progress "Getting hot posts"
  bearerToken <- getBearerToken

  let opts = defaults
           & authorizationHeader bearerToken
           & userAgentHeader
  resp <- lift' $ getWith opts hotPostsUrl
  return $ resp ^. responseBody . key "data" . key "children" . _Array



_ByteString :: Getting BSS8.ByteString Value BSS8.ByteString
_ByteString = _String . Getter.to (T.unpack) . Getter.to (BSS8.pack)

processPost :: (Member (Reader BearerToken) r1, Member (Writer String) r1, SetMember Lift (Lift IO) r1, Member (Exc IOError) r1) => Value -> Free (Union r1) ()
processPost post = do
  let fullname = post ^. postFullname
  let flair_text = post ^. postFlairText
  let flair_css = post ^. postFlairCss
  let title = post ^. postTitle
  let stickied = fromMaybe False $ post ^? key "data" . key "stickied" . _Bool
  progressP' $ fullname <> ": " <> title <> " [" <> flair_text <> "/" <> flair_css <> "]"
  when stickied $ progressP " [Stickied]"
  progress ""

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
           | postDate == now -> forceFlair post "Today" "today"
           | postDate < now && not stickied -> forceFlair post "Archived" "archived"
           | postDate < now && stickied -> forceFlair post "" ""

      Left e -> progress $ "    Skipping: Date did not parse: " <> (show e)

    let interestCheck = (T.toCaseFold "[Interest") `T.isPrefixOf` (T.toCaseFold title)
    progress $ "    Interest check? " <> (show interestCheck)

    when interestCheck $ forceFlair post "Interest Check" "interestcheck"

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

getCurrentLocalTime :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r LocalTime
getCurrentLocalTime = do
  nowUTC <- lift' $ getCurrentTime
  tz <- lift' $ getCurrentTimeZone
  return $ utcToLocalTime tz nowUTC

postFlairText :: Getting T.Text Value T.Text
postFlairText = key "data" . key "link_flair_text" . _String

postFlairCss :: Getting T.Text Value T.Text
postFlairCss = key "data" . key "link_flair_css_class" . _String

postTitle :: Getting T.Text Value T.Text
postTitle = key "data" . key "title" . _String

-- | unclear to me if there is a nicer way to
-- make a lens that uses multiple lenses...
-- Vaguely feels to me that there should be
-- something applicative here?
postFullname :: Getting T.Text Value T.Text
postFullname f post = let
  kind = post ^. key "kind" . _String
  i = post ^. key "data" . key "id" . _String
  fullname = kind <> "_" <> i
  const_ra = f fullname
  const_r = getConst const_ra
  in Const const_r

forceFlair :: (Member (Reader BearerToken) r, Member (Writer String) r, SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Value -> T.Text -> T.Text -> Eff r ()
forceFlair post forced_flair forced_flair_css = do
  let fullname = post ^. postFullname
  progress' $ "    Setting flair for " <> fullname <> " to " <> forced_flair <> " if necessary"
  let flair_text = post ^. postFlairText
  let flair_css = post ^. postFlairCss
  if flair_text == forced_flair && flair_css == forced_flair_css
    then progress "    No flair change necessary"
    else do progress "    Updating flair"
            bearerToken <- getBearerToken
            let opts = defaults
                     & authorizationHeader bearerToken
                     & param "api_type" .~ ["json"]
                     & param "link" .~ [fullname]
                     & param "text" .~ [forced_flair]
                     & param "css_class" .~ [forced_flair_css]

            lift' $ postWith opts "https://oauth.reddit.com/r/LondonSocialClub/api/flair" ([] :: [Part])
            -- TODO check if successful
            return ()

-- we've moved logging to a Writer effect. Previously, this hPutStrLn
-- has thrown exceptions, which we were catching, printing and ignoring
-- (eg for unicode conversion problems)
-- How does that work with a writer effect handler?
-- Needs some thinking/playing to see if (or not) it keeps those
-- semantics or if the error thrown at print (by the effect handler)
-- isn't caught by skipExceptions correctly. I think if the
-- handler for writers sits further out in the effect stack than
-- IO and Exc IOError and uses lift', this should be ok, but unsure?

-- c.f. Trace effect. but maybe i want log levels etc, which is why
-- i'm keeping it a bit separate

-- ToString is annoying here, because string literals don't get
-- automatically left alone: there's an ambiguity about which
-- intermediate form will be used (and I don't think defaulting
-- can help?). ugh type classes. Putting a signature on every
-- literal is ugly.
progress :: (Member (Writer String) r) => String -> Eff r ()
progress s = progressP $ s ++  "\n"

progress' :: (Member (Writer String) r, ToString s) => s -> Eff r ()
progress' s = progress (toString s)

progressP' :: (Member (Writer String) r, ToString s) => s -> Eff r ()
progressP' s = progressP (toString s)

progressP :: (Member (Writer String) r) => String -> Eff r ()
progressP s = tell (toString s)

handleWriter :: Eff (Writer String :> Exc IOError :> Lift IO :> r) a -> Eff (Exc IOError :> Lift IO :> r) a
handleWriter = loop
  where
    loop = freeMap
           (return)
           (\u -> handleRelay u loop write)
    write (Writer w v) = do
      lift' $ hPutStr stdout w
      lift' $ hFlush stdout
      loop v

-- | sleeps for specified number of minutes

sleep :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Int -> Eff r ()
sleep mins = lift' $ threadDelay (mins * 60 * 1000000)

withConfiguration act = do
  c <- readConfiguration
  runReader act c

getConfiguration :: (Member (Reader Configuration) r) => Eff r Configuration
getConfiguration = ask

readConfiguration :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r Configuration
readConfiguration = do
  configYaml :: Value <- fromMaybe (error "Cannot parse config file")  <$> (lift' $ decodeFile "secrets.yaml")
  return $ Configuration {
    username = configYaml ^. key "username" . _String,
    password = configYaml ^. key "password" . _String,
    app_id = configYaml ^. key "app_id" . _ByteString,
    app_secret = configYaml ^. key "app_secret" . _ByteString
  }
