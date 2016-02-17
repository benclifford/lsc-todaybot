{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE GADTs #-} -- needed for extensible-effects lift IO to work?

{- XLANGUAGE PartialTypeSignatures -} -- playing round with interposing

{-# LANGUAGE DeriveFunctor #-} -- for Sleep functor (and any other effect types we might derive)

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
import System.IO (hPutStr, hPutStrLn, hFlush, stderr, stdout)
import System.IO.Error (tryIOError)
import System.IO.Unsafe (unsafePerformIO)
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

-- | helper to run the supplied
-- action and print the return
-- value
p a = do
 v <- a
 print v


runStack a = runLift
           $ (runExc :: Eff (Exc IOError :> Lift IO :> Void) () -> Eff (Lift IO :> Void) (Either IOError ()))
           $ handleSleep
           $ handleWriter
           $ handleGetCurrentLocalTime
           $ (flip catchExc) (progress . (\e -> "TOP LEVEL EXCEPTION HANDLER: " ++ (show :: IOError -> String) e))
           $ a

main = p $ runStack $ do
  progress "todaybot"

  withConfiguration $ forever $ do
    logExceptions $ mainLoop
    sleep 1

{-
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
-}

-- | skipExceptions implements a deliberate "log, abandon this bit, and
-- carry on" policy. Problems this has dealt with in the past:
-- swedish letters in post titles, causing the logging system to
-- throw an exception if running in a $LANG that cannot output those;
-- network problems problems causing an entire run to be abandoned.
-- skipExceptions a = a `catch` \(e :: SomeException) -> progress $ "Exception: " <> (show e)
-- no implementation of exception handling in extensible effects branch...
-- I don't understand how IO exceptions work in this model, so leave
-- it 'till later?

{-
  -- catchExc (interposeIOExceptions act) handleException
  interposeIOExceptions act
    where 
      -- act gets run in some interspersing mode where 'lift' turns into
      -- an exception throwing version
      interposeIOExceptions = loop
      loop = freeMap
             (return)  -- when we get a value, but we can never return values
             (\effreq -> interpose effreq loop interposeLift) -- when we get an effect

      -- | handleException is what to do
      -- handleException (e :: IOError) = progress $ "Skipping because of exception: " <> (show e)

      -- | l is a Lift value
      -- interposeLift :: (SetMember Lift (Lift IO) r) => Lift (IO) (a -> Eff r v) -> Eff r v
      interposeLift (Lift ioa k) = do
        intermediate <- (lift ioa)
        lift $ putStrLn "IO EFFECT INTERSPERSED"
        k intermediate
-}
{-
handleWriter :: Eff (Writer String :> Lift IO :> r) a -> Eff (Lift IO :> r) a
handleWriter = loop
  where
    loop = freeMap
           (return)
           (\u -> handleRelay u loop write)
    write (Writer w v) = do
      lift $ hPutStr stdout w
      lift $ hFlush stdout
      loop v
-}


{-

-- | The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
runReader m !e = loop m where
  loop = freeMap
         return
         (\u -> handleRelay u loop (\(Reader k) -> loop (k e)))

-}

{- BLOG POST about this paragraph? for writing a "nop" interpose.
The problem with this implementation is that the 'interpose'
switches between two paths depending on the type of effect
message; but there is not enough here to infer that.
So it needs to be pinned somewhere.
skipExceptions :: SetMember Lift (Lift IO) r => Eff r a -> Eff r a
skipExceptions = loop
  where
    loop = freeMap
           (return)
           (\u -> interpose u loop nop)
    nop u = (send $ inj u) >>= loop
-}

{- blog post 2 -}
-- this implementation indicates when IO is called within
-- the parameter block, but it does not intercept any IO
-- that comes from the log Writer.
-- but exceptions arising there are very much what I want
-- to catch: exceptions from HTTP errors when they are
-- going to be in their own effect.
-- if I put an exception transformer lower in the stack can
-- I make exceptions more visible? with the interceptor sitting
-- right after runLift.

-- also interesting in this implementation is that I can
-- do send $ inj $ Lift but I can't use "lift" directly!
{-
skipExceptions :: SetMember Lift (Lift IO) r => Eff r a -> Eff r a
skipExceptions = loop
  where
    loop = freeMap
           (return)
           (\u -> interpose u loop nop)
    nop (u :: Lift IO _) = do
      v <- send $ inj u
      send $ inj $ Lift (putStrLn "INTERCEPT IO in skipExceptions") id
      loop v

-}

{-
-- BLOG 3: this took ages to figure out all the type
-- anotations for. ugh.
-- but it runs and annotates the outputs, and if put
-- right after runLift, annotates Writer based logging
-- too.
skipExceptions :: SetMember Lift (Lift IO) r => Eff r a -> Eff r a
skipExceptions act = do
  let 
    loop :: SetMember Lift (Lift IO) r => Eff r q -> Eff r q
    loop = freeMap
           (return)
           (\u -> interpose u loop nop)
    nop :: SetMember Lift (Lift IO) r => (Lift IO (Eff r q)) -> Eff r q
    nop u = do
      lift $ putStrLn "Pre-IO"
      v <- send $ inj u
      lift $ putStrLn "Post-IO"
      loop v
  loop act
-}

{- BLOG 4
This one catches the IO, replaces the IO with something else
(that wraps both the IO and the continuation of what we're
going to do afterwards). The continuation of what we're
going to do afterwards, we actually discard. So when we run,
we get up to first IO, and get pre eff, pre wrap, IO, post wrap,
post eff, and then we get the error aborting the computation
(that comes from completely replacing the continuation with this error)

skipExceptions :: SetMember Lift (Lift IO) r => Eff r a -> Eff r a
skipExceptions act = do
  loop act  where 

    loop :: SetMember Lift (Lift IO) r => Eff r q -> Eff r q
    loop = freeMap
           (return)
           (\u -> interpose u loop handleEff)
    handleEff :: SetMember Lift (Lift IO) r => (Lift IO (Eff r q)) -> Eff r q
    handleEff u@(Lift ioact k) = do -- what should I do with that continuation k?
                              -- it will be used as part of the send when
                              -- talking about u, so I suppose I'll just
                              -- preserve it but no idea if that is right...
      lift $ putStrLn "Pre-IO eff"

      let ioact' = transformIOAction ioact
      let k' = transformK k -- can modify k here... k :: a -> Eff r v. So if we're modifying the output type of ioact' I suppose we should modify the input type of k?
      let u' = Lift ioact' k' -- we can fiddle with the IO action (and do) but we can also fiddle with (k :: a -> Eff r v) too
      rest <- send $ inj u' -- rest is an eff, not the raw result of the IO action; rest represents, I think, the rest of the program to run, with the return value of the IO action appropriately inserted.
      lift $ putStrLn "Post-IO eff"
      loop rest

    transformIOAction act = do
      putStrLn "Pre-IO wrap"
      v <- {- tryIOError -} act
      putStrLn "Post-IO wrap"
      return v

    transformK :: (a -> Eff r v) -> a -> Eff r v
    -- transformK k v = k v
    transformK k v = fail "DISCARD AFTER ANY IO"  -- so discard the rest of the computation
-}

{- BLOG 5: demonstrating using return () to do delimited cutting out
   continuation. note that we have to make all the return types () here
(or rather, we need them to be of a type that we can pick some value
for in the case of failure...)
skipExceptions :: SetMember Lift (Lift IO) r => Eff r () -> Eff r ()
skipExceptions act = do
  loop act  where 

    loop :: SetMember Lift (Lift IO) r => Eff r () -> Eff r ()
    loop = freeMap
           (return)
           (\u -> interpose u loop handleEff)
    handleEff :: SetMember Lift (Lift IO) r => (Lift IO (Eff r ())) -> Eff r ()
    handleEff u@(Lift ioact k) = do -- what should I do with that continuation k?
                              -- it will be used as part of the send when
                              -- talking about u, so I suppose I'll just
                              -- preserve it but no idea if that is right...
      lift $ putStrLn "Pre-IO eff"

      let ioact' = transformIOAction ioact
      let k' = transformK k -- can modify k here... k :: a -> Eff r v. So if we're modifying the output type of ioact' I suppose we should modify the input type of k?
      let u' = Lift ioact' k' -- we can fiddle with the IO action (and do) but we can also fiddle with (k :: a -> Eff r v) too
      rest <- send $ inj u' -- rest is an eff, not the raw result of the IO action; rest represents, I think, the rest of the program to run, with the return value of the IO action appropriately inserted.
      lift $ putStrLn "Post-IO eff"
      loop rest

    transformIOAction act = do
      putStrLn "Pre-IO wrap"
      v <- {- tryIOError -} act
      putStrLn "Post-IO wrap"
      return v

    transformK :: (a -> Eff r ()) -> a -> Eff r ()
    -- transformK k v = k v
    transformK k v = return ()  -- discard rest of computation (delimited by skipExceptions) - because we cause loop to return a pure value and stop iterating.
-}

{- BLOG 6:
now we can catch exceptions, and abort the subprogram wrapped by
skipExceptions if we get one, firing out a putStrLn with the
exception. (but not a Writer, so not using logging system...)

we're still missing the ability to catch IO exceptions fired
in the log writer (or any Effect -> IO converter), I think.

how can I throw exceptions from a lower layer to be caught
by a higher layer? I guess I need to pay attention to the
execution model a bit more? maybe time to draw on paper

skipExceptions :: SetMember Lift (Lift IO) r => Eff r () -> Eff r ()
skipExceptions act = do
  loop act  where 

    loop :: SetMember Lift (Lift IO) r => Eff r () -> Eff r ()
    loop = freeMap
           (return)
           (\u -> interpose u loop handleEff)
    handleEff :: SetMember Lift (Lift IO) r => (Lift IO (Eff r ())) -> Eff r ()
    handleEff u@(Lift ioact k) = do -- what should I do with that continuation k?
                              -- it will be used as part of the send when
                              -- talking about u, so I suppose I'll just
                              -- preserve it but no idea if that is right...
      lift $ putStrLn "Pre-IO eff"

      let ioact' = transformIOAction ioact
      let k' = transformK k -- can modify k here... k :: a -> Eff r v. So if we're modifying the output type of ioact' I suppose we should modify the input type of k?
      let u' = Lift ioact' k' -- we can fiddle with the IO action (and do) but we can also fiddle with (k :: a -> Eff r v) too
      rest <- send $ inj u' -- rest is an eff, not the raw result of the IO action; rest represents, I think, the rest of the program to run, with the return value of the IO action appropriately inserted.
      lift $ putStrLn "Post-IO eff"
      loop rest

    transformIOAction act = do
      putStrLn "Pre-IO wrap"
      v <- tryIOError act
      putStrLn "Post-IO wrap"
      return v

    transformK :: (SetMember Lift (Lift IO) r) => (a -> Eff r ()) -> Either IOError a -> Eff r ()
    transformK k ev = case ev of
      Right v -> k v -- IO returned a value, so run the rest of the subprogram, passing in that value
      Left ex -> do
        lift $ putStrLn $ "transformK handling an exception: " ++ show ex
        return () -- discard the rest of the subprogram
-}

convertIOExceptions :: (Member (Exc IOError) r, SetMember Lift (Lift IO) r) => Eff r () -> Eff r ()
convertIOExceptions = freeMap
           (return)
           (\u -> interpose u convertIOExceptions handleEff)

  where
    handleEff :: (Member (Exc IOError) r, SetMember Lift (Lift IO) r) => (Lift IO (Eff r ())) -> Eff r ()
    handleEff (Lift ioact k) = do
      rest <- send . inj $ Lift (transIO ioact) (transK k)
      convertIOExceptions rest

    transIO act = do
      -- Can do pre ...
      v <- tryIOError act 
      -- ... and post activity using IO actions here.
      return v

    transK :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => (a -> Eff r ()) -> Either IOError a -> Eff r ()
    transK k ev = case ev of
      Right v -> k v
      Left ex -> throwExc ex

-- this doesn't end up catching *any* exceptions! skipExceptions, which
-- translates IO errors into Exc exceptions, only passes exceptions to
-- catches which are wrapping around 'skipExceptions' -- it doesn't
-- throw exceptions to anything higher in the stack that wants to
-- catch them, which is what I want to happen... I don't know if that
-- is even possible with this interpose approach? Maybe I need to
-- go back to the lift' approach? But that also won't get exceptions
-- from eg the logger, which are generated low in the stack.
-- so the logging and other actions have to generate the exception
-- effect themselves, rather than it being generated way down in
-- the stack by an effect interpreter?
logExceptions act = do
  (convertIOExceptions act) `catchExc` (\(e :: IOError) -> progress $ "Caught exception and skipping: " ++ show e)

-- mainLoop :: (Member (Reader Configuration) r, Member (Writer String) r, SetMember Lift (Lift IO) r) => Eff r ()
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
    mapM_ (logExceptions . processPost) posts -- this could be a traversible rather than a monad?
  logExceptions $ lift $ fail "BENC DELIBERATE FAIL, BUT PASS COMPLETED."
  progress "Pass completed."

{-
lift' :: (SetMember Lift (Lift IO) r) => IO a -> Eff r a
lift' a = do
  r <- lift $ tryIOError a
  case r of
    (Right v) -> return v
    (Left ex) -> throwExc ex
-}

userAgentHeader = header "User-Agent" .~ ["lsc-todaybot by u/benclifford"]
authorizationHeader bearerToken = header "Authorization" .~ ["bearer " <> (TE.encodeUtf8 bearerToken)]

-- can't put Reader BearerToken as :> on the parameter, because the caller
-- of this, mainLoop, does not have a concrete type representation of the
-- effect stack?
-- what am I trying to express here? that the supplied action has 
-- effects of bearer token reader, config reader, logger, and
-- the IO effects.
withAuthentication :: (Member (Reader Configuration) e, Member (Writer String) e, SetMember Lift (Lift IO) e
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

  resp <- lift $ postWith opts ("https://www.reddit.com/api/v1/access_token") ([] :: [Part])

  let bearerToken = resp ^. responseBody . key "access_token" . _String
  runReader act bearerToken

getBearerToken :: (Member (Reader BearerToken) r) => Eff r BearerToken
getBearerToken = ask

hotPostsUrl = "https://oauth.reddit.com/r/LondonSocialClub/hot?limit=100"

getHotPosts :: (Member (Reader BearerToken) r, Member (Writer String) r, SetMember Lift (Lift IO) r) => Eff r (V.Vector Value)
getHotPosts = do
  progress "Getting hot posts"
  bearerToken <- getBearerToken

  let opts = defaults
           & authorizationHeader bearerToken
           & userAgentHeader
  resp <- lift $ getWith opts hotPostsUrl
  return $ resp ^. responseBody . key "data" . key "children" . _Array



_ByteString :: Getting BSS8.ByteString Value BSS8.ByteString
_ByteString = _String . Getter.to (T.unpack) . Getter.to (BSS8.pack)

processPost :: (Member (Reader LocalTime) r1, Member (Reader BearerToken) r1, Member (Writer String) r1, SetMember Lift (Lift IO) r1) => Value -> Free (Union r1) ()
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

-- current time effect
-- we can use Reader (Localtime, TimeZone) rather than defining a new
-- effect datatype.

getCurrentLocalTime :: (Member (Reader LocalTime) r) => Eff r LocalTime
getCurrentLocalTime = ask

{- move this stuff down into the handler for the Reader LocalTime effect -}
{- note that there can only be one localtime reader here. we might plausibly
   have another -- for example, if we were in a post context, there might
   be the post created local time... so in that case we'd have to do
   something more fancy -}
{- contrast this impl with handleWriter - similar implementation. can't
   use one of the standard reader effects - because want to implement
   this in terms of other effects -}
{- specifically we want to run the code to get the value *every time*
   the effect is invoked/called/(?term)
-}

handleGetCurrentLocalTime :: (SetMember Lift (Lift IO) r) => Eff (Reader LocalTime :> r) a -> Eff r a
handleGetCurrentLocalTime = loop
  where
    loop :: (SetMember Lift (Lift IO) r) => Eff (Reader LocalTime :> r) a -> Eff r a
    loop = freeMap
           (return)
           (\u -> handleRelay u loop readTime)

    readTime :: (SetMember Lift (Lift IO) r) => Reader LocalTime (Eff (Reader LocalTime :> r) a) -> Eff r a
    readTime (Reader k) = do
      v <- lift $ do
        nowUTC <- getCurrentTime
        tz <- getCurrentTimeZone
        return $ utcToLocalTime tz nowUTC
      loop (k v)

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

forceFlair :: (Member (Reader BearerToken) r, Member (Writer String) r, SetMember Lift (Lift IO) r) => Value -> T.Text -> T.Text -> Eff r ()
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

            lift $ postWith opts "https://oauth.reddit.com/r/LondonSocialClub/api/flair" ([] :: [Part])
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
-- IO and Exc IOError and uses lift, this should be ok, but unsure?

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
progressP s = tell s

handleWriter :: (Member (Exc IOError) r, SetMember Lift (Lift IO) r) => Eff (Writer String :> r) a -> Eff r a
handleWriter =
           freeMap
           (return)
           (\u -> handleRelay u handleWriter write)
  where
    write :: (Member (Exc IOError) r, SetMember Lift (Lift IO) r) => (Writer String (Eff (Writer String :> r) a)) -> Eff r a
    write (Writer w v) = do
      lift $ hPutStr stdout w
      lift $ hFlush stdout
      handleWriter v

-- | sleeps for specified number of minutes

data Sleep k = Sleep Int k
  deriving Functor

sleep :: (Member Sleep r) => Int -> Eff r ()
sleep time = send $ inj $ Sleep time ()

handleSleep :: SetMember Lift (Lift IO) r => Eff (Sleep :> r) a -> Eff r a
handleSleep = freeMap
              (return)
              (\u -> handleRelay u handleSleep sleep)
  where
    sleep :: (SetMember Lift (Lift IO) r) => (Sleep (Eff (Sleep :> r) a)) -> Eff r a
    sleep (Sleep mins k) = do
      -- should probably handle exceptions from threadDelay somehow...
      lift $ do hPutStrLn stdout "Sleeping"
                hFlush stdout
                threadDelay (mins * 60 * 1000000)
      handleSleep k

-- | unlike handleWriter, this uses a pre-defined
-- handler (which probably uses interpose?)
withConfiguration :: SetMember Lift (Lift IO) r
  => Eff (Reader Configuration :> r) v -> Eff r v
withConfiguration act = do
  c <- readConfiguration
  runReader act c

getConfiguration :: (Member (Reader Configuration) r) => Eff r Configuration
getConfiguration = ask

readConfiguration :: (SetMember Lift (Lift IO) r) => Eff r Configuration
readConfiguration = do
  configYaml :: Value <- fromMaybe (error "Cannot parse config file")  <$> (lift $ decodeFile "secrets.yaml")
  return $ Configuration {
    username = configYaml ^. key "username" . _String,
    password = configYaml ^. key "password" . _String,
    app_id = configYaml ^. key "app_id" . _ByteString,
    app_secret = configYaml ^. key "app_secret" . _ByteString
  }


-- blog: lots of "the impossible happened" ghc bugs with 7.10.3 related to skolem/escaping
-- biggest takeaway -- type signatures. type sigs more required, unfamiliar form, type errors are nuts

-- goal was to go deliberately all-out on effects.
