-- |
-- Module      : Effectful.HTTP
-- Description : HTTP requests as an effect
-- License     : BSD-3-Clause
--
-- Sending a request and getting the whole response, as an effect. The
-- connections are opened by an interpreter: 'runHttpClear' here, for
-- @http@; the one of @effectful-streamly-http-tls@ for @https@ too; or
-- 'runHttpWith', which answers from a function, for tests.
--
-- > import Effectful
-- > import Effectful.Error.Static (runErrorNoCallStack)
-- > import Effectful.HTTP
-- >
-- > main :: IO ()
-- > main = do
-- >   outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttpClear defaultLimits $
-- >     fetch Http (get "example.org" 80 "/")
-- >   print (fmap (.status) outcome)
--
-- A status is never a failure: look at 'status'. The response is read
-- whole into memory: bound it with 'maxBodySize' when the server is not
-- trusted.
module Effectful.HTTP
  ( -- * Effect
    Http (..)
  , fetch

    -- * Requests and responses
  , Scheme (..)
  , Request (..)
  , RequestBody (..)
  , Header
  , get
  , withHeaders
  , withBody
  , Response (..)
  , header

    -- * Interpreters
  , runHttpClear
  , runHttpWith
  , Limits (..)
  , defaultLimits
  , noLimits

    -- * Failures
  , HttpFailure (..)
  , HttpError (..)
  ) where

import Control.Exception (IOException, displayException, try)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, send)
import Effectful.Error.Static (Error, throwError)
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP
  ( Header
  , HttpError (..)
  , Limits (..)
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , defaultLimits
  , fetchWithin
  , get
  , header
  , noLimits
  , withBody
  , withHeaders
  )
import Streamly.External.Network.HTTP.URL (Scheme (..))

-- | Exchanges with HTTP servers.
data Http :: Effect where
  -- | One request to the host and the port it names, in clear or over
  -- TLS, and its whole response.
  Fetch :: Scheme -> Request -> Http m (Response ByteString)

type instance DispatchOf Http = Dynamic

-- | Send a request to the host and the port it names, in clear ('Http')
-- or over TLS ('Https'), and get the whole response. No redirect is
-- followed.
fetch :: Http :> es => Scheme -> Request -> Eff es (Response ByteString)
fetch scheme request = send (Fetch scheme request)

-- | Why an exchange gave no response.
data HttpFailure
  = -- | The request could not be written, the response could not be read,
    -- or a limit of time or size was exceeded: which.
    RequestFailed HttpError
  | -- | The connection could not be opened, or broke. Carries the cause.
    ConnectFailed Text
  | -- | The TLS connection could not be set up, among others because the
    -- certificate of the server was not accepted. Carries the cause.
    TlsFailed Text
  | -- | A request over TLS given to an interpreter that only speaks in
    -- clear.
    TlsUnavailable
  deriving stock (Eq, Show)

-- | Interpret the effect over plain TCP connections, one for each
-- request, within limits of time and size. A request over TLS fails with
-- 'TlsUnavailable': see @effectful-streamly-http-tls@.
runHttpClear :: (IOE :> es, Error HttpFailure :> es) => Limits -> Eff (Http : es) a -> Eff es a
runHttpClear limits = interpret_ $ \case
  Fetch Https _ -> throwError TlsUnavailable
  Fetch Http request ->
    liftIO (try (try (fetchWithin limits request))) >>= \case
      Left (problem :: IOException) -> throwError (ConnectFailed (Text.pack (displayException problem)))
      Right (Left (problem :: HttpError)) -> throwError (RequestFailed problem)
      Right (Right response) -> pure (StreamlyBS.fromArray <$> response)

-- | Interpret the effect with a function from a request to its response:
-- for tests, and for other ways to reach a server.
runHttpWith :: (Scheme -> Request -> Eff es (Response ByteString)) -> Eff (Http : es) a -> Eff es a
runHttpWith exchange = interpret_ $ \case
  Fetch scheme request -> exchange scheme request
