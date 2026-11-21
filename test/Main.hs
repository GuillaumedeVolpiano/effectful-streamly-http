-- | The effect answered from a function, then over TCP against a server
-- on the loopback interface.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.HTTP
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

-- | Run a server that answers one connection: it reads the request head,
-- sends the answer, then holds the connection for a while or closes it.
-- Returns the request with the result.
withServer :: Bool -> [ByteString] -> (Int -> IO a) -> IO (ByteString, a)
withServer hold answer action =
  bracket open Net.close $ \listener -> do
    port <- Net.socketPort listener
    withAsync (serve listener) $ \server -> do
      result <- action (fromIntegral port)
      request <- if hold then pure "" else wait server
      pure (request, result)
  where
    open = do
      listener <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
      Net.setSocketOption listener Net.ReuseAddr 1
      Net.bind listener (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
      Net.listen listener 1
      pure listener
    serve listener =
      bracket (fst <$> Net.accept listener) Net.close $ \peer -> do
        request <- readHead peer ""
        mapM_ (NetBS.sendAll peer) answer
        if hold then threadDelay 10_000_000 else pure ()
        pure request
    readHead peer seen
      | "\r\n\r\n" `BS.isInfixOf` seen = pure seen
      | otherwise = do
          more <- NetBS.recv peer 4096
          if BS.null more then pure seen else readHead peer (seen <> more)

clear :: Limits -> Eff '[Http, Error HttpFailure, IOE] a -> IO (Either HttpFailure a)
clear limits = runEff . runErrorNoCallStack . runHttpClear limits

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "effectful-streamly-http"
      [ testCase "answered from a function, without any server" $ do
          let answer scheme request =
                pure Response {status = 200, reason = "OK", headers = [("x-scheme", if scheme == Https then "https" else "http")], body = request.method <> " " <> request.target}
          (first, second) <- runEff . runHttpWith answer $ do
            first <- fetch Http (get "example.org" 80 "/a")
            second <- fetch Https (withBody (BodyBytes "x") (get "example.org" 443 "/b") {method = "POST"})
            pure (first, second)
          (first.body, header "x-scheme" first) @?= ("GET /a", Just "http")
          (second.body, header "x-scheme" second) @?= ("POST /b", Just "https")
      , testCase "a request in clear goes over TCP, and its response comes back whole" $ do
          let content = BS.pack (take 100_000 (cycle [0 .. 255]))
              answer = ["HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\nTransfer-Encoding: chunked\r\n\r\n186a0\r\n", content, "\r\n0\r\n\r\n"]
          (request, outcome) <- withServer False answer $ \port ->
            clear defaultLimits (fetch Http (withHeaders [("Accept", "image/jpeg")] (get "127.0.0.1" port "/image?size=3")))
          BS.takeWhile (/= 13) request @?= "GET /image?size=3 HTTP/1.1"
          BS.isInfixOf "\r\nAccept: image/jpeg\r\n" request @?= True
          fmap (\response -> (response.status, header "content-type" response, response.body)) outcome @?= Right (200, Just "image/jpeg", content)
      , testCase "the requests of one interpreter to a server share a connection" $ do
          connections <- newIORef (0 :: Int)
          bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \listener -> do
            Net.bind listener (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
            Net.listen listener 5
            port <- fromIntegral <$> Net.socketPort listener
            let answerAll peer buffered = case BS.breakSubstring "\r\n\r\n" buffered of
                  (found, rest) | not (BS.null rest) -> do
                    let target = BS.takeWhile (/= 32) (BS.drop 4 found)
                    NetBS.sendAll peer ("HTTP/1.1 200 OK\r\nContent-Length: " <> BS.pack (map (fromIntegral . fromEnum) (show (BS.length target))) <> "\r\n\r\n" <> target)
                    answerAll peer (BS.drop 4 rest)
                  _ -> do
                    more <- NetBS.recv peer 4096
                    if BS.null more then pure () else answerAll peer (buffered <> more)
                serve = forever . bracket (fst <$> Net.accept listener) Net.close $ \peer ->
                  atomicModifyIORef' connections (\count -> (count + 1, ())) >> answerAll peer ""
            withAsync serve $ \_ -> do
              outcome <- clear defaultLimits (mapM (\target -> (.body) <$> fetch Http (get "127.0.0.1" port target)) ["/a", "/b", "/c"])
              outcome @?= Right ["/a", "/b", "/c"]
              readIORef connections >>= (@?= 1)
              -- Another interpreter has connections of its own.
              again <- clear defaultLimits ((.body) <$> fetch Http (get "127.0.0.1" port "/d"))
              again @?= Right "/d"
              readIORef connections >>= (@?= 2)
      , testCase "a status is not a failure" $ do
          (_, outcome) <- withServer False ["HTTP/1.1 404 Not Found\r\nContent-Length: 4\r\n\r\ngone"] $ \port ->
            clear defaultLimits (fetch Http (get "127.0.0.1" port "/"))
          fmap (\response -> (response.status, response.reason, response.body)) outcome @?= Right (404, "Not Found", "gone")
      , testCase "the limits of the interpreter apply" $ do
          (_, tooLarge) <- withServer False ["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n0123456789"] $ \port ->
            clear defaultLimits {maxBodySize = Just 9} (fetch Http (get "127.0.0.1" port "/"))
          fmap (.status) tooLarge @?= Left (RequestFailed (BodyTooLarge 9))
          (_, silent) <- withServer True [] $ \port ->
            clear defaultLimits {idleTimeout = Just 150_000} (fetch Http (get "127.0.0.1" port "/"))
          fmap (.status) silent @?= Left (RequestFailed ResponseTimedOut)
      , testCase "what cannot be written or read is reported" $ do
          invalid <- clear defaultLimits (fetch Http (get "127.0.0.1" 1 "/a b"))
          case invalid of
            Left (RequestFailed (InvalidRequest _)) -> pure ()
            other -> assertFailure ("expected InvalidRequest, got " <> show (fmap (.status) other))
          (_, notHttp) <- withServer False ["MOO/1 COMPLETE Success\n\n"] $ \port ->
            clear defaultLimits (fetch Http (get "127.0.0.1" port "/"))
          case notHttp of
            Left (RequestFailed (MalformedResponse _)) -> pure ()
            other -> assertFailure ("expected MalformedResponse, got " <> show (fmap (.status) other))
      , testCase "a refused connection is reported" $ do
          port <- bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \socket -> do
            Net.bind socket (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
            fromIntegral <$> Net.socketPort socket
          outcome <- clear defaultLimits (fetch Http (get "127.0.0.1" port "/"))
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show (fmap (.status) other))
      , testCase "TLS is not for the interpreter in clear" $ do
          outcome <- clear defaultLimits (fetch Https (get "example.org" 443 "/"))
          fmap (.status) outcome @?= Left TlsUnavailable
      ]
