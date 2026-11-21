# effectful-streamly-http

HTTP requests as an [Effectful](https://hackage.haskell.org/package/effectful)
effect: send a request, get the whole response. The interpreter of this
package runs over
[streamly-http](https://github.com/GuillaumedeVolpiano/streamly-http), in
clear;
[effectful-streamly-http-tls](https://github.com/GuillaumedeVolpiano/effectful-streamly-http-tls)
adds `https`.

```haskell
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HTTP

main :: IO ()
main = do
  outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttpClear defaultLimits $
    fetch Http (get "example.org" 80 "/")
  print (fmap (.status) outcome)
```

## What it gives

- `Http`, the effect, with one operation: `fetch`, for a request in clear
  (`Http`) or over TLS (`Https`).
- `runHttpClear`: connections over plain TCP, one for each request, within
  `Limits` of time and size. A request over TLS fails with
  `TlsUnavailable`.
- `runHttpWith`: the effect answered from a function. Code written against
  the effect is tested without any server.
- `HttpFailure`: why an exchange gave no response. A status is never a
  failure.

## Limits of this version

- The whole response is read into memory: set `maxBodySize` when the
  server is not trusted. There is no streaming of a body.
- No redirect is followed.

## Building

streamly-http is not on Hackage yet: `cabal.project` fetches it from
GitHub, at its release. To build against a copy checked out next to this
repository instead, pass `--project-file=cabal.project.siblings`.

Tested with GHC 9.12.4 and effectful 2.7.1.
