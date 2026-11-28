# effectful-streamly-http

HTTP requests as an [Effectful](https://hackage.haskell.org/package/effectful)
effect: send a request, get the whole response. The interpreter of this
package runs over
[streamly-http](https://hackage.haskell.org/package/streamly-http), in
clear;
[effectful-streamly-http-tls](https://github.com/GuillaumedeVolpiano/effectful-streamly-http-tls)
adds `https`.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HTTP (HttpFailure, Response (..), Scheme (Http), defaultLimits,
                       fetch, get, runHttpClear)

main :: IO ()
main = do
  outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttpClear defaultLimits $
    fetch Http (get "example.org" 80 "/")
  print (fmap (.status) outcome)
```

## What it gives

- `Http`, the effect, with one operation: `fetch`, for a request in clear
  (`Http`) or over TLS (`Https`).
- `runHttpClear`: connections over plain TCP, within `Limits` of time and
  size, kept from one request to the next for each server as long as the
  interpreter runs. A request over TLS fails with
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

To build against a copy of
[streamly-http](https://hackage.haskell.org/package/streamly-http) checked
out next to this repository, pass `--project-file=cabal.project.siblings`.

Tested with GHC 9.8.4, 9.10.3 and 9.12.4, and effectful 2.7.1.
