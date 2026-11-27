# Revision history for effectful-streamly-http

## 0.1.0.2 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.
* The links of the documentation say which name they mean, where a type
  and its constructor, or two modules, share one.

## 0.1.0.1 -- 2026-11-21

* Built on streamly-http 0.3.

## 0.1.0.0 -- 2026-11-21

* `Effectful.HTTP`: the `Http` effect and `fetch`; `runHttpClear` over
  streamly-http, within limits of time and size; `runHttpWith`, from a
  function.
* `runHttpClear` keeps its connections, for each server, as long as it
  runs.
