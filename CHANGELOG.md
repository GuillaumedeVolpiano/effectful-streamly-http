# Revision history for effectful-streamly-http

## 0.1.0.1 -- 2026-11-21

* Built on streamly-http 0.3.

## 0.1.0.0 -- 2026-11-21

* `Effectful.HTTP`: the `Http` effect and `fetch`; `runHttpClear` over
  streamly-http, within limits of time and size; `runHttpWith`, from a
  function.
* `runHttpClear` keeps its connections, for each server, as long as it
  runs.
