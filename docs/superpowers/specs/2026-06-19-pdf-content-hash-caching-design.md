# Content-hash PDF caching — design

**Date:** 2026-06-19
**Repo:** pdfServer2
**Status:** Approved (MVP scope)

## Problem

`POST /pdf` recompiles every document from scratch: download images, then run
xelatex (1–3 passes). Even after the rerun-detection optimization, a real document
takes ~3.6s on the prod box ("rose"). Re-requesting an **unchanged** document — e.g.
re-opening the same export — pays that full cost again. There is no caching today: a
warm repeat of an identical document still recompiles.

## Goal

When a document with identical inputs is re-requested, return the PDF immediately,
skipping both image download and all xelatex passes (~3.6s → ~0.5s, transmit only).

## Cache key

`SHA-256(content ++ urlList ++ packageList)`, hex-encoded.

A pure function of exactly the inputs that determine the rendered PDF. Consequences:

- Any change to the LaTeX body, a referenced image URL, or the package list produces a
  different key → cache miss → recompile. No stale output.
- Identical documents — even from different users — share one cache entry, because the
  output depends only on these inputs. Each requester still receives a PDF named for
  their own document (see Flow, step 2).

`content` and `urlList` come from the decoded `Document`. `packageList` is included if
the `Document` decoder exposes it; in practice the frontend sends `[]`. (Implementation
note: confirm the `Document` record's accessible fields; if `packageList` is not
exposed, hash `content ++ urlList` — functionally equivalent given it is empty.)

## Storage

A dedicated `pdfcache/` directory at the server root, holding `<hash>.pdf` files. Kept
separate from `inbox/`/`outbox/`, which are cleaned aggressively on every request.

## Flow (`/pdf` handler in `Main.hs`)

1. Decode `Document`; compute `hash`.
2. **Cache hit** (`pdfcache/<hash>.pdf` exists):
   - Copy it to `outbox/<docPdfName>` (the document's normal output name), so the
     existing `GET /pdf/:id` route serves it with no change.
   - Touch `pdfcache/<hash>.pdf` to refresh its mtime (keeps actively-used docs warm).
   - Return `{"pdf": <docPdfName>, "hasErrors": false}`.
   - Skip `prepareData` (no image download) and `Pdf.create` (no compile) entirely.
3. **Cache miss:** run `prepareData` + `Pdf.create` as today.
   - On `PdfSuccess`: copy the produced `outbox/<docPdfName>` to `pdfcache/<hash>.pdf`
     before responding, then respond as today.
   - `PdfWithErrors` and `PdfError`: respond as today; **not** cached.

Only the `POST /pdf` endpoint is cached. `POST /json` and `POST /tex` keep current
behavior for now (can adopt the same lookup later if wanted).

## Eviction

Extend the existing outbox cleanup to also prune `pdfcache/` by mtime:
`find pdfcache -type f -mtime +7 -delete`. Time-based only; no size cap (see Non-goals).

## Modules

A small, unit-testable `Cache` module so `Main.hs` stays thin:

- `contentHash :: Document -> String` — the hex SHA-256 of the keyed inputs.
- `cacheLookup :: String -> FilePath -> IO Bool` (hash, destination outbox path) —
  returns `True` and copies + touches on a hit, `False` on a miss.
- `cacheStore :: String -> FilePath -> IO ()` — copy a freshly-produced PDF into the
  cache under its hash.

Hashing uses a library already in the dependency tree (e.g. `crypton`/`Crypto.Hash`
or `cryptohash-sha1`; SHA-1 is acceptable for a cache key if SHA-256 is inconvenient).

## Error handling

- Cache directory missing or unreadable: treat as a miss and compile normally; create
  `pdfcache/` on startup or lazily before first store.
- Copy/touch failures on a hit: fall through to a normal compile rather than failing
  the request.

## Concurrency

Two identical in-flight requests both compile and both write the same bytes to
`pdfcache/<hash>.pdf` — harmless (last write wins, identical content). No locking in
the MVP.

## Testing

- `contentHash` is deterministic and changes when content / urlList / packageList
  change; stable when they do not.
- `cacheStore` then `cacheLookup` round-trips: lookup returns `True` and the PDF lands
  at the destination path.
- `cacheLookup` on an absent hash returns `False` and copies nothing.
- Integration: first `POST /pdf` compiles and populates the cache; an identical second
  `POST /pdf` returns the same PDF without invoking xelatex (assert via timing and/or a
  compile-count probe), and the bytes match.

## Non-goals (deferred — see memory `project_pdfcache_yagni`)

- Caching error results (`PdfWithErrors` / `PdfError`).
- Cross-request locking / single-flight de-duplication.
- Size-based (LRU / total-bytes) eviction cap.
- Cache-hit metrics / observability.
