<p align="center">
  <img src="assets/logo.png" alt="nostr.el logo" width="220">
</p>

# nostr.el

A public-social Nostr client for Emacs.

Emacs owns relay networking, SQLite storage, timeline/thread/profile buffers,
composition, reactions, reposts, relay management, notifications, search, and
opt-in inline media previews. A small Rust CLI shim handles signing,
verification, key generation, and NIP-19 helpers through `nostr-sdk`.

## Status

This repository is usable for local testing, but not packaged for general
distribution yet. The Emacs package does not currently download or install the
Rust backend automatically.

## Requirements

- Emacs 30.1 or newer
- `transient`
- `emacsql`
- `websocket`
- `plz`
- the `nostr-el-backend` Rust shim on `exec-path` or configured by
  `nostr-backend-command`

On GNU Guix, use the project manifest:

```sh
guix shell -m manifest.scm
```

## Build The Backend

From the repository root:

```sh
guix shell -m manifest.scm
```

Then build:

```sh
env CC=gcc cargo build --release
```

The binary will be:

```text
target/release/nostr-el-backend
```

Either put that binary on `PATH`, or point Emacs at it with
`nostr-backend-command`.

## use-package

```elisp
(use-package nostr
  :vc (:url "https://github.com/trevarj/nostr.el"
       :branch "master")
  :commands (nostr-open
             nostr-close
             nostr-create-note
             nostr-check-backend
             nostr-account-status
             nostr-generate-private-key
             nostr-setup-import-private-key)
  :custom
  (nostr-backend-command "nostr-el-backend")
  (nostr-private-key-path
   (expand-file-name "nostr-private.gpg" user-emacs-directory))
  (nostr-db-path
   (expand-file-name "nostr-db.sqlite" user-emacs-directory))
  (nostr-relay-urls
   '("wss://relay.primal.net" "wss://relay.damus.io")))
```

Set `nostr-backend-command` to an absolute path if `nostr-el-backend` is not on
`exec-path`.

## First Run

1. Build or install `nostr-el-backend`.
2. Evaluate your `use-package` form.
3. Run `M-x nostr-check-backend`.
4. Create a test account with `M-x nostr-generate-private-key`, or import an
   existing secret with `M-x nostr-setup-import-private-key`.
5. Run `M-x nostr-open`.

Most buffers expose their actions through `?`, which opens the buffer-local
transient menu.

Inline media is opt-in. Use `M-x nostr-media-load-at-point` on a media
placeholder, or set `nostr-media-auto-preview` after reviewing the size and
content-type limits.

## Backend Distribution

The package should not silently fetch and execute a binary. The preferred
distribution path is:

- publish `nostr-el-backend` binaries in GitHub Releases;
- verify checksums before installation;
- provide an explicit interactive command that asks before downloading;
- keep source builds documented for Guix and other reproducible setups.

Until that exists, users must build the backend locally or install a trusted
release binary themselves and set `nostr-backend-command`.

## Verification

Run the offline suite:

```sh
guix shell -m manifest.scm
```

Then run:

```sh
env CC=gcc cargo test
env CC=gcc cargo fmt --check
emacs --batch -Q -L . -L tests \
  -l bech32-test.el \
  -l nostr-test.el \
  -l tests/nostr-ui-card-test.el \
  -l tests/nostr-ui-buffers-test.el \
  -l tests/nostr-operational-buffers-test.el \
  -l tests/nostr-media-nip-test.el \
  -l tests/nostr-setup-test.el \
  -l tests/nostr-relay-list-test.el \
  -l tests/nostr-relay-lifecycle-test.el \
  -l tests/nostr-live-relay-smoke-test.el \
  -l tests/nostr-share-test.el \
  -f ert-run-tests-batch-and-exit
```

Run the opt-in live relay receive smoke:

```sh
guix shell -m manifest.scm
```

Then run:

```sh
NOSTR_LIVE_RELAY_TEST=1 emacs --batch -Q -L . -L tests \
  -l tests/nostr-live-relay-smoke-test.el \
  --eval '(setq nostr-backend-command (expand-file-name "target/release/nostr-el-backend" default-directory))' \
  -f ert-run-tests-batch-and-exit
```

Run the opt-in live relay publish smoke:

```sh
guix shell -m manifest.scm
```

Then run:

```sh
NOSTR_LIVE_RELAY_PUBLISH_TEST=1 emacs --batch -Q -L . -L tests \
  -l tests/nostr-live-relay-smoke-test.el \
  --eval '(setq nostr-backend-command (expand-file-name "target/release/nostr-el-backend" default-directory))' \
  -f ert-run-tests-batch-and-exit
```
