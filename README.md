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

Linux x86_64 users can download a release binary:

```text
https://github.com/trevarj/nostr.el/releases/latest/download/nostr-el-backend-x86_64-unknown-linux-gnu.tar.gz
```

Verify the checksum from the matching `.sha256` file in the release, then put
`nostr-el-backend` on `PATH` or point Emacs at it with `nostr-backend-command`.

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
