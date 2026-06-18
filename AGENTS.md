# AGENTS.md

## Project

`nostr.el` is an Emacs Nostr client with:

- Modular Emacs Lisp UI, storage, relay networking, and workflows.
- A Rust `nostr-sdk` CLI backend in `backend/` for signing and NIP helpers.
- SQLite cache storage via `emacsql`.
- Guix-managed development dependencies through `manifest.scm`.

Do not use Magit or `magit-section` for UI primitives. The client uses local custom section UI in `nostr-ui.el`.

## Environment

Use the project manifest for all build and test commands:

```sh
guix shell -m manifest.scm -- <command>
```

The `.envrc` should contain:

```sh
use guix
export CC=gcc
```

Do not use global package managers or install dependencies into the user profile.

## Architecture

Top-level entrypoint:

- `nostr.el`

Core Elisp modules:

- `nostr-backend.el`: JSON protocol wrapper for the Rust backend.
- `nostr-relay.el`: websocket relay IO and subscription handling.
- `nostr-db.el`: SQLite schema and query helpers.
- `nostr-event.el`: Nostr event/tag normalization helpers.
- `nostr-url.el`: SSRF host-safety guards for outbound HTTP(S) from relay-supplied URLs; used by `nostr-media` and `nostr-nip`.
- `nostr-ui.el`: custom section rendering and navigation.
- `nostr-timeline.el`, `nostr-thread.el`, `nostr-profile.el`, `nostr-search.el`, `nostr-notifications.el`, `nostr-relays.el`: user-facing buffers.
- `nostr-discover.el`: server-ranked provider-backed discovery feeds (e.g. Primal Explore).
- `nostr-dispatch.el`: public NIP-19 / hex identifier open dispatcher.
- `nostr-reactions.el`: reaction detail buffers (accounts that reacted to a note).
- `nostr-compose.el`, `nostr-actions.el`, `nostr-share.el`, `nostr-upload.el`, `nostr-media.el`, `nostr-nip.el`, `nostr-setup.el`: workflow helpers (`nostr-upload.el` is the shared Blossom upload backend used by compose and profile editing).

Backend:

- `backend/src/lib.rs`: backend protocol and nostr-sdk operations.
- `backend/src/main.rs`: CLI stdin/stdout entrypoint.
- `backend/tests/cli.rs`: CLI protocol tests.

## Development Rules

- Prefer existing module boundaries. Add new UI behavior near the relevant buffer module, and shared behavior in a small dedicated module.
- Keep secrets off argv and out of logs. Signing keys are loaded from the configured local GPG-encrypted key file and sent to the backend over stdin JSON only.
- Use structured JSON and existing helpers for Nostr events, NIP-19, relay messages, and DB rows.
- Preserve batch-testability. UI work should be verifiable with `emacs --batch` by inspecting buffers and selected section data.
- Commit each contained feature or fix separately after it is implemented and verified.
- Do not leave generated `.elc` files or Cargo `target/` directories in the worktree.

## Verification

Run the full ERT suite:

```sh
guix shell -m manifest.scm -- emacs --batch -Q -L . -L tests \
  -l nostr-test.el \
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

Run the opt-in public relay receive smoke test with network access:

```sh
NOSTR_LIVE_RELAY_TEST=1 guix shell -m manifest.scm -- emacs --batch -Q -L . -L tests \
  -l tests/nostr-live-relay-smoke-test.el \
  -f ert-run-tests-batch-and-exit
```

Run the opt-in public relay publish smoke test with an ephemeral generated key:

```sh
NOSTR_LIVE_RELAY_PUBLISH_TEST=1 guix shell -m manifest.scm -- emacs --batch -Q -L . -L tests \
  -l tests/nostr-live-relay-smoke-test.el \
  -f ert-run-tests-batch-and-exit
```

Run backend tests and formatting:

```sh
guix shell -m manifest.scm -- env CC=gcc cargo test
guix shell -m manifest.scm -- env CC=gcc cargo fmt --check
```

Check Elisp forms and byte compilation:

```sh
guix shell -m manifest.scm -- emacs --batch -Q -L . \
  --eval '(progn (dolist (file (append (directory-files default-directory t "[.]el$") (directory-files "tests" t "[.]el$"))) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens))) (princ "all parens ok"))'

guix shell -m manifest.scm -- emacs --batch -Q -L . -L tests \
  --eval '(progn (dolist (file (append (directory-files default-directory nil "^nostr.*[.]el$") (directory-files "tests" t "[.]el$"))) (byte-compile-file file)) (princ "byte compile ok"))'
```

Clean generated artifacts after checks:

```sh
rm -f *.elc tests/*.elc
guix shell -m manifest.scm -- env CC=gcc cargo clean
```

Confirm cleanup:

```sh
fd --hidden --no-ignore --glob --color never '*.elc' . --type file
fd --hidden --no-ignore --glob --color never 'target' . --type directory --max-depth 2
```

## Current Product Direction

The goal is a fully usable public-social Nostr client for Emacs. Prioritize:

- Reliable relay connectivity and cached operation.
- Clear timeline, thread, profile, search, notification, and relay buffers.
- Safe compose, reply, quote, reaction, repost, follow, and share workflows.
- NIP-05, NIP-19, NIP-65, media preview, and setup/account status flows.
- Practical keyboard-first UI with visible state and predictable actions.

Remaining areas that likely need work:

- More timeline feed modes and filters.
- More profile social context.
- Stronger selected-section affordances and buffer-local help/action surfaces.
- Broader live-relay validation beyond batch tests.
