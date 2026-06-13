# End-to-end tests (real Emacs)

These exercise nostr.el in a **real `emacs` process** (not `--batch`), because the
refresh/redisplay recursion class can only happen with real windows and
redisplay — `--batch` never fires `window-buffer-change-functions`.

## Run

```sh
sh tests/e2e/run-e2e.sh
```

It spawns `emacs -nw` under a pseudo-terminal (`script(1)`), so window-change
functions and redisplay run for real. Override the binary with `EMACS=...`.

## What it does

`nostr-e2e-driver.el` seeds an **offline** timeline (temp DB, a fixed account,
follows + 60 notes; `nostr-relay-urls` is nil so nothing touches the network),
runs `nostr-open`, then hammers the refresh path: 50 iterations of simulated
event ingestion (store note → run `nostr-relay-event-hook`) interleaved with
window churn (split / switch / delete-other-windows + forced `redisplay`). It
scans `*Messages*` for recursion/error markers and writes a result file.

Two modes:

- **fixed** (current code) — must be clean: renders the feed and completes all
  ticks with no `max-lisp-eval-depth` / process / timer errors.
- **buggy** — reinstalls the old synchronous `window-buffer-change-functions`
  handler and triggers a deterministic deep recursion in a timer, to prove the
  harness *detects* the failure class the user reported.

## Known limitation

The reported crash was on a **graphical** frame, where refreshing re-enters
redisplay via image loading / fontification / mode-line evaluation. A TTY frame
(used here, since CI has no X server) cannot re-enter redisplay the same way, so
the fixed-mode run is a regression guard for the refresh/window path rather than
a faithful reproduction of the original GUI recursion. To reproduce on a real
graphical Emacs, run the driver in a GUI frame with `NOSTR_E2E_FORCE_BUGGY=1`.
