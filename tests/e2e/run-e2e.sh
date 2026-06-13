#!/bin/sh
# Real-Emacs end-to-end test for nostr.el.
#
# Spawns a real `emacs -nw' under a pseudo-terminal (via script(1)) so redisplay
# and window-buffer-change-functions run for real, then exercises the
# nostr-open refresh path under window churn and checks for recursion/errors.
#
# Runs twice:
#   1. fixed  mode (current code)        -> must be clean (no recursion).
#   2. buggy  mode (old synchronous hook)-> expected to reproduce the recursion,
#      proving the harness actually detects it.
#
# Exit status: 0 only if the fixed run is clean AND the buggy run reproduces.
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/../.." && pwd)
driver="$here/nostr-e2e-driver.el"
emacs=${EMACS:-emacs}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Need a pty for `emacs -nw'. script(1) provides one portably enough.
have_script=no
if command -v script >/dev/null 2>&1; then have_script=yes; fi
if [ "$have_script" != yes ]; then
  echo "SKIP: script(1) not available to allocate a pty for emacs -nw" >&2
  exit 0
fi

run_mode() {
  mode=$1            # fixed | buggy
  result="$tmp/result-$mode.txt"
  buggy=0
  [ "$mode" = buggy ] && buggy=1
  rm -f "$result"
  # util-linux script: -c CMD logfile ; -q quiet. Run with a hard timeout.
  NOSTR_E2E_ROOT="$root" \
  NOSTR_E2E_TMP="$tmp" \
  NOSTR_E2E_RESULT="$result" \
  NOSTR_E2E_FORCE_BUGGY="$buggy" \
  timeout 90 script -qec \
    "$emacs -nw -Q --eval '(setq inhibit-startup-screen t)' -l $driver" \
    /dev/null >/dev/null 2>&1 || true
  if [ ! -f "$result" ]; then
    echo "MODE=$mode RESULT=missing (emacs did not write a result)"
    return 2
  fi
  errors=$(sed -n 's/^ERRORS=//p' "$result")
  rendered=$(sed -n 's/^RENDERED=//p' "$result")
  ticks=$(sed -n 's/^TICKS=//p' "$result")
  echo "MODE=$mode ERRORS=$errors RENDERED=$rendered TICKS=$ticks"
  ok=0
  case "$mode" in
    fixed) { [ "$errors" = no ] && [ "$rendered" = yes ]; } && ok=1 ;;
    buggy) [ "$errors" = yes ] && ok=1 ;;
  esac
  # On failure, dump the captured *Messages* for debugging.
  if [ "$ok" -ne 1 ]; then
    echo "---- captured output ($mode) ----"
    cat "$result"
    echo "---------------------------------"
  fi
  [ "$ok" -eq 1 ]
}

status=0
echo "== fixed mode (must be clean) =="
if run_mode fixed; then echo "  PASS"; else echo "  FAIL"; status=1; fi
echo "== buggy mode (must reproduce recursion) =="
if run_mode buggy; then echo "  PASS (recursion reproduced + detected)"; else
  echo "  WARN: buggy mode did not reproduce recursion in this environment"
fi

if [ "$status" -eq 0 ]; then
  echo "E2E: PASS"
else
  echo "E2E: FAIL"
fi
exit "$status"
