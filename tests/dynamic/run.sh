#!/bin/sh
# Dynamic tests. Each package under tests/dynamic/ is a binary that
# depends on this checkout (uploaded to a scratch copy of the repository
# first); it must build and exit 0. Used where a property cannot be
# expressed in types (e.g. which value a comparison returns).
#
# usage: tests/dynamic/run.sh <repository-dir>   (bats must be on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
cp -R "$1" "$TMP/repo"
(cd "$ROOT" && bats upload --repository "$TMP/repo" >/dev/null)

# A test that hangs must fail, not stall CI. `timeout` is GNU coreutils;
# where it is missing the binary runs without a limit.
LIMIT=""
if command -v timeout >/dev/null 2>&1; then LIMIT="timeout 300"; fi

fail=0
for d in "$ROOT"/tests/dynamic/*/; do
  [ -f "$d/bats.toml" ] || continue
  n=$(basename "$d"); w="$TMP/w-$n"
  cp -R "$d" "$w"
  if (cd "$w" && bats lock --repository "$TMP/repo" && bats build --only debug --only native --repository "$TMP/repo") > "$TMP/$n.log" 2>&1 \
     && (cd "$w" && $LIMIT "./dist/debug/$n") > "$TMP/$n.out" 2>&1; then
    echo "ok   $n"
  else
    echo "FAIL $n"; grep -E 'error|FAIL' "$TMP/$n.log" "$TMP/$n.out" 2>/dev/null | head -10; fail=1
  fi
done
exit $fail
