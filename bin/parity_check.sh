#!/usr/bin/env bash
# bin/parity_check.sh — one run, two rulers: in-process vs external energy.
#
# Builds parity_load, runs it ONCE under tools/parity.py (which samples the
# same sensor from the outside while the binary reports its in-process
# numbers), then prints internal vs external energy, cpu_s and wall with the
# percentage differences.
#
# Usage: bin/parity_check.sh [seconds]   (default 5)
set -euo pipefail
cd "$(dirname "$0")/.."

SECONDS_RUN="${1:-5}"
FPM="${FPM:-fpm}"

"$FPM" build >&2
BIN="$(find build -type f -name parity_load | head -n 1)"
if [ -z "$BIN" ]; then
  # Force the example to build, then look again.
  "$FPM" run --example parity_load -- 0.1 >/dev/null 2>&1
  BIN="$(find build -type f -name parity_load | head -n 1)"
fi
if [ -z "$BIN" ]; then
  echo "parity_check: parity_load binary not found under build/" >&2
  exit 1
fi

RUNLOG="$(mktemp)"
trap 'rm -f "$RUNLOG"' EXIT

# Single run: the binary's stdout (PARITY lines) streams through parity.py.
EXT_JSON="$(python3 tools/parity.py -- "$BIN" "$SECONDS_RUN" | tee "$RUNLOG" | tail -n 1)"
# The last line is the external JSON; the PARITY line is further up.
INT_LINE="$(grep '^PARITY internal_j=' "$RUNLOG" | head -n 1)"

if [ -z "$INT_LINE" ]; then
  echo "parity_check: no PARITY line from $BIN; full output:" >&2
  cat "$RUNLOG" >&2
  exit 1
fi

python3 - "$INT_LINE" "$EXT_JSON" <<'EOF'
import json, re, sys

int_line, ext_json = sys.argv[1], sys.argv[2]
m = dict(re.findall(r'(internal_j|wall_s|cpu_s|cores_busy)=([0-9.]+)', int_line))
ms = re.search(r'sensor="(.*)" kind="(.*)"', int_line)
ext = json.loads(ext_json)

ij, iw, ic = float(m['internal_j']), float(m['wall_s']), float(m['cpu_s'])
ej, ew, ec = float(ext['external_j']), float(ext['wall_s']), float(ext.get('cpu_s') or 0.0)

def pct(a, b):
    denom = max(abs(a), abs(b))
    return 0.0 if denom == 0 else 100.0 * (a - b) / denom

print('sensor (internal): %s [%s]' % (ms.group(1), ms.group(2)))
print('sensor (external): %s [%s]' % (ext['sensor'], ext['kind']))
print('%-10s %12s %12s %9s' % ('metric', 'internal', 'external', 'diff_%'))
print('%-10s %12.4f %12.4f %8.2f%%' % ('energy_J', ij, ej, pct(ij, ej)))
print('%-10s %12.4f %12.4f %8.2f%%' % ('cpu_s', ic, ec, pct(ic, ec)))
print('%-10s %12.4f %12.4f %8.2f%%' % ('wall_s', iw, ew, pct(iw, ew)))
EOF
