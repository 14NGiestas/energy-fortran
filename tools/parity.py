#!/usr/bin/env python3
"""parity.py — measure the SAME energy sensor from OUTSIDE the process.

Runs a command as a child while sampling the sensor the Fortran library
would use, then prints one JSON line with the external measurement:

  {"sensor": ..., "kind": "counter"|"power"|"none", "external_j": ...,
   "wall_s": ..., "cpu_s": ..., "user_hz": ...}

Sensor discovery mirrors src/fortran_energy.f90:

  (a) /sys/class/powercap/*/energy_uj — hardware counter in microjoules,
      preferring a name containing "package" or "psys" (whole socket) over
      subdomains (core/uncore/dram). Counter wrap is handled with
      max_energy_range_uj: a backwards step smaller than the range adds the
      range; a larger backwards jump (driver reset) is ignored so energy
      never goes negative.
  (b) /sys/class/hwmon/hwmon*/power1_input — instantaneous power in
      microwatts, accepted only for hwmon names amdgpu, zenpower,
      amd_energy, rapl, coretemp, k10temp. Energy is the trapezoid integral
      over samples taken while the child runs (default 20 Hz).
  (c) nothing readable -> kind "none", external_j = 0.0 (CPU/wall still
      reported; mirrors the library's graceful degradation).

The child's stdout/stderr stream through untouched, so a load binary that
prints its own in-process numbers (examples/parity_load.f90) can be
compared against this external line from a SINGLE run.

Stdlib only. Usage:

  python3 tools/parity.py [--sample-hz HZ] -- <cmd> [args...]
"""

import argparse
import json
import os
import subprocess
import sys
import time

HW_NAMES = ("amdgpu", "zenpower", "amd_energy", "rapl", "coretemp", "k10temp")


def read_int(path):
    try:
        with open(path) as fh:
            return int(fh.read().strip().split()[0])
    except (OSError, ValueError, IndexError):
        return None


def read_text(path):
    try:
        with open(path) as fh:
            return fh.read().strip()
    except OSError:
        return None


def discover():
    """Return (kind, path, aux) mirroring the Fortran discovery order."""
    cands = []
    try:
        entries = sorted(os.listdir("/sys/class/powercap"))
    except OSError:
        entries = []
    for entry in entries:
        path = "/sys/class/powercap/%s/energy_uj" % entry
        if read_int(path) is None:
            continue
        name = read_text("/sys/class/powercap/%s/name" % entry) or entry
        cands.append((path, name))
    if cands:
        cands.sort(key=lambda c: (("package" not in c[1] and "psys" not in c[1]), c[0]))
        path, name = cands[0]
        return ("counter", path, name)
    try:
        hwmon = sorted(os.listdir("/sys/class/hwmon"))
    except OSError:
        hwmon = []
    for want in HW_NAMES:
        for entry in hwmon:
            base = "/sys/class/hwmon/%s" % entry
            if (read_text(base + "/name") or "") != want:
                continue
            if read_int(base + "/power1_input") is None:
                continue
            return ("power", base + "/power1_input", want)
    return ("none", "none", "")


def proc_cpu_seconds(pid, hz):
    """utime+stime of pid in seconds, or None if unreadable (exited)."""
    try:
        with open("/proc/%d/stat" % pid) as fh:
            line = fh.read()
    except OSError:
        return None
    rparen = line.rfind(")")
    if rparen < 0:
        return None
    fields = line[rparen + 1 :].split()
    try:
        utime = int(fields[11])
        stime = int(fields[12])
    except (IndexError, ValueError):
        return None
    return (utime + stime) / float(hz)


def main():
    ap = argparse.ArgumentParser(description="external energy parity sampler")
    ap.add_argument("--sample-hz", type=float, default=20.0)
    ap.add_argument("command", nargs=argparse.REMAINDER)
    args = ap.parse_args()
    cmd = args.command
    if cmd and cmd[0] == "--":
        cmd = cmd[1:]
    if not cmd:
        ap.error("usage: parity.py [--sample-hz HZ] -- <cmd> [args...]")

    kind, path, aux = discover()
    hz = 100
    try:
        hz = os.sysconf("SC_CLK_TCK")
    except (AttributeError, OSError, ValueError):
        pass
    interval = 1.0 / max(1.0, args.sample_hz)

    energy = 0.0
    last_raw = None
    counter_range = 0
    last_watts = None
    last_t = None
    if kind == "counter":
        last_raw = read_int(path)
        counter_range = read_int(os.path.join(os.path.dirname(path), "max_energy_range_uj")) or 0
    elif kind == "power":
        raw = read_int(path)
        last_t = None
        if raw is not None:
            last_watts = raw * 1e-6
            last_t = time.monotonic()

    t0 = time.monotonic()
    proc = subprocess.Popen(cmd, stdout=sys.stdout, stderr=sys.stderr)
    cpu_s = 0.0
    while proc.poll() is None:
        time.sleep(interval)
        now = time.monotonic()
        if kind == "counter":
            raw = read_int(path)
            if raw is not None and last_raw is not None:
                delta = raw - last_raw
                if delta < 0:
                    if 0 < counter_range and -delta <= counter_range:
                        delta += counter_range
                    else:
                        delta = 0
                energy += delta * 1e-6
                last_raw = raw
        elif kind == "power":
            raw = read_int(path)
            if raw is not None and last_watts is not None and last_t is not None:
                watts = raw * 1e-6
                energy += 0.5 * (last_watts + watts) * (now - last_t)
                last_watts = watts
                last_t = now
        sample = proc_cpu_seconds(proc.pid, hz)
        if sample is not None:
            cpu_s = sample
    wall_s = time.monotonic() - t0
    # One last sensor read so the tail of the run is included.
    if kind == "counter":
        raw = read_int(path)
        if raw is not None and last_raw is not None:
            delta = raw - last_raw
            if delta < 0:
                if 0 < counter_range and -delta <= counter_range:
                    delta += counter_range
                else:
                    delta = 0
            energy += delta * 1e-6
    elif kind == "power":
        raw = read_int(path)
        now = time.monotonic()
        if raw is not None and last_watts is not None and last_t is not None:
            energy += 0.5 * (last_watts + raw * 1e-6) * (now - last_t)
    sample = proc_cpu_seconds(proc.pid, hz)
    if sample is not None:
        cpu_s = sample

    print(
        json.dumps(
            {
                "sensor": path,
                "kind": kind,
                "external_j": energy,
                "wall_s": wall_s,
                "cpu_s": cpu_s,
                "user_hz": hz,
            }
        ),
        flush=True,
    )
    return proc.returncode


if __name__ == "__main__":
    sys.exit(main())
