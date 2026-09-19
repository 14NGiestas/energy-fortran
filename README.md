# energy-fortran

[![CI](https://github.com/14NGiestas/energy-fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/14NGiestas/energy-fortran/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

In-process measurement of **energy, power, CPU time, I/O and named phases**
for Fortran programs: joules, watts, `cpu_s`, `cores_busy` and MB
read/written, straight from `/sys` and `/proc`, with **no external wrapper**
and **zero dependencies**. Works for any Fortran program — serial, OpenMP,
MPI, or HPC batch jobs — on Linux.

```fortran
use fortran_energy_mod
call energy_init()
call energy_mark('setup')            ! close the phase that just ended
... work ...
call energy_mark('solve', iv=iv)     ! J/wall/cpu/IO of THAT phase
call energy_report()                 ! one key=value line per phase
```

## Why measure inside the process

Energy is usually measured **outside** the process: a wrapper script samples a
hardware counter (or a wall plug) around the whole job, and per-region numbers
are attributed afterwards by apportionment (e.g. `J_job × steps_region /
steps_job`). That has two problems: the attribution is an approximation, and
it only works if somebody remembers to wrap the run.

Measuring inside makes each region's number **exact by construction**: the
joules consumed between two marks *are* that region's joules. It also makes
per-phase attribution possible at all — setup, solve, I/O, halo exchange —
which is where the interesting HPC questions live (e.g. how much energy the
I/O of a large output dump costs versus one solver iteration). The numbers
travel with the program's own logs or output files instead of living in a
sidecar that can get lost.

## What is measured, and from where

* **Energy (J)**
  * **(a)** `/sys/class/powercap/*/energy_uj` — accumulated hardware counter
    in microjoules. `max_energy_range_uj` is read next to it and used to
    handle **counter wrap** (a reading smaller than the previous one means the
    counter rolled over). Among RAPL domains, `package-*`/`psys` (the whole
    socket) is preferred over a subdomain (`core`/`uncore`/`dram`), which
    would measure only part of the CPU.
  * **(b)** `/sys/class/hwmon/hwmon*/power1_input` — instantaneous power in
    microwatts, accepted only for an `hwmon` whose `name` is one of `amdgpu`,
    `zenpower`, `amd_energy`, `rapl`, `coretemp`, `k10temp`. There is no
    counter there, so energy is a **trapezoid integral** over `system_clock`.
    Calling `energy_joules()` more often makes this integral more accurate.
  * **(c)** nothing readable → `kind = none`, energy stays `0.0`, and
    CPU/I/O/phases keep working. The fallback is useful on purpose: on a
    machine with no sensor you still get `cpu_s`, `cores_busy`, `cpu_pct` and
    per-phase I/O volume.
* **CPU** — `/proc/self/stat` fields 14 and 15 (`utime+stime` in ticks).
  Field 2 (`comm`) may contain spaces and parentheses, so parsing starts at
  the **last** `)` of the line.
* **Threads** — `/proc/self/status` (`Threads:`), the basis of `cpu_pct`.
* **I/O** — `/proc/self/io` (`read_bytes`, `write_bytes`). The *energy* of
  I/O cannot be measured from inside the process; the volume is what makes
  the phase numbers interpretable (a phase writing 400 MB is not the same
  kind of phase as one writing 4 MB).

### Sensor discovery order

1. The `sensor` argument to `energy_init`, if present: forces that exact path
   (a counter path is read as a counter, a path containing `power` as instant
   power). Pointing it at an unreadable path switches measurement off
   (`kind = none`) without falling back to auto-discovery.
2. `ENERGY_SENSOR` environment variable, if set: same semantics, from the
   outside — useful to pin a domain on a multi-socket box, or to switch
   measurement off, without touching the code.
2. `powercap` counters, preferring `package-*`/`psys` over subdomains.
3. `hwmon` instantaneous power, in the order `amdgpu`, `zenpower`,
   `amd_energy`, `rapl`, `coretemp`, `k10temp`.
4. A fixed list of well-known paths (in case directory listing is
   unavailable), then graceful degradation to `kind = none`.

## API

| procedure | what it gives |
|---|---|
| `energy_init(ticks, sensor)` | find a sensor (or stay neutral), zero accumulators and phases. `ticks` overrides `USER_HZ` (default 100); `sensor` forces that exact path — the inside control, for pinning a domain or a test file without touching the environment |
| `energy_ready()` | `.true.` when an energy sensor was found |
| `energy_joules()` | joules accumulated since init (samples/integrates the sensor) |
| `energy_watts()` | average power since the previous call (or since init) |
| `energy_cpu_seconds(ticks)` | process CPU seconds (`utime+stime` from `/proc/self/stat`) |
| `energy_cpu_percent()` | `100·cpu_s/(wall_s·threads)` since init |
| `energy_cores_busy()` | `cpu_s/wall_s` — equivalent cores kept busy since init |
| `energy_threads()` | threads the process has now (from `/proc/self/status`) |
| `energy_seconds()` | monotonic wall clock (seconds), the same one used internally |
| `energy_ticks()` | the `USER_HZ` in use |
| `energy_interval(iv, tokens)` | `energy_interval_t` with the deltas since the last call (advances the interval base) |
| `energy_peek(iv, tokens)` | same delta as `energy_interval` but **without** advancing the interval — for progress traces that must not disturb the phase accounting |
| `energy_mark(label, tokens, iv)` | closes the open interval and accumulates it under `label` (repeats accumulate; `n` counts them). `iv` returns the interval, so the caller gets the region's numbers and the phase accounting in one call |
| `energy_sensor()`, `energy_scope()`, `energy_kind_name()` | which sensor was used, what it covers, and the kind (`counter` / `power` / `none`) |
| `energy_report(unit)` | one `key=value` line per phase plus a `total` line |
| `energy_report_json()` | the same report as one JSON line, for structured logs |

`energy_interval_t` is the **complete record**: the interval (`j`, `wall_s`,
`cpu_s`, `cpu_pct`, `cores_busy`, `rd_mb`, `wr_mb`, `tokens`, `j_per_token`,
`w_mean`, `threads`) **plus the run-level context at the moment it was closed**
(`j_total`, `sensor`, `scope`, `kind`, `self_measured`). The API fills both
halves, so a consumer — a log line, a results file, a CSV row — never defines
a mirror struct nor assembles the context by hand:

```fortran
program my_run
  use fortran_energy_mod
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  type(energy_interval_t) :: rec
  integer(int64) :: n
  call energy_init()                          ! once, at the very start
  ... setup ...
  call energy_mark('setup')                   ! attributes everything so far
  do n = 1, nsteps
     call do_work(...)
     if (mod(n, report_every) == 0) then
        call energy_mark('work', iv=rec)      ! the numbers for THIS region
        write (*, '(A,F0.3,A,F0.3)') 'J=', rec%j, ' W_mean=', rec%w_mean
     end if
  end do
  call energy_mark('io')                      ! the cost of writing output
  call energy_report()                        ! per-phase lines
  print '(A)', energy_report_json()           ! or one JSON line
end program my_run
```

`fpm run --example measure_run` runs a complete three-phase example
(setup / work / I/O) printing both report formats.

## Build, test, depend on it

```bash
fpm build
fpm test
fpm test --profile debug --flag "-Wall -Wextra -Wcharacter-truncation -fcheck=all -fbacktrace -finit-real=snan"
fpm run --example measure_run
```

As a dependency:

```toml
[dependencies]
fortran_energy = { git = "https://github.com/14NGiestas/energy-fortran", tag = "v0.1.0" }
```

Zero dependencies: only `iso_fortran_env`, `ieee_arithmetic` and
`iso_c_binding` (libc `readdir` for listing `/sys`, since pure Fortran has no
glob and shelling out for a directory listing would be worse). If the
directory listing fails (unexpected libc layout), the module falls back to a
fixed list of known `/sys` paths — degrading to "no sensor", never crashing.

## Verification

`tools/parity.py` (stdlib-only Python) measures the **same** sensor from
**outside** the process while the example `parity_load` (a fixed N-second CPU
load) reports its in-process numbers; `bin/parity_check.sh` runs both in a
single run and prints the comparison:

```bash
bin/parity_check.sh 15
```

```
sensor (internal): /sys/class/hwmon/hwmon7/power1_input [amdgpu] [power]
sensor (external): /sys/class/hwmon/hwmon7/power1_input [power]
metric         internal     external    diff_%
energy_J       639.9969     621.3026     2.92%
cpu_s           15.0000      15.0000     0.00%
wall_s          15.0014      15.0207    -0.13%
```

(15 s fixed single-thread CPU load, both sides integrating the `amdgpu`
`power1_input` sensor; the RAPL counters on that machine are root-only, so
both sides used the `hwmon` fallback. CPU seconds come from the same kernel
accounting, hence the exact match.)

On short 5 s windows the energy difference varies between ~3% and ~9%: the
idle power of that GPU jitters (σ ≈ 8 W around a ≈ 38 W mean at 5 Hz
sampling), and the in-process side integrates only the interval endpoints
while the external side samples at 20 Hz. Longer windows average the jitter
out. This is inherent to the sampled-power path — a hardware counter agrees
much more tightly — and is why the documentation recommends calling
`energy_joules()` often on the `power` path.

## What the test proves (and how, without hardware)

`fpm test` runs in well under 5 s and needs no sensor:

1. **no sensor** (forced with `sensor='/nonexistent/energy_uj'`): `energy_ready()`
   is `.false.`, energy is `0.0`, and `cpu_s`/`cores_busy`/`cpu_pct` are still
   measured and reported;
2. **a fake counter** written by the test itself: J matches
   `delta_uj × 1e-6` exactly, is monotone, **wrap** adds
   `max_energy_range_uj`, and a backwards jump larger than the range (a driver
   reset, or a counter whose width changed) leaves J flat — never negative,
   never invented;
3. **the machine's real sensor**, when there is one: `energy_joules()` never
   decreases and `energy_watts()` lands in `[0, 500] W`;
4. **phases**: repeated marks with the same label accumulate (`n=2`, sums of
   J/wall/cpu/tokens);
5. **the reports**: the `key=value` line per phase and the JSON line carry the
   expected fields, with numbers always valid JSON;
6. **peek**: `energy_peek()` reads the open interval without consuming it.

## Limitations (honest list)

* **Linux only for the measurements.** It reads `/sys` and `/proc`. On
  another OS every read fails, `energy_init` reports `kind=none` and the
  module degrades to wall-clock + phase accounting (no energy, no `cpu_s`).
  That path is exercised by the test but has **not** been run on macOS/BSD.
* **One domain, not a sum.** On a multi-socket machine `energy_joules()`
  measures the single preferred domain (normally `package-0`, with `psys`
  preferred when present); `energy_scope()` says exactly which one. Sockets
  are not summed, and there is no cross-node/MPI aggregation helper — each
  rank measures its own node, and combining ranks is left to the caller
  (e.g. an `MPI_Reduce` of `iv%j`).
* **The `hwmon` path is a sampled integral, the RAPL path is a hardware
  counter.** With sampled power, `J` carries the trapezoid error over short
  phases; with a RAPL `energy_uj` counter you get the hardware's own
  accumulation. `energy_kind_name()` tells you which one you are looking at.
* **`USER_HZ = 100` is assumed** for the `/proc/self/stat` ticks (true on
  x86 Linux); override it with the `ticks` argument or `ENERGY_TICKS` if your
  kernel differs.
* **No sensor → wall/CPU only, without failing.** Every sysfs/proc read
  failure returns "no data" instead of raising an error; measurement is
  observability, not a prerequisite.
* **The sensor measures a package (the whole machine/socket), not your
  process.** On a shared machine the joules include everybody else's work.
  Interpret J together with `cpu_s`/`cores_busy`: if your process burned
  5 CPU-seconds over 5 wall-seconds (`cores_busy ≈ 1`) while the package
  reports 600 J, most of those joules are the machine's baseline and other
  tenants — your attributable share is roughly `cores_busy / machine_cores`
  of the dynamic part. For clean numbers, measure on an otherwise idle
  machine.
* **`read_bytes`/`write_bytes` are the kernel's accounting of block I/O**
  for the process, not a count of `write()` calls.
* **`cpu_pct` is against the thread count *now*.** If the process had more
  threads during the phase (e.g. OpenMP threads that have already exited),
  `cpu_pct` can read above 100%. `cores_busy` is the absolute measure
  (`cpu_s/wall_s`); use it when threads come and go.
* **Not thread-safe** (counter/integrator/phase state): call it from one
  thread, outside OpenMP regions.
* **Not verified** on macOS/Windows, nor against a real 32-bit wrapping
  counter (the wrap logic is tested with a simulated counter instead).
* The `readdir` listing uses the glibc `struct dirent` layout; if the layout
  does not match (other libc), a sanity check on `d_reclen` aborts the
  listing and the module falls back to a fixed list of known `/sys` paths.

## License

MIT — see [LICENSE](LICENSE).
