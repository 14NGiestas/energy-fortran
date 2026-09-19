! lib/fortran_energy.f90 — energy, CPU time, IO and PHASE measurement in pure Fortran.
!
! WHY THIS EXISTS: energy is usually measured from the OUTSIDE (a wrapper
! script sampling a hardware counter around the whole job) with per-region
! numbers attributed afterwards by apportionment. Measuring INSIDE the process
! makes each region's number exact by construction: the J spent between the
! previous mark and this one. The field energy.self_measured='1' says the
! number came from here.
!
! MINIMAL USE (5 lines):
!     use fortran_energy_mod
!     call energy_init()                       ! find a sensor (or stay neutral)
!     call energy_mark('setup')                ! close the phase that just ended
!     ... work ...
!     call energy_mark('solve', tokens=n)      ! J/wall/cpu/IO OF THAT phase
!     call energy_report()                     ! one key=value line per phase
!
! WHAT IS MEASURED, AND FROM WHERE:
!   energy    (a) /sys/class/powercap/*/energy_uj — ACCUMULATED counter in
!                 microjoules, with max_energy_range_uj next to it to handle
!                 WRAP (a reading smaller than the previous one means the counter
!                 rolled over). Among RAPL domains it prefers package-*/psys (the
!                 whole socket) over a subdomain (core/uncore/dram), which would
!                 measure only part of the CPU.
!             (b) /sys/class/hwmon/hwmon*/power1_input — instantaneous power in
!                 microwatts, accepted only for hwmon whose `name` is in
!                 HW_NAMES (amdgpu, zenpower, amd_energy, rapl, coretemp,
!                 k10temp); integrated by TRAPEZOID with system_clock, since
!                 there is no counter.
!             (c) nothing -> kind=0 and energy stays 0.0; CPU/IO/phases keep
!                 working (the fallback HAS to be useful, not empty).
!   CPU       /proc/self/stat fields 14 and 15 (utime+stime in ticks; default 100
!             ticks/s, overridable by argument or by ENERGY_TICKS). Field 2
!             (comm) may contain spaces and parentheses, so parsing starts at the
!             LAST ')' of the line — the shape that does not break on an exotic
!             process name.
!   threads   /proc/self/status ("Threads:") -> basis of cpu_pct
!   IO        /proc/self/io (read_bytes, write_bytes) — IO volume per phase. The
!             ENERGY of IO cannot be measured from inside the process; the volume
!             helps interpret it (e.g. a big output dump vs a solver sweep).
!
! ENUMERATING /sys WITHOUT A SHELL: pure Fortran has no glob, and calling /bin/sh
! would be worse (fork, quoting, and sysfs has ':' in names — the sh glob does not
! even accept '#'). Listing uses readdir (libc) through iso_c_binding, which is
! not a "dependency" in the package sense: it is in every Linux. If `struct
! dirent` does not match the machine's libc, the listing comes back empty and we
! fall back to a FIXED list of known paths (hwmon0..hwmon31, intel-rapl:*) —
! degrading to "no sensor", never crashing.
!
! PHASES: energy_mark(label) closes the open interval and accumulates (J, wall,
! cpu_s, IO) under `label`. The label names the phase that JUST happened:
! call energy_mark('io') right after writing output. Repeated calls with the
! same label ACCUMULATE (n counts how many).
!
! NOT thread-safe (counter/integrator/phase state): call it from one thread,
! outside OpenMP regions.
!
! These are the three things the module measures, in one call each:
!   energy_joules()      J since init (samples the sensor; call often on the
!                        sampled-power path for a tighter trapezoid integral)
!   energy_cpu_seconds() CPU seconds of the process (utime+stime)
!   energy_mark(label)   closes a PHASE: J + wall + cpu + IO + tokens under a name
!   energy_peek(iv)      the same delta WITHOUT closing the interval (trace)
!
! SELF-CONTAINED: nothing from any other repository or module — only
! iso_fortran_env, ieee_arithmetic and iso_c_binding (libc for readdir). It is
! meant to be depended on as its own fpm package; see README.md for the roadmap
! (multi-socket RAPL, measuring another pid, 32-bit counter wrap).
module fortran_energy_mod
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int8_t, c_int16_t, &
      c_int64_t, c_null_char, c_ptr, c_f_pointer, c_associated
  implicit none
  private

  public :: energy_init, energy_ready, energy_joules, energy_watts, energy_sensor, &
            energy_scope, energy_kind_name, energy_seconds, energy_cpu_seconds, &
            energy_cpu_percent, energy_cores_busy, energy_threads, energy_ticks, &
            energy_interval, energy_peek, energy_interval_t, energy_mark, &
            energy_report, energy_report_json

  ! sensor kind
  integer, parameter :: EK_NONE = 0, EK_COUNTER = 1, EK_POWER = 2
  ! accepted hwmon names, in lookup order
  integer, parameter :: NHW = 6
  character(len=16), parameter :: HW_NAMES(NHW) = [character(len=16) :: &
      'amdgpu', 'zenpower', 'amd_energy', 'rapl', 'coretemp', 'k10temp']
  ! phases: fixed vector (no allocation, no dependency); overflow goes to 'other'
  integer, parameter :: MAX_PHASES = 24, MAX_LABEL = 24

  ! One measured interval: what a phase/region consumed since the last mark.
  ! It is ALSO the complete record a consumer needs -- log line, results file,
  ! CSV row -- so nobody has to define a mirror struct or copy fields around: the
  ! run-level context (totals, sensor, scope, kind) comes filled by the API.
  type, public :: energy_interval_t
    ! ---- the interval itself
    real(real64) :: j = 0.0_real64          ! joules in the interval (0.0 without sensor)
    real(real64) :: wall_s = 0.0_real64     ! wall-clock seconds
    real(real64) :: cpu_s = 0.0_real64      ! CPU seconds (utime+stime)
    real(real64) :: cpu_pct = 0.0_real64    ! 100*cpu_s/(wall_s*threads)
    real(real64) :: cores_busy = 0.0_real64 ! cpu_s/wall_s (equivalent cores)
    real(real64) :: rd_mb = 0.0_real64      ! MB (10^6 bytes) read from disk
    real(real64) :: wr_mb = 0.0_real64      ! MB (10^6 bytes) written
    real(real64) :: tokens = 0.0_real64     ! tokens attributed to the interval
    real(real64) :: j_per_token = 0.0_real64 ! j/tokens (0 when tokens=0)
    real(real64) :: w_mean = 0.0_real64     ! j/wall_s (mean power in the interval)
    integer :: threads = 1
    ! ---- context of the whole run at the moment the interval was closed
    real(real64) :: j_total = 0.0_real64    ! j since energy_init (the run so far)
    character(len=256) :: sensor = 'none'   ! what measured the energy
    character(len=256) :: scope = ''        ! what that sensor covers
    character(len=16) :: kind = 'none'      ! 'counter' | 'power' | 'none'
    ! .true. = these numbers were measured by THIS process (not by an external
    ! wrapper or by dividing a job total). Stays .true. with kind='none': the
    ! CPU/IO/phase numbers are still self-measured, only J is unavailable.
    logical :: self_measured = .true.
  end type energy_interval_t

  type :: phase_t
    character(len=MAX_LABEL) :: label = ''
    integer :: n = 0
    real(real64) :: j = 0.0_real64, wall_s = 0.0_real64, cpu_s = 0.0_real64
    real(real64) :: rd_mb = 0.0_real64, wr_mb = 0.0_real64, tokens = 0.0_real64
  end type phase_t

  ! ---- sensor state
  integer, save :: e_kind = EK_NONE
  character(len=512), save :: e_path = ''
  character(len=256), save :: e_sensor = 'none'
  character(len=256), save :: e_scope = 'no sensor (fields stay neutral)'
  integer(int64), save :: e_range_uj = 0_int64
  integer(int64), save :: e_last_raw = 0_int64
  real(real64), save :: e_joule = 0.0_real64
  real(real64), save :: e_w_prev = 0.0_real64
  real(real64), save :: e_t_prev = 0.0_real64
  real(real64), save :: e_t_init = 0.0_real64
  real(real64), save :: e_t_watts = -1.0_real64
  real(real64), save :: e_j_watts = 0.0_real64
  ! ---- CPU/IO/clock
  real(real64), save :: e_ticks = 100.0_real64     ! USER_HZ (Linux x86 = 100)
  real(real64), save :: e_cpu_init = 0.0_real64    ! process cpu_s at init
  ! ---- base of the open interval (interval/mark)
  real(real64), save :: e_t_base = 0.0_real64, e_j_base = 0.0_real64
  real(real64), save :: e_cpu_base = 0.0_real64
  real(real64), save :: e_rd_base = 0.0_real64, e_wr_base = 0.0_real64
  ! ---- phases
  type(phase_t), save :: e_phases(MAX_PHASES)
  integer, save :: e_nphase = 0

  ! glibc struct dirent (x86_64)
  type, bind(C) :: c_dirent
    integer(c_int64_t) :: d_ino
    integer(c_int64_t) :: d_off
    integer(c_int16_t) :: d_reclen
    integer(c_int8_t) :: d_type
    character(c_char) :: d_name(256)
  end type c_dirent

  interface
    function c_opendir(path) bind(C, name="opendir") result(d)
      import :: c_char, c_ptr
      character(kind=c_char), dimension(*) :: path
      type(c_ptr) :: d
    end function c_opendir

    function c_readdir(d) bind(C, name="readdir") result(e)
      import :: c_ptr
      type(c_ptr), value :: d
      type(c_ptr) :: e
    end function c_readdir

    function c_closedir(d) bind(C, name="closedir") result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: d
      integer(c_int) :: rc
    end function c_closedir
  end interface

contains

  ! ================================================================== plumbing
  ! Monotonic seconds from some fixed instant (system_clock). Only DIFFERENCES
  ! matter; count and count_rate in the SAME kind (int64) because F2023 forbids
  ! different kinds in this call.
  function energy_seconds() result(t)
    real(real64) :: t
    integer(int64) :: cnt, rate
    call system_clock(cnt, rate)
    if (rate <= 0_int64) then
      t = 0.0_real64
    else
      t = real(cnt, real64)/real(rate, real64)
    end if
  end function energy_seconds

  function energy_ticks() result(t)
    real(real64) :: t
    t = e_ticks
  end function energy_ticks

  ! Every sysfs/proc read goes through here and NO failure raises an error:
  ! missing file, no permission (intel-rapl energy_uj is 0400 root!) or an
  ! unreadable value return .false. and the caller decides. Measurement is
  ! observability, not a prerequisite.
  logical function read_i64(path, v)
    character(*), intent(in) :: path
    integer(int64), intent(out) :: v
    integer :: u, ios
    v = 0_int64
    read_i64 = .false.
    open (newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return
    read (u, *, iostat=ios) v
    close (u)
    read_i64 = (ios == 0)
  end function read_i64

  logical function read_txt(path, s)
    character(*), intent(in) :: path
    character(*), intent(out) :: s
    integer :: u, ios
    s = ''
    read_txt = .false.
    open (newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return
    read (u, '(A)', iostat=ios) s
    close (u)
    read_txt = (ios == 0)
  end function read_txt

  ! Read the whole file (for /proc/self/status, which is multiline).
  subroutine read_all(path, txt, ok)
    character(*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: txt
    logical, intent(out) :: ok
    character(len=1024) :: line
    integer :: u, ios
    txt = ''
    ok = .false.
    open (newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return
    do
      read (u, '(A)', iostat=ios) line
      if (ios /= 0) exit
      txt = txt//trim(line)//achar(10)
    end do
    close (u)
    ok = .true.
  end subroutine read_all

  logical function path_exists(path)
    character(*), intent(in) :: path
    inquire (file=path, exist=path_exists)
  end function path_exists

  pure function dirname_of(path) result(d)
    character(*), intent(in) :: path
    character(len=:), allocatable :: d
    integer :: i
    d = ''
    do i = len_trim(path), 1, -1
      if (path(i:i) == '/') then
        d = path(1:i - 1)
        return
      end if
    end do
  end function dirname_of

  pure function basename_of(path) result(b)
    character(*), intent(in) :: path
    character(len=:), allocatable :: b
    integer :: i
    b = trim(path)
    do i = len_trim(path), 1, -1
      if (path(i:i) == '/') then
        b = path(i + 1:len_trim(path))
        return
      end if
    end do
  end function basename_of

  ! k-th space-separated field of `line`, as an integer.
  subroutine field_int(line, k, val, ok)
    character(*), intent(in) :: line
    integer, intent(in) :: k
    integer(int64), intent(out) :: val
    logical, intent(out) :: ok
    character(len=32) :: tok
    integer :: i, L, n, lt, ios
    val = 0_int64
    ok = .false.
    L = len_trim(line)
    n = 0
    i = 1
    do while (i <= L)
      do while (i <= L .and. line(i:i) == ' ')
        i = i + 1
      end do
      if (i > L) exit
      tok = ''
      lt = 0
      do while (i <= L .and. line(i:i) /= ' ')
        lt = lt + 1
        if (lt <= len(tok)) tok(lt:lt) = line(i:i)
        i = i + 1
      end do
      n = n + 1
      if (n == k) then
        read (tok, *, iostat=ios) val
        ok = (ios == 0)
        return
      end if
    end do
  end subroutine field_int

  ! readdir: names inside a directory (no '.' and no '..'). Empty = could not.
  subroutine list_dir(dir, names, n)
    character(*), intent(in) :: dir
    character(len=*), intent(out) :: names(:)
    integer, intent(out) :: n
    type(c_ptr) :: d, e
    type(c_dirent), pointer :: ent
    character(len=1) :: buf(1024)
    character(len=256) :: nm
    integer :: i, L, rc
    n = 0
    L = min(len_trim(dir), size(buf) - 1)
    buf = c_null_char
    do i = 1, L
      buf(i) = dir(i:i)
    end do
    buf(L + 1) = c_null_char
    d = c_opendir(buf)
    if (.not. c_associated(d)) return
    do
      e = c_readdir(d)
      if (.not. c_associated(e)) exit
      call c_f_pointer(e, ent)
      ! Layout guard: `struct dirent` differs across libcs (glibc vs musl vs BSD).
      ! d_reclen is the kernel's own record length, so if it is not a sane small
      ! number the struct we mapped is not the one this libc returned -- giving up
      ! here keeps a wrong layout from making us read garbage past the record (the
      ! caller then falls back to the fixed path list).
      if (ent%d_reclen <= 0 .or. ent%d_reclen > 4096) exit
      nm = ''
      do i = 1, 255
        if (ent%d_name(i) == c_null_char) exit
        nm(i:i) = ent%d_name(i)
      end do
      nm = trim(nm)
      if (len_trim(nm) == 0) cycle
      if (nm(1:1) == '.') cycle
      n = n + 1
      if (n > size(names)) then
        n = n - 1
        exit
      end if
      names(n) = trim(nm)
    end do
    rc = c_closedir(d)
  end subroutine list_dir

  ! ============================================================ /proc (CPU, IO)
  ! utime+stime of the process, in CPU seconds. Parsing starts at the LAST ')'
  ! of the line: field 2 (comm) may contain spaces and parentheses. After the ')'
  ! the 12th field is utime and the 13th is stime (state, ppid, pgrp, session,
  ! tty_nr, tpgid, flags, minflt, cminflt, majflt, cmajflt, utime, stime).
  function energy_cpu_seconds(ticks) result(s)
    real(real64), intent(in), optional :: ticks
    real(real64) :: s, tk
    character(len=4096) :: line
    integer(int64) :: u, st
    integer :: p, ios, un
    logical :: ok1, ok2
    tk = e_ticks
    if (present(ticks)) then
      if (ticks > 0.0_real64) tk = ticks
    end if
    s = 0.0_real64
    line = ''
    ! unit number and iostat in DIFFERENT variables: with newunit=ios + iostat=ios
    ! the iostat overwrites the unit number and the read ends up on unit 0 (which
    ! is stderr) -- that was exactly the bug that zeroed cpu_s in the first version.
    open (newunit=un, file='/proc/self/stat', status='old', action='read', iostat=ios)
    if (ios /= 0) return
    read (un, '(A)', iostat=ios) line
    close (un)
    if (ios /= 0) return
    p = index(line, ')', back=.true.)
    if (p <= 0) return
    call field_int(line(p + 1:), 12, u, ok1)
    call field_int(line(p + 1:), 13, st, ok2)
    if (.not. (ok1 .and. ok2)) return
    s = real(u + st, real64)/tk
  end function energy_cpu_seconds

  ! Process threads (cpu_pct basis). 1 when unreadable.
  function energy_threads() result(n)
    integer :: n
    character(len=:), allocatable :: txt
    character(len=32) :: tok
    integer :: p, q, i, ios
    logical :: ok
    n = 1
    call read_all('/proc/self/status', txt, ok)
    if (.not. ok) return
    p = index(txt, 'Threads:')
    if (p <= 0) return
    q = p + 8
    tok = ''
    i = 0
    do while (q <= len(txt))
      if (txt(q:q) == achar(10)) exit
      if (txt(q:q) /= ' ' .and. txt(q:q) /= achar(9)) then
        i = i + 1
        if (i <= len(tok)) tok(i:i) = txt(q:q)
      else if (i > 0) then
        exit
      end if
      q = q + 1
    end do
    if (i > 0) then
      read (tok, *, iostat=ios) n
      if (ios /= 0 .or. n < 1) n = 1
    end if
  end function energy_threads

  ! read_bytes / write_bytes from /proc/self/io (bytes accounted to the process).
  subroutine energy_io_bytes(rd, wr)
    real(real64), intent(out) :: rd, wr
    character(len=:), allocatable :: txt
    character(len=64) :: tok
    integer :: i, p, q, ios
    integer(int64) :: v
    logical :: ok
    rd = 0.0_real64
    wr = 0.0_real64
    call read_all('/proc/self/io', txt, ok)
    if (.not. ok) return
    p = index(txt, 'read_bytes:')
    if (p > 0) then
      q = p + 11
      tok = ''
      i = 0
      do while (q <= len(txt))
        if (txt(q:q) == achar(10)) exit
        if (txt(q:q) /= ' ') then
          i = i + 1
          if (i <= len(tok)) tok(i:i) = txt(q:q)
        else if (i > 0) then
          exit
        end if
        q = q + 1
      end do
      if (i > 0) then
        read (tok, *, iostat=ios) v
        if (ios == 0) rd = real(v, real64)
      end if
    end if
    p = index(txt, 'write_bytes:')
    if (p > 0) then
      q = p + 12
      tok = ''
      i = 0
      do while (q <= len(txt))
        if (txt(q:q) == achar(10)) exit
        if (txt(q:q) /= ' ') then
          i = i + 1
          if (i <= len(tok)) tok(i:i) = txt(q:q)
        else if (i > 0) then
          exit
        end if
        q = q + 1
      end do
      if (i > 0) then
        read (tok, *, iostat=ios) v
        if (ios == 0) wr = real(v, real64)
      end if
    end if
  end subroutine energy_io_bytes

  ! ========================================================= sensor (energy)
  subroutine sensor_reset()
    e_kind = EK_NONE
    e_path = ''
    e_sensor = 'none'
    e_scope = 'no sensor (energy fields neutral; cpu/io still measured)'
    e_range_uj = 0_int64
    e_last_raw = 0_int64
    e_joule = 0.0_real64
    e_w_prev = 0.0_real64
    e_t_prev = 0.0_real64
    e_t_watts = -1.0_real64
    e_j_watts = 0.0_real64
  end subroutine sensor_reset

  logical function use_counter(path)
    character(*), intent(in) :: path
    integer(int64) :: raw
    character(len=256) :: nm
    use_counter = .false.
    if (.not. read_i64(path, raw)) return
    e_kind = EK_COUNTER
    e_path = path
    e_last_raw = raw
    e_joule = 0.0_real64
    if (.not. read_i64(dirname_of(path)//'/max_energy_range_uj', e_range_uj)) then
      e_range_uj = 0_int64
    end if
    nm = ''
    if (.not. read_txt(dirname_of(path)//'/name', nm)) nm = basename_of(dirname_of(path))
    nm = trim(nm)
    e_sensor = trim(path)//' ['//nm//']'
    if (index(nm, 'package') > 0 .or. index(nm, 'psys') > 0) then
      e_scope = trim(nm)//' (RAPL accumulated counter: CPU package = whole socket)'
    else
      e_scope = trim(nm)//' (RAPL accumulated counter: one power domain only)'
    end if
    use_counter = .true.
  end function use_counter

  logical function use_power(path, hw_name)
    character(*), intent(in) :: path, hw_name
    integer(int64) :: uw
    use_power = .false.
    if (.not. read_i64(path, uw)) return
    e_kind = EK_POWER
    e_path = path
    e_w_prev = real(uw, real64)*1.0e-6_real64
    e_t_prev = energy_seconds()
    e_joule = 0.0_real64
    e_sensor = trim(path)//' ['//trim(hw_name)//']'
    e_scope = trim(hw_name)//' (instantaneous power in uW, integrated by trapezoid)'
    use_power = .true.
  end function use_power

  ! Find the sensor, zero accumulators and phases. Safe to call again.
  ! `ticks` = USER_HZ from /proc (default 100); ENERGY_TICKS also accepted.
  ! `sensor` = explicit path (counter or power file): the inside control, like
  ! mfi's mfi_force_gpu. Without it, ENERGY_SENSOR (the outside control, set by
  ! the caller or the launcher) is honored; without either, auto-discovery runs.
  ! NEVER aborts: without a sensor J stays 0 and the rest (cpu/io/phases) holds.
  subroutine energy_init(ticks, sensor)
    real(real64), intent(in), optional :: ticks
    character(*), intent(in), optional :: sensor
    character(len=256) :: env, nm, hw
    character(len=512) :: p, cand
    character(len=512) :: names(64)
    integer :: n, i, j
    logical :: found

    e_ticks = 100.0_real64
    env = ''
    call get_environment_variable('ENERGY_TICKS', env)
    if (len_trim(env) > 0) then
      read (env, *, iostat=i) e_ticks
      if (i /= 0 .or. e_ticks <= 0.0_real64) e_ticks = 100.0_real64
    end if
    if (present(ticks)) then
      if (ticks > 0.0_real64) e_ticks = ticks
    end if

    call sensor_reset()
    e_t_init = energy_seconds()
    e_cpu_init = energy_cpu_seconds()
    e_nphase = 0
    e_phases%label = ''
    e_phases%n = 0
    e_phases%j = 0.0_real64
    e_phases%wall_s = 0.0_real64
    e_phases%cpu_s = 0.0_real64
    e_phases%rd_mb = 0.0_real64
    e_phases%wr_mb = 0.0_real64
    e_phases%tokens = 0.0_real64

    ! (0) explicit override: the `sensor` argument first, ENERGY_SENSOR second.
    ! It forces a sensor (or a test file) and also switches measurement OFF by
    ! pointing at a missing path — an explicit request does not fall back to
    ! automatic discovery.
    env = ''
    if (present(sensor)) env = sensor
    if (len_trim(env) == 0) call get_environment_variable('ENERGY_SENSOR', env)
    if (len_trim(env) > 0) then
      if (index(env, 'power') > 0) then
        if (use_power(trim(env), basename_of(trim(env)))) then
          call base_reset()
          return
        end if
      else
        if (use_counter(trim(env))) then
          call base_reset()
          return
        end if
      end if
      e_scope = 'explicit sensor '//trim(env)//' (unreadable, energy stays 0)'
      call base_reset()
      return
    end if

    ! (a) powercap: accumulated counter; prefers package-*/psys (whole socket).
    names = ''
    call list_dir('/sys/class/powercap', names, n)
    p = ''
    do i = 1, n
      cand = '/sys/class/powercap/'//trim(names(i))//'/energy_uj'
      if (.not. path_exists(trim(cand))) cycle
      nm = ''
      if (read_txt(dirname_of(trim(cand))//'/name', nm)) nm = trim(nm)
      if (index(nm, 'package') > 0 .or. index(nm, 'psys') > 0) then
        p = trim(cand)
        exit
      end if
      if (len_trim(p) == 0) p = trim(cand)
    end do
    if (len_trim(p) > 0) then
      if (use_counter(trim(p))) then
        call base_reset()
        return
      end if
    end if

    ! (b) hwmon: instantaneous power, only for accepted names, in HW_NAMES order.
    names = ''
    call list_dir('/sys/class/hwmon', names, n)
    do j = 1, NHW
      do i = 1, n
        cand = '/sys/class/hwmon/'//trim(names(i))
        hw = ''
        if (.not. read_txt(trim(cand)//'/name', hw)) cycle
        if (trim(hw) /= trim(HW_NAMES(j))) cycle
        if (use_power(trim(cand)//'/power1_input', trim(hw))) then
          call base_reset()
          return
        end if
      end do
    end do

    ! (c) fallback without readdir: fixed list of known paths.
    do i = 1, 4
      select case (i)
      case (1); cand = '/sys/class/powercap/intel-rapl:0/energy_uj'
      case (2); cand = '/sys/class/powercap/intel-rapl:0:0/energy_uj'
      case (3); cand = '/sys/class/powercap/amd_energy/energy_uj'
      case (4); cand = '/sys/class/powercap/psys/energy_uj'
      end select
      if (use_counter(trim(cand))) then
        call base_reset()
        return
      end if
    end do
    do i = 0, 31
      write (cand, '(A,I0)') '/sys/class/hwmon/hwmon', i
      hw = ''
      if (.not. read_txt(trim(cand)//'/name', hw)) cycle
      found = .false.
      do j = 1, NHW
        if (trim(hw) == trim(trim(HW_NAMES(j)))) found = .true.
      end do
      if (.not. found) cycle
      if (use_power(trim(cand)//'/power1_input', trim(hw))) then
        call base_reset()
        return
      end if
    end do

    e_scope = 'no sensor found (powercap/hwmon absent or unreadable; energy=0)'
    call base_reset()
  end subroutine energy_init

  ! Zero the base of the open interval (called at init).
  subroutine base_reset()
    e_j_base = e_joule
    e_t_base = energy_seconds()
    e_cpu_base = energy_cpu_seconds()
    call energy_io_bytes(e_rd_base, e_wr_base)
  end subroutine base_reset

  subroutine sample()
    integer(int64) :: raw, uw
    real(real64) :: d, t, dt, w
    if (e_kind == EK_COUNTER) then
      if (.not. read_i64(e_path, raw)) return      ! transient read: do not invent J
      d = real(raw - e_last_raw, real64)
      if (d < 0.0_real64) then
        ! COUNTER WRAP: it rolled over -> add the range (when known). Without a
        ! range there is no way to know how much passed: ignore instead of guessing.
        if (e_range_uj > 0_int64) then
          d = d + real(e_range_uj, real64)
        else
          d = 0.0_real64
        end if
        ! Still negative after adding the range means the counter was RESET (driver
        ! reload, domain disabled), not wrapped. A reset is not energy: keep the
        ! accumulator flat so J never goes backwards.
        if (d < 0.0_real64) d = 0.0_real64
      end if
      e_joule = e_joule + d*1.0e-6_real64
      e_last_raw = raw
    else if (e_kind == EK_POWER) then
      if (.not. read_i64(e_path, uw)) return
      w = real(uw, real64)*1.0e-6_real64
      t = energy_seconds()
      dt = t - e_t_prev
      if (dt > 0.0_real64) then
        e_joule = e_joule + 0.5_real64*(e_w_prev + w)*dt
        e_t_prev = t
        e_w_prev = w
      end if
    end if
  end subroutine sample

  ! ============================================================= energy API
  logical function energy_ready()
    energy_ready = (e_kind /= EK_NONE)
  end function energy_ready

  ! J accumulated since init. Samples the sensor (counter: adds the delta; power:
  ! trapezoid integration) — calling it more often is more accurate, not costly.
  function energy_joules() result(j)
    real(real64) :: j
    call sample()
    j = e_joule
  end function energy_joules

  ! Rate since the PREVIOUS call to energy_watts() (on the first one, since init).
  ! Negative (bad reading) becomes 0: negative power does not exist.
  function energy_watts() result(w)
    real(real64) :: w, jn, tn, dt, j0
    jn = energy_joules()
    tn = energy_seconds()
    if (e_kind == EK_NONE) then
      w = 0.0_real64
      return
    end if
    if (e_t_watts < 0.0_real64) then
      j0 = 0.0_real64
      dt = tn - e_t_init
    else
      j0 = e_j_watts
      dt = tn - e_t_watts
    end if
    if (dt > 0.0_real64) then
      w = (jn - j0)/dt
    else
      w = 0.0_real64
    end if
    if (w < 0.0_real64) w = 0.0_real64
    e_t_watts = tn
    e_j_watts = jn
  end function energy_watts

  ! ================================================================= CPU / IO
  ! CPU % since init, against the threads the process has NOW:
  ! cpu_s / (wall_s * threads) * 100. (A saturated single thread gives ~100%.)
  function energy_cpu_percent() result(p)
    real(real64) :: p, wall, cpu
    integer :: nth
    p = 0.0_real64
    wall = energy_seconds() - e_t_init
    if (wall <= 0.0_real64) return
    cpu = energy_cpu_seconds() - e_cpu_init
    nth = energy_threads()
    if (nth < 1) nth = 1
    p = 100.0_real64*cpu/(wall*real(nth, real64))
  end function energy_cpu_percent

  ! Equivalent cores busy since init (cpu_s/wall_s).
  function energy_cores_busy() result(c)
    real(real64) :: c, wall, cpu
    c = 0.0_real64
    wall = energy_seconds() - e_t_init
    if (wall <= 0.0_real64) return
    cpu = energy_cpu_seconds() - e_cpu_init
    c = cpu/wall
  end function energy_cores_busy

  ! =============================================================== intervals
  ! Deltas since the last call (or since init). ADVANCES the base: it is the
  ! building block of energy_mark and what callers use per region.
  subroutine energy_interval(iv, tokens)
    type(energy_interval_t), intent(out) :: iv
    integer(int64), intent(in), optional :: tokens
    call interval_read(iv, tokens, .true.)
  end subroutine energy_interval

  ! The same delta WITHOUT advancing the base. Two uses:
  !   * a progress trace that must not disturb the per-region interval;
  !   * reading the numbers twice (report + output) without the second read
  !     seeing an empty interval.
  ! energy_mark() is energy_interval() + the phase accounting; energy_peek() is
  ! the read-only twin.
  subroutine energy_peek(iv, tokens)
    type(energy_interval_t), intent(out) :: iv
    integer(int64), intent(in), optional :: tokens
    call interval_read(iv, tokens, .false.)
  end subroutine energy_peek

  subroutine interval_read(iv, tokens, advance)
    type(energy_interval_t), intent(out) :: iv
    integer(int64), intent(in), optional :: tokens
    logical, intent(in) :: advance
    real(real64) :: jn, tn, cn, rdn, wrn
    jn = energy_joules()
    tn = energy_seconds()
    cn = energy_cpu_seconds()
    call energy_io_bytes(rdn, wrn)
    iv%j = max(0.0_real64, jn - e_j_base)
    iv%wall_s = max(0.0_real64, tn - e_t_base)
    iv%cpu_s = max(0.0_real64, cn - e_cpu_base)
    iv%rd_mb = max(0.0_real64, (rdn - e_rd_base))*1.0e-6_real64
    iv%wr_mb = max(0.0_real64, (wrn - e_wr_base))*1.0e-6_real64
    iv%threads = energy_threads()
    iv%cores_busy = 0.0_real64
    iv%cpu_pct = 0.0_real64
    if (iv%wall_s > 0.0_real64) then
      iv%cores_busy = iv%cpu_s/iv%wall_s
      iv%cpu_pct = 100.0_real64*iv%cpu_s/(iv%wall_s*real(iv%threads, real64))
    end if
    iv%tokens = 0.0_real64
    if (present(tokens)) iv%tokens = real(tokens, real64)
    iv%j_per_token = 0.0_real64
    iv%w_mean = 0.0_real64
    if (iv%wall_s > 0.0_real64) then
      iv%w_mean = iv%j/iv%wall_s
      if (iv%tokens > 0.0_real64) iv%j_per_token = iv%j/iv%tokens
    end if
    ! run-level context, filled here so a consumer never assembles it by hand
    iv%j_total = jn
    iv%sensor = e_sensor
    iv%scope = e_scope
    iv%kind = energy_kind_name()
    iv%self_measured = .true.
    if (advance) then
      e_j_base = jn
      e_t_base = tn
      e_cpu_base = cn
      e_rd_base = rdn
      e_wr_base = wrn
    end if
  end subroutine interval_read

  ! Close the open interval and accumulate it under `label` (the label names the
  ! phase that JUST ended). `iv` returns the closed interval; `tokens` feeds the
  ! phase's J/token.
  subroutine energy_mark(label, tokens, iv)
    character(*), intent(in) :: label
    integer(int64), intent(in), optional :: tokens
    type(energy_interval_t), intent(out), optional :: iv
    type(energy_interval_t) :: s
    character(len=MAX_LABEL) :: lab
    integer :: i, k
    call energy_interval(s, tokens)
    if (present(iv)) iv = s
    lab = adjustl(label)
    if (len_trim(lab) == 0) lab = 'unlabeled'
    k = 0
    do i = 1, e_nphase
      if (trim(e_phases(i)%label) == trim(lab)) then
        k = i
        exit
      end if
    end do
    if (k == 0) then
      if (e_nphase >= MAX_PHASES) then
        lab = 'other'                        ! never lose the measurement, just group
        do i = 1, e_nphase
          if (trim(e_phases(i)%label) == 'other') then
            k = i
            exit
          end if
        end do
        if (k == 0) then
          e_nphase = e_nphase + 1
          k = e_nphase
          e_phases(k)%label = trim(lab)
        end if
      else
        e_nphase = e_nphase + 1
        k = e_nphase
        e_phases(k)%label = trim(lab)
      end if
    end if
    e_phases(k)%n = e_phases(k)%n + 1
    e_phases(k)%j = e_phases(k)%j + s%j
    e_phases(k)%wall_s = e_phases(k)%wall_s + s%wall_s
    e_phases(k)%cpu_s = e_phases(k)%cpu_s + s%cpu_s
    e_phases(k)%rd_mb = e_phases(k)%rd_mb + s%rd_mb
    e_phases(k)%wr_mb = e_phases(k)%wr_mb + s%wr_mb
    e_phases(k)%tokens = e_phases(k)%tokens + s%tokens
  end subroutine energy_mark

  ! ================================================================= reports
  ! One key=value line per phase (plus a "total" line), in the format job logs
  ! already use. CPU and IO show up even without a sensor: the fallback has to be
  ! useful, not empty.
  subroutine energy_report(unit)
    integer, intent(in), optional :: unit
    integer :: u, i, nth
    real(real64) :: jt, wall, cpu, rd, wr
    character(len=:), allocatable :: pct, cores, jpt
    u = 6
    if (present(unit)) u = unit
    nth = max(1, energy_threads())
    jt = energy_joules()
    wall = energy_seconds() - e_t_init
    cpu = energy_cpu_seconds() - e_cpu_init
    call energy_io_bytes(rd, wr)
    pct = '0'
    cores = '0'
    if (wall > 0.0_real64) then
      pct = jnum(100.0_real64*cpu/(wall*real(nth, real64)), 2)
      cores = jnum(cpu/wall, 4)
    end if
    write (u, '(20A)') 'energy total J=', jnum(jt, 4), ' wall_s=', jnum(wall, 4), &
        ' cpu_s=', jnum(cpu, 4), ' cpu_pct=', pct, ' cores_busy=', cores, &
        ' rd_mb=', jnum(rd*1.0e-6_real64, 4), ' wr_mb=', jnum(wr*1.0e-6_real64, 4), &
        ' kind=', trim(energy_kind_name()), ' sensor="'//trim(e_sensor)//'"', &
        ' scope="'//trim(e_scope)//'"'
    do i = 1, e_nphase
      pct = '0'
      cores = '0'
      if (e_phases(i)%wall_s > 0.0_real64) then
        pct = jnum(100.0_real64*e_phases(i)%cpu_s/ &
                   (e_phases(i)%wall_s*real(nth, real64)), 2)
        cores = jnum(e_phases(i)%cpu_s/e_phases(i)%wall_s, 4)
      end if
      jpt = '0'
      if (e_phases(i)%tokens > 0.0_real64) jpt = jnum(e_phases(i)%j/e_phases(i)%tokens, 6)
      write (u, '(22A)') 'energy phase=', trim(e_phases(i)%label), ' n=', &
          jnum(real(e_phases(i)%n, real64), 0), ' J=', jnum(e_phases(i)%j, 4), &
          ' wall_s=', jnum(e_phases(i)%wall_s, 4), ' cpu_s=', jnum(e_phases(i)%cpu_s, 4), &
          ' cpu_pct=', pct, ' cores_busy=', cores, &
          ' rd_mb=', jnum(e_phases(i)%rd_mb, 4), &
          ' wr_mb=', jnum(e_phases(i)%wr_mb, 4), ' J_per_tok=', jpt
    end do
  end subroutine energy_report

  ! The same report as JSON (one line, for structured logs).
  function energy_report_json() result(js)
    character(len=:), allocatable :: js
    character(len=MAX_LABEL) :: lab
    real(real64) :: jt, wall, cpu, rd, wr, pct, cores
    integer :: i, nth
    jt = energy_joules()
    wall = energy_seconds() - e_t_init
    cpu = energy_cpu_seconds() - e_cpu_init
    call energy_io_bytes(rd, wr)
    nth = max(1, energy_threads())
    pct = merge(100.0_real64*cpu/(wall*real(nth, real64)), 0.0_real64, wall > 0)
    cores = merge(cpu/wall, 0.0_real64, wall > 0)
    js = '{"kind":"'//trim(energy_kind_name())//'","sensor":"'//trim(e_sensor)// &
         '","scope":"'//trim(e_scope)//'","self_measured":"'// &
         merge('1', '0', e_kind /= EK_NONE)//'","ticks":'//jnum(e_ticks, 0)// &
         ',"total":{"J":'//jnum(jt, 4)//',"wall_s":'//jnum(wall, 4)// &
         ',"cpu_s":'//jnum(cpu, 4)//',"cpu_pct":'//jnum(pct, 2)// &
         ',"cores_busy":'//jnum(cores, 4)//',"rd_mb":'//jnum(rd*1.0e-6_real64, 4)// &
         ',"wr_mb":'//jnum(wr*1.0e-6_real64, 4)//'},"phases":{'
    do i = 1, e_nphase
      lab = e_phases(i)%label
      if (i > 1) js = js//','
      js = js//'"'//trim(lab)//'":{"n":'//jnum(real(e_phases(i)%n, real64), 0)// &
           ',"J":'//jnum(e_phases(i)%j, 4)//',"wall_s":'//jnum(e_phases(i)%wall_s, 4)// &
           ',"cpu_s":'//jnum(e_phases(i)%cpu_s, 4)// &
           ',"cpu_pct":'//jnum(merge(100.0_real64*e_phases(i)%cpu_s/ &
                                    (e_phases(i)%wall_s*real(nth, real64)), &
                                    0.0_real64, e_phases(i)%wall_s > 0), 2)// &
           ',"cores_busy":'//jnum(merge(e_phases(i)%cpu_s/e_phases(i)%wall_s, &
                                        0.0_real64, e_phases(i)%wall_s > 0), 4)// &
           ',"rd_mb":'//jnum(e_phases(i)%rd_mb, 4)// &
           ',"wr_mb":'//jnum(e_phases(i)%wr_mb, 4)// &
           ',"tokens":'//jnum(e_phases(i)%tokens, 0)//'}'
    end do
    js = js//'}}'
  end function energy_report_json

  ! Number as text with VALID JSON guaranteed: Fortran's 'F0.4' writes '.0026'
  ! (no leading zero) and 'F0.0' writes '1.' -- both break a JSON parser. Here the
  ! leading zero and the trailing dot are fixed up.
  pure function jnum(x, ndec) result(s)
    real(real64), intent(in) :: x
    integer, intent(in) :: ndec
    character(len=:), allocatable :: s
    character(len=64) :: buf
    real(real64) :: ax
    ax = abs(x)
    if (ieee_is_nan(x) .or. ax > 1.0e15_real64) then
      s = '0'                                  ! NaN/infinity never reaches JSON
      return
    end if
    if (ndec <= 0) then
      write (buf, '(I0)') nint(x, int64)
    else if (ax > 0.0_real64 .and. ax < 1.0e-4_real64) then
      write (buf, '(ES13.4E2)') x
    else
      select case (ndec)
      case (2); write (buf, '(F0.2)') x
      case default; write (buf, '(F0.4)') x
      end select
    end if
    s = trim(adjustl(buf))
    ! No `.and.` here on purpose: Fortran does NOT short-circuit, so
    ! `len(s) > 1 .and. s(1:2) == '-.'` evaluates s(1:2) even for a one-character
    ! string ('0' or '1', which is what ONE-digit integers produce) and blows up
    ! under -fcheck=all. Nested IFs make the order explicit.
    if (len(s) > 0) then
      if (s(1:1) == '.') then
        s = '0'//s
      else if (len(s) > 2) then
        if (s(1:2) == '-.') s = '-0'//s(3:)
      end if
      if (s(len(s):len(s)) == '.') s = s(1:len(s) - 1)//'0'
    end if
  end function jnum

  function energy_sensor() result(s)
    character(len=:), allocatable :: s
    s = trim(e_sensor)
  end function energy_sensor

  function energy_scope() result(s)
    character(len=:), allocatable :: s
    s = trim(e_scope)
  end function energy_scope

  function energy_kind_name() result(s)
    character(len=:), allocatable :: s
    select case (e_kind)
    case (EK_COUNTER); s = 'counter'
    case (EK_POWER); s = 'power'
    case default; s = 'none'
    end select
  end function energy_kind_name

end module fortran_energy_mod
