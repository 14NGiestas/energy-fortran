! test/test_energy.f90 — energy/CPU/IO/phase measurement without depending on hardware.
!
! What this test proves:
!   1. WITHOUT A SENSOR nothing breaks: energy_init() leaves ready=.false., energy
!      stays 0.0 and EVEN SO cpu_s/cores_busy/cpu_pct are measured and reported
!      (the fallback has to be useful, not empty);
!   2. with a KNOWN COUNTER (a FAKE sensor written by the test itself) the
!      arithmetic is exact: J = accumulated delta_uj * 1e-6, monotone, and counter
!      WRAP (a reading smaller than the previous one) adds max_energy_range_uj;
!   3. with the machine's REAL sensor (when present): energy_joules() never
!      decreases and energy_watts() lands in a plausible band (0..500 W);
!   4. PHASES accumulate: two marks with the same label give n=2 and sum
!      J/wall/cpu;
!   5. the report (key=value and JSON) comes out with the expected fields;
!   6. energy_peek() reads the open interval WITHOUT consuming it (the mark after
!      a peek still sees the whole span) -- that is what a progress trace needs.
!
! Sensor control is an argument, never the environment: energy_init(sensor=...)
! points at a fake counter, a missing path (forced degradation), or nothing
! (auto-discovery). The ENERGY_SENSOR environment variable offers the same
! override from the outside (CI/shell); the test never writes the environment.
! Runs in <5 s.

program test_energy
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use fortran_energy_mod
  implicit none

  character(len=*), parameter :: DIR = 'build/energy_test'
  integer :: fail_count = 0
  real(real64) :: s

  ! Portable directory creation: `mkdir -p` is not valid on Windows cmd, and a
  ! plain `mkdir` fails when the directory already exists, so the exit status
  ! is ignored on purpose. The parent (build/) always exists under fpm.
  call make_dir(DIR)

  print '(A)', '== energy: fake sensor, no sensor and the real sensor =='
  call test_no_sensor()
  call test_fake_counter()
  call test_real_sensor()
  call test_phases_and_report()
  call test_peek_does_not_consume()

  print '(A,I0,A)', '===', fail_count, ' failures ==='
  if (fail_count > 0) call exit(1)

contains

  subroutine check(cond, label)
    logical, intent(in) :: cond
    character(*), intent(in) :: label
    if (cond) then
      print '(A,A)', '  ok    ', label
    else
      print '(A,A)', '  FAIL: ', label
      fail_count = fail_count + 1
    end if
  end subroutine check

  ! ------------------------------------------- 5. peek() is not destructive
  ! A progress trace wants "how much so far" without closing the interval that the
  ! next mark will report. With the fake counter the arithmetic is exact: two
  ! 0.5 J deltas around a peek must both land in the interval the mark closes.
  subroutine test_peek_does_not_consume()
    type(energy_interval_t) :: pk, iv
    character(len=:), allocatable :: cnt
    print '(A)', '-- 5. energy_peek(): read without consuming the interval'
    cnt = DIR//'/energy_uj'
    call write_counter(cnt, 0_int64)
    call write_counter(DIR//'/max_energy_range_uj', 1000000000_int64)
    call energy_init(sensor=cnt)
    call check(energy_ready(), 'peek: fake counter is live')
    call write_counter(cnt, 500000_int64)         ! +0.5 J
    call energy_peek(pk)
    call check(abs(pk%j - 0.5_real64) < 1.0e-9_real64, &
        'peek: J = 0.5 J of the open interval ('//trim(fmt(pk%j, 9))//')')
    call write_counter(cnt, 1000000_int64)        ! +0.5 J more
    call energy_mark('peeked', iv=iv)
    call check(abs(iv%j - 1.0_real64) < 1.0e-9_real64, &
        'mark after peek sees BOTH deltas = 1.0 J ('//trim(fmt(iv%j, 9))//')')
    call energy_peek(pk)
    call check(pk%j == 0.0_real64 .and. pk%cpu_s >= 0.0_real64, &
        'peek after mark: empty interval, cpu_s still measured')
    ! The record is self-describing: run-level context travels WITH the interval,
    ! so a consumer (results file, CSV, log) needs no second type and no manual copy.
    call check(iv%j_total >= iv%j .and. abs(iv%w_mean - iv%j/iv%wall_s) < 1.0e-9_real64, &
        'record: j_total >= j and w_mean = j/wall_s')
    call check(len_trim(iv%sensor) > 0 .and. len_trim(iv%scope) > 0 .and. &
               len_trim(iv%kind) > 0 .and. iv%self_measured, &
        'record: sensor/scope/kind present and self_measured=.true.')
    call check(trim(iv%kind) == trim(energy_kind_name()), &
        'record: kind matches energy_kind_name()')
  end subroutine test_peek_does_not_consume

  ! ------------------------------------------------------------- utilities
  subroutine make_dir(dir)
    character(*), intent(in) :: dir
    integer :: st
    call execute_command_line('mkdir '//dir, exitstat=st)
  end subroutine make_dir

  subroutine write_counter(path, v)
    character(*), intent(in) :: path
    integer(int64), intent(in) :: v
    integer :: u, ios
    open (newunit=u, file=path, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      call check(.false., 'could not write the fake sensor '//path)
      return
    end if
    write (u, '(I0)') v
    close (u)
  end subroutine write_counter

  ! Real CPU work: the result is USED (otherwise -O2 deletes the loop and there is
  ! no CPU time to measure).
  subroutine burn(n)
    integer, intent(in) :: n
    real(real64) :: x
    integer :: i
    do i = 1, n
      x = real(i, real64)*1.0e-7_real64
      s = s + sin(x)*cos(x)
    end do
  end subroutine burn

  ! ------------------------------------------------------------ 1. no sensor
  subroutine test_no_sensor()
    real(real64) :: j0, j1, w, cpu0, cpu1
    character(len=:), allocatable :: js
    print '(A)', '-- 1. no sensor: neutral energy, CPU/IO keep working'
    call energy_init(sensor='/nonexistent/energy_uj')
    call check(.not. energy_ready(), 'no sensor: energy_ready() = .false.')
    call check(energy_kind_name() == 'none', 'no sensor: kind = none')
    j0 = energy_joules()
    call check(j0 == 0.0_real64, 'no sensor: energy_joules() = 0.0')
    w = energy_watts()
    call check(w == 0.0_real64, 'no sensor: energy_watts() = 0.0')
    cpu0 = energy_cpu_seconds()
    s = 0.0_real64
    call burn(2000000)
    call check(s > 0.0_real64, 'CPU loop ran (result used)')
    cpu1 = energy_cpu_seconds()
    j1 = energy_joules()
    call check(j1 == 0.0_real64, 'no sensor: J stays 0.0 after the work')
    call check(cpu1 >= cpu0, 'no sensor: cpu_s is still measured')
    ! /proc ticks are 10 ms: over a short wall span the quantized cpu_s can read
    ! a tick above wall (e.g. 30 ms of ticks over 25 ms of wall), so the bound
    ! carries a +10pp tolerance. It still catches insane values, which is the point.
    call check(energy_cpu_percent() >= 0.0_real64 .and. &
               energy_cpu_percent() <= 100.0_real64*real(energy_threads(), real64) + 10.0_real64, &
        'no sensor: cpu_pct within [0, 100*nthreads] (+tick tolerance)')
    call check(energy_cores_busy() >= 0.0_real64, 'no sensor: cores_busy >= 0')
    js = energy_report_json()
    call check(index(js, '"kind":"none"') > 0 .and. index(js, '"cpu_s"') > 0, &
        'no sensor: the JSON reports kind=none plus the CPU fields')
    call check(index(energy_sensor(), 'none') >= 0, 'no sensor: energy_sensor() answers')
  end subroutine test_no_sensor

  ! ------------------------------------------- 2. fake counter (exact deltas)
  subroutine test_fake_counter()
    character(len=*), parameter :: cnt = DIR//'/energy_uj'
    character(len=*), parameter :: rng = DIR//'/max_energy_range_uj'
    integer(int64), parameter :: TICKS = 20_int64, DELTA = 500000_int64  ! 0.5 J
    integer(int64), parameter :: RANGE = 1000000000_int64               ! 1000 J
    integer(int64) :: raw, i
    real(real64) :: j, jprev, jexp
    real(real64) :: w
    integer :: u, ios
    print '(A)', '-- 2. fake counter: exact J, monotone, with WRAP'
    call write_counter(cnt, 1000_int64)
    open (newunit=u, file=rng, status='replace', action='write', iostat=ios)
    write (u, '(I0)') RANGE
    close (u)
    call energy_init(sensor=cnt)
    call check(energy_ready(), 'fake counter: ready')
    call check(energy_kind_name() == 'counter', 'fake counter: kind = counter')
    call check(index(energy_sensor(), 'energy_uj') > 0, 'fake counter: sensor = path')
    jprev = energy_joules()
    call check(jprev == 0.0_real64, 'fake counter: first J = 0 (baseline at init)')
    raw = 1000_int64
    do i = 1, TICKS
      raw = raw + DELTA
      call write_counter(cnt, raw)
      j = energy_joules()
      jexp = real(i*DELTA, real64)*1.0e-6_real64
      call check(abs(j - jexp) < 1.0e-9_real64 .and. j >= jprev, &
          'fake counter: J = '//trim(fmt(j, 6))//' J (expected '//trim(fmt(jexp, 6))//')')
      jprev = j
    end do
    ! WRAP: a physical roll over. The counter is near the end of its range, then
    ! restarts at 50 -> the delta is (50 - (RANGE-5)) + RANGE = 55 uJ.
    call write_counter(cnt, RANGE - 5_int64)
    jprev = energy_joules()
    call write_counter(cnt, 50_int64)
    j = energy_joules()
    call check(j > jprev .and. abs((j - jprev) - 55.0e-6_real64) < 1.0e-9_real64, &
        'fake counter: WRAP added the range (delta='//trim(fmt(j - jprev, 9))//' J, expected 0.000000055)')
    ! A backwards jump LARGER than the range cannot be a wrap: it is a driver
    ! reset (or a counter whose width changed and whose range file is stale). A
    ! reset is not energy, so J stays FLAT -- never negative, never invented.
    call write_counter(cnt, 2000000000_int64)      ! bogus: bigger than the range
    jprev = energy_joules()
    call write_counter(cnt, 7_int64)
    j = energy_joules()
    call check(abs(j - jprev) < 1.0e-12_real64, &
        'fake counter: backwards jump > range does not move J (delta='// &
        trim(fmt(j - jprev, 12))//' J)')
    w = energy_watts()
    call check(w >= 0.0_real64, 'fake counter: watts >= 0')
    ! an unreadable override must not explode nor invent J
    call write_counter(cnt, 0_int64)
    call energy_init(sensor=DIR//'/does_not_exist')
    call check(.not. energy_ready() .and. energy_joules() == 0.0_real64, &
        'invalid override: neutral, no crash')
  end subroutine test_fake_counter

  ! ------------------------------------------------------ 3. real sensor
  subroutine test_real_sensor()
    real(real64) :: j1, j2, j3, w
    integer :: n
    print '(A)', '-- 3. the machine s real sensor (when present)'
    call energy_init()
    if (.not. energy_ready()) then
      print '(A)', '  skip  no sensor on this machine (powercap/hwmon)'
      return
    end if
    print '(3A)', '  (sensor: ', energy_sensor(), ')'
    print '(3A)', '  (scope : ', energy_scope(), ')'
    j1 = energy_joules()
    s = 0.0_real64
    call burn(3000000)
    j2 = energy_joules()
    call burn(3000000)
    j3 = energy_joules()
    call check(j2 >= j1 .and. j3 >= j2, 'real sensor: energy_joules() never decreases')
    call check(j3 >= j1, 'real sensor: total J >= 0')
    w = energy_watts()
    call check(w >= 0.0_real64 .and. w <= 500.0_real64, &
        'real sensor: watts within [0,500] ('//trim(fmt(w, 2))//' W)')
    n = energy_threads()
    call check(n >= 1, 'real sensor: threads >= 1')
    call check(energy_cpu_seconds() >= 0.0_real64, 'real sensor: cpu_s >= 0')
  end subroutine test_real_sensor

  ! ------------------------------------------------------------- 4. phases
  subroutine test_phases_and_report()
    type(energy_interval_t) :: iv
    character(len=:), allocatable :: js
    character(len=512) :: line
    integer :: u, ios
    logical :: saw_a, saw_tot
    print '(A)', '-- 4. phases (marks) accumulate and the report comes out'
    call energy_init()
    s = 0.0_real64
    call burn(1000000)
    call energy_mark('phase_a', tokens=1024_int64, iv=iv)
    call check(iv%wall_s > 0.0_real64 .and. iv%cpu_s >= 0.0_real64, &
        'mark returns the interval (wall>0, cpu>=0)')
    call burn(1000000)
    call energy_mark('phase_a', tokens=2048_int64)
    call burn(500000)
    call energy_mark('phase_b')
    js = energy_report_json()
    call check(index(js, '"phase_a"') > 0 .and. index(js, '"n":2') > 0, &
        'JSON: phase_a accumulated n=2')
    call check(index(js, '"phase_b"') > 0, 'JSON: phase_b present')
    call check(index(js, '"cpu_s"') > 0 .and. index(js, '"cores_busy"') > 0, &
        'JSON: cpu_s and cores_busy present')
    call check(index(js, '"tokens":3072') > 0, 'JSON: phase tokens summed (1024+2048)')
    ! One-digit numbers must not break the JSON (this was a real bug: '.0026' and
    ! '1.' are not valid JSON, and a one-character field blew up under -fcheck).
    call check(index(js, '"n":1') > 0 .and. index(js, '":.') == 0 .and. &
               index(js, ':.') == 0 .and. index(js, '1.}') == 0, &
        'JSON: numbers are well formed (no leading dot, no trailing dot)')
    ! the key=value report prints one line per phase (scratch file)
    open (newunit=u, file=DIR//'/report.txt', status='replace', action='write', iostat=ios)
    call energy_report(u)
    close (u)
    saw_a = .false.
    saw_tot = .false.
    open (newunit=u, file=DIR//'/report.txt', status='old', action='read', iostat=ios)
    do
      read (u, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (index(line, 'energy total') == 1) saw_tot = .true.
      if (index(line, 'energy phase=phase_a') == 1 .and. index(line, 'n=2') > 0) saw_a = .true.
    end do
    close (u)
    call check(saw_tot .and. saw_a, 'report: total line + phase_a with n=2')
  end subroutine test_phases_and_report

  pure function fmt(x, ndec) result(t)
    real(real64), intent(in) :: x
    integer, intent(in) :: ndec
    character(len=:), allocatable :: t
    character(len=40) :: b
    select case (ndec)
    case (12); write (b, '(F0.12)') x
    case (9); write (b, '(F0.9)') x
    case (6); write (b, '(F0.6)') x
    case (2); write (b, '(F0.2)') x
    case default; write (b, '(F0.4)') x
    end select
    t = trim(adjustl(b))
  end function fmt

end program test_energy
