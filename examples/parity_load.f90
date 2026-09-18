! examples/parity_load.f90 — fixed CPU load for N seconds, for parity checks.
!
! Runs a reproducible busy loop for N seconds (default 5) while measuring
! itself with fortran_energy, then prints one parseable line:
!
!   PARITY internal_j=<J> wall_s=<s> cpu_s=<s> cores_busy=<c> sensor="<s>" kind="<s>"
!
! tools/parity.py runs THIS binary as a child while sampling the same sensor
! from the outside, so one run yields both the in-process and the external
! number. bin/parity_check.sh compares them.
program parity_load
  use, intrinsic :: iso_fortran_env, only: real64
  use fortran_energy_mod
  implicit none

  type(energy_interval_t) :: iv
  real(real64) :: seconds, t0, sink, x
  character(len=32) :: arg
  integer :: i, ios

  seconds = 5.0_real64
  if (command_argument_count() >= 1) then
    call get_command_argument(1, arg)
    read (arg, *, iostat=ios) seconds
    if (ios /= 0 .or. seconds <= 0.0_real64) seconds = 5.0_real64
  end if

  call energy_init()
  t0 = energy_seconds()
  sink = 0.0_real64
  ! Busy loop: real arithmetic whose result is used below, so the optimizer
  ! cannot delete it. Single-threaded by design (one core busy).
  do
    do i = 1, 200000
      x = real(i, real64)*1.0e-7_real64
      sink = sink + sin(x)*cos(x)
    end do
    if (sink > 1.0e30_real64) sink = 0.0_real64  ! never true; keeps sink live
    if (energy_seconds() - t0 >= seconds) exit
  end do

  call energy_interval(iv)
  write (*, '(A,F0.4,A,F0.4,A,F0.4,A,F0.4,A,A,A,A,A)') &
      'PARITY internal_j=', iv%j, ' wall_s=', iv%wall_s, &
      ' cpu_s=', iv%cpu_s, ' cores_busy=', iv%cores_busy, &
      ' sensor="', trim(iv%sensor), '" kind="', trim(iv%kind), '"'
  write (*, '(A,F0.6)') 'PARITY sink=', sink
end program parity_load
