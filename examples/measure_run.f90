! measure_run.f90 — the 5-line usage, runnable.
!
!   fpm run --example measure_run
!
! It times three phases of a synthetic workload (setup, work, IO) and prints both
! report formats. Run it on a laptop and on a cluster node and the numbers are
! comparable: J is what the hardware counter says, cpu_s/wall_s is what the kernel
! says, and nothing here wraps the process from the outside.
program measure_run
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use fortran_energy_mod
  implicit none

  type(energy_interval_t) :: iv
  real(real64), allocatable :: big(:)
  real(real64) :: sink, x, w_mean
  integer :: i, u

  call energy_init()
  write (*, '(2A)') 'sensor: ', energy_sensor()
  write (*, '(2A)') 'scope : ', energy_scope()

  ! ---- phase 1: setup (allocate + fill)
  allocate (big(2000000))
  do i = 1, size(big)
    big(i) = real(i, real64)*1.0e-6_real64
  end do
  call energy_mark('setup')

  ! ---- phase 2: work (a real CPU loop; the result is used below)
  sink = 0.0_real64
  do i = 1, 4000000
    x = big(1 + mod(i, size(big)))*1.0e-6_real64
    sink = sink + sin(x)*cos(x)
  end do
  call energy_mark('work', tokens=1024_int64, iv=iv)
  w_mean = 0.0_real64
  if (iv%wall_s > 0.0_real64) w_mean = iv%j/iv%wall_s
  write (*, '(A,F0.4,A,F0.6,A,F0.4,A,F0.4,A,F0.4)') &
      'work: J=', iv%j, ' J/token=', iv%j_per_token, ' W_mean=', w_mean, &
      ' cpu_s=', iv%cpu_s, ' cores_busy=', iv%cores_busy

  ! ---- phase 3: IO (write the buffer out and read it back)
  open (newunit=u, file='measure_run.bin', access='stream', form='unformatted', &
        status='replace')
  write (u) big
  close (u)
  open (newunit=u, file='measure_run.bin', access='stream', form='unformatted', &
        status='old')
  read (u) big
  close (u)
  deallocate (big, stat=i)
  call energy_mark('io')

  write (*, '(A,F0.6)') 'sink (keeps the loop from being optimized away): ', sink
  write (*, '(A)') ''
  call energy_report()                       ! one key=value line per phase
  write (*, '(2A)') 'json: ', energy_report_json()

end program measure_run
