!
! mpmt1.f08: A simple example of Fortran parallel processing using OpenMP
!
! STATUS:
!  Under development
! License:
!   Apache License, Version 2.0
! History:
!   2024/08/04 v0.1 Initial version based
! Author:
!   Masanori Itoh <masanori.itoh@gmail.com>
! BUILD:
!   * `$ gfortran -o mpmt1f08 mpmt1.f08 -fopenmp`
! TODO:
!   * Use other multi-processing frameworks
! REFERENCES:
!   * https://gcc.gnu.org/onlinedocs/gfortran/OpenMP.html
!
program mpmt1
  integer i, argc
  integer :: num_context = 3, duration = 5, mode = 0
  real :: start, end
  character(len=64) arg

  argc = command_argument_count()
  if (argc >= 1) then
     call get_command_argument(1, arg)
     read (arg,'(I64)') num_context
  end if
  if (argc >= 2) then
     call get_command_argument(2, arg)
     read (arg,'(I64)') duration
  end if

  print '("num_context: ", i0, " duration: ", i0)', num_context, duration

  call cpu_time(start)
  !$OMP PARALLEL DO
  do i=1,num_context
     call busy_loop(duration)
  end do
  !$OMP END PARALLEL DO
  call cpu_time(end)
  print *, 'cputime(sec.): ', end-start
end program mpmt1

subroutine busy_loop(duration)
  integer :: duration
  integer :: count, count_rate, count_max
  integer :: start

  call system_clock(count, count_rate, count_max)
  start= count
  do
     call system_clock(count, count_rate, count_max)
     if ((count - start) / count_rate >= duration) then
        write (*,*) 'Expired'
        return
     end if
  end do
end subroutine busy_loop
