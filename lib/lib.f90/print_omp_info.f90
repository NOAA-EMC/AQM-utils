!-----------------------------------------------------------------------------
!
! print_omp_info.f90 -- Print OpenMP diagnostics.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! program for CMAQ forecast outputs.
!
! 2023-mar-27	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
! This version must be compiled with either the compiler's OpenMP
! option, or an alternate OpenMP stubs library.
!
!-----------------------------------------------------------------------------

module print__omp_info
contains

subroutine print_omp_info
   use omp_lib
   implicit none

   character vstring*20
   integer vstat

!-----------------------

   print *
   print '(9a)', 'Environment variables:'

   call get_environment_variable ('OMP_NUM_TEAMS',    vstring, status=vstat)
   if (vstat == 1) vstring = '(not set)'
   print '(9a)',   'OMP_NUM_TEAMS        = ', trim (vstring)

   call get_environment_variable ('OMP_NUM_THREADS',  vstring, status=vstat)
   if (vstat == 1) vstring = '(not set)'
   print '(9a)',   'OMP_NUM_THREADS      = ', trim (vstring)

   call get_environment_variable ('OMP_THREAD_LIMIT', vstring, status=vstat)
   if (vstat == 1) vstring = '(not set)'
   print '(9a)',   'OMP_THREAD_LIMIT     = ', trim (vstring)

   call get_environment_variable ('OMP_DISPLAY_ENV',  vstring, status=vstat)
   if (vstat == 1) vstring = '(not set)'
   print '(9a)',   'OMP_DISPLAY_ENV      = ', trim (vstring)

!-----------------------

   print *
   print '(a,i0)', 'OMP status functions:'
   print '(a,i0)', 'omp_get_num_procs    = ', omp_get_num_procs   ()

   print *
   print '(a,i0)', 'omp_get_thread_num   = ', omp_get_thread_num  ()
   print '(a,i0)', 'omp_get_num_threads  = ', omp_get_num_threads ()
   print '(a,i0)', 'omp_get_max_threads  = ', omp_get_max_threads ()
   print '(a,i0)', 'omp_get_thread_limit = ', omp_get_thread_limit ()

   print *
   print '(a,i0)', 'omp_get_num_teams    = ', omp_get_num_teams   ()
   print '(a,i0)', 'omp_get_max_teams    = ', omp_get_max_teams   ()

!-----------------------

!! Intel fortran accepts this for generic serial compile, but not gfortran 12.
!!
!!   print *
!!   print '(a,i0)', 'Fortran images:'
!!   print '(a,i0)', 'THIS_IMAGE           = ', this_image ()
!!   print '(a,i0)', 'NUM_IMAGES           = ', num_images ()

!-----------------------

   call omp_display_env (verbose=.true.)

end subroutine print_omp_info
end module print__omp_info
