!-----------------------------------------------------------------------------
!
! OpenMP stubs library -- For compiling OpenMP programs in serial mode.
!
! This stubs library supports compilers which lack a clean
! built-in method for compiling an OpenMP fortran program in
! pure serial mode, such as gfortran 12.
!
! 2023-mar-27	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from stubs.f90 in openmp.org V5.2 code distribution.
!
! This version provides a replacement for the standard "omp_lib"
! fortran 90 module in OpenMP.  This is a minimal version which
! provides only the limited number of OpenMP query functions that
! are needed for the NOAA/NCEP AQM bias correction project.
!
! This is a special stubs version which returns nonsensical
! function results, rather than attempting to emulate actual
! OpenMP behavior with a single available processor.  The
! purpose is diagnostic identification, not emulation.
!
! Do not use this module with any program that relies on a
! meaningful result from any OpenMP inquiry function.
!
! For full OpenMP emulation, please use one of the standard
! emulating stubs modules, such as this one from OpenMP.org:
!
!   https://github.com/OpenMP/sources/tree/main/stubs/stubs.f90
!
! This module is not needed for programs which use only simple
! default OpenMP constructs, such as !$omp parallel do.  This
! module is only needed for programs that use actual OpenMP API
! functions, such as omp_get_num_threads.
!
! If the compiler provides its own OpenMP stubs mechanism, such
! as Intel fortran, then please use that method rather than this
! external stubs module.
!
!-----------------------------------------------------------------------------

module omp_lib
contains

subroutine omp_display_env (verbose)
  logical, intent(in) :: verbose
  if (verbose) continue				! squelch pedantic warning
  print *
  print *, 'OPENMP DISPLAY ENVIRONMENT:'
  print *, '  This program was compiled with an OpenMP stubs module.'
  print *, '  Presumably this program was compiled in serial mode,'
  print *, '  and OpenMP is not active.'
  print *, '  Stubs module ID = omp_lib_stubs.f90, version 2023-mar-27'
end subroutine omp_display_env

integer function omp_get_num_threads ()
  omp_get_num_threads = -99
end function

integer function omp_get_max_threads ()
  omp_get_max_threads = -99
end function

integer function omp_get_thread_num ()
  omp_get_thread_num = -99
end function

integer function omp_get_num_procs ()
  omp_get_num_procs = -99
end function

integer function omp_get_thread_limit ()
  omp_get_thread_limit = -99
end function

integer function omp_get_num_teams ()
  omp_get_num_teams = -99
end function

integer function omp_get_team_num ()
  omp_get_team_num = -99
end function

integer function omp_get_max_teams ()
  omp_get_max_teams = -99
end function omp_get_max_teams

integer function omp_get_teams_thread_limit ()
  omp_get_teams_thread_limit = -99
end function omp_get_teams_thread_limit

end module omp_lib
