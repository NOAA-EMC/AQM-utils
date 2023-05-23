!------------------------------------------------------------------------------
!
! print_library_info.f90 -- Print version information for support libraries.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2020-nov-08	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!
!------------------------------------------------------------------------------

module print__library_info
contains

subroutine print_library_info

   use bufrlib
   use netcdf
   implicit none

   character vstring*200

   print *
   call bvers (vstring)
   print '(2a)', 'BUFRLIB library version = ', trim (vstring)

   print '(2a)', 'Netcdf library version  = ', trim (nf90_inq_libvers())

end subroutine print_library_info
end module print__library_info
