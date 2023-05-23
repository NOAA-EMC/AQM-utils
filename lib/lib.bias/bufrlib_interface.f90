!-----------------------------------------------------------------------------
!
! bufrlib_interface.f90 -- Fortran 90 interfaces for F77 BUFRLIB routines.
!
! 2014-may-24	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Minimal version.  Include only routines needed for the
!		  NOAA NCO/ARL/PSD bias correction system.
! 2014-jun-08	Simple workaround for sequence problem in ifort 14.0.1.
!
! 2020-nov-08	Add routines bvers, nemdefs (lib version, and units string).
!		Minimum required BUFRLIB version is now 11.3.0.
!
! Interfaces are adapted directly from BUFRLIB source code.
! See BUFRLIB for subroutine documentation.
!
!-----------------------------------------------------------------------------

module bufrlib
implicit none
interface

   subroutine bvers (cverstr)
      character(*),     intent (out  ) :: cverstr
   end subroutine bvers

   subroutine dxdump (lunit, ldxot)
      integer,          intent (in   ) :: lunit, ldxot
   end subroutine dxdump

   function getbmiss () result (xmiss)
      double precision                 :: xmiss
   end function getbmiss

   subroutine nemdefs (lunit, nemo, celem, cunit, iret)
      integer,          intent (in   ) :: lunit
      character(*),     intent (in   ) :: nemo
      character(*),     intent (out  ) :: celem
      character(*),     intent (out  ) :: cunit
      integer,          intent (out  ) :: iret
   end subroutine nemdefs

   subroutine openbf (lunit, io, lundx)
      integer,          intent (in   ) :: lunit
      character(*),     intent (in   ) :: io
      integer,          intent (in   ) :: lundx
   end subroutine openbf

   subroutine readmg (lunxx, subset, jdate, iret)
      integer,          intent (in   ) :: lunxx
      character(8),     intent (out  ) :: subset
      integer,          intent (out  ) :: jdate, iret
   end subroutine readmg

   subroutine readsb (lunit, iret)
      integer,          intent (in   ) :: lunit
      integer,          intent (out  ) :: iret
   end subroutine readsb

   subroutine ufbint (lunin, usr, i1, i2, iret, str)
      integer,          intent (in   ) :: lunin
      integer,          intent (in   ) :: i1, i2	! ifort 14.0.1 breaks
      double precision, intent (inout) :: usr(i1,i2)	! if these 2 reversed
      integer,          intent (out  ) :: iret
      character(*),     intent (in   ) :: str
   end subroutine ufbint

   subroutine ufdump (lunit, luprt)
      integer,          intent (in   ) :: lunit, luprt
   end subroutine ufdump

end interface
end module bufrlib
