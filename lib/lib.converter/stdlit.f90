!------------------------------------------------------------------------------
!
! stdlit -- Standard literal names for Fortran programming.
!
! Rev	Date		Notes
! 1.00	2001-aug-27	Original version, adapted from gsc v2.11
! 1.01	2001-aug-31	Change from include file to module, f90 free format
! 1.02	2009-feb-19	Add leftmost, rightmost.
! 1.03	2011-oct-27	Add backward. "back" is deprecated, but too widespread.
! 1.04	2014-feb-11	Add pi (math constant) in double precision.
!
!------------------------------------------------------------------------------

module stdlit

   character space, tab, lf, cr, ctrl_z
   parameter (space=' ', tab=char(9), lf=char(10))
   parameter (cr=char(13), ctrl_z=char(26))

   integer normal, soft, fail, eof, fatal	! generic status values
   parameter (normal=1, soft=2, fail=3, eof=4, fatal=5)

   integer required, optional, prohibited	! generic mode vaues
   parameter (required=1, optional=2, prohibited=3)

   logical, parameter ::   &
      forward   = .false., &
      back      = .true.,  &
      backward  = .true.,  &
      leftmost  = .false., &
      rightmost = .true.		! directions for index function

! Math constants.

   double precision, parameter :: pi = 4 * atan (1d0)	! should be accurate
   							! to last bit

end module stdlit
