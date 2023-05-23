!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! 2012-apr-20	windDirError.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!
! 2014-feb-28	wind_dir_error.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2014-mar-10	Change function to subroutine, for better interface checking.
!
! 2020-nov-15	Replace old compiler bug workarounds with modern array code.
!		Enable automatic reallocation for output array.
!
!-----------------------------------------------------------------------------

module wind__dir_error
  use config, only : dp

  private				! visibility controls
  public wind_dir_error			! standard generic access

  interface wind_dir_error
    module procedure wind_dir_error_scalar, wind_dir_error_2d
  end interface

contains

!-----------------------------------------------------------------------------

subroutine wind_dir_error_scalar (dir1, dir2, output)
  implicit none

  real(dp), intent (in)  :: dir1, dir2	! input wind directions to compare
  real(dp), intent (out) :: output	! result wind direction, degrees 0-360

  real(dp) sol1, sol2			! local variables

  sol1   = abs (dir1 - dir2)		! minimum absolute angular separation
  sol2   = abs (sol1 - 360)		! on the 360 degree circle
  output = min (sol1, sol2)

end subroutine wind_dir_error_scalar

!-----------------------------------------------------------------------------

subroutine wind_dir_error_2d (dir1, dir2, output)
  implicit none

  real(dp), intent (in)               :: dir1(:,:), dir2(:,:)
  real(dp), intent (out), allocatable :: output(:,:)

  real(dp), allocatable :: sol1(:,:), sol2(:,:)		! local vars

  sol1   = abs (dir1 - dir2)		! all are array expressions
  sol2   = abs (sol1 - 360)
  output = min (sol1, sol2)

end subroutine wind_dir_error_2d

end module wind__dir_error
