!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! stdev = stdTNcirc(array, vmiss,var_set,isCircular)
! Calculates the stdev of 'array', discounting elements of 'vmiss'
!
! Author: Thomas Nippen
! Revisions:
! - Badrinath Nagarajan Apr 2011 (NCAR/RAL)
! - Converted to embedded MATLAB to facilitate conversion to C
! - !b refers to modifications done by Badri.
!
! 2012-apr-20	stdTNcirc.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used by Djalalova and Wilczak, NOAA/ESRL/PSD3, for
!		  development of CMAQ bias correction method.
!
! 2014-mar-02	stdevTNcirc.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Replace array compacting with vector subscripting, for
!		  efficiency and simplification.
!		Remove var_set parameter, compute results over the whole
!		  var dimension (all variables).
!		Change from function to subroutine, to get automatic shape
!		  checking for the result array.
! 2014-jun-23	Minor.  Switch is_circular to type logical.
! 2014-jul-15	Guard against negative sqrt input from roundoff error.
!
! Input:   array(X,V) = data series for multiple variables V.
!	   vmiss = missing value code for both input and output.
!	   is_circular(V) = flags for type of each variable.
!		F = normal variable, T = angular position variables.
!
! Notes:
!
! Data for angular position variables, such as wind direction,
! are in units of degrees, e.g. 0 to 360.  Angles outside of 0 to
! 360 are handled correctly, interpreted modulo 360.
!
!-----------------------------------------------------------------------------

module stdev_TNcirc
contains

subroutine stdevTNcirc (array, vmiss, is_circular, stdev_out)

  use config, only : dp
  use stdlit, only : pi
  use std__dev
  implicit none

  real(dp), intent (in ) :: array(:,:)		! multi var data series (X,V)
  real(dp), intent (in ) :: vmiss		! missing value in data
  logical,  intent (in ) :: is_circular(:)	! T=angular variable, 0-360

  real(dp), intent (out) :: stdev_out(:)	! result, std dev for each var

! Local variables.

  integer v, x, nvars, nx, nx_valid
  real(dp) e, s, c

  real(dp) mean, variance			! unused std_dev output vars

  integer,  allocatable :: xind(:)		! indices of valid x positions
  real(dp), allocatable :: dir_rad(:)
  logical,  allocatable :: valid(:)

! Program parameters.

  real(dp), parameter :: rad2deg = pi / 180	   ! conversion factor
  real(dp), parameter :: b = 2 / sqrt (3d0) - 1    ! originally b = 0.1547

! Redundant parameter for std_dev library routine.
! Require 100% data present for valid standard deviations.

  real(dp), parameter :: std_thresh = 1.0

!------------------------------------------------------
! Exclude data rows with one or more missing values.
!------------------------------------------------------

  nx    = size (array, 1)			! get input dimensions
  nvars = size (array, 2)

  allocate (valid(nx))				! allocate valid flags array
  allocate (xind(nx))				! allocate indices array
  nx_valid = 0

  do x = 1, nx
    valid(x) = (all (array(x,:) /= vmiss))	! valid = no missing data

    if (valid(x)) then				! if valid row...
      nx_valid = nx_valid + 1			! count valid rows
      xind(nx_valid) = x			! accumulate valid X indices
    end if
  end do

! If no valid rows, return all missing standard deviations.

  if (nx_valid == 0) then
    stdev_out(:) = vmiss
    return
  end if

! Compute standard deviation independently for each data variable.

var_loop: &
  do v = 1, nvars

!------------------------------------------------------
! Circular standard deviation.
!------------------------------------------------------

    if (is_circular(v)) then

      dir_rad = array(xind(1:nx_valid), v) * rad2deg	! degrees to radians

      s = sum (sin (dir_rad(:))) / nx_valid	  ! means of sines & cosines
      c = sum (cos (dir_rad(:))) / nx_valid

      e = sqrt (max (1 - (s**2 + c**2), 0d0))	  ! Yamartino estimator
						  ! guard against roundoff error

      stdev_out(v) = asin (e) * (1 + b * (e**3))  ! standard deviation, radians

      stdev_out(v) = stdev_out(v) / rad2deg	  ! convert back to degrees

!------------------------------------------------------
! Normal standard deviation.
!------------------------------------------------------

    else
      call std_dev (array(:,v), valid(:), vmiss, std_thresh, mean, variance, &
        stdev_out(v))				! use std dev library routine
    end if

  end do var_loop

end subroutine stdevTNcirc
end module stdev_TNcirc
