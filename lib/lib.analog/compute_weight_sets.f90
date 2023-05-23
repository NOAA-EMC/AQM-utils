!-----------------------------------------------------------------------------
!
! compute_weight_sets.f90 -- Compute all possible weight combinations
! for given numbers of predictor variables and weight increments.
!
! This is a support routine for the NOAA/NCAR bias correction
! system for CMAQ forecast outputs.
!
! 2014-oct-01   comp_weight:
!		Original subroutine version.  By Stefano Alessandrini, NCAR.
!		Embedded in main_analog.F90, NCAR version 2015-dec-01.
!
! 2017-may-17	Non-recursive version.  In-line within weight_control.f90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!
! 2017-may-31	compute_weight_sets.f90:
!		Break out from weight_control.f90.
!		Add equal weights as the guaranteed first combination.
!		(One duplicate set with equal weights will be present
!		  in certain cases.)
!
! Note:  Function result array is auto-allocated.
!
!-----------------------------------------------------------------------------

module compute__weight_sets
contains

function compute_weight_sets (nvars, nweights) result (new_weights)

   use config, only : dp
   use multisets_mod
   implicit none

   integer, intent (in)   :: nvars	    ! number of predictor variables
   integer, intent (in)   :: nweights	    ! number of equal weight increments
   					    !   to distribute across predictors,
   					    !   so as to sum to 1.0

   real (dp), allocatable :: new_weights(:,:)	! function result array,
   						! all weight sets (sets, vars)
! Local variables.

  integer nsets_final
  integer, allocatable :: msets(:,:)   ! all multiset combinations (sets, vars)

! Get all possible multiset combinations.  Do this first to get dimensions.

   msets = multisets (nvars, nweights)			! (sets, vars)

! Allocate space for one extra combination.

   nsets_final = 1 + size (msets, 1)
   allocate (new_weights(nsets_final,nvars))		! (sets, vars)

! Insert all equal weights as the very first weights combination.
! Normalize for the number of predictors.

   new_weights(1,:)  = 1.0 / dble (nvars)

! Compute all weight combinations from multisets, and insert
! into the rest of the output array.  Normalize for the number
! of distributed weight increments.

! This will include one duplicate set with all equal weights,
! when the number of weight increments is an even multiple of
! the number of predictors.

   new_weights(2:,:) = msets(:,:) / dble (nweights)	! (sets, vars)

end function compute_weight_sets
end module compute__weight_sets
