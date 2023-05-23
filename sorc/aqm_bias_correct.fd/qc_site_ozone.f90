!------------------------------------------------------------------------------
!
! qc_site_ozone.f90 -- QC for AIRNow ozone hourly time series, single site.
!
! 2017-apr-10	New QC routine for ozone.  By Dave Allured and Irina Djalalova.
!		Adapted from qc_site_pm25.f90.
!		Initial test #2, remove constant values over 24 hours or more.
! 2017-apr-11	Use new library routine find_runs.f90.
!		Add test #3, remove constant values at same hour of day.
!
! 2023-apr-08	Add low/high limits for AirNow input values.
!
! Primary input:  Raw AIRNow hourly time series of ozone concentrations
! for single site.  Complete days, start on hour 0, end on hour 23.
!
! Primary output:  QC'ed time series.
!
! This is linked as an include file within module qc_single_site.f90.
!
!------------------------------------------------------------------------------

subroutine qc_site_ozone (y, obs_min_input, obs_max_input, vmiss, diag, site_id)

   use config, only : dp
   use find__runs
   implicit none

   real(dp),     intent (inout) :: y(:)		 ! hourly time series for 1 site
   real(dp),     intent (in   ) :: obs_min_input ! obs min valid input threshold
   real(dp),     intent (in   ) :: obs_max_input ! obs max valid input threshold
   real(dp),     intent (in   ) :: vmiss	 ! missing value in time series
   integer,      intent (in   ) :: diag		 ! diag verbosity level, 0-N
   character(*), intent (in   ) :: site_id	 ! site ID for diagnostic

! Local variables.

   character fmt1*80

   integer ihr, ntimes, min_length
   integer nrej_runs, nrej_hours, nrej_lower, nrej_upper, nrej_all

   real(dp) tolerance_rel

   logical is_missing(size(y)), mask_hours(size(y)), mask_all(size(y))
   logical, allocatable :: mask_runs(:), slice_mask(:)
   logical, allocatable :: mask_lower(:), mask_upper(:)

! Initialize.

   ntimes = size (y)				! get length of time series

   is_missing(:) = (y(:) == vmiss)		! original missing values

!---------------------------------------------------------------------
! From "Ozone QC procedure", 2017 January 21, by Irina Djalalova.
!
! 1) Observed data is eliminated if an absolute difference between
! observational and model data exceeds 100 ppbv. This technique is
! applied to every site on an hourly basis.
!
! 2017-apr-11:  Not yet implemented.
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! 2) The constant ozone data over whole day is eliminated.
!---------------------------------------------------------------------

! Define "day" as any consecutive series 24 hours or longer, along
! the whole time series, even if it crosses between two calendar days.

   min_length    = 24		! minimum number of time steps to qualify
   tolerance_rel = 0.0001	! relative tolerance within "constant"

! Find all runs of near-constant values.
! Returns mask_runs = true at all values within runs.

   call find_runs (y, vmiss, min_length, tolerance_rel, mask_runs)

!---------------------------------------------------------------------
! 3) The constant ozone data for the same hour of the day through
! several days should be eliminated.
!---------------------------------------------------------------------

   min_length    = 15		! minimum number of days with same hour
   tolerance_rel = 0.0001	! relative tolerance within "constant"

! Find all runs within each of 24 same-hour slices.

   do ihr = 1, 24
      call find_runs (y(ihr::24), vmiss, min_length, tolerance_rel, slice_mask)
					! stride by 24 to get same-hour slices
      mask_hours(ihr::24) = slice_mask(:)	! collect slices into full mask
   end do

!---------------------------------------------------------------------
! 4) An automated technique to validate observational isolated
! spikes has been considered. This technique is applied to each
! observed site on an hourly basis. Only observational data
! comparison is included. The major point of this method is to
! find out if an unusual spike in observed data at the particular
! site (Xsite) is isolated compared to the neighboring
! observational sites. For this purpose, four closest neighbor
! sites are found, each in a separate quadrant of the geographical
! position of Xsite. If at least two neighbors exist and the
! difference in ozone values exceeds a threshold value (XTHRESH)
! and the existing neighbor sites are within the chosen range
! (~100 km), then this observed value is eliminated.
!
! The bigger problem here is to choose an appropriate XTHRESH.
! Based on the initial calculations, we may pick up 70 ppbv as a
! possible threshold value, although further investigations will
! be performed.
!
! 2017-apr-11:  Not yet implemented.
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! Apply min/max input limits.
!
! New filter, 2023 April 8, specifically for possible AirNow
! negative input values.
!---------------------------------------------------------------------

   mask_lower = (y(:) < obs_min_input)
   mask_upper = (y(:) > obs_max_input)

!---------------------------------------------------------------------
! Final step.  Merge all filter masks, set reject values to missing.
!---------------------------------------------------------------------

! Merge all reject values from all individual tests.
! True = reject, so combine with "or".

   mask_all(:) = mask_runs .or. mask_hours .or. mask_lower .or. mask_upper

   where (mask_all(:)) y(:) = vmiss

! Site diagnostics.

   if (diag >= 3) then
      nrej_runs  = count (mask_runs(:)  .and. .not. is_missing(:))
      nrej_hours = count (mask_hours(:) .and. .not. is_missing(:))
      nrej_lower = count (mask_lower(:) .and. .not. is_missing(:))
      nrej_upper = count (mask_upper(:) .and. .not. is_missing(:))

      nrej_all   = count (mask_all(:)   .and. .not. is_missing(:))

      fmt1 = "('    Site ', a, ', count of ', a, i6)"

      if (nrej_runs  /= 0) print fmt1, site_id, &
                         'rejected constant values in runs      =', nrej_runs
      if (nrej_hours /= 0) print fmt1, site_id, &
                         'rejected constant values in same hour =', nrej_hours
      if (nrej_lower /= 0) print fmt1, site_id, &
                         'rejected values below lower limit     =', nrej_lower
      if (nrej_upper /= 0) print fmt1, site_id, &
                         'rejected values above upper limit     =', nrej_upper

      if (nrej_all   /= 0) print fmt1, site_id, &
                         'combined rejected values for site     =', nrej_all
   end if

end subroutine qc_site_ozone
