!------------------------------------------------------------------------------
!
! probability.f90 -- Make gridded probability forecasts, and write output file.
!
! This is the top level routine of the probability forecast module,
! one of the main components of the NOAA NCO/ARL/PSD bias correction
! system for CMAQ forecast outputs.
!
! 2019-aug-07	probability.f90:
!		Original module to integrate probability forecasts into
!		  bias correction, using optimal interpolation method.
!		By Dave Allured, NOAA/ESRL/PSL/CIRES.
!
! 2020-aug-03	Reduce to method dispatch only.  Add method switch.
!		Rename original version to opt_interp_method.f90.
!		Add new linear regression method, with new input arrays.
!		At this time there is no shareable common code, other than
!		  lower level subroutines.
!		Therefore, all main procedures are exported into the method
!		  routines.
!
! This probability module incorporates six major functions:
!
! * Read gridded model forecasts, as needed.
! * Get analog filter results and spreading results, as needed.
! * Compute the necessary averages and climatologies.
! * Compute analysis grids by the selected probability method.
! * Compute final probability grids.
! * Write probability output file.
!
! Primary inputs, varies by selected method:
!
! * Gridded raw forecast files for current and previous forecast cycles.
! * Best analogs at site locations, for current forecast.
! * Bias corrected forecast values at sites, for full training period.
! * Obs data at site locations, for full training period.
! * Bias corrected grids for current forecast.
! * Grid coordinates and elevations.
! * Site coordinates.
!
! Primary output:
!
! * Single data file with several daily probability forecast grids.
!
! Notes:
!
! Input and output file paths are specified with path templates.
! These contain YYYY MM DD HH substitution strings that will
! resolve to desired file names for the given forecast cycle.
! Templates may include full or relative paths, and they may
! begin with an $ENV environnment variable.
!
! Forecast date index:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
!------------------------------------------------------------------------------

module probability_mod
contains

subroutine probability (in_gridded_template, grid_coord_file, &
      output_file_template, target_var, forecast_date, cycle_time, &
      base_year, calendar, filter_result_all, best_analogs, obs_reshaped, &
      corr_grids, site_lats, site_lons, vmiss, prob, diag)

   use config, only : dp
   use opt__interp_method
   use probability_type
   use regress__method
   implicit none

! Input arguments.

   character(*), intent(in) :: in_gridded_template  ! gridded input template
   character(*), intent(in) :: grid_coord_file	    ! aux. grid coordinate file
   character(*), intent(in) :: output_file_template ! output filename template
   character(*), intent(in) :: target_var	    ! target variable name

   integer,      intent(in) :: forecast_date	    ! target date index
   integer,      intent(in) :: cycle_time	    ! forecast cycle time
   integer,      intent(in) :: base_year	    ! base year for date indexes
   character(*), intent(in) :: calendar	  	    ! calendar system in use

   real(dp),     intent(in) :: filter_result_all(:,:,:)  ! corrected forecast
						    ! values, all days in period
						    ! (days, hours, sites)

   real(dp),     intent(in) :: best_analogs(:,:,:)  ! best analogs, forecast day
						    ! (hours, analogs, sites)
  						    ! sort order does not matter

   real(dp),     intent(in) :: obs_reshaped(:,:,:)  ! obs reshaped to model data
   						    ! (days, hours, sites)
						    ! vestigial var dim. removed

   real(dp),     intent(in) :: corr_grids (:,:,:)   ! bias corrected grids from
   						    ! spreading (X, Y, hours)

   real(dp),        intent(in) :: site_lats(:)	    ! site coordinates
   real(dp),        intent(in) :: site_lons(:)
   real(dp),        intent(in) :: vmiss		    ! common missing value code
   type(prob_type), intent(in) :: prob		    ! probability config params
   integer,         intent(in) :: diag		    ! diag verbosity level, 0-N

! Dispatch to the selected probability method.

   if (diag >= 2) print *
   if (diag >= 2) print *, 'probability: Begin probability forecast module.'
   if (diag >= 2) print *, 'Selected probability method = ' &
      // trim (prob%probability_method)

   if (prob%probability_method == 'optimal interpolation') then

      call opt_interp_method (in_gridded_template, grid_coord_file, &
         output_file_template, target_var, forecast_date, cycle_time, &
         base_year, calendar, best_analogs, site_lats, site_lons, vmiss, &
         prob, diag)

   else if (prob%probability_method == 'linear regression') then

      call regress_method (grid_coord_file, output_file_template, &
         target_var, forecast_date, cycle_time, base_year, calendar, &
         filter_result_all, obs_reshaped, corr_grids, vmiss, prob, diag)

   else
      print *
      print *, '*** Abort, unknown name for selected probability method.'
      print *, '*** Selected method = ' // trim (prob%probability_method)
      call exit (1)
   end if

end subroutine probability
end module probability_mod
