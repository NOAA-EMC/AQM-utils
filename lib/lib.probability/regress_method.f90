!------------------------------------------------------------------------------
!
! regress_method.f90 -- Make probability forecasts by linear regression.
!
! This routine writes the probability output file, in addition to calculations.
!
! This is one of the top level method routines of the probability
! forecast module, a main component of the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2020-aug-04	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!		Algorithm by Irina Djalalova, NOAA/ESRL/PSL/CIRES.
!
! * Remember to update the date in the module_id below.
!
! This probability module incorporates several major functions:
!
! * Read gridded model forecasts for a given forecast cycle.
! * Compute daily averages or 8-hour maximums of obs and corrected site data.
! * Compute cube root best fit of aggregate daily obs and corrected values.
! * Apply linear correction to cube root of forecast grids.
! * Compute final probability grids using cumulative distribution function.
! * Write probability output file.
!
! Primary inputs:
!
! * Gridded raw forecast file for current forecast cycle.
! * Bias corrected forecast values at sites, for full training period.
! * Obs data at site locations, for full training period.
! * Grid coordinates and elevations.
!
! Primary output:
!
! * Single data file with several computed daily probability forecast grids.
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

module regress__method
contains

subroutine regress_method (grid_coord_file, output_file_template, &
      target_var, forecast_date, cycle_time, base_year, calendar, &
      filter_result_all, obs_reshaped, corr_grids, vmiss, prob, diag)

   use cdf__to_probability
   use config, only : dp
   use expand__filename
   use index_to_date_mod
   use make__daily
   use probability_type
   use read__grid_coords
   use regress_mod
   use write__probability_netcdf_lr
   implicit none

   character(*), parameter :: &
      module_id = 'regress_method.f90 version 2020-aug-04'

! Input arguments.

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

   real(dp),     intent(in) :: obs_reshaped(:,:,:)  ! obs reshaped to model data
   						    ! (days, hours, sites)
						    ! vestigial var dim. removed

   real(dp),     intent(in) :: corr_grids (:,:,:)   ! bias corrected grids from
   						    ! spreading (X, Y, hours)

   real(dp),        intent(in) :: vmiss		    ! common missing value code
   type(prob_type), intent(in) :: prob		    ! probability config params
   integer,         intent(in) :: diag		    ! diag verbosity level, 0-N

! Local variables.

   character fdate_str*24, outfile*200
   character fmt1*60, fmt2*60, fmt3*60

   integer nx, ny
   integer d1, d2, fday, pday, npdays
   integer ndays_filter, ndays_avail, ndays_required
   integer year, month, day

   integer ithresh, nthresh
   integer nmiss_c, nmiss_f, nmiss_g, nmiss_o
   integer nvalid_c, nvalid_f, nvalid_g, nvalid_o

   real(dp) a, b, sigma
   real(dp) cmin, cmax, gmin, gmax, omin, omax

! Dynamic arrays.

   real(dp), allocatable :: grid_lats(:,:)	   ! grid coordinates (X, Y)
   real(dp), allocatable :: grid_lons(:,:)

   logical,  allocatable :: valid_mask(:,:)	   ! valid data mask
						   !   (tdays, sites, pdays)

   real(dp), allocatable :: obs_daily(:,:,:)	   ! daily averages or maximums
   real(dp), allocatable :: filter_daily(:,:,:)	   !   (tdays, sites, pdays)

   real(dp), allocatable :: grid_daily(:,:,:)	   ! daily averages or maximums
   						   !   (X, Y, pdays)

   real(dp), allocatable :: grid_cube_root(:,:)	   ! cube root original and
   real(dp), allocatable :: grid_fitted(:,:)	   !   fitted daily grids (X, Y)

   real(dp), allocatable :: error_variance(:,:)	   ! variance grid for CDF (X,Y)

   real(dp), allocatable :: thresh_cube_root(:)	   ! cube root prob. thresholds

   real(dp), allocatable :: obs_1d(:)		   ! aggregated obs and filter
   real(dp), allocatable :: filter_1d(:)	   !   packed daily values

! Probability output grids.  Map to CMAQ standard dimensions:
!   (X, Y, thresh, days) --> (X, Y, LEV, TSTEP)

   real(dp), allocatable :: probability(:,:,:,:)   ! (X, Y, thresh, pdays)
   real(dp), allocatable :: zscore(:,:,:,:)
   real(dp), allocatable :: cdf(:,:,:,:)

!-------------------------------------------------------
! Initialize.
!-------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, 'regress_method: Begin linear regression method.'
   if (diag >= 2) print *, '  Module ID = ' // module_id

! Get dimensions.

   nx      = size (grid_lats, 1)	! grid dims from CMAQ coordinate grid
   ny      = size (grid_lats, 2)

   nthresh = size (prob%thresh)		! number of requested threshold values

! Decode the date index for the current forecast cycle.

   call index_to_date (forecast_date, year, month, day, base_year, calendar)
      						! get current Y M D integers

! Read CMAQ 2-D coordinate grids.

   if (diag >= 2) print *
   if (diag >= 2) print *, '  Read CMAQ 2-D coordinate grids.'

   call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)

! Input diagnostics.

   if (diag >= 2) then
      print *
      print *, '  regress_method: Input summary:'

      fmt1 = '(5x,a,99i6)'
      print fmt1, 'shape (obs_reshaped)      = ', shape (obs_reshaped)
      print fmt1, 'shape (filter_result_all) = ', shape (filter_result_all)
      print fmt1, 'shape (corr_grids)        = ', shape (corr_grids)

      nmiss_o  = count (obs_reshaped      == vmiss)
      nmiss_f  = count (filter_result_all == vmiss)
      nmiss_g  = count (corr_grids        == vmiss)

      nvalid_o = count (obs_reshaped      /= vmiss)
      nvalid_f = count (filter_result_all /= vmiss)
      nvalid_g = count (corr_grids        /= vmiss)

      print *
      fmt2 = "(5x, a, ' valid =', i9, ', missing =', i9))"
      print fmt2, 'obs_reshaped,     ', nvalid_o, nmiss_o
      print fmt2, 'filter_result_all,', nvalid_f, nmiss_f
      print fmt2, 'corr_grids,       ', nvalid_g, nmiss_g
   end if

!-----------------------------------------------------------
! Make daily averages or maximums for best fit period.
!-----------------------------------------------------------

! The best fit period is the last N days of the training period.

   ndays_filter = size (filter_result_all, 1)  ! no. of bias corr. forecast days

   fday = ndays_filter			! forecast day index = last input day
   					! = day after end of training period

   d1 = fday - prob%ndays_best_fit	! start of best fit period
   d1 = max (1, d1)			! keep start day within training period

   d2 = fday - 1			! last day = end of training period

   ndays_avail    = d2 - d1 + 1		! actual days in best fit period
   ndays_required = 2			! minimum # days for best fit period

   if (ndays_avail < ndays_required-999) then
      print *
      print *, '*** regress_method: Insufficient training period for' &
         // ' probability forecast.  Abort.'
      print '(a,i0)',' *** Number of actual days available    = ',ndays_avail
      print '(a,i0)',' *** Number of days required for method = ',ndays_required
      call exit (1)
   end if

   if (diag >= 2) then
      print *
      fmt1 = '(5x,a,99i6)'
      print fmt1, 'd1, d2 for fit period  = ', d1, d2
      print fmt1, 'shape (obs_reshaped)   = ', shape (obs_reshaped)
      print fmt1, 'shape (obs fit subset) = ', shape (obs_reshaped(d1:d2,:,:))
   end if

! Make daily values for hourly input site data and hourly forecast grids,
! over best fit period.  Dimension order is indicated when calling.
! TD = training period days, FD = forecast days

   obs_daily    = make_daily (obs_reshaped(d1:d2,:,:),      'X,H,Y', vmiss, &
                             'obs_reshaped', &
                     cycle_time, prob, diag)	! (TD, S, FD) <-- (TD, H, S)

   filter_daily = make_daily (filter_result_all(d1:d2,:,:), 'X,H,Y', vmiss, &
                             'filter_result_all', &
                     cycle_time, prob, diag)	! (TD, S, FD) <-- (TD, H, S)

   grid_daily   = make_daily (corr_grids(:,:,:),            'X,Y,H', vmiss, &
                             'corr_grids', &
                     cycle_time, prob, diag)	! (X, Y, FD)  <-- (X, Y, H)

   if (diag >= 2) then
      print *
      fmt1 = '(5x,a,99i6)'
      print fmt1, 'shape (obs_daily)    = ', shape (obs_daily)
      print fmt1, 'shape (filter_daily) = ', shape (filter_daily)
      print fmt1, 'shape (grid_daily)   = ', shape (grid_daily)

      nmiss_o  = count (obs_daily    == vmiss)
      nmiss_f  = count (filter_daily == vmiss)
      nmiss_g  = count (grid_daily   == vmiss)

      nvalid_o = count (obs_daily    /= vmiss)
      nvalid_f = count (filter_daily /= vmiss)
      nvalid_g = count (grid_daily   /= vmiss)

      print *
      fmt2 = "(5x, a, ' valid =', i9, ', missing =', i9))"
      print fmt2, 'obs_daily,   ', nvalid_o, nmiss_o
      print fmt2, 'filter_daily,', nvalid_f, nmiss_f
      print fmt2, 'grid_daily,  ', nvalid_g, nmiss_g
   end if

!-------------------------------------------------------
! Initialize for main loop over forecast days.
!-------------------------------------------------------

! Get dimensions.

   nx      = size (grid_lats, 1)	! grid dims from CMAQ coordinate grid
   ny      = size (grid_lats, 2)

   npdays  = size (obs_daily, 3)	! no. probability days from daily maker
   nthresh = size (prob%thresh)		! number of requested threshold values

! Allocate fixed work arrays that span multiple forecast days.

   allocate (probability (nx, ny, nthresh, npdays))	! probability
   allocate (zscore      (nx, ny, nthresh, npdays))	! intermediate and
   allocate (cdf         (nx, ny, nthresh, npdays))	! result arrays

!------------------------------------------------------------
! Main loop over each available probability forecast day.
!------------------------------------------------------------

pday_loop: &
   do pday = 1, npdays

      call fdate (fdate_str)
      print *
      print '(a)','------------------------------------------------------------'
      print '(2a,i0,a)', fdate_str, '  Start probability forecast day ',pday,'.'
      print '(a)','------------------------------------------------------------'
      print *

! Combine all valid (obs + corrected) pairs for all sites into
! unordered 1-D arrays.  Omit missing values.

      valid_mask =       (obs_daily   (:,:,pday) /= vmiss) &
                   .and. (filter_daily(:,:,pday) /= vmiss)

      obs_1d     = pack (obs_daily   (:,:,pday), valid_mask)
      filter_1d  = pack (filter_daily(:,:,pday), valid_mask)
						! (N) <-- (TD, S, FD)

!-------------------------------------------------------
! Input diagnostics for linear regression.
!-------------------------------------------------------

! Summary of input data for current forecast day.

      if (diag >= 2) then
         omin = minval (obs_1d)
         omax = maxval (obs_1d)

         cmin = minval (filter_1d)
         cmax = maxval (filter_1d)

         gmin = minval (grid_daily(:,:,pday), (grid_daily(:,:,pday) /= vmiss))
         gmax = maxval (grid_daily(:,:,pday), (grid_daily(:,:,pday) /= vmiss))

         fmt1 = '(4x,a,2g14.4)'
         fmt3 = '(4x,a,99i6)'

         print '(a,i0,a)', ' regress_method: Daily input summary for' &
            // ' forecast day ', pday, ':'

         print *
         print fmt3, 'shape (obs_1d)    = ', shape (obs_1d)
         print fmt3, 'shape (filter_1d) = ', shape (filter_1d)
         print fmt3, 'size (grid 1 day) = ', size (grid_daily(:,:,pday))

         print *
         print fmt1, 'Daily site obs       min, max =', omin, omax
         print fmt1, 'Daily site corrected min, max =', cmin, cmax
         print fmt1, 'Daily grid corrected min, max =', gmin, gmax

         nmiss_o  = count (obs_daily   (:,:,pday) == vmiss)
         nmiss_c  = count (filter_daily(:,:,pday) == vmiss)
         nmiss_g  = count (grid_daily  (:,:,pday) == vmiss)

         nvalid_o = count (obs_daily   (:,:,pday) /= vmiss)
         nvalid_c = count (filter_daily(:,:,pday) /= vmiss)
         nvalid_g = count (grid_daily  (:,:,pday) /= vmiss)

         print *
         fmt2 = "(4x, a, ' valid =', i9, ', missing =', i9))"
         print fmt2, 'Daily site obs,      ', nvalid_o, nmiss_o
         print fmt2, 'Daily site corrected,', nvalid_c, nmiss_c
         print fmt2, 'Daily grid corrected,', nvalid_g, nmiss_g
         print *
      end if

!-----------------------------------------------------------
! Compute best fit of cube roots, all sites aggregated.
!-----------------------------------------------------------

! Convert to cube roots.  Missing values were previously eliminated.

      if (diag >= 2) print *, ' regress_method: Calculate cube roots.'

      thresh_cube_root = prob%thresh(:) ** (1d0 / 3)

      obs_1d    = obs_1d    ** (1d0 / 3)	! all valid data; missing values
      filter_1d = filter_1d ** (1d0 / 3)	!   were previously excluded

      grid_cube_root = grid_daily(:,:,pday)	! copy 1 grid for current day
      						! include missing values

      where (grid_cube_root(:,:) /= vmiss) &		! preserve missing vals
         grid_cube_root = grid_cube_root ** (1d0 / 3)	! in this calculation

      if (diag >= 2) then
         omin = minval (obs_1d)
         omax = maxval (obs_1d)

         cmin = minval (filter_1d)
         cmax = maxval (filter_1d)

         gmin = minval (grid_cube_root, (grid_cube_root /= vmiss))
         gmax = maxval (grid_cube_root, (grid_cube_root /= vmiss))

         fmt1 = '(4x,a,2g14.4)'
         print fmt1, 'Cube root site obs       min, max =', omin, omax
         print fmt1, 'Cube root site corrected min, max =', cmin, cmax
         print fmt1, 'Cube root grid corrected min, max =', gmin, gmax
         print *
      end if

! Compute best fit by linear regression.

      if (diag >= 2) print *, ' Compute site obs to site corrected best fit,' &
         // ' by linear regression.'

      call regress (obs_1d, filter_1d, diag, b, a, sigma)

      if (diag >= 2) then
         fmt1 = '(4x,a,g14.4)'
         print fmt1, 'a     = ', a
         print fmt1, 'b     = ', b
         print fmt1, 'sigma = ', sigma
         print *
      end if

! Compute fitted cube root daily forecast grid.

      if (diag >= 2) print *, ' Compute fitted cube root daily forecast grid.'

      grid_fitted = grid_cube_root(:,:)		! copy 1 grid for current day
      						! include missing values

      where (grid_cube_root(:,:) /= vmiss) &	! preserve missing values
         grid_fitted = a + b * grid_cube_root	! in this calculation

      if (diag >= 2) then
         gmin = minval (grid_fitted, (grid_fitted /= vmiss))
         gmax = maxval (grid_fitted, (grid_fitted /= vmiss))

         fmt1 = '(4x,a,2g14.4)'
         print fmt1, 'Cube root grid fitted min, max    =', gmin, gmax
         print *
      end if

!-------------------------------------------------------
! Compute probability grids using CDF.
!-------------------------------------------------------

      print *, ' Compute probability grids for ' // trim (target_var) // '.'

! Convert scalar sigma into variance grid for this routine.

      if (.not. allocated (error_variance)) allocate (error_variance(nx, ny))

      error_variance(:,:) = sigma ** 2

! Compute probabiliy grids for each requested threshold.

      do ithresh = 1, nthresh
         print *
         fmt1 = '(4x,2a,f8.3,1x,a)'
         print fmt1, trim (target_var), ', original threshold  =', &
            prob%thresh(ithresh), trim (prob%thresh_units)
         print fmt1, trim (target_var), ', cube root threshold =', &
            thresh_cube_root(ithresh)

         call cdf_to_probability ('fitted', grid_fitted, &
            error_variance, grid_lats, grid_lons, &
            thresh_cube_root(ithresh), vmiss, diag, &
            probability(:,:,ithresh,pday), &
            zscore(:,:,ithresh,pday), cdf(:,:,ithresh,pday))
      end do

   end do pday_loop

!-------------------------------------------------------
! Write probability output file.
!-------------------------------------------------------

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  regress_method: Write output file.'

! Create actual output file name for current forecast cycle.

   call expand_filename (output_file_template, year, month, day, cycle_time, &
      outfile)

! Write output file.  Data and coordinate vars will be converted from
! double to single precision, when writing to file.

   call write_probability_netcdf_lr (outfile, target_var, grid_lats, &
      grid_lons, probability, prob, diag, grid_daily, zscore, cdf)

   if (diag >= 2) print *, &
      'regress_method: All done.  Return to main program.'

end subroutine regress_method
end module regress__method
