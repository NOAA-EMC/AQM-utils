!------------------------------------------------------------------------------
!
! daily_max.f90 -- Compute daily 8-hour maximums for forecast time series.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-may-16	datanetcdf_preparation_for_probability_code.pro:
!		dataascii_preparation_for_probability_code.pro:
!		Original IDL versions.  By Irina Djalalova, NOAA/OAR/ESRL/PSD.
!
! 2019-jul-10	daily_max.f90:
!		Original F90 version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Daily max calculations split off from site_climatology.f90,
!		   version 2019-jun-27.
!		Make common array calculations for both model and site data.
!		This embeds the current calculation for ozone daily 8-hour
!		   maximums, emulating the EPA-defined method.
! 2019-aug-06	Move run parameters from hard coded, to config file.
!
! Summary of daily max calculations:
!
! * Input:  3-D array of forecast time series, one dimension being
!      forecast hours.
! * Input may be either gridded model data, or best analogs series.
! * Output: 3-D array of forecast daily values, one dimension being
!      forecast days.
! * Determine quaifying search windows within available forecast hours.
! * Compute sliding 8-hour averages within available forecast hours.
! * Find daily 8-hour maximums for each qualifying forecast day,
!      and each grid point.
! * All grid points (matrix time series) are computed independently.
!
! Call this routine once per forecast cycle, to compute daily
! 8-hour maximums for each conforming day in that forecast cycle.
!
! Note, the input data array is currently single precision, to
! match current gridded data input routines.
!
! The output array daily_out must be pre-allocated.  The forecast
! days dimension must be sufficient to hold the maximum possible
! number of computed forecast days.  This may not known be known
! in advance, so the array may be over-dimensioned.  The actual
! number of computed forecast days is returned in fdays_valid.
!
!------------------------------------------------------------------------------

module daily__max
contains

subroutine daily_max (indata, vmiss, cycle_time, prob, diag, daily_out, &
      fdays_valid)

   use config, only : dp
   use probability_type
   implicit none

! Calling arguments.

   real,           intent(in) :: indata(:,:,:)	! forecast series (X, Y, hours)
   real(dp),       intent(in) :: vmiss		! common missing value code
   integer,        intent(in) :: cycle_time	! forecast cycle time
   type(prob_type),intent(in) :: prob		! probability config parameters
   integer,        intent(in) :: diag		! diag. verbosity level, 0-N

   real(dp), intent(out) :: daily_out(:,:,:)	! daily max output (X, Y, fdays)
   integer,  intent(out) :: fdays_valid		! number of valid days in output

! Local variables.

   integer x, y, nx, ny
   integer h1, h2, hv1, hv2, nhours
   integer fday, navg

   integer end_utc, end_fhour
   integer start_fhour_first, start_fhour_last
   integer start_fhour_last_avail, start_fhour_max
   integer offset_last, dif_hours

! Dynamic arrays.

   real(dp), allocatable :: vavg(:,:,:)    ! sliding 8-h averages (X, Y, hours)

!-----------------------------------------------------------
! First calculate sliding 8-hour averages.
!-----------------------------------------------------------

! Note, only full 8-hour averaging intervals are included.
! Partial averaging intervals are not included.

   if (diag >= 2) print *, 'daily_max: Start.'

   nx     = size (indata, 1)			! get dimensions
   ny     = size (indata, 2)
   nhours = size (indata, 3)

   if (diag >= 2) print *, '  Make sliding 8-hour averages.'

! Config file parameters for sliding averages, with prob% prefix.

! daily_max_len_avg        = number of hours in each sliding average
! daily_max_navg_min       = minimum non-missing hours for valid sliding average

   navg = nhours - (prob%daily_max_len_avg - 1)  ! no. available sliding avgs.
   start_fhour_last_avail = navg	! fhour index of last available average

   allocate (vavg(nx, ny, navg))

   do x = 1, nx
      do y = 1, ny
         do h1 = 1, navg	! loop limits only for full averaging intervals
            h2 = h1 + (prob%daily_max_len_avg - 1)
            if (count (indata(x, y, h1:h2) /= vmiss) &
                                  >= prob%daily_max_navg_min) then
               vavg(x, y, h1) = sum ( indata(x, y, h1:h2), &
                                      (indata(x, y, h1:h2) /= vmiss) ) &
                              / count (indata(x, y, h1:h2) /= vmiss)
				! (sum over mask) divided by (count over mask)
            else
               vavg(x, y, h1) = vmiss
            end if
         end do
      end do
   end do

!-----------------------------------------------------------
! Find valid days and search windows for daily maximums.
!-----------------------------------------------------------

   if (diag >= 2) print *, '  Find daily maximums.'

! Config file parameters for daily search windows, with prob% prefix.

! daily_max_start_time_utc = UTC instantaneous start time for daily maximums
! daily_max_nhours         = nominal number of sliding averages to search
! daily_max_nhours_min     = minimum required number of sliding averages
!                              in partial windows (minimum window size)

! Determine first valid search window, possibly truncated.
! Work backward from the implied UTC ending hour.
! Based only on the specified UTC start time, plus limit parameters.

! First determine the UTC time of the last forecast hour in the
! first proposed time window.  Forecast hours are backward averaged.
! Therefore each forecast hour's UTC label is one hour later than
! its UTC instantaneous start time.  The following calculation of
! end_utc accounts for this backward averaging.

   end_utc   = prob%daily_max_start_time_utc + prob%daily_max_nhours
   				! UTC forecast hour of START of LAST 8-hour
   				! average in daily max search, not normalized!

   end_fhour = end_utc - cycle_time	! BASE forecast hour of START of same
					! LAST 8-hour average

   if (diag >= 2) print '(7x,a,i0)', 'end_utc              = ', end_utc
   if (diag >= 2) print '(7x,a,i0)', 'end_fhour original   = ', end_fhour

   end_fhour = 1 + modulo ((end_fhour - 1), 24)   ! normalize to first available
   						  ! forecast hours 1-24

   if (diag >= 2) print '(7x,a,i0)', 'end_fhour normalized = ', end_fhour

! Check for sufficient number of 8-hour averages between first available,
! and this LAST 8-hour average in the proposed daily search window.

   if (end_fhour < prob%daily_max_nhours_min) &	 ! if insufficient averages:
      end_fhour = end_fhour + 24		 ! advance to next day window

   start_fhour_first = end_fhour - (prob%daily_max_nhours - 1)
   				! Get forecast hour for START of chosen
				! first day search window.
				! Must be NOT NORMALIZED for hour base of loop!

   if (diag >= 2) print '(7x,a,i0)','end_fhour sufficient = ', end_fhour
   if (diag >= 2) print '(7x,a,i0)','start_fhour_first    = ', start_fhour_first

! Determine last valid search window, possibly truncated.
! Work forward from the start of the first day search window,
! plus hour of last available average, and limit parameters.

   start_fhour_max = start_fhour_last_avail - (prob%daily_max_nhours_min - 1)
   				! forecast hour of last possible window start
				! with SUFFICIENT NUMBER of 8-hour averages

   offset_last = modulo ((start_fhour_max - start_fhour_first), 24)
   				! backward offset to chosen last window start,
   				! always 0 to 23 hours back from last possible

   start_fhour_last = start_fhour_max - offset_last
   				! step back to start hour of last daily search

! Now determine number of valid statistical days for the result arrays.

   dif_hours   = start_fhour_last - start_fhour_first
   fdays_valid = 1 + (dif_hours / 24)

! Consistency checks.

   if (   (modulo (dif_hours, 24) /= 0) &
     .or. (start_fhour_first > start_fhour_last) ) then
      print '(a,i0)', '*** daily_max: FATAL: Algorithm failure.'
      print '(a,i0)', '*** Derived search windows for daily max are invalid.'
      print '(a,i0)', '*** start_fhour_first    = ', start_fhour_first
      print '(a,i0)', '*** start_fhour_last     = ', start_fhour_last
      print '(a,i0)', '*** no. hours difference = ', dif_hours
      call exit (1)
   end if

! Note.  The key parameters at this point are start_fhour_first,
! start_fhour_last, and daily_max_nhours.  Combined, these represent
! a set of VIRTUAL search windows to be overlaid on the available
! 8-hour averages.  There is one window for each valid statistical day
! in the search results.
!
! The VIRTUAL windows are always the same size.  But NOTE, because of
! alignment, the first and last ACTUAL windows in 8-hour averages may
! be smaller than the VIRTUAL windows.  It is left to the next search
! stage, to limit to available data and array bounds.
!
! This parameter stage guarantees that every defined VIRTUAL window
! is a valid search window.  In other words, every VIRTUAL window
! will contain at least the minimum number of averages specified by
! the user parameter daily_max_nhours_min.

!-----------------------------------------------------------
! Now find daily maximums of the 8-hour averages.
!-----------------------------------------------------------

day_loop: &
   do fday = 1, fdays_valid		! loop over each valid result day

      hv1 = start_fhour_first + 24 * (fday - 1)  ! get current virtual window
      hv2 = hv1 + (prob%daily_max_nhours - 1)

      h1 = max (hv1, 1)			! limit to available 8-hour averages
      h2 = min (hv2, navg)		! may result in partial windows

      if (diag >= 2) then
         if (fday == 1) print *, '    Search windows, starting forecast' &
            // ' hours of 8-hour averages:'
         print '(7x,a,i0,3(a,2i3))', 'Day ', fday, &
            '   First, last virtual =', hv1, hv2, &
            '   Actual =', h1,  h2, '   Count =', (h2-h1+1)
      end if

! Find daily maximums for current window, i.e. current result day.

! Note, only one non-missing value is sufficient.  This could be parameterized
! as a minimum count requirement, but currently this is just hard coded.

      do x = 1, nx
         do y = 1, ny
            if (any (vavg(x, y, h1:h2) /= vmiss)) then
               daily_out(x, y, fday) = maxval ( vavg(x, y, h1:h2), &
                                               (vavg(x, y, h1:h2) /= vmiss) )
						! find maximum over mask
            else
               daily_out(x, y, fday) = vmiss
            end if
         end do
      end do

   end do day_loop

   if (diag >= 2) print *, 'daily_max: Return.'

end subroutine daily_max
end module daily__max
