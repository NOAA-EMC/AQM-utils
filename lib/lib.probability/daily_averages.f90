!------------------------------------------------------------------------------
!
! daily_averages.f90 -- Compute daily averages for forecast time series.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-jul-11	daily_averages.f90:
!		Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Adapted from daily_max.f90 version 2019-jul-10.
!		Make common array calculations for both model and site data.
! 2019-aug-06	Move run parameters from hard coded, to config file.
!
! 2020-aug-04	Minor.  Clean up messages.
!
! Summary of daily average calculations:
!
! * Input:  3-D array of forecast time series, one dimension being
!      forecast hours.
! * Input may be either gridded model data, or best analogs series.
! * Output: 3-D array of forecast daily values, one dimension being
!      forecast days.
! * Determine quaifying averaging windows within available forecast hours.
! * Compute daily averages for each qualifying forecast day, and
!      each grid point.
! * All grid points (matrix time series) are computed independently.
!
! Call this routine once per forecast cycle, to compute daily
! averages for each qualifying interval in that forecast cycle.
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

module daily__averages
contains

subroutine daily_averages (indata, vmiss, cycle_time, prob, diag, &
      daily_out, fdays_valid)

   use config, only : dp
   use probability_type
   implicit none

! Calling arguments.

   real,           intent(in) :: indata(:,:,:)  ! forecast series (X, Y, hours)
   real(dp),       intent(in) :: vmiss		! common missing value code
   integer,        intent(in) :: cycle_time	! forecast cycle time
   type(prob_type),intent(in) :: prob		! probability config parameters
   integer,        intent(in) :: diag		! diag. verbosity level, 0-N

   real(dp), intent(out) :: daily_out(:,:,:)	! daily avg output (X, Y, fdays)
   integer,  intent(out) :: fdays_valid		! number of valid days in output

! Local variables.

   integer x, y, nx, ny, fday
   integer h1, h2, hv1, hv2, nhours

   integer end_utc, end_fhour
   integer start_fhour_first, start_fhour_last
   integer start_fhour_max
   integer offset_last, dif_hours

! Initialize.

   if (diag >= 2) print *, 'daily_averages: Start.'

   nx     = size (indata, 1)			! get dimensions
   ny     = size (indata, 2)
   nhours = size (indata, 3)

!-----------------------------------------------------------
! Find valid days and time windows for daily averages.
!-----------------------------------------------------------

! Time window parameters from config file, with prob% prefix.

! daily_avg_start_time_utc = UTC instantaneous start time for daily averages
! daily_avg_nhours	   = nominal number of hours in each daily average
! daily_avg_nhours_min     = minimum number of hours for valid daily average
! daily_avg_navg_min       = minimum number of non-missing values for valid
!                              daily average

! Determine the first valid averaging window, possibly truncated.
! Work backward from the implied UTC ending hour.
! Based only on the specified UTC start time, plus limit parameters.

! First determine the UTC time of the last forecast hour in the
! first proposed time window.  Forecast hours are backward averaged.
! Therefore each forecast hour's UTC label is one hour later than
! its UTC instantaneous start time.  The following calculation of
! end_utc accounts for this backward averaging.

   end_utc   = prob%daily_avg_start_time_utc + prob%daily_avg_nhours
   					! last UTC forecast hour in first
                                        ! daily average, not normalized!

   end_fhour = end_utc - cycle_time	! convert UTC to BASE forecast hour num.

   if (diag >= 2) print '(7x,a,i0)', 'end_utc              = ', end_utc
   if (diag >= 2) print '(7x,a,i0)', 'end_fhour original   = ', end_fhour

   end_fhour = 1 + modulo ((end_fhour - 1), 24)   ! normalize to first available
   						  ! forecast hours, 1-24

   if (diag >= 2) print '(7x,a,i0)', 'end_fhour normalized = ', end_fhour

! Check for sufficient number of hours in this proposed first day window.

   if (end_fhour < prob%daily_avg_nhours_min) &	  ! if insufficient hours:
      end_fhour = end_fhour + 24		  ! advance to next day window

   start_fhour_first = end_fhour - (prob%daily_avg_nhours - 1)
   				! Get forecast hour for START of chosen
				! first day averaging window.
				! Must be NOT NORMALIZED for hour base of loop!

   if (diag >= 2) print '(7x,a,i0)','end_fhour sufficient = ', end_fhour
   if (diag >= 2) print '(7x,a,i0)','start_fhour_first    = ', start_fhour_first

! Determine last valid averaging window, possibly truncated.
! Work forward from the start of the first day averaging window,
! plus limit parameters.

   start_fhour_max = nhours - (prob%daily_avg_nhours_min - 1)
   				! forecast hour of last possible window start
				! with SUFFICIENT NUMBER of hours

   offset_last = modulo ((start_fhour_max - start_fhour_first), 24)
   				! backward offset to chosen last window start,
   				! always 0 to 23 hours back from last possible

   start_fhour_last = start_fhour_max - offset_last
   				! step back to start hour of last daily average

! Now determine number of valid statistical days for the result arrays.

   dif_hours   = start_fhour_last - start_fhour_first
   fdays_valid = 1 + (dif_hours / 24)

! Consistency checks.

   if (   (modulo (dif_hours, 24) /= 0) &
     .or. (start_fhour_first > start_fhour_last) ) then
      print '(a,i0)', '*** daily_averages: FATAL: Algorithm failure.'
      print '(a,i0)', '*** Derived time windows for daily averages are invalid.'
      print '(a,i0)', '*** start_fhour_first    = ', start_fhour_first
      print '(a,i0)', '*** start_fhour_last     = ', start_fhour_last
      print '(a,i0)', '*** no. hours difference = ', dif_hours
      call exit (1)
   end if

! Note.  The key parameters at this point are start_fhour_first,
! start_fhour_last, and daily_avg_nhours.  Combined, these represent
! a set of VIRTUAL averaging windows to be overlaid on the available
! forecast hours.  There is one window for each valid daily
! averaging interval.
!
! The VIRTUAL windows are always the same size.  But NOTE, because
! of alignment, the first and last ACTUAL windows in daily averages
! may be smaller than the VIRTUAL windows.  It is left to the next
! stage, to limit to available data and array bounds.
!
! This parameter stage guarantees that every defined VIRTUAL window
! is a valid averaging window.  In other words, every VIRTUAL window
! will contain at least the minimum number of hours specified by
! the user parameter daily_avg_nhours_min.

!-----------------------------------------------------------
! Now compute daily averages over the valid time windows.
!-----------------------------------------------------------

day_loop: &
   do fday = 1, fdays_valid		! loop over each valid result day

      hv1 = start_fhour_first + 24 * (fday - 1)  ! get current virtual window
      hv2 = hv1 + (prob%daily_avg_nhours - 1)

      h1 = max (hv1, 1)			! limit to available forecast hours
      h2 = min (hv2, nhours)		! may result in partial windows

      if (diag >= 2) then
         if (fday == 1) print *,'    Forecast hours of valid averaging windows:'
         print '(7x,a,i0,3(a,2i3))', 'Day ', fday, &
            '   First, last virtual =', hv1, hv2, &
            '   Actual =', h1,  h2, '   Nhours =', (h2-h1+1)
      end if

! Compute daily averages for current window, i.e. current result day.

      do x = 1, nx
         do y = 1, ny
            if (count (indata(x, y, h1:h2) /= vmiss) &
                                      >= prob%daily_avg_navg_min) then
               daily_out(x,y,fday) = sum (indata(x, y, h1:h2), &
                                           (indata(x, y, h1:h2) /= vmiss) ) &
                                   / count (indata(x, y, h1:h2) /= vmiss)
				! (sum over mask) divided by (count over mask)
            else
               daily_out(x,y,fday) = vmiss
            end if
         end do
      end do

   end do day_loop

   if (diag >= 2) print *, 'daily_averages: Return.'

end subroutine daily_averages
end module daily__averages
