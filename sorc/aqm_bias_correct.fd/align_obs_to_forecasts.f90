!------------------------------------------------------------------------------
!
! align_obs_to_forecasts.f90  -- Align obs data with interp. forecast arrays.
!
! * Align obs data with interpolated forecast arrays, by site ID's.
! * Reshape and duplicate into conforming N-hour overlapping subsets.
! * Phase shift obs hours for correct alignment with forecast hours.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jun-19	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2014-jun-26	Format bug fix, caught by ifort.
!
! 2015-jun-13	Quick fix for obs time alignment problem.
!		Align obs HOUR+1 with model HOUR.  See below.
!
! 2020-apr-17	Minor.  Remove invalid missing value count in diagnostic.
! 2020-may-03	Comment fix only.  Change 48-hour forecasts to N-hour.
!
! 2023-apr-05	New time alignment standard, hourly forward averaged
!		  convention, for all obs input sources.
!		Remove one-hour time adjustment from 2015-jun-13.
!		See time alignment notes in read_obs_series_bufr.f90.
!
! * Remember to update the date in the module_id below.
!
! Primary inputs:
!
! * Obs straight hourly time series for bias correction target variable.
! * Obs site ID's and coordinates.
! * Site ID's and coordinates for the interpolated forecast data.
!
! Primary output:
!
! * Obs data reshaped and duplicated as needed, to conform to the
!   N-hour forecast arrays (48, 72, etc.)
!
! Time alignment of obs and model data:
!
! Obs input time series are assumed to always start on hour 00
! of some starting date.  Obs are assumed to be HOURLY FORWARD
! AVERAGED with respect to each time label.
!
! Caller's model arrays are assumed to start on some arbitrary
! hour, 00 to 23, of the SAME starting date as the obs data.
! E.g. 06Z or 12Z.  This hour is specified in the cycle_time
! calling argument.
!
! Obs output data from this routine are aligned on these
! assumptions.
!
! Notes:
!
! Obs are matched to interpolated locations by site ID's.
! Any obs sites that are not matched to interpolated sites are
! discarded, with diagnostic messages.
!
! Obs data are stretched to conform to N-hour model forecast
! cycles, by copying N-hour time segments for each 24 hour
! increment in the forecast starting date and hour.  Thus,
! duplicate obs data is returned in the output array.
!
!------------------------------------------------------------------------------

module align__obs_to_forecasts
contains

subroutine align_obs_to_forecasts (obs_in, cycle_time, ndays, nhours, &
      vmiss, obs_ids, obs_lats, obs_lons, site_ids, site_lats, site_lons, &
      diag, obs_reshaped)

   use config, only : dp
   implicit none

   character(*), parameter :: &
      module_id = 'align_obs_to_forecasts.f90 version 2023-apr-05'

! Input arguments.

   real(dp),     intent (in) :: obs_in(:,:)	! obs original time series
   						!   (time, sites)
   integer,      intent (in) :: cycle_time	! start hour of forecast cycle,
   						!   0-23 (e.g. 06Z, 12Z)
   integer,      intent (in) :: ndays		! number of days in output
   						!   array, to match model arrays
   integer,      intent (in) :: nhours		! number of hours in forecast
   						!   cycle, e.g. 48 hours
   real(dp),     intent (in) :: vmiss		! missing value code in data

   character(*), intent (in) :: obs_ids(:)	! obs site ID strings (S)
   real(dp),     intent (in) :: obs_lats(:)	! obs site coordinates (S)
   real(dp),     intent (in) :: obs_lons(:)

   character(*), intent (in) :: site_ids(:)	! model site ID strings (S)
   real(dp),     intent (in) :: site_lats(:)	! model site coordinates (S)
   real(dp),     intent (in) :: site_lons(:)

   integer,      intent (in) :: diag		! diag verbosity level, 0-N

! Output argument.

   real(dp), intent (out), allocatable :: obs_reshaped(:,:,:,:)  ! output array
  						! (days, hours, vars, sites)
  						! vars = dummy size 1 dimension
! External function definition.

   real distance_btw_coords

! Local parameters.

! Limit for discrepancy between obs coordinates and model interpolated
! coordinates, for the same site ID.
! This is the actual surface distance between two points on the globe.
! Currently makes only warning messages, when exceeded.

   real, parameter :: max_coordinate_drift = 0.01   ! kilometers

! Local variables.

   integer si_obs, si_model, si2
   integer nsites_obs_in, nsites_model
   integer ntimes_in, nvars
   integer nvalid, nmiss_out
   integer nmatch, no_match
   integer iday, ivar, ti1, ti2
   integer ti_obs_hour0, nhours_copy

   real(dp) dist, vmin, vmax, percent_miss

   logical found

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 1) print *
   if (diag >= 1) print '(a)', 'align_obs_to_forecasts: Start.'
   if (diag >= 2) print '(a)', '  Module ID = ' // module_id

   nsites_obs_in = size (obs_ids)		! get dimensions
   nsites_model  = size (site_ids)

   ntimes_in     = size (obs_in, 1)		! total HOURS in straight
   						! obs input tine series

   nmatch   = 0					! clear counters
   no_match = 0

! Allocate obs reshaped array.  Site dimension is for sites in model arrays.

   if (diag >= 2) print *, '  Allocate obs reshaped array.'

   nvars = 1			! dummy dimension needed by analog routine

   allocate (obs_reshaped(ndays, nhours, nvars, nsites_model))

! Clear output array to all missing values, in case of unmatched sites.

   obs_reshaped(:,:,:,:) = vmiss

!-------------------------------------------------
! Main loop over each input obs site.
!-------------------------------------------------

site_loop: &
   do si_obs = 1, nsites_obs_in

! Find input obs site in the list of model site ID's.

      do si2 = 1, nsites_model				! match by site ID's
         si_model = si2
         found = (obs_ids(si_obs) == site_ids(si_model))
         if (found) exit
      end do

! Check for site not found.

      if (.not. found) then
         if (diag >= 1) &
            print '(3a)', '*** Obs site ', obs_ids(si_obs), &
               ' not found in interpolated data.  Site discarded.'
         no_match = no_match + 1		! count sites not matched
         cycle site_loop
      end if

      nmatch = nmatch + 1			! count sites matched

! Check for mismatched coordinates.  Warning only.

      dist = distance_btw_coords ( &
         real (obs_lats (si_obs  )), real (obs_lons (si_obs  )), &
         real (site_lats(si_model)), real (site_lons(si_model))  )

      if (dist > max_coordinate_drift .and. diag >= 1) then
         print *
         print *, '*** align_obs_to_forecasts: Warning:'
         print *, '*** Difference in coordinates between obs' &
            // ' and interpolated data exceeds limit.'
         print *, '*** Site ID = ' // trim (obs_ids(si_obs))
         print '(a,2f12.5)', ' *** Obs coordinates          =', &
            obs_lats(si_obs), obs_lons(si_obs)
         print '(a,2f12.5)', ' *** Interpolated coordinates =', &
            site_lats(si_model), site_lons(si_model)
         print '(a,f12.3,a)', ' *** Surface offset =', dist, ' Km'
         print '(a,f12.3,a)', ' *** Limit          =', &
            max_coordinate_drift, ' Km'
         print '(2a)', ' *** Difference ignored, interpolated coordinates', &
            ' will be used for final output.'
      end if

! Align and copy N-hour conforming obs segments into output array.
! ti1, ti2, etc. are time indices into OBS continuous time series.

      do iday = 1, ndays		! for each cycle starting date...
         ti_obs_hour0 = (iday * 24) - 23	! obs hour 0 of start date

         ti1 = ti_obs_hour0 + cycle_time	! forecast start hour, this date

         ti2 = ti1 + nhours - 1			! last hour of forecast cycle
         ti2 = min (ti2, ntimes_in)		! limit to available obs

         nhours_copy = ti2 - ti1 + 1		! number of obs hours to copy

         if (nhours_copy <= 0) exit	! stop if past end of available obs

         ivar = 1			! subscript for dummy var dimension

         obs_reshaped(iday, 1:nhours_copy, ivar, si_model) &
            = obs_in(ti1:ti2, si_obs)	! copy conforming segment, maybe partial
      end do

   end do site_loop

!-----------------------------------------------------------
! Show final obs aligned statistics.
!-----------------------------------------------------------

   print *
   print *, 'align_obs_to_forecasts:  Final obs aligned summary:'

   vmin  = minval ( obs_reshaped, (obs_reshaped /= vmiss) )
   vmax  = maxval ( obs_reshaped, (obs_reshaped /= vmiss) )

! Count missing values after discarding sites and reshaping.
! Will include multiple counts for values in overlap areas.

   nvalid       = count (obs_reshaped /= vmiss)
   nmiss_out    = size (obs_reshaped) - nvalid
   percent_miss = (nmiss_out * 100.0_dp) / size (obs_reshaped)

   print '(a,i0)',         '   Number of sites included  = ', nmatch
   print '(a,i0)',         '   Number of sites discarded = ', no_match
   print '(2(a,g0.4))',    "   Min, max aligned data     = ", vmin, ', ', vmax

   print '(a,i0,a,f0.1,a)','   Number of missing values  = ', nmiss_out, &
      ' (', percent_miss, '%)'
   print '(2a)',           '     after discarding sites and reshaping'
   print *

   if (diag>=3) print *, 'align_obs_to_forecasts: Done. Return to main program.'

end subroutine align_obs_to_forecasts
end module align__obs_to_forecasts
