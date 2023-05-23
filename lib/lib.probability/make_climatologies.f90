!------------------------------------------------------------------------------
!
! make_climatologies.f90 -- Make all climatologies for probability module.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-may-16	Data preprocessors by Irina Djalalova, NOAA/OAR/ESRL/PSD/CIRES:
!		  dataascii_preparation_for_probability_code.pro
!		  datanetcdf_preparation_for_probability_code.pro
!		These are prototype routines to calculate recent climatologies
!		  for forecast days.
!
! 2019-aug-07	probability.f90:
!		Release version, original module to integrate probability
!		  forecasts into bias correction.
!
! 2019-dec-27	make_climatologies.f90:
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Climatology routines split off from probability.f90
!		  version 2019-aug-07.
!		No functional changes.
!
! Primary inputs:
!
! * Gridded raw forecast files for current forecast cycle.
! * Best analogs at site locations, memory array.
!
! Primary outputs:
!
! * Forecast day recent climatologies for gridded and site inputs.
! * Mean and variance arrays for each kind of climatology.
!
! Forecast date index:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
!------------------------------------------------------------------------------

module make__climatologies
contains

subroutine make_climatologies (in_gridded_template, grid_coord_file, &
      target_var, forecast_date, cycle_time, base_year, calendar, &
      best_analogs, vmiss, prob, diag, climo_mean, climo_variance, &
      analog_mean, analog_variance)

   use config, only : dp
   use gridded__climatology
   use probability_type
   use site__climatology
   implicit none

! Input arguments.

   character(*), intent(in) :: in_gridded_template  ! gridded input template
   character(*), intent(in) :: grid_coord_file	    ! aux. grid coordinate file
   character(*), intent(in) :: target_var	    ! target variable name

   integer,      intent(in) :: forecast_date	    ! target date index
   integer,      intent(in) :: cycle_time	    ! forecast cycle time
   integer,      intent(in) :: base_year	    ! base year for date indexes
   character(*), intent(in) :: calendar	  	    ! calendar system in use

   real(dp),     intent(in) :: best_analogs(:,:,:)  ! best analogs, forecast day
						    ! (hours, analogs, sites)
  						    ! sort order does not matter

   real(dp),        intent(in) :: vmiss		    ! common missing value code
   type(prob_type), intent(in) :: prob		    ! probability config params
   integer,         intent(in) :: diag		    ! diag verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: &
                          climo_mean(:,:,:), &	    ! climo result grids
                          climo_variance(:,:,:), &  !   (X, Y, fdays)
                          analog_mean(:,:), &	    ! site climos for analogs
                          analog_variance(:,:)	    !   (sites, fdays)

! Local variables.

   character fmt1*60, fmt2*60

   integer nhours_expect
   integer status
   integer count_mod, count_ana
   integer nonmiss_mod, nonmiss_ana

   real(dp) min_mod1, min_mod2, max_mod1, max_mod2
   real(dp) min_ana1, min_ana2, max_ana1, max_ana2

!-------------------------------------------------------
! Compute model climatology grids from past forecasts.
!-------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, '  make_climatologies: Call gridded_climatology.'

! Get expected number of forecast hours before calling reader.
! Actual forecast hours for target var in file should normally be the same.

   nhours_expect = size (best_analogs, 1)

! Gridded climatology module:  Read block of past gridded forecasts.
! Compute requested averages and maximums, then compute climatology
! means and variances.

   call gridded_climatology (in_gridded_template, grid_coord_file, &
      target_var, forecast_date, cycle_time, nhours_expect, base_year, &
      calendar, vmiss, prob, diag, climo_mean, climo_variance, status)

! Model climo result grids, mean and variance: (X, Y, fdays)

!-------------------------------------------------------
! Compute climatologies at sites for best analogs.
!-------------------------------------------------------

   print *
   print *, 'make_climatologies: Call site_climatology.'

! For site data, climatologies are defined over the range of best analogs,
! rather than over a discrete historical interval.  Date referencing
! is not needed in this calculation.

   call site_climatology (best_analogs, vmiss, cycle_time, prob, diag, &
      analog_mean, analog_variance)

! Analog climo result arrays, mean and variance: (sites, fdays)

!-------------------------------------------------------
! Limit variances to prevent them from going too low.
!-------------------------------------------------------

   print *
   print *, 'make_climatologies: Apply lower limit to variances.'

! Get diagnostics before limiting.

   min_mod1     = minval (climo_variance,  (climo_variance  /= vmiss))
   max_mod1     = maxval (climo_variance,  (climo_variance  /= vmiss))

   min_ana1     = minval (analog_variance, (analog_variance /= vmiss))
   max_ana1     = maxval (analog_variance, (analog_variance /= vmiss))

   count_mod    = count  ( (climo_variance  /= vmiss) &
                     .and. (climo_variance  < prob%model_variance_low_limit ) )

   count_ana    = count  ( (analog_variance /= vmiss) &
                     .and. (analog_variance < prob%analog_variance_low_limit) )

! Apply lower limits.

   print '(a,g0.4)', '   Limit model  climo variances to lower limit of ', &
      prob%model_variance_low_limit

   where (climo_variance /= vmiss) &
      climo_variance  = max (climo_variance,  prob%model_variance_low_limit)

   print '(a,g0.4)', '   Limit analog climo variances to lower limit of ', &
      prob%analog_variance_low_limit

   where (analog_variance /= vmiss) &
      analog_variance = max (analog_variance, prob%analog_variance_low_limit)

! Print summary.

   min_mod2    = minval (climo_variance,  (climo_variance  /= vmiss))
   max_mod2    = maxval (climo_variance,  (climo_variance  /= vmiss))

   min_ana2    = minval (analog_variance, (analog_variance /= vmiss))
   max_ana2    = maxval (analog_variance, (analog_variance /= vmiss))

   nonmiss_mod = count  (climo_variance  /= vmiss)
   nonmiss_ana = count  (analog_variance /= vmiss)

   fmt1 = '(5x,a,2g15.4)'
   fmt2 = '(5x,a,i15)'

   print *
   print fmt1, 'Model  variance min, max before limit = ', min_mod1, max_mod1
   print fmt1, 'Model  variance min, max after  limit = ', min_mod2, max_mod2
   print fmt2, 'Model  variance count below limit     = ', count_mod
   print fmt2, 'Model  variance count non-missing     = ', nonmiss_mod

   print *
   print fmt1, 'Analog variance min, max before limit = ', min_ana1, max_ana1
   print fmt1, 'Analog variance min, max after  limit = ', min_ana2, max_ana2
   print fmt2, 'Analog variance count below limit     = ', count_ana
   print fmt2, 'Analog variance count non-missing     = ', nonmiss_ana

end subroutine make_climatologies
end module make__climatologies
