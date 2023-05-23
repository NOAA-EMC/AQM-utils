!-----------------------------------------------------------------------------
!
! probability_type.f90 -- Parameter structure for probability routines.
!
! This is a support file for the probability forecast module of the
! NOAA NCO/ARL/PSL bias correction system for CMAQ forecast outputs.
!
! 2019-aug-05	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2020-jul-31	Add parameters for linear regression method.
!
!-----------------------------------------------------------------------------

module probability_type
  use config, only : dp
  implicit none

  type prob_type
    character probability_method*60		! selected probability method
    character daily_statistic_type*60		! type of daily statistic

    real(dp), allocatable :: thresh(:)		! probability threshold levels
    character thresh_units*60			! probability threshold units
    real(dp)  horizontal_length_scale_rho	! horizontal scale, kilometers
    real(dp)  vertical_length_scale_vdconst	! vertical length scale, meters

    real(dp)  analog_climo_thresh	! 0-1, frac.valid for analog climatology
    real(dp)  model_climo_thresh	! 0-1, frac.valid for model climatology
    integer   model_ndays_climo		! number of days in model climo period
    integer   model_ndays_end		! climo end days before current forecast
    real(dp)  analog_variance_low_limit	! min. variance for analog climatology
    real(dp)  model_variance_low_limit	! min. variance for model climatology

    integer   ndays_best_fit		! best fit period for linear regression

    integer   daily_avg_start_time_utc	! UTC start time for daily averages
    integer   daily_avg_nhours		! number of hours in daily averages
    integer   daily_avg_nhours_min	! min. no. hours for valid daily average
    integer   daily_avg_navg_min	! min. non-missing for valid daily avg.

    integer   daily_max_start_time_utc	! UTC start time for daily maximums
    integer   daily_max_nhours		! number of sliding averages to search
    integer   daily_max_len_avg		! number of hours in sliding averages
    integer   daily_max_navg_min	! min. valid for valid sliding average
    integer   daily_max_nhours_min	! min. no. sliding avgs. for daily max.

    logical   write_supplemental_vars	! write supplemental vars to output file
  end type prob_type

end module probability_type
