!-----------------------------------------------------------------------------
!
! read_config_file_main.f90 -- Read custom config file for bias_correct.f90.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jul-08	Original version.  By Dave Allured.
!		Adapted from the config reader for interpolate_update.
!
! 2016-jan-20	Add site_exception_file parameter.
! 2016-feb-08	Add filter method, number of analogs, common debug controls.
! 2016-feb-09	Check for blank strings in required parameters.
!
! 2017-apr-28	Code restructuring.  Import setup of filter parameter
!		  structures from main_analog.f90 version 2016-feb-15.
! 2017-may-25	Add parameters for predictor weights.
! 2017-jun-01	Add parameters for obs blackout dates.
! 2017-jun-06	Add common code to resolve the optional SUBSET keyword
!		  in the weight file name template.
! 2017-jun-15	Enforce filter method = AnEnMean for weight generation.
!
! 2018-jan-19	Add parameter to select bias formula.
!
! 2019-may-29	Add derived variable controls for wind speed & direction.
! 2019-jun-17	Move lower/upper var limits to separate arrays.
! 2019-aug-02	Add parameters for probability forecasts.
!
! 2020-may-26	Parameter changes for analogs and site weighting.
! 2020-jun-11	Add site_result_file_template.  Change related parameter names.
! 2020-jul-31	Add parameters for linear regression probability method.
! 2020-nov-10	Split off the var table reader into a separate subroutine.
!		Add parameter "analog search window offsets".
!
! 2021-apr-25	Add parameters "site blocking list" and "number of days
!                 for kalman filter".
!
! 2022-apr-11	Add reader code and number of forecast hours, for RRFS-CMAQ.
!		Remove dependency on get_free_unit, use fortran 2008 newunit.
! 2022-may-27	Add params for obs max valid input, and short training period.
!
! 2023-mar-27	Add parameters for number of analogs for short training period.
! 2023-apr-08	Add "obs minimum valid input".
!
! Notes:
!
! The configuration file is a simple text file containing file
! paths and specification tables for data variables to be
! processed.
!
! The configuration file contains comments, and is self-
! documenting.  See a typical bias_correct configuration file
! for more details.
!
!-----------------------------------------------------------------------------

module read__config_file_main
contains

subroutine read_config_file_main (config_file, obs_file_template, &
      interp_file_template, in_gridded_template, reader_code_gridded, &
      hourly_output_template, probability_output_template, &
      new_site_list_template, grid_coord_file, pred_weights_file, &
      site_exception_file, site_blocking_list, nhours, target_obs_var, &
      target_model_var, analog_vars, filter_method, output_limit_method, &
      site_array_file_template, site_result_file_template, &
      day_array_file_template, stop_after_filter, obs_min_input, &
      obs_max_input, obs_blackout_start, obs_blackout_end, &
      apar, dpar, fpar, kpar, prob, wpar)

   use analog__ensemble,   only : apar_type
   use compute__wind,      only : dpar_type
   use config,             only : dp
   use find__analog,       only : fpar_type
   use get_param_module
   use get__window_offsets
   use kf__luca,           only : kpar_type
   use probability_type,   only : prob_type
   use read__var_table
   use stdlit,             only : normal
   use string_utils
   use weight__control,    only : wpar_type
   implicit none

   character(*), intent(in ) :: config_file	! name of config file to read

   character(*), intent(out)              :: obs_file_template
   character(*), intent(out)              :: interp_file_template
   character(*), intent(out)              :: in_gridded_template
   character(*), intent(out)              :: reader_code_gridded
   character(*), intent(out)              :: hourly_output_template
   character(*), intent(out)              :: probability_output_template
   character(*), intent(out)              :: new_site_list_template
   character(*), intent(out)              :: grid_coord_file
   character(*), intent(out)              :: pred_weights_file
   character(*), intent(out)              :: site_exception_file
   character(*), intent(out)              :: site_blocking_list
   integer,      intent(out)              :: nhours
   character(*), intent(out)              :: target_obs_var
   character(*), intent(out)              :: target_model_var

   character(*), intent(out), allocatable :: analog_vars(:)

   character(*), intent(out)              :: filter_method
   character(*), intent(out)              :: output_limit_method
   character(*), intent(out)              :: site_array_file_template
   character(*), intent(out)              :: site_result_file_template
   character(*), intent(out)              :: day_array_file_template
   real(dp),     intent(out)              :: obs_min_input, obs_max_input
   integer,      intent(out)              :: obs_blackout_start(3)	! M-D-H
   integer,      intent(out)              :: obs_blackout_end(3)	! M-D-H
   logical,      intent(out)              :: stop_after_filter

   type (apar_type), intent(out)          :: apar	! subsystem parameter
   type (dpar_type), intent(out)          :: dpar	! structures
   type (fpar_type), intent(out)          :: fpar
   type (kpar_type), intent(out)          :: kpar
   type (prob_type), intent(out)          :: prob
   type (wpar_type), intent(inout)        :: wpar	! contains prior setting

! Local variables.

   character(200) errmsg
   character(200) weight_file_template
   character subset_str*30
   character required_gen_method*60
   character string*60

   integer ios, status
   integer cf					! unit number for config file
   integer lnum					! line number within config file

! Analog var limits from var table.  Will be converted to output arguments
! when QC for gridded analog var inputs is implemented.

   real(dp), allocatable :: var_lower_limits(:)
   real(dp), allocatable :: var_upper_limits(:)

! Open config file for input.

   print *
   print '(a)', 'Read configuration file.'
   print '(a)', '  File = ' // trim (config_file)
   print *

   open (newunit=cf, file=config_file, status='old', action='read', &
      iostat=ios, iomsg=errmsg)

   lnum = 0					! init line counter

   if (ios /= 0) then
      print '(a,i0)', '*** Fatal: Error opening config file, iostat = ', ios
      print '(2a)',   '*** Iomsg = ', trim (errmsg)
      call exit (1)
   end if

!-----------------------------------------------------------
! Read initial specification lines, in the order listed.
!-----------------------------------------------------------

! Item labels are case sensitive.
! Helper routines skip over comment lines and blank lines.

read_file: &
   do			! one pass control block, for escape handling only

      print '(a)', '* File control parameters:'
      print *

      call get_param_string ('obs file template', obs_file_template, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('interp file template', interp_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('gridded input file template',in_gridded_template,&
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('reader code for gridded', reader_code_gridded, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      print *

      call get_param_string ('hourly output template', hourly_output_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('probability output template', &
         probability_output_template, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('new site list template', new_site_list_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      print *

      call get_param_string ('grid coordinate file', grid_coord_file, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('predictor weights file', weight_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('site exception file', site_exception_file, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('site blocking list', site_blocking_list, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_int ('number of forecast hours', nhours, cf, status, lnum)
      if (status /= normal) exit read_file

      print *

      call get_param_string ('target obs variable', target_obs_var, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('target model variable', target_model_var, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read analog var table.
!-----------------------------------------------------------

      call read_var_table (analog_vars, var_lower_limits, var_upper_limits, &
         apar%fvar, fpar, kpar, target_model_var, cf, status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read derived variable controls.
!-----------------------------------------------------------

      print '(a)', '* Derived variable controls:'
      print *

      call get_param_string ('derived wind direction variable', &
         dpar%derived_wind_dir_var, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('derived wind speed variable', &
         dpar%derived_wind_speed_var, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('derivative input U wind', &
         dpar%uwind_var, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('derivative input V wind', &
         dpar%vwind_var, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read input filter controls.
!-----------------------------------------------------------

      print *
      print '(a)', '* Input filter controls:'
      print *

      call get_param_real ('obs minimum valid input', obs_min_input, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('obs maximum valid input', obs_max_input, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_mdhz ('obs blackout start date', obs_blackout_start, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_mdhz ('obs blackout end date', obs_blackout_end, cf, &
         status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read analog filter controls.
!-----------------------------------------------------------

      print *
      print '(a)', '* Analog filter controls:'
      print *

      call get_param_string ('filter method', filter_method, cf, status, lnum, &
         nonblank)
      if (status /= normal) exit read_file

! Enforce required AnEnMean method if weight generation is selected.

      required_gen_method = 'AnEnMean'

      if ((wpar%gen_weights) .and. (filter_method /= required_gen_method)) then
         print *, '*** read_config_file_main: Switching to filter method = ' &
            // trim (required_gen_method) // '.'
         print *, '*** This method is required when weight generation is' &
            // ' selected.'
         print *
         filter_method = required_gen_method
      end if

      call get_param_int ('number of analogs', apar%num_analogs, cf, status, &
         lnum)
      if (status /= normal) exit read_file

      call get_param_int ('minimum number of analogs', apar%min_num_analogs, &
         cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_string ('analog weights', apar%weight_type, cf, status, &
         lnum, nonblank)
      if (status /= normal) exit read_file

! Formula to calculate AnEnMean.  Originally misnamed as "bias formula".

      call get_param_string ('analog mean', string, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      if (string == 'mean (forecast plus model predictions) plus bias') then
         apar%analog_mean = 'mean plus bias'	! less cumbersome internal name
      else
         apar%analog_mean = string	! names will be checked internally
      end if

      print *

      call get_window_offsets ('analog search window offsets', fpar%window, &
         cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of days for kalman filter', &
         kpar%ndays_kalman, cf, status, lnum)
      if (status /= normal) exit read_file

      print *

      call get_param_real ('obs threshold to shorten training period', &
         apar%short_train_thresh, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of days in short training period', &
         apar%short_train_ndays, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of analogs for short training period', &
         apar%num_analogs_short, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int('minimum number of analogs for short training period',&
         apar%min_num_analogs_short, cf, status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read post processing controls.
!-----------------------------------------------------------

      print *

      call get_param_string ('output limit method', output_limit_method, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read weight generation controls.
!-----------------------------------------------------------

      print *
      print '(a)', '* For weight generation only:'
      print *

      call get_param_int ('number of weight increments', wpar%nweights, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of days in test period', &
         wpar%ndays_test_period, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('last forecast hour for weight generation', &
         wpar%forecast_last_hour, cf, status, lnum)
      if (status /= normal) exit read_file

!-------------------------------------------------------
! Read probability forecast controls.
!-------------------------------------------------------

      print *
      print '(a)', '* Probability forecast controls:'
      print *

      call get_param_string ('probability method', prob%probability_method, &
         cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_string ('type of daily statistic', &
         prob%daily_statistic_type, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_list ('probability threshold levels', prob%thresh, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_string ('probability threshold units', prob%thresh_units, &
         cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('horizontal length scale (rho), kilometers', &
         prob%horizontal_length_scale_rho, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('vertical length scale (vdconst), meters', &
         prob%vertical_length_scale_vdconst, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of days in linear best fit period', &
         prob%ndays_best_fit, cf, status, lnum)
      if (status /= normal) exit read_file

!-------------------------------------------------------
! Read controls for climatology inputs for probability.
!-------------------------------------------------------

      print *

      call get_param_real ('percent valid daily values for valid analog' &
         // ' climatology', prob%analog_climo_thresh, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('percent valid daily values for valid model' &
         // ' climatology', prob%model_climo_thresh, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of days in model climatology period', &
         prob%model_ndays_climo, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('end of model climatology period, number of days' &
         // ' before current forecast', prob%model_ndays_end, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('minumum variance for analog climatology', &
         prob%analog_variance_low_limit, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_real ('minumum variance for model climatology', &
         prob%model_variance_low_limit, cf, status, lnum)
      if (status /= normal) exit read_file

!-------------------------------------------------------
! Read controls for daily averages for probability.
!-------------------------------------------------------

      print *

      call get_param_int ('UTC start time for daily averages', &
         prob%daily_avg_start_time_utc, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('nominal number of hours in each daily average', &
         prob%daily_avg_nhours, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('minimum number of hours for valid daily average', &
         prob%daily_avg_nhours_min, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('minimum number of non-missing values for valid' &
         // ' daily average', prob%daily_avg_navg_min, cf, status, lnum)
      if (status /= normal) exit read_file

!-------------------------------------------------------
! Read controls for daily 8-h maximums for probability.
!-------------------------------------------------------

      print *

      call get_param_int ('UTC start time for daily maximums', &
         prob%daily_max_start_time_utc, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of sliding averages to search for daily' &
         // ' maximum', prob%daily_max_nhours, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('number of hours in sliding averages for daily' &
         // ' maximum', prob%daily_max_len_avg, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('minimum number of non-missing values for valid' &
         // ' sliding average', prob%daily_max_navg_min, cf, status, lnum)
      if (status /= normal) exit read_file

      call get_param_int ('minimum number of sliding averages for valid daily' &
         // ' maximum', prob%daily_max_nhours_min, cf, status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read diagnostic controls.
!-----------------------------------------------------------

      print *
      print '(a)', '* Dignostic controls:'
      print *

      call get_param_string ('site array file template', &
         site_array_file_template, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('site result file template', &
         site_result_file_template, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('day array file template', &
         day_array_file_template, cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      print *

      call get_param_yesno ('stop after filter', stop_after_filter, cf, &
         status, lnum)
      if (status /= normal) exit read_file

      call get_param_yesno ('write supplemental probability variables', &
         prob%write_supplemental_vars, cf, status, lnum)
      if (status /= normal) exit read_file

      print *
      print '(a)', '* End of config file.'
      print *

      exit read_file		! normal exit for single pass block structure

   end do read_file

!-----------------------------------------------------------
! End of read_file block.  Exit on read errors.
!-----------------------------------------------------------

   if (status /= normal) then
      print '(a,i0)', '*** read_config_file_main: Abort, config file line' &
         // ' number = ', lnum
      call exit (1)
   end if

!-------------------------------------------------
! Configuration guards.
!-------------------------------------------------

!-------------------------------------------------
! Resolve the weight file name template.
!-------------------------------------------------

! This is common code needed for both the read and write subroutines
! for predictor weight files.

! The embedded SUBSET keyword is optional.  The weight file name will
! remain unchanged in any case, if the SUBSET keyword is not present.

! If weight subsetting is used, then replace the SUBSET keyword string
! with weight subset numbers.  The subset numbers are optionally
! specified on the program command line.  Negative means subsetting
! is not selected.

   pred_weights_file = weight_file_template

   if (wpar%subset1 >= 0) then
      write (subset_str, "(i0,'-',i0)") wpar%subset1, wpar%subset2
      				! variable length integers, no leading zeros
      call replace_substring (pred_weights_file, 'SUBSET', trim (subset_str))

! If subsetting is not selected, then OMIT the SUBSET keyword with
! one adjacent period character.  This applies to both normal bias
! correction mode, as well as full weight generation with no subsetting.

   else
      call replace_substring (pred_weights_file, '.SUBSET.', '.')
   end if

!-------------------------------------------------
! Additional fixed parameters.
!-------------------------------------------------

! Imported from main_analog version 2016-feb-15.
! These parameters could be migrated into the config file, as needed.

! Note, some items within these structures are now dynamically managed
! within filter methods, not here.

   apar%skipMissingAnalogs = 1	! Applies to both ANKF and AN
				! 1: Always use num_an analogs, even if
				! some of the best ones are missing
   fpar%useRealTrends = 0	! 1: Use trend, 0: use neighbours
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2

! Correction for speed or ozone or PM2.5.

   kpar%enforce_positive = 1	! enforce correction for values > 0

   fpar%lowerMetric = 0.00001	! Lower bound for allowed metric value

! Kalman filter parameters.

   kpar%varo = 0.005		! variance of observation variance
   kpar%varp = 1		! variance prediction variance

   kpar%ratio = 0.1		! KF method parameter (sigma_ratio)

   kpar%start = (/ 0, 0 /)	! starting point to compute statistics
   kpar%timeZone = 0		! timeZone of measurements (for output graphics)

! Analog parameters.

   fpar%trend = (/ 1,1,1 /)	! check hour-1, hour & hour+1 for the var trend
  				! with equal weighting coefficients
				! MUST have odd number of elements.
				! Numbers are the weighted coefficients.
				! 2014-feb-19, MUST have exactly 3 elements.

! Normal exit.  Output parameters and structures are now initialized
! as much as possible at this point.

end subroutine read_config_file_main
end module read__config_file_main
