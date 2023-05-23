!------------------------------------------------------------------------------
!
! aqm_bias_correct.f90
!
! Main program to perform bias correction on a gridded forecast
! time series for one variable, and one forecast cycle.
!
! This is the combined QC, analog filter, and spreading components
! of the NOAA NCO/ARL/PSL bias correction system for CMAQ forecast
! outputs.
!
! 2014-jul-16	bias_correct.f90:
!		Original version of main program and process control.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
! 2014-jul-23	Fix program name in error message.
!
! 2015-jun-13	Stop reading obs data at the day before current forecast date.
!		For retrospective testing, this prevents results too perfect,
!		  due to current day obs that are unavailable in normal
!		  real-time operation.
! 2015-oct-27	Pass site ID's and coordinates into main_analog, for
!		  diagnostics and site exception handling.
!
! 2016-jan-20	Parallel support.  Move site exception list reader out of
!		  site loop, up to main program.
! 2016-feb-09	Add config parameters: filter method, debug controls, etc.
! 2016-feb-10	Move test file writers out of main_analog, up to main program.
!
! 2017-apr-04	Minor.  Add cycle_time to test file names.
! 2017-apr-29	Code restructured for weight generation, parallel control, etc.
!		Module analog_control replaces main_analog.
!		Run parameters consolidated into structures apar, fpar, kpar.
! 2017-may-17	Add support for site-specific predictor weighting.
! 2017-jun-05	Add support for obs blackout dates.
! 2017-jun-06	Add support for predictor weight generation by subsets.
!
! 2018-jan-19	Patch release.  Add new bias formula for present forecast only.
!		Add config parameter "bias_formula".
!
! 2019-may-31	Add support for varying number of forecast hours per variable.
!		Add option to compute vector fields after interpolation;
!		  wind speed and direction in this case.
!
! 2019-jun-18	Extract best analogs from lower levels, for probability.
! 2019-jul-05	Fix indexing bug in blackout_obs_data.f90 that caused crashes.
! 2019-aug-07	Add probability forecasts module and options.
! 2019-nov-20	Release version for ozone probability.
!
! 2020-feb-12	Bug fix for wind direction formula in compute_wind.f90.
! 2020-may-14	Prevent use of retrospective future obs when generating weights.
! 2020-may-26	Update config parameters for site weight generation.
! 2020-jun-11	Add write_site_result_files.f90 with new test file format.
! 2020-aug-05	Add linear regression method for probability forecasts.
!		Reverse order, make hourly forecasts before probability.
! 2020-nov-16	Fix support for search window wider than current forecast hour.
!		Add support for asymmetric search window bounds.
!
! 2021-mar-24	Fix support for mixed forecast lengths in spreading.f90.
! 2021-apr-20	Add site bias arrays to main output file, for diagnostics.
! 2021-apr-25	Add option for reduced number of days for Kalman filtering.
! 2021-apr-27	Add site blocking list to remove problem intervals in obs data.
! 2021-nov-11	Improve off-grid site diagnostics.
!
! 2022-apr-20	Add support for hourly gridded input files for RRFS-CMAQ.
!		The number of forecast hours is now pre-determined in the
!		  config file, rather than adapting to the input data set.
! 2022-apr-24	Include forecast hour zero, because it is actually the
!		  first legitimate forecast hour in RRFS-CMAQ.
! 2022-may-25	Add automatic units conversion, as needed, to make analog
!		  arrays in ppm/ppmV conform to new forecast grids in ppb.
!		Add units attributes to output files.
! 2022-may-27	Add option for short training period when high obs detected.
!		Add maximum value limit for input PM2.5.
!
! 2022-jun-03	aqm_bias_correct.f90:
!		Main program name change to conform with NCEP/NCO.
!		Add output routine to conform with new forecast file layout.
! 2022-dec-03	RRFS: Ignore *.f000 files, start with *.f001 = forecast hour 1.
!
! 2023-mar-28	Modify number of analogs when short training period is selected.
!		Add OpenMP diagnostics.
!		Fix PM2.5 QC for higher input threshold values.
! 2023-apr-06	Add support for AirNow Netcdf files, derived from AirNow
!		  HourlyAQ files in text/CSV format.
!		Increase site ID strings from 9 to 12 characters maximum.
! 2023-apr-06	New time alignment standard, hourly forward averaged
!		  convention, for all obs input sources.
!		Internal change only.  Affects only the obs_in array.
!		Compensated in align_obs_to_forecasts.f90.
!		See time alignment notes in read_obs_series_bufr.f90.
! 2023-apr-11	Add lower limit for AirNow negative input values.
!		Add low/high input limits for ozone as well as PM2.5.
!
! * Remember to update the program_id below.
!
! Credits:
!
! The filter component is the analog/Kalman filter for forecast
! time series interpolated to observation site locations.  The
! original Matlab code and filter algorithms are by Luca Delle
! Monache, Thomas Nipen, Badrinath Nagarajan, and Will Cheng
! of NCAR.  Stefano Alessandrini of NCAR contributed refinements
! in Fortran versions.
!
! Core interpolation and spreading routines were developed by
! Irina Djalalova and James Wilczak of NOAA/OAR/ESRL/PSD.
!
! Probability forecast module was developed by Tom Hamill and
! Irina Djalalova of NOAA/OAR/ESRL/PSD, and others.
!
! Primary inputs:
!
! * Configuration file with data specs and other run parameters.
! * AIRNow obs data for target correction variable, for the extent
!   of the training period.
! * CMAQ model forecast data interpolated to station coordinates,
!   training period through target forecast cycle.  This is a data
!   archive produced by the companion program "interpolate_update".
! * CMAQ original gridded hourly forecasts for current forecast cycle.
! * Coordinates and elevations on CMAQ grid.
!
! Primary outputs:
!
! * Bias corrected hourly gridded forecasts for the target variable.
! * Gridded probability forecasts for the target variable.
! * Coordinate list for all sites currently validated by the
!   quality control module.
!
! The two major outputs, bias corrected forecasts and probability
! forecasts, are selected independently via their corresponding
! output filename templates in the config file.  They may be run
! together or separately.  There is some common processing, in
! particular the primary data inputs and analog filter
! calculations.
!
!------------------------------------------------------------------------------
!
! Usage, run command with four or more arguments:
!
! ./aqm_bias_correct config-file forecast-cycle start-date forecast-date \
!                                [options]
!
! Required arguments in fixed order:
!
! config-file	  Configuration file for current platform and data set
!		  set configuration.  Includes file name templates for
!		  input and output files, and variable lists for
!		  analog variables and bias correction target.
!
! forecast-cycle  CC, initial hour for the target forecast cycle.
!		  Currently 06 or 12.  Set to $Cyc for WCOSS real-time
!		  operation.
!
! start-date	  YYYYMMDD: Start date of desired training period.
!
! forecast-date	  YYYYMMDD: Forecast initial date for bias correction.
!		  The training period ends on the day before this date.
!
! Optional arguments, may be in any order following the required args:
!
! diag=N	  Set verbosity level N.  Optional argument, default
!		  is set below.  0 = errors only, 1 = milestones,
!		  2 = brief progress, 3 and up = increasing details.
!		  Caution, 6 and up generate huge outputs.
!
! gen_weights	  Generate new predictor weights file (simple
!		  keyword argument).  Caution, very long run time.
!
! weight1=N	  First and last weight set index numbers, for
! weight2=N	  breaking up weight generation into weight subsets.
!
! Examples:
!
! ./aqm_bias_correct config.pm2.5.5pred 12z 20140515 20140529
! ./aqm_bias_correct config.pm2.5.5pred 06z 20140515 20140612 diag=4
! ./aqm_bias_correct config.pm2.5.5pred 06z 20140515 20140612 gen_weights diag=3
!
! ./aqm_bias_correct config.ozone.gen 06z 20160701 20170331 \
!                                     gen_weights weight1=101 weight2=150
!
! ./aqm_bias_correct config.ozone.probability 06z 20181115 20190807
!
!------------------------------------------------------------------------------

program aqm_bias_correct

   use align__obs_to_forecasts
   use analog__control
   use analog__ensemble,       only : apar_type
   use blackout__obs_data
   use compute__wind,          only : dpar_type
   use config,                 only : dp
   use find__analog,           only : fpar_type
   use get__command_args
   use expand__filename
   use kf__luca,               only : kpar_type
   use index_to_date_mod
   use print__library_info
   use print__omp_info
   use probability_mod
   use probability_type,       only : prob_type
   use read__config_file_main
   use read__exception_list
   use read__grid_coords
   use read__interp_forecasts
   use read__obs_qc
   use read__predictor_weights
   use spreading_mod
   use stdlit
   use weight__control,        only : wpar_type
   use write__predictor_weights
   use write__site_list
   use write__site_result_files
   use write__test_day_files
   use write__test_site_files
   implicit none

   character(*), parameter :: &
      program_id = 'aqm_bias_correct.f90 version 2023-apr-11'

! Local variables.

   integer, parameter :: id_len = 12		! station ID string length
   						! for 12-digit AirNow site ID's
   character(200) config_file, site_list_title
   character(60)  title_varname
   character(24)  fdate_str

   integer ndays, diag, cycle_time, forecast_day_num
   integer start_date, training_end_date, forecast_date, base_year
   integer di, hi, vi, ndays_show

   real(dp) vmiss, standard_vmiss

   logical ex, enable_spreading

! Config file parameters.

   character(200) obs_file_template, interp_file_template
   character(200) in_gridded_template, reader_code_gridded
   character(200) hourly_output_template, probability_output_template
   character(200) new_site_list_template, grid_coord_file
   character(200) pred_weight_file
   character(200) site_exception_file, site_blocking_list
   character(60)  target_obs_var, target_model_var
   character(200) filter_method, output_limit_method
   character(200) day_array_file_template
   character(200) site_array_file_template, site_result_file_template

   integer nhours, obs_blackout_start(3), obs_blackout_end(3)

   real(dp) obs_min_input, obs_max_input

   logical stop_after_filter

   type (apar_type) apar		! subsystem parameter structures
   type (dpar_type) dpar
   type (fpar_type) fpar
   type (kpar_type) kpar
   type (prob_type) prob
   type (wpar_type) wpar

! Analog var table (config file).

   character(60), allocatable :: analog_vars(:)		! var config data (V)

! Obs input data.

   character(id_len), allocatable :: obs_ids(:)		! obs site ID's (S)
   real(dp), allocatable :: obs_lats(:), obs_lons(:)	! obs site coordinates
   real(dp), allocatable :: obs_in(:,:)		 	! obs input time series
  							!  (hours, sites)
   character*80 obs_units				! obs var units

   real(dp), allocatable :: obs_reshaped(:,:,:,:)  ! obs reshaped to model data
   						   ! (days, hours, 1 var, sites)
! Interpolated model input data.

   character(id_len), allocatable :: interp_ids(:) ! model site IDs (S)
   real(dp), allocatable :: interp_lats(:)	   ! model site coordinates (S)
   real(dp), allocatable :: interp_lons(:)
   real(dp), allocatable :: model_in(:,:,:,:)	   ! interpolated. model data
   						   ! (days, hours, vars, sites)
! Grid coordinates.

   real(dp), allocatable :: grid_lats(:,:)	   ! (X, Y)
   real(dp), allocatable :: grid_lons(:,:)

! Target var, uncorrected and corrected data at site locations & forecast date.

   real(dp), allocatable :: uncorr_sites(:,:)	   ! (hours, sites)
   real(dp), allocatable :: corr_sites(:,:)	   ! (hours, sites)

! Target var, corrected forecast values at site locations, all days in period.
! For probability forecasts and diagnostics.

   real(dp), allocatable :: filter_result_all(:,:,:)  ! (days, hours, sites)

! Target var, best found analogs for current forecast, original obs values.
! In ascending order, best analogs last (descending metrics).

   real(dp), allocatable :: best_analogs_obs(:,:,:)   ! (hours, analogs, sites)

! Target var, bias corrected forecast grids from spreading module.
! For passing to probability modle when needed.

   real(dp), allocatable :: corr_grids (:,:,:)	      ! (X, Y, hours)

! Site-specific predefined predictor weights.

   real(dp), allocatable :: pred_weights(:,:)	      ! (vars, sites)

! Site-specific generated predictor weights with best RMSE's.

   real(dp), allocatable :: new_weights(:,:)	      ! (vars, sites)
   real(dp), allocatable :: new_rmse(:)		      ! (sites)

! Site exception bias thresholds.

   real(dp), allocatable :: excep_thresh_low(:)       ! (sites)
   real(dp), allocatable :: excep_thresh_high(:)

! Program parameters.

   character(*), parameter :: prog_name = 'aqm_bias_correct'
   character(*), parameter :: calendar  = 'gregorian'

!-----------------------------------------------------------------
! Initialize.
!-----------------------------------------------------------------

   call fdate (fdate_str)
   print *
   print *, &
      '======================================================================='
   print '(2a)', fdate_str, '  aqm_bias_correct.f90: Start.'
   print '(2a)', 'Program ID = ', program_id

! Set dynamic run parameters.

   diag = 2	! Set default verbosity: 0 = errors only, 1 = milestones,
   		! 2 = brief progress, 3 and up = increasing detail level.
                ! May be changed on command line.

! Assumed missing value.

! Currently, 2014-jun-12, model forecast data is read with an
! assumed missing value.  However, the AIRNow obs reader performs
! actual missing value conversion on the original obs missing value.

   standard_vmiss  = -999		! assumed missing value in data
   vmiss = standard_vmiss		! also set vmiss for general usage

! Get command line parameters.

   call get_command_args (prog_name, calendar, config_file, cycle_time, &
      start_date, forecast_date, base_year, diag, wpar%gen_weights, &
      wpar%subset1, wpar%subset2)

! Date check, tighter than the command line input routine.

   if (forecast_date <= start_date) then
      print *
      print *, '*** aqm_bias_correct: Abort, date error on command line.'
      print *, '*** Forecast date must be at least one day later than' &
         // ' start date.'
      print *, '*** The training period must be at least one day long.'
      print *, '*** The forecast date is not included in the training period.'
      call exit (1)
   end if

! Print environment information, including library versions.

   if (diag >= 2) call print_library_info
   if (diag >= 2) call print_omp_info

!-----------------------------------------------------------------
! Read and process the configuration file.
!-----------------------------------------------------------------

   call fdate (fdate_str)
   print *
   print '(2a)', fdate_str, '  Call read_config_file_main.'

   call read_config_file_main (config_file, obs_file_template, &
      interp_file_template, in_gridded_template, reader_code_gridded, &
      hourly_output_template, probability_output_template, &
      new_site_list_template, grid_coord_file, pred_weight_file, &
      site_exception_file, site_blocking_list, nhours, &
      target_obs_var, target_model_var, analog_vars, &
      filter_method, output_limit_method, site_array_file_template, &
      site_result_file_template, day_array_file_template, stop_after_filter, &
      obs_min_input, obs_max_input, obs_blackout_start, obs_blackout_end, &
      apar, dpar, fpar, kpar, prob, wpar)

!-----------------------------------------------------------------
! Consistency checks for weight generation.
!-----------------------------------------------------------------

   if (wpar%gen_weights) then

! Prevent invalid file name.

      if (pred_weight_file == 'equal weights') then
         print *
         print *, '*** Weight generation is selected, but weight file name' &
            // ' is invalid.'
         print *, '*** File name = ' // trim (pred_weight_file)
         print *, '*** Abort.'
         call exit (1)
      end if

! Overwrite protect for predictor weights file.
! Better to find out now, instead of at end of very long weight generation.

      inquire (file=pred_weight_file, exist=ex)

      if (ex) then
         print *
         print *, '*** Overwrite protect.'
         print *, '*** Weight generation is selected, but there is a' &
            // ' previous weight file.'
         print *, '*** File = ' // trim (pred_weight_file)
         print *, '*** Please change target file name, or move the' &
            // ' previous file.'
         print *, '*** Abort.'
         call exit (1)
      end if

   end if

!-----------------------------------------------------------------
! Read grid coordinate file.
!-----------------------------------------------------------------

! Grid coordinates are needed needed for the obs QC module.

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_grid_coords.'

   call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)

!---------------------------------------------------------------------
! Read quality-controlled observational data.
!---------------------------------------------------------------------
!
! * Read original obs time series for training period.
! * Remove site obs data specified in site blocking list.
! * Convert data units to units needed by this program.
! * Convert file missing values to program missing values.
! * Run quality control procedures on straight obs time series.
!
! Secondary output is an updated list of valid site coordinates.
! Normally this file is safely ignored.  But when differences in
! valid sites are sufficient, this site list may then used to
! generate a new local archive of interpolated previous forecasts.
!
!---------------------------------------------------------------------

   print '(a)', 'Read obs data for target variable, and run quality control.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_obs_qc.'

   training_end_date = forecast_date - 1	! read only training period,
   						! NOT current forecast date

   call read_obs_qc (obs_file_template, target_obs_var, start_date, &
      training_end_date, base_year, obs_min_input, obs_max_input, &
      standard_vmiss, grid_lats, grid_lons, site_blocking_list, diag, &
      obs_ids, obs_lats, obs_lons, obs_in, obs_units)

!---------------------------------------------------------------------
! Write updated coordinate list for QC validated sites.
!---------------------------------------------------------------------

   title_varname = target_obs_var		! make comprehensible var name

   if (target_obs_var == 'COPOPM') title_varname = 'PM2.5'
   if (target_obs_var == 'COPO'  ) title_varname = 'ozone'

   site_list_title = 'List of AIRNow ' // trim (title_varname) &
      // ' sites for bias correction.'

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call write_site_list.'

   call write_site_list (new_site_list_template, forecast_date, cycle_time, &
      base_year, calendar, site_list_title, obs_ids, obs_lats, obs_lons, diag)

!---------------------------------------------------------------------
! Remove obs data within blackout dates/times.
!---------------------------------------------------------------------

   call blackout_obs_data (obs_in, standard_vmiss, start_date, &
      training_end_date, base_year, calendar, obs_blackout_start, &
      obs_blackout_end, target_obs_var, obs_units, diag)

!---------------------------------------------------------------------
! Read interpolated forecast data for same training period.
!---------------------------------------------------------------------
!
! Read interpolated forecast data (model data) from the start of
! the training period, through the target forecast date.  Note that
! the forecast date is included here, but was NOT read for obs data.
!
! Data for the target forecast date must be present for all given
! analog variables, to enable the bias correction process.
!
! Starting with version 2019 May 29, some interpolated analog
! variables may optionally be computed on the fly.  Currently,
! 10-meter wind speed and direction are computed from interpolated
! U and V wind.
!
!---------------------------------------------------------------------

   print *, 'Read interpolated forecast data, all analog variables.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_interp_forecasts.'

   call read_interp_forecasts (interp_file_template, analog_vars, start_date, &
      forecast_date, base_year, cycle_time, nhours, dpar, apar%fvar, &
      standard_vmiss, diag, interp_ids, interp_lats, interp_lons, model_in)

   ndays  = size (model_in, 1)		! (days, hours, vars, sites)
   forecast_day_num = ndays		! forecast day = last day in model input

!---------------------------------------------------------------------
! Align obs data with interpolated forecast arrays.
!---------------------------------------------------------------------
!
! * Align obs data with interpolated forecast arrays, by site ID's.
! * Reshape and duplicate into conforming 48- or 72-hour overlapping subsets.
! * Phase shift obs hours for correct alignment with forecast hours.
!
!---------------------------------------------------------------------

   print *, 'Align obs data with interpolated forecast data.'
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call align_obs_to_forecasts.'

   call align_obs_to_forecasts (obs_in, cycle_time, ndays, nhours, vmiss, &
      obs_ids, obs_lats, obs_lons, interp_ids, interp_lats, interp_lons, &
      diag, obs_reshaped)			! (days, hours, 1 var, sites)

   if (diag >= 2) then
      vi = 1				! dummy var subscript for obs
      ndays_show = min (ndays, 3)
      print *, trim (target_obs_var) // ':    (sample of obs reshaped array)'
      print '(3i6, 5f12.4)', ((di, hi, vi, obs_reshaped(di, hi, vi, 1:5), &
         hi = 1, 3), di = 1, ndays_show)
      print *
   end if

!---------------------------------------------------------------------
! Read supplemental files.
!---------------------------------------------------------------------

! Read predictor weights file, if any.

   if (.not. wpar%gen_weights) then
      call read_predictor_weights (pred_weight_file, analog_vars, interp_ids, &
         diag, pred_weights)
   end if

! Read the site bias threshold exception list, which is a sparse list.
! Output arrays are aligned with current site indexing.

   call read_exception_list (site_exception_file, interp_ids, vmiss, diag, &
      excep_thresh_low, excep_thresh_high)

!---------------------------------------------------------------------
! Apply Kalman/analog filter to interpolated forecasts.
!---------------------------------------------------------------------

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call analog_control.'

   call analog_control (filter_method, model_in, obs_reshaped, vmiss, &
      target_model_var, analog_vars, apar, fpar, kpar, wpar, pred_weights, &
      excep_thresh_low, excep_thresh_high, diag, interp_ids, interp_lats, &
      interp_lons, uncorr_sites, corr_sites, filter_result_all, &
      best_analogs_obs, new_weights, new_rmse)

   if (diag >= 3) print *, 'analog_control returned.'

!-----------------------------------------------------------------
! Write new predictor weights file, if weight generation mode.
!-----------------------------------------------------------------

   if (wpar%gen_weights) then
      call write_predictor_weights (pred_weight_file, analog_vars, &
         interp_ids, new_weights, new_rmse, diag)

      call fdate (fdate_str)
      print '(2a)', fdate_str, &
         '  aqm_bias_correct.f90: Stop after new weight generation.'
      call exit
   end if

!-----------------------------------------------------------------
! Write intermediate test files, if selected.
!-----------------------------------------------------------------

   if (day_array_file_template /= 'none') &
      call write_test_day_files (day_array_file_template, filter_result_all, &
         vmiss, cycle_time, diag, interp_ids, interp_lats, interp_lons)

   if (site_array_file_template /= 'none') &
      call write_test_site_files (site_array_file_template, filter_result_all, &
         vmiss, cycle_time, diag, interp_ids, interp_lats, interp_lons)

   if (site_result_file_template /= 'none') &
      call write_site_result_files (site_result_file_template, uncorr_sites, &
         corr_sites, diag, forecast_day_num, forecast_date, base_year, &
         calendar, cycle_time, analog_vars, filter_method, interp_ids, &
         interp_lats, interp_lons)

! Check for early stop request.

   if (stop_after_filter) then
      print *, '*** "Stop after filter" is selected (config file).'
      call fdate (fdate_str)
      print '(2a)', fdate_str, '  aqm_bias_correct.f90: Stop.'
      call exit
   end if

! Recover memory from large arrays, no longer needed.
! Output modules will need large arrays.

! 2020-aug-3: Do not deallocate obs_reshaped, may be needed by regress_method.

   deallocate (model_in)

!---------------------------------------------------------------------
! Make spreading and corrected hourly forecasts, as needed.
!---------------------------------------------------------------------

   enable_spreading =      (hourly_output_template /= 'none') &
                      .or. (prob%probability_method == 'linear regression')

   if (.not. enable_spreading) then
      if (diag >= 2) print *, '*** Hourly forecast outputs are suppressed.'

   else
      call fdate (fdate_str)
      print '(2a)', fdate_str, '  Make corrected hourly forecast grids.'

! Spread bias corrections to hourly forecast grids.
! Write output file, if selected by the output template.

      call spreading (in_gridded_template, reader_code_gridded, &
         grid_coord_file, hourly_output_template, target_model_var, &
         output_limit_method, forecast_date, cycle_time, base_year, calendar, &
         uncorr_sites, corr_sites, interp_ids, interp_lats, interp_lons, &
         vmiss, diag, corr_grids)
   end if

!---------------------------------------------------------------------
! Make probability forecasts, if selected.
!---------------------------------------------------------------------

   print *

   if (probability_output_template == 'none') then
      if (diag >= 2) print *, '*** Probability forecast outputs are suppressed.'

   else
      call fdate (fdate_str)
      print '(2a)', fdate_str, '  Make probability forecasts.'

! Make probability forecasts, and write output file.

      call probability (in_gridded_template, grid_coord_file, &
         probability_output_template, target_model_var, forecast_date, &
         cycle_time, base_year, calendar, filter_result_all, &
         best_analogs_obs, obs_reshaped(:,:,1,:), corr_grids, &
         interp_lats, interp_lons, vmiss, prob, diag)
   end if

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  aqm_bias_correct.f90: Done.'

end program aqm_bias_correct
