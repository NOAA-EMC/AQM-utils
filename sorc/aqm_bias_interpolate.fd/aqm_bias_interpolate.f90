!------------------------------------------------------------------------------
!
! aqm_bias_interpolate.f90
!
! Interpolate gridded raw hourly forecasts to site locations.
! Update day files in a local archive, as needed.
!
! This is the "interpolation module", one of the four major
! components of the NOAA NCO/ARL/PSL bias correction system
! for CMAQ forecast outputs.
!
! This same program is used both to generate the original
! interpolated local archive, and to add current day files
! on the fly in production mode.
!
! 2014-may-10	interpolate_update.f90:
!		Original version of main program and process control.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Core interpolation routines by Irina Djalalova, NOAA/ESRL/PSD3.
! 2014-may-12	Optimize sparse progress display for diag level 2.
! 2014-jul-23	Fix program name in error message.
!
! 2015 Dec  ?	Jianping: Add support for 6-hour forecasts, 0Z and 18Z cycles.
!		In CMAQ v4.7.2, v5.1.0, etc.
!
! 2019-apr-25	Dave A: Switch to 72-hour forecasts, preliminary testing only.
!
! 2019-may-21	Generalize support for any number of forecast hours.
!		Add command line argument, number of forecast hours.
!		Support differing number of forecast hours per variable.
!		Upgrade to properly interpolate vector fields; wind speed
!		  and direction in this case.
!		Calculate derived fields U10 and V10 up front in interpolator.
!
! 2019-may-21	File format changes:
!		* Interpolated files now contain varying forecast hours
!		  for different variables, mirroring the source data.
!		* Add nhours_valid attribute for number of valid forecast
!		  hours for each variable.
!		* Add missing_value attribute for each variable.
!		  Used mainly for padding at end of valid forecast hours.
!
! 2020-feb-12	Bug fix in derivatives.f90.  Fix sign reversal in V wind.
!
! 2022-apr-20	Add support for hourly gridded input files for RRFS-CMAQ.
!		Specify number of forecast hours in config file, rather
!		  than on command line.
!		Change from hard coded derivatives, to formula expressions.
!		Add support for summed derived fields, such as NOX = NO + NO2.
! 2022-apr-24	Include forecast hour zero, because it is actually the
!		  first legitimate forecast hour in RRFS-CMAQ.
! 2022-may-23	Propagate units attributes to output files.
!
! 2022-jun-03	aqm_bias_interpolate.f90:
!		Main program name change to conform with NCEP/NCO.
! 2022-dec-03	RRFS: Ignore *.f000 files, start with *.f001 = forecast hour 1.
!
! 2023-apr-11	Increase site ID's from 9 to 12 characters maximum, for AirNow.
!
! * Remember to update the program_id below.
!
! Input:
!
! Configuration file, with paths and list of vars to include.
! Site file with list of site ID's and coordinates.
! Gridded raw forecast files for MET and CMAQ models.
! A few hard coded parameters, see below.
!
! Output:
!
! Forecast data interpolated to site coordinates.
! One file per day, all variables in each file.
!
! Notes:
!
! Output files are created only if they are not already present
! in the interpolated archive.  This version does not overwrite
! or update existing files.
!
! If updates or changes to existing interpolated files are
! needed, then first delete the old files.  Then run this program
! to make replacements.
!
! If the site file changes, the entire archive should be
! regenerated from scratch, so that site ordering and site
! coordinates will remain consistent between interpolated files.
!
!------------------------------------------------------------------------------
!
! Usage, run command with four or five arguments:
!
! ./aqm_bias_interpolate config-file cycle-time date1 date2 diag=N
!
! config-file	Interpolator config file for current platform and
!		data set configuration.  Includes explicit file
!		pointers and list of target variables to be
!		included in the local archive.
!
! cycle-time	HH, model cycle time, currently 06 or 12.
!		Set to $Cyc for WCOSS real-time operation.
!
! date1, date2	YYYYMMDD, start and end forecast dates to process.
!
! diag=N	Set verbosity level N.  Optional argument.
!		The default is set below in this program.
!
! Example:
!
! ./aqm_bias_interpolate config.interp.ozone.7-vars 12z 20230101 20230331
!
!------------------------------------------------------------------------------

program aqm_bias_interpolate

   use config, only : dp
   use derivatives_mod
   use get__command_args
   use expand__filename
   use index_to_date_mod
   use interpolate_mod
   use read__config_file_interp
   use read__gridded_vars
   use read__site_list
   use stdlit
   use write__interp_netcdf
   implicit none

   character(*), parameter :: &
      program_id = 'aqm_bias_interpolate.f90 version 2023-apr-11'

! Local variables.

   integer, parameter :: id_len = 12		! site ID length for AirNow
   						!  site ID's up to 12 characters

   character(200) config_file, site_file, grid_coord_file
   character(200) interp_file, interp_file_template
   character fmt1*20, fdate_str*24

   integer nsites, nhours_spec, nhours_actual_max
   integer cycle_time, first_date, last_date, base_year
   integer date_index, year, month, day
   integer diag, status
   integer ndays_total, nskip_exist, ndays_error, nwrite
   integer nvars, nsave, vi

   real vmiss
   logical ex

! Dynamic arrays.

   character(80),  allocatable :: varnames(:)		 ! var config data (V)
   character(30),  allocatable :: reader_codes(:)	 ! (V)
   character(200), allocatable :: grid_file_templates(:) ! (V)
   character(200), allocatable :: formulas(:)		 ! for derivatives (D)
   character(60),  allocatable :: units(:)		 ! units attributes (V)

   logical,        allocatable :: var_save(:)		! (V)
   integer,        allocatable :: ind_save(:)		! (V subset)

   integer,        allocatable :: nhours_actual(:)	! actual hrs read (V)

   character(id_len), allocatable :: site_ids(:)	! site ID's (S)
   real(dp),       allocatable :: site_lats(:)		! site coordinates (S)
   real(dp),       allocatable :: site_lons(:)

   real(dp),       allocatable :: grid_lats(:,:)	! grid coordinates (X,Y)
   real(dp),       allocatable :: grid_lons(:,:)

   real,           allocatable :: grid_data(:,:,:,:)	! gridded forecast data
  							!  (X, Y, vars, hours)
   real,           allocatable :: interp_data(:,:,:)	! interpolated data
  							!  (sites, vars, hours)
! Program parameters.

   character(*), parameter :: prog_name = 'aqm_bias_interpolate'
   character(*), parameter :: calendar  = 'gregorian'

! Run parameters.

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  aqm_bias_interpolate.f90: Start.'
   print '(2a)', 'Program ID = ', program_id

   diag = 2		! set default verbosity: 0 = errors only,
   			! 1 = milestones, 2 = brief progress,
                        ! 3 and up = increasing detail level.

   vmiss  = -999.0	! assumed missing value in data

! Initialize.

   nwrite      = 0				! clear statistics
   nskip_exist = 0
   ndays_error = 0

! Get command line parameters.

   call get_command_args (prog_name, calendar, config_file, cycle_time, &
      first_date, last_date, base_year, diag)

! Read and process the configuration file.

   call read_config_file_interp (config_file, site_file, grid_coord_file, &
      interp_file_template, nhours_spec, varnames, reader_codes, &
      grid_file_templates, formulas, var_save)

! Read site coordinates for interpolation.

   call read_site_list (site_file, site_ids, site_lats, site_lons, nsites)

   if (diag >= 2)  print *

!-----------------------------------------------------------
! Main loop over each date in requested processing period.
!-----------------------------------------------------------

date_loop: &
   do date_index = first_date, last_date

      call index_to_date (date_index, year, month, day, base_year, calendar)
      					! get current Y M D integers

      call expand_filename (interp_file_template, year, month, day, &
         cycle_time, interp_file)	! make interp file name for current date

! Skip this date if an interpolated file for for this date is already made.
! This program only makes new output files, it does not overwrite or update.

      inquire (file=interp_file, exist=ex)

      if (ex) then
         if (diag >= 2) &
            print '(2a)', ' Skipping, made previously: ', trim (interp_file)
         nskip_exist = nskip_exist + 1		! count days skipped because
         cycle date_loop			! file made previously
      end if

! Read all gridded variables to be included in interpolated archive.
! To include all possible file configurations, this routine also reads
! the companion grid coordinates.

! Output arrays are automatically allocated or reallocated each time.
! In particular, the hours dimension of "grid_data" may change between
! calls, in support of varying length forecasts.

      call read_gridded_vars (varnames, reader_codes, grid_file_templates, &
         grid_coord_file, year, month, day, cycle_time, nhours_spec, vmiss, &
         diag, grid_data, nhours_actual, nhours_actual_max, units, grid_lats, &
         grid_lons, status)

! Skip this date if there is a serious problem with the input data.

      if (status /= normal) then
         ndays_error = ndays_error + 1		! count days with input errors
         cycle date_loop
      end if

! Calculate derived gridded variables, as needed, before interpolating.
! Pass the whole active part of the main data array, for both input and
! output fields.

! Derivatives will be filled into the reserved slots in the main array.
! Special keywords in "reader_codes" identify the derived vars to calculate.

      call derivatives (varnames, reader_codes, formulas, grid_data, &
         nhours_actual, units, vmiss, diag)

! Now exclude any variables marked "nosave".
! They are no longer needed after derivatives are calculated.
! Generate indices for only vars to save, using masked constructor.

      nvars    = size  (var_save)
      nsave    = count (var_save)
      ind_save = pack ( (/ (vi, vi = 1, nvars) /), var_save)

      if (diag >= 3) then
         print '(a,i0)',         '   Var summary:'
         print '(a,i0)',         '     nvars           = ', nvars
         print '(a,i0)',         '     nsave           = ', nsave
         print '(a,i0)',         '     size (ind_save) = ', size (ind_save)
         print '(a,999(i0,1x))', '     ind_save        = ', ind_save
      end if

! Interpolate all remaining vars to site locations, for current date.
! Include derivatives marked "save".  Use vector subscripting method.
! Limit to maximum valid hours only.

! The output array "interp_data" will be automatically allocated or
! reallocated each time.  The hours dimension may change between calls,
! in support of varying length forecasts.

      call interpolate (grid_data(:,:,ind_save,1:nhours_actual_max), vmiss, &
         grid_lats, grid_lons, site_lats, site_lons, site_ids, diag, &
         interp_data)

! Write the output file for this date.  "Interp_data" is now properly
! subset for only the variables and forecast hours to save.

! "nhours_actual" indicates the number of valid hours for each of the
! varying-length output variables.

      call write_interp_netcdf (interp_file, site_ids, site_lats, site_lons, &
         varnames(ind_save), units(ind_save), nhours_actual(ind_save), &
         interp_data, vmiss, program_id, diag)

      nwrite = nwrite + 1			! count files actually written

   end do date_loop

! Print run summary.

   call fdate (fdate_str)

   ndays_total = last_date - first_date + 1

   fmt1 = '(1x,a,i0)'

   print *,    '-------------------------------------------------------------'
   print *,    'Summary of interpolation run:'
   print fmt1, 'Total number of days in run period       = ', ndays_total
   print fmt1, 'Days skipped, files already created      = ', nskip_exist
   print fmt1, 'Days skipped, bad or missing input files = ', ndays_error
   print fmt1, 'Number of new day files written          = ', nwrite

   print *
   print '(2a)', fdate_str, '  aqm_bias_interpolate.f90: Done.'

end program aqm_bias_interpolate
