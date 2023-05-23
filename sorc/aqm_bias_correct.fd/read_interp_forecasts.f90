!------------------------------------------------------------------------------
!
! read_interp_forecasts.f90 -- Read interpolated model forecast time series.
!
! This routine reads interpolated 48- or 72-hour forecasts from a
! Netcdf data set, over a selected date range and selected variables.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jun-18	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!
! 2019-may-30	Add option to compute interpolated wind speed and direction
!	`	  on the fly, from U and V wind.
!		Add support for varying number of forecast hours per variable.
!
! 2022-apr-10	Add support for hourly gridded input files for RRFS-CMAQ.
!		The number of forecast hours is now pre-determined in the
!		  config file, rather than adapting to the input data set.
! 2022-may-31	Add automatic units conversion for the target forecast
!		  variable, ppb to ppm.
!
! 2023-apr-09	Minor update for 12-character site ID's.
!
! Primary inputs:
!
! * Data set of daily Netcdf files containing MET and CMAQ 48-hour
!   forecasts interpolated to site locations.  Data set is prepared
!   and maintained by interpolate_update.f90.
! * Names of requested forecast variables.
! * Specified range of forecast start dates.  The final date should
!   be the start date of the target forecast cycle for bias
!   correction.
!
! Primary outputs:
!
! * Master 4-D array containing interpolated 48- or 72-hour
!   data over the requested variables, range of dates, and all
!   available site locations.
! * Site ID's and lat/lon coordinates for interpolation sites.
!
! Notes:
!
! The interpolated input data set is specified with a path
! template.  The template contains YYYY MM DD CC substitution
! strings that will resolve to day file names for the requested
! forecast cycles.  The template may include full or relative
! paths, and it may begin with an $ENV environnment variable.
!
! Output arrays are auto-allocated for all available sites, and
! for the requested range of dates.
!
! The assumed missing value "vmiss" is used as needed to fill
! gaps in the input data set.
!
! Units conversion is provided only for the target forecast
! variable.  Input units of ppb are automatically converted to
! ppm, to conform with existing analog filter code. Otherwise,
! no units conversion is performed on any other input variable.
!
! Units strings are not currently used by the analog code, so
! none are returned to the caller.
!
! Start and end date indexes:  Integer Gregorian dates, relative
! to 1 = January 1 of base_year.  See index_to_date library
! routine for details.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = sparse,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
! Error handling:
!
! Missing files are treated as soft errors, and ignored.  Data
! for such files is included in the output array as all missing
! values.
!
! Some serious errors may result in a diagnostic message and
! program abort.
!
!------------------------------------------------------------------------------

module read__interp_forecasts
contains

subroutine read_interp_forecasts (infile_template, varnames, start_date, &
      end_date, base_year, cycle_time, nhours, dpar, fvar, vmiss, diag, &
      site_ids, site_lats, site_lons, out_data)

   use compute__wind
   use config,                only : dp, i64
   use expand__filename
   use index_to_date_mod
   use netcdf,                only : nf90_get_att, nf90_noerr
   use netcdf_sup
   use print__interp_summary
   use read__netcdf_var
   use stdlit,                only : normal
   use string_utils
   implicit none

! Input arguments.

   character(*),   intent(in) :: infile_template ! template for input file paths
   character(*),   intent(in) :: varnames(:)	 ! requested model var names (V)
   integer,        intent(in) :: start_date	 ! starting date index
   integer,        intent(in) :: end_date	 ! ending date index
   integer,        intent(in) :: base_year	 ! base year for date indexes
   integer,        intent(in) :: cycle_time	 ! start hour of forecast cycle
   integer,        intent(in) :: nhours		 ! number of forecast hours
   type(dpar_type),intent(in) :: dpar		 ! controls for derived vars
   integer,        intent(in) :: fvar		 ! index of target forecast var
   real(dp),       intent(in) :: vmiss		 ! caller's missing value code
   integer,        intent(in) :: diag		 ! diag verbosity level, 0-N

! Output arguments.

   character(*), intent(out), allocatable :: site_ids(:)   ! site ID strings (S)
   real(dp),     intent(out), allocatable :: site_lats(:)  ! site coords (S)
   real(dp),     intent(out), allocatable :: site_lons(:)
   real(dp),     intent(out), allocatable :: out_data(:,:,:,:)  ! output array
  						   ! (days, hours, vars, sites)
! Local program parameters.

   character(*), parameter :: calendar = 'gregorian'

! Local variables.

   character(len(varnames)) varname, fvarname, derived_vars(2)
   character(60) units(size(varnames)), in_units
   character(60) units_orig, units_orig2, units_needed

   character(200) infile, save_infile
   character fmt1*50, mbstr*50, yes_no*3

   integer year, month, day
   integer vi, idate, date_index, i
   integer nhours_valid, nhours_copy
   integer status, status1, status2, att_status
   integer ncid, varid
   integer ndays_valid, ndays_missing, ndays_error
   integer with_data

   integer(i64) ndays, nvars, nsites		! long integers
   integer(i64) nbytes, total_size		! for total size calculations

   real(dp) mbytes, multiplier

   logical ex, udata_valid, vdata_valid, need_conversion
   logical need_units(size(varnames))

! Dynamic arrays.

   character(len(site_ids)), allocatable :: save_ids(:)	  ! (sites)
   real(dp), allocatable :: save_lats(:), save_lons(:)	  ! (sites)
   real(dp), allocatable :: file_data(:,:)		  ! (sites, hours)
   real(dp), allocatable :: udata(:,:), vdata(:,:)	  ! (sites, hours)

!-----------------------------------------------------------
! Initialize.
!-----------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print '(a)', &
      '======================================================================='
   if (diag >= 1) print '(a)',  'read_interp_forecasts: Start.'

   nvars = size (varnames)		! get number of requested analog vars

   ndays_valid   = 0			! init statistics
   ndays_missing = 0
   ndays_error   = 0

   derived_vars = (/ dpar%derived_wind_dir_var, dpar%derived_wind_speed_var /)

   units(:) = ' '			! default units strings to blank
   need_units = .true.

!-----------------------------------------------------------
! Main loop over each input file.  One file per day.
!-----------------------------------------------------------

   ndays = end_date - start_date + 1

date_loop: &
   do idate = 1, ndays

! Create input file name for the current date.  The current assumption
! is that all requested variables are in the same Netcdf file.

      date_index = start_date + idate - 1

      call index_to_date (date_index, year, month, day, base_year, calendar)
      					! get current Y M D integers

      call expand_filename (infile_template, year, month, day, cycle_time, &
         infile)

! Check for file missing for current date.

      inquire (file=infile, exist=ex)

      if (.not. ex) then
         if (diag >= 2) &
            print '(2a)', '  File missing, skipping this date: ', trim (infile)
         ndays_missing = ndays_missing + 1	! count missing input files
         cycle date_loop
      end if

! Read input file for this date.

      if (diag >= 4) print *
      if (diag >= 2) print '(2a)', '  Read file: ', trim (infile)

!-----------------------------------------------------------
! Read and check site metadata.
!-----------------------------------------------------------

! Read site ID's.

      call read_netcdf_var (infile, 'site_id', diag, site_ids, status)

      if (status /= normal) then
         fmt1 = '(4a,i5.4,3i3.2,a)'
         print '(2a)', '*** read_interp_forecasts: Error reading site ID', &
            ' variable.'
         print '(2a)', '*** File = ', trim (infile)
         print '(2a)', '*** Treat as soft error, insert missing values', &
            ' for this date.'
         print *
         ndays_error = ndays_error + 1		! count bad input files
         cycle date_loop
      end if

      nsites = size (site_ids)

      if (diag>=4) print '(a,i0)', '   Number of sites in file = ', nsites

! Read lat and lon site coordinates.

      call read_netcdf_var (infile, 'site_lat', diag, site_lats, status1)
      call read_netcdf_var (infile, 'site_lon', diag, site_lons, status2)

      if (status1 /= normal .or. status2 /= normal) then
         fmt1 = '(4a,i5.4,3i3.2,a)'
         print '(2a)', '*** read_interp_forecasts: Error reading lat', &
            ' or lon variables.'
         print '(2a)', '*** File = ', trim (infile)
         print '(2a)', '*** Treat as soft error, insert missing values', &
            ' for this date.'
         print *
         ndays_error = ndays_error + 1		! count bad input files
         cycle date_loop
      end if

! Print sites and coordinates at higher diag levels.

      if (diag >= 5 .or. (diag >= 3 .and. .not. allocated (save_ids)) ) then
         print *
         print '(4x,a)', ' Site ID           Lat          Lon'
         print '(4x,a)', '---------       ---------    ---------'
         fmt1 = '(4x,a,2f13.5)'
         print fmt1, (site_ids(i), site_lats(i), site_lons(i), i=1,nsites)
         print *
      end if

! First file only, save a copy of metadata for diagnostics.

      if (.not. allocated (save_ids)) then
         if (diag >= 3) print '(2a)', '    Allocate check arrays for site', &
            ' metadata.'

         allocate (save_ids(nsites))
         allocate (save_lats(nsites), save_lons(nsites))

         save_ids(:)  = site_ids(:)
         save_lats(:) = site_lats(:)
         save_lons(:) = site_lons(:)

         save_infile = infile
      end if

! Check for matching site ID's.  The ordering must also be identical.
! Mismatch is funtamentally serious, so must be a fatal error.

      if (any (site_ids(:) /= save_ids(:))) then

         if (diag >= 3) then			! dump all ID's at diag level
            print *
            print '(4x,a)', 'File 1 ID    File 2 ID'
            print '(4x,a)', '---------    ---------'
            print '(4x,a)', site_ids(:) // '    ' //  save_ids(:)
            print '(4x,a)', '---------    ---------'
            print '(4x,a)', 'File 1 ID    File 2 ID'
         end if

         print *
         print '(2a)', "*** read_interp_forecasts: Site ID's do not match", &
            ' between input files.'
         print '(2a)', '*** File 1 = ', trim (save_infile)
         print '(2a)', '*** File 2 = ', trim (infile)

         if (diag < 3) then
            print *
            print '(2a)', '*** Enable diag level 3 or higher to dump both', &
               " sets of ID's."
         end if

         print '(2a)', '*** Fatal error.  Abort.'
         call exit (1)
      end if

! Maybe should check lat/lon coordinates as well.  Add later.
! Maybe should check embedded date.  Add later.

!---------------------------------------------------------------
! Allocate the main output array, on first valid input file.
!---------------------------------------------------------------

alloc_once: &
      if (.not. allocated (out_data)) then

         if (diag >= 2) then

! For diagnostic, determine total allocation size.
! STORAGE_SIZE is a fortran 2008 intrinsic for bit size of one element.

            nbytes     = storage_size (out_data) / 8	! element bit size / 8
            total_size = ndays * nhours * nvars * nsites
            mbytes     = total_size * nbytes / 1.0e6

! Formatted diagnostic for main allocation.

            write (mbstr, '(i6)') nint (mbytes) ! cleaner version of G format
            if (mbytes < 10) write (mbstr, '(f8.1)') mbytes
            if (mbytes < 1)  write (mbstr, '(f8.2)') mbytes
            if (mbstr(1:1) == '*' .or. mbytes < 0.01) &
               write (mbstr, '(es10.1)') mbytes
            mbstr = adjustl (mbstr)
            if (mbstr(1:1) == '.') mbstr = '0' // mbstr

            print *
            print '(2a)', 'read_interp_forecasts:  Allocate main array', &
               ' for interpolated forecast data.'
            print '(a,4(2x,i0))', '     ndays, nhours, nvars, nsites =', &
               ndays, nhours, nvars, nsites
            print '(a,i0,3a)','     Total array size             = ', &
               total_size, ' elements (', trim (mbstr), ' megabytes)'
            print *
         end if

         allocate (out_data(ndays, nhours, nvars, nsites))

! Clear to all missing, in case of possible gaps.

         if (diag >= 3) print *, '  Clear main array to all missing.'

         out_data(:,:,:,:) = vmiss
      end if alloc_once

!-----------------------------------------------------------
! Read optional input variables for computing derivatives,
! before main loop for analog variables.
!-----------------------------------------------------------

! See detailed reader comments in the next section.

! Read optional U wind.

      udata_valid = .false.

      if (dpar%uwind_var /= 'none') then
         call read_netcdf_var (infile, dpar%uwind_var, diag, udata, status)
         					! udata (sites, hours)
         if (status == normal) then
            udata_valid = .true.
         else
            fmt1 = '(4a,i5.4,3i3.2,a)'
            print fmt1, ' *** read_interp_forecasts: Warning: Error', &
               ' reading "', trim (dpar%uwind_var), '" for cycle', year, &
               month, day, cycle_time, 'Z.'
            print *
         end if
      end if

! Read optional V wind.

      vdata_valid = .false.

      if (dpar%vwind_var /= 'none') then
         call read_netcdf_var (infile, dpar%vwind_var, diag, vdata, status)
         					! vdata (sites, hours)
         if (status == normal) then
            vdata_valid = .true.
         else
            fmt1 = '(4a,i5.4,3i3.2,a)'
            print fmt1, ' *** read_interp_forecasts: Warning: Error', &
               ' reading "', trim (dpar%vwind_var), '" for cycle', year, &
               month, day, cycle_time, 'Z.'
            print *
         end if
      end if

!-----------------------------------------------------------
! Read each requested analog variable from current file.
! But compute derived variables from other input variables.
!-----------------------------------------------------------

      with_data = 0

var_loop: &
      do vi = 1, nvars
         varname = varnames(vi)

! Deallocate, then reallocate the data input array each time,
! in support of varying number of forecast hours per variable.

         if (allocated (file_data)) then
            deallocate (file_data)
         end if

! Compute derived analog variable on the fly.

var_type: &
         if (any (varname == derived_vars(:)) ) then

            call compute_wind (varname, dpar, udata, vdata, udata_valid, &
               vdata_valid, vmiss, diag, file_data, nhours_valid, status)

            need_units(vi) = .false.	! for now, can not propagate the wind
            				! units attributes; so leave blank

! Read normal analog variable directly from file.

! Internal detail:  This Netcdf reader is optimized for reading
! several variables in sequence from the same input file.  For
! efficiency, the file is not closed and re-opened between variables.

         else
            call read_netcdf_var (infile, varname, diag, file_data, status, &
               nc_id=ncid, var_id=varid)	! file_data (sites, hours)

! Try to read actual hours attribute for this variable.

            if (status == normal) then
               att_status = nf90_get_att (ncid, varid, 'nhours_valid', &
                  nhours_valid)

               if (att_status /= nf90_noerr) then	! if no attribute...
                  nhours_valid = size (file_data, 2)	! use return array size
               end if
            end if
         end if var_type

! Failure to read a single variable should not occur by design of
! the current data set.  But if it does, treat this as a soft error
! and continue reading.

         if (status /= normal) then
            fmt1 = '(4a,i5.4,3i3.2,a)'		! nice message for sparse log
            print fmt1, ' *** read_interp_forecasts: Warning: Error', &
               ' reading "', trim (varname), '" for cycle', year, month, &
               day, cycle_time, 'Z.'
            print '(2a)', ' *** Treat as soft error, insert missing values', &
               ' for this variable.'
            print *
            cycle var_loop
         end if

! Read successful.  Insert current file, current var data into main array.

         nhours_copy = min (nhours, nhours_valid)

         if (diag >= 3) print '(3a,i0)', &
            '   Insert input data into main array: ', trim (varname), &
            ', nhours_copy = ', nhours_copy

         out_data(idate,1:nhours_copy,vi,:) &
            = transpose (file_data(:,1:nhours_copy))	! (D,H,V,S) <-- (S,H)
            			! any shortfall is already filled with missing

! Check for all missing data in current variable.

         if (any (file_data(:,:) /= vmiss)) then
            with_data = with_data + 1
         else
            if (diag >= 1) print *, '   *** All missing data: ', trim (varname)
         end if

! Read units attribute, one time only for each requested variable.
! Assume that units for each variable are the same across all dates
! and all forecast hours in the entire input file set.

         if (need_units(vi)) then
            att_status = nf90_get_att_trim (ncid, varid, 'units', in_units)
            if (att_status == nf90_noerr) then
               if (in_units /= ' ') then	! units valid only when found
                  units(vi)  = in_units		! units attribute, and it is
                  need_units(vi) = .false.	! not blank
               end if
            end if
         end if

      end do var_loop

! Check for all missing data in entire file.

      if (with_data == 0) then
         print '(2a)', '*** read_interp_forecasts: Warning: No valid data', &
            ' in current file.'
         print '(2a)', '*** File = ', trim (infile)
         print '(2a)', '*** Soft error, continue running.'
         print *
         ndays_error = ndays_error + 1		! count bad input files
         cycle date_loop
      end if

      ndays_valid = ndays_valid + 1		! count valid input files
   end do date_loop

   if (diag >= 2) print *
   if (diag >= 2) print *, ' End of requested time range.'

!-----------------------------------------------------------
! All files read.  Intermediate diagnostics.
!-----------------------------------------------------------

! Print input summary, part 1.

   fmt1 = '(1x,a,i0)'

   print *
   print *,    '-------------------------------------------------------------'
   print *,    'Input summary, all interpolated files combined:'
   print fmt1, 'Total number of days in input period  = ', ndays
   print fmt1, 'Days skipped, missing input files     = ', ndays_missing
   print fmt1, 'Days skipped, invalid input files     = ', ndays_error
   print fmt1, 'Number of valid days read             = ', ndays_valid

! Controlled halt if all files missing.

   if (ndays_valid == 0) then
      print *
      print '(2a)', '*** read_interp_forecasts: Fatal error, no valid', &
         ' input files.  Abort.'
      call exit (1)
   end if

! Part 2.  Print summary statistics for each input variable.

   call print_interp_summary (varnames, units, out_data, vmiss)

!-----------------------------------------------------------
! Automatic units conversion for target var, ppb to ppm.
!-----------------------------------------------------------

! In this preliminary version, units of ppb are converted blindly,
! without checking the species type.  Should be fine for both
! ozone and PM2.5.

   print *
   print *, 'read_interp_forecasts:  Check for units conversion.'

   fvarname    = varnames(fvar)
   units_orig  = units(fvar)
   units_orig2 = units_orig
   call lowercase (units_orig2)

   units_needed    = 'ppm'
   need_conversion = (any (units_orig2 == (/ 'ppb ', 'ppbv' /) ))
   yes_no          = merge ('YES', 'NO ', need_conversion)

   if (diag >= 2) then
      print *, '  Interpolated var name      = ', trim (fvarname)
      print *, '  Input data units           = ', trim (units_orig)
      print *, '  Units needed               = ', trim (units_needed)
      print *, '  Units conversion needed    = ', trim (yes_no)
      print *
   end if

convert_ppb: &
   if (need_conversion) then
      multiplier = 0.001_dp			! ppb to ppm

      print '(99a)', ' read_interp_forecasts:  Apply units conversion, ', &
         trim (units_orig), ' to ', trim (units_needed), '.'

      if (diag >= 2) print '(a,es11.4)', '   Multiplier                 =', &
         multiplier

      where (out_data(:,:,fvar,:) /= vmiss)
         out_data(:,:,fvar,:) = out_data(:,:,fvar,:) * multiplier
      end where

      units(fvar) = units_needed

! Part 3, conditional.  Final diagnostics after units conversion.

      print *
      print *, 'Input summary after interpolated ' // trim (fvarname) &
         // ' units conversion to ' // trim (units_needed) // ':'

      call print_interp_summary (varnames, units, out_data, vmiss)
      print *

   end if convert_ppb

   if (diag >= 3) print *, 'read_interp_forecasts: Return.'
   if (diag >= 3) print *

end subroutine read_interp_forecasts
end module read__interp_forecasts
