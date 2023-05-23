!------------------------------------------------------------------------------
!
! read_gridded_vars.f90 -- Read gridded time series for raw CMAQ and MET
!			   forecasts, multiple selected variables.
!
! This is a support routine for interpolate_update.f90, part of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
! 2014-may-13	Adjust verbosity for 2 = sparse progress display.
!
! 2019-may-17	Handle varying number of forecast hours.
!		Get forecast hours from input file, rather than predetermined.
!		Skip derived variables in the list of variables to read.
! 2019-may-20	Add nhours_actual_max output for caller's convenience.
!
! 2022-apr-13	Restructure for RRFS-CMAQ gridded files, one per forecast hour.
!		Retain support for previous concatenated AQM format.
!		Number of forecast hours is now specified by caller,
!		  rather than adaptive, to simplify program flow.
!		Optimize so that every input file is opened only once, and
!		  all variables from that file are read at the same time.
! 2022-may-23	Read and return the units attributes.
! 2022-dec-03	RRFS: Ignore *.f000 files, start with *.f001 = forecast hour 1.
!
! Input:   Input file specs for Netcdf input files with multiple variables.
!          List of of requested Netcdf variables, with related parameters.
!	   Grid coordinate file.
!          diag = verbosity control, 0 = errors only.  See below for more.
!
! Output:  grid_data = 4-D array for time series data.
!		(forecast cycle, forecast hour, variable, station)
!	   Note, there are two time dimensions, AKA "forecast cycle" or
!		"date", and "forecast hour".
!	   units = units attribute string for each input variable
!
! Notes:
!
! In a single call, this version reads a given list of variables
! from the indicated set of input files.  Gridded data for all
! variables is returned in a single multidimensional array.
!
! Two-dimensional coordinate grids are also read and returned.
! The units attribute string for each variable is also returned.
! A blank string is returned if there is any problem reading each
! units attribute, such as missing attribute.
!
! Variables marked "derived" in the reader_codes list are skipped,
! to be computed later in a different module.  However, slots for
! derived variables are incuded in the master output array.
!
! Output arrays are allocated by this routine, and need not be
! pre-allocated.  Any previously allocated arrays are deallocated
! on entry, then re-allocated.
!
! If the 2-D coordinate grids are already allocated, then this
! routine assumes that they were previously read, and skips
! reading the coordinate file a second time.
!
! This version supports input variables with differing numbers of
! forecast hours.  In particular, MET variables currently include
! one more forecast hour than AQM variables.  This is because
! MET variables include the initialization hour, whereas AQM
! variables do not.
!
! This version assumes that CMAQ and MET gridded input files do
! not contain any missing values.  No recognition or processing
! of input missing values or missing value attributes is
! currently done.
!
! However, missing values are inserted for several other reasons:
!
! * For missing single-hour forecast files.
! * Padding at the end of varying-length forecast time series.
! * Filling the allocated array slots for derived variables.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = sparse,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
!------------------------------------------------------------------------------

module read__gridded_vars
contains

!---------------------------------------------------------------------
! Public reader interface.  Returns multi-variable 4-D array.
!---------------------------------------------------------------------

subroutine read_gridded_vars (varnames, reader_codes, grid_file_templates, &
      grid_coord_file, year, month, day, cycle_time, nhours, vmiss, diag, &
      grid_data, nhours_actual, nhours_actual_max, units, grid_lats, &
      grid_lons, status)

   use config, only : dp
   use expand__filename
   use read__grid_coords
   use read__gridded_aqm
   use read__gridded_hourly
   use stdlit, only : normal
   use string_utils
   implicit none

   character(*), parameter :: &
      module_id = 'read_gridded_vars.f90 version 2022-dec-03'

! Input arguments.

   character(*), intent(in) :: varnames(:)	      ! requested var names
   character(*), intent(in) :: reader_codes(:)	      ! reader type codes
   character(*), intent(in) :: grid_file_templates(:) ! input filename templates
   character(*), intent(in) :: grid_coord_file	      ! aux. coordinate file

   integer,      intent(in) :: year, month, day	      ! forecast start date
   integer,      intent(in) :: cycle_time	      ! forecast cycle time
   integer,      intent(in) :: nhours		      ! number of forecast hours
   real,         intent(in) :: vmiss		      ! missing value for fills
   integer,      intent(in) :: diag		      ! verbosity level, 0-N

! Output arguments.

   real, intent(out), allocatable :: grid_data(:,:,:,:)	 ! gridded forecast data
  							 !  (X, Y, vars, hours)

   integer, intent(out),allocatable :: nhours_actual(:)  ! actual hours each var
   integer, intent(out)             :: nhours_actual_max ! maximum hours used

   character(*), intent(out), allocatable :: units(:)	 ! units attrs (vars)

   real(dp),intent(inout), allocatable :: grid_lats(:,:) ! grid coordinates
   real(dp),intent(inout), allocatable :: grid_lons(:,:) !  (X, Y)

   integer,  intent(out)               :: status	 ! normal or fail

! Local variables.

   character(len(varnames)) varname
   character(len(reader_codes)) reader_code, prefix
   character(len(grid_file_templates)) data_file, template
   character fmt1*50

   integer vi, vi_first, nx, ny, nvars
   integer nhours_dim, nhours_expect
   integer igroup, ngroups
   integer group_first_var_num(size(varnames))

   logical, save :: first_call = .true.

   logical var_select(size(varnames),size(varnames))	! square matrix

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 3) print '(a,i0,99(a,i0.2))', '============================ ', &
      year, '-', month, '-', day, ' ', cycle_time, 'Z'

   if (diag >= 3) print *, 'read_gridded_vars: Start.'

   if ( (diag >= 2) .and. (first_call) ) then
      print *, '  Module ID = ' // module_id
      first_call = .false.
   end if

   nvars = size (varnames)

   if (diag >= 3) print *, 'Number of requested variables = ', nvars
   if (diag >= 3) print '(999(1x,a))', 'Requested variables =', &
      (trim (varnames(vi)), vi = 1, nvars)

! Read AQM 2-D grid coordinates.

! In some cases, the 2-D coordinate grids could be read from one of
! the input data files.  However, this version reads the coordinate
! grids up front from a supplemental file, in all cases.  This
! simplifies the program flow.

! Read the coordinate file one time only.
! Can use allocated for the test, with intent (inout).

   if (allocated (grid_lats)) then
      if (diag >= 3) print *, '  Skipping read_coord_file, already done.'
   else
      call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons)
   end if

! Get grid dimensions from coordinate grid.

   nx = size (grid_lats, 1)
   ny = size (grid_lats, 2)

! Cheap adaptation for GFS-CMAQ.  When gridded GFS MET files are
! included, then overdimension the master input array  to handle
! varying number of forecast hours.  This retains consistency with
! previous BC versions.  Shorter variables will be padded with
! standard missing values.

   if (any (reader_codes(:) == 'reader.met')) then
      nhours_dim = nhours + 1		! add one extra hour for MET files
   else
      nhours_dim = nhours		! otherwise use specified no. of hours
   end if

! Allocate the master gridded data array for the current forecast
! cycle, all variables.  Includes empty slots for derived variables.

   if (diag >= 3) print *, 'read_gridded_vars: Allocate main array for one' &
      // ' forecast cycle, all variables.'
   if (diag >= 3) print '(a,4(1x,i0))', '   nx, ny, nvars, nhours =', nx, ny, &
      nvars, nhours_dim

   allocate (grid_data(nx, ny, nvars, nhours_dim))

   grid_data(:,:,:,:) = vmiss		! pre-fill with all missing values

! Set up to track actual hours read for each variable.

   allocate (nhours_actual(nvars))
   nhours_actual(:) = 0

! Clear all units attributes to blanks, including derivatives.

   allocate (units(nvars))
   units(:) = ' '

!-----------------------------------------------------
! Group variables by their common file templates.
!-----------------------------------------------------

! Each input filename template is shared by a group of one or more
! input variables.  Each unique template corresponds 1:1 to a unique
! input file or set of input files.  For each shared template, we
! will read all files and all variables associated with that
! template.  This is an optimization strategy to open and read each
! input file one time only.

   var_select(:,:) = .false.
   igroup = 0

   do vi = 1, nvars
      if (any (var_select (:,vi))) cycle	! skip vars already assigned

! Skip derived variables, to be computed later in a different module.

      reader_code = reader_codes(vi)
      prefix      = reader_code(1:7)		! check for keyword prefix only
      call lowercase (prefix)			! case insensitive here

      if (prefix == 'derived') cycle

! Start a new group.

      igroup = igroup + 1
      group_first_var_num(igroup) = vi

! Find and mark all other variables that use the same input file template.

      template             = grid_file_templates(vi)
      var_select(igroup,:) = (grid_file_templates(:) == template)
      					      ! assigned mask for current group
   end do

   ngroups = igroup

!-----------------------------------------------------
! Main loop over each template group.
!-----------------------------------------------------

template_loop: &
   do igroup = 1, ngroups
      vi_first    = group_first_var_num(igroup)
      template    = grid_file_templates(vi_first)
      reader_code = reader_codes(vi_first)
      call lowercase (reader_code)

! Resolve the current template to a single file name for the current date.
! For hourly files, the forecast hour (FFF) remains unresolved.

      call expand_filename (grid_file_templates(vi_first), year, month, day, &
         cycle_time, data_file)

! Call the specific reader associated with the current template.
! Read one or more variables as selected, into the master input array.

! Common reader for the original MET/CMAQ gridded file format,
! all forecast hours in a single file.  There are two similar
! file types, differing only in expected number of forecast hours.
! This reader quietly removes the vestigial "LAY" dimension in NCEP files.

      if (any (reader_code == (/ 'reader.aqm', 'reader.met' /) )) then

         if (reader_code == 'reader.aqm') then
            nhours_expect = nhours		! for warnings only
         else
            nhours_expect = nhours + 1		! MET files have one extra hour
         end if

         call read_gridded_aqm (data_file, varnames, var_select(igroup,:), &
            nhours_expect, diag, grid_data, nhours_actual, units, status)
				! (X, Y, vars, hours) from (COL, ROW, TSTEP)

! Common reader for RRFS/CMAQ gridded file format, separate file
! for each forecast hour.

      else if (reader_code == 'reader.hourly') then
         call read_gridded_hourly (data_file, varnames, var_select(igroup,:), &
            diag, grid_data, nhours_actual, units, status)
					  ! (X, Y, vars, hours) from (X, Y)
! Diagnostic.

      else
         print *, '*** read_gridded_vars: Unknown reader code "' &
            // trim (reader_code) // '" in config file.'
         print *, '*** Var name = ' // trim (varname) // '.'
         print *, '*** Fundamental configuration problem.  Abort.'
         call exit (1)
      end if

! Abort this entire forecast cycle, on failure to read any single variable.

      if (status /= normal) then
         fmt1 = '(2a,i5.4,3i3.2,"Z")'		! nice message for sparse log
         print fmt1, ' *** read_gridded_vars: Error reading gridded data', &
            ' for cycle', year, month, day, cycle_time
         print *, '*** Abort output file for this cycle.'
         print *
         return			! return abort "status" from reader subroutine
      end if

   end do template_loop

! Return maximum hours across all vars, for extent of complete data array.

   nhours_actual_max = maxval (nhours_actual(:))

! All variables read successfully.  Return success status from last reader.

   if (diag >= 3) print *, 'read_gridded_vars: Return success.'

end subroutine read_gridded_vars
end module read__gridded_vars
