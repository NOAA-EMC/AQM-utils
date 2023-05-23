!------------------------------------------------------------------------------
!
! read_obs_series.f90 -- Read obs site time series from a specified data set.
!
! This is a generic routine to read assembled hourly obs time
! series from an input data set.  This routine handles the switch
! between different formats, such as BUFR and Netcdf.  One call
! reads a complete time series for the specified obs variable,
! and all available sites within the requested time range.
!
! 2014-jun-05	read_obs_series.f90:
!		Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		For EPA AIRNow BUFR data sets received by NOAA NWS/NCO.
!
! 2021-nov-08	Last BUFR-only version.
!		See read_obs_series_bufr.f90 for full history of BUFR version.
!
! 2023-mar-09	read_obs_series.f90:
!	`	Break out BUFR reader into separate routine.
!		Add Netcdf reader.
!		This routine converted from BUFR reader, to simple dispatcher.
!		Reduced to switching between data sets, plus common diagnostics.
! 2023-apr-05	Update comments only.
!		New time alignment standard, hourly forward averaged
!		  convention, for all obs input sources.
!		BUFR input is now offset one hour backward, to conform.
!		See time alignment notes in read_obs_series_bufr.f90.
!
! Input:   infile_template = path template for input data set.  May include
!	     leading environment var, and YYYY MM DD substitution strings.
!          varname = Name of requested obs file variable.
!	   start_date, end_date = first and last dates of requested time series.
!          Also see secondary input parameters below.
!
! Output:  site_ids = site ID strings.
!	   site_lats, site_lons = site coordinates.
!	   out_data = 2-D array for time series data.  (Time, sites).
!	     Contigious hourly time series, no skipped days.
!	   vmiss = missing value code in data.
!	   units = units string for requested variable in the current data set.
!
! Notes:
!
! In a single call, this version reads full time series for one
! variable from multiple input files.  Hourly time series are
! returned in a 2-D array, time x sites.
!
! The input filename template indicates the data set type.
! Ending in ".nc" indicates Netcdf.  Otherwise, BUFR is assumed.
!
! This version does not currently support mixed input data sets,
! such as mixed BUFR and Netcdf.
!
! Output arrays are auto-allocated for all available sites, and
! for the requested time interval.
!
! Missing values are encoded with the original missing value code
! in the data set.  This code is returned in the vmiss output.
!
! No units conversion is performed.  Data are returned in the
! original units as stored in the data set.
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
! Single-value soft errors are handled invisibly by a lower layer.
! Console warning messages may be produced for some kinds of
! these errors,  Single-value errors are replaced with missing
! values in the result array.
!
! Missing or invalid files are also treated as soft errors, and
! ignored.  Data for such files is included in the output time
! series as all missing values.
!
! Serious errors may be detected at a lower level, resulting in
! a diagnostic message and program abort.
!
!------------------------------------------------------------------------------

module read__obs_series
contains

subroutine read_obs_series (infile_template, varname, start_date, end_date, &
      base_year, diag, site_ids, site_lats, site_lons, out_data, vmiss, units)

   use config, only : dp
   use expand__filename
   use index_to_date_mod
   use read__obs_series_bufr
   use read__obs_series_netcdf
   use string_utils
   implicit none

! Input arguments.

   character(*), intent(in) :: infile_template	! template for input file paths
   character(*), intent(in) :: varname		! requested file var name
   integer,      intent(in) :: start_date	! starting date index
   integer,      intent(in) :: end_date		! ending date index
   integer,      intent(in) :: base_year	! base year for date indexes
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

   character(*), intent(out), allocatable :: site_ids(:)   ! site ID strings (S)
   real(dp),     intent(out), allocatable :: site_lats(:)  ! site coords (S)
   real(dp),     intent(out), allocatable :: site_lons(:)
   real(dp),     intent(out), allocatable :: out_data(:,:) ! time series (T,S)
   real(dp),     intent(out)              :: vmiss	   ! missing value code
   character(*), intent(out)              :: units	   ! units str for var.

! Local program parameters.

   integer, parameter :: nhours = 24		! hour dimension size

! Local variables.

   character fmt1*50, fmt2*50, obs_type*10, ext*3

   integer p1, si, nsites, hour_dim, ndays
   integer ndays_valid, ndays_missing, ndays_error
   integer nfiles_coord_mismatch, all_missing_count

! Local accumulators for site data.  Site dimension will be oversized.

   character(len(site_ids)), allocatable :: tids(:)
   real(dp), allocatable :: tlats(:), tlons(:)
   real(dp), allocatable :: tdata(:,:)		! site time series (H,S)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print '(a)', &
      '======================================================================='
   if (diag >= 1) print '(a)', 'read_obs_series: Start.'

   ndays    = end_date - start_date + 1		! get time dimensions
   hour_dim = nhours * ndays

! Determine obs data type from contents of the input file template.

   p1  = len_trim (infile_template) - 2		! get ".nc" filename extension
   p1  = max (p1, 1)
   ext = infile_template(p1:)
   call lowercase (ext)

   if (ext == '.nc') then			! detect netcdf or BUFR
      obs_type = 'Netcdf'
   else
      obs_type = 'BUFR'
   end if

   if (diag >= 2) print *, '  Detected obs data type = ' // trim (obs_type)

!-------------------------------------------------
! Dispatch to the specific data type reader.
!-------------------------------------------------

   if (obs_type == 'BUFR') then
      call read_obs_series_bufr (infile_template, varname, start_date, &
         end_date, base_year, diag, nsites, tids, tlats, tlons, tdata, &
         vmiss, units, ndays_missing, ndays_error, ndays_valid, &
         nfiles_coord_mismatch)

   else
      call read_obs_series_netcdf (infile_template, varname, start_date, &
         end_date, base_year, diag, nsites, tids, tlats, tlons, tdata, &
         vmiss, units, ndays_missing, ndays_error, ndays_valid, &
         nfiles_coord_mismatch)
   end if

!-------------------------------------------------
! Print input summary, part 1.
!-------------------------------------------------

   fmt1 = '(1x,a,i0)'
   fmt2 = '(1x,9a)'

   print *,    '-------------------------------------------------------------'
   print fmt2, 'Input summary, all ', trim (obs_type), ' files combined:'
   print fmt1, 'Total number of days in input period   = ', ndays
   print fmt1, 'Days skipped, missing input files      = ', ndays_missing
   print fmt1, 'Days skipped, invalid input files      = ', ndays_error
   print fmt1, 'Number of valid days read              = ', ndays_valid
   print *
   print fmt1, 'Number of files with mismatched coords = ',nfiles_coord_mismatch

! Controlled halt if all files missing.

   if (ndays_valid == 0) then
      print *
      print '(2a)', '*** read_obs_series: Fatal error, no valid input', &
         ' files.  Abort.'
      call exit (1)
   end if

!-----------------------------------------------------------
! Return all site data to caller.
!-----------------------------------------------------------

! Allocate output arrays for actual number of sites, all files combined.

   if (diag >= 4) print *, ' read_obs_series: Allocate output arrays.'

   allocate (site_ids(nsites), site_lats(nsites), site_lons(nsites))
   allocate (out_data(hour_dim,nsites))

! Copy all site data.

   if (diag >= 4) print *, ' Copy all site data to output arrays.'

   site_ids(:)   = tids(1:nsites)		! copy site ID's
   site_lats(:)  = tlats(1:nsites)		! copy coordinates
   site_lons(:)  = tlons(1:nsites)

   out_data(:,:) = tdata(:,1:nsites)		! copy data values (hours,sites)

! Count number of sites with all missing data.

   all_missing_count = 0

   do si = 1, nsites
      if (all (tdata(:,si) == vmiss)) all_missing_count = all_missing_count + 1
   end do

! Print input summary, part 2.

   print *
   print fmt1, 'Number of unique sites from all files = ', nsites
   print fmt1, 'Number of sites with all missing data = ', all_missing_count

   if (diag >= 2) print *
   if (diag >= 3) print *, 'read_obs_series: Return.'

end subroutine read_obs_series

end module read__obs_series
