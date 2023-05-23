!------------------------------------------------------------------------------
!
! read_obs_qc.f90 -- Read obs time series data for the bias correction system.
!		     Perform data quality control on the fly.
!
! This is the top level routine of the quality control module,
! one of the four main components of the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-jun-10	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Adapted from read_gridded_vars.f90, version 2014-may-13.
!		For EPA AIRNow BUFR data sets received by NOAA NWS/NCO.
! 2014-jun-12	Switch from symbolic var name to obs var name, for
!		  deciding units conversion.
!		Eliminate sites with all missing data.
! 2014-jun-19	Remove new_site_list_template calling argument.
!		-- This functionality moved to main program.
!		Add a few time logs.
! 2014-jun-22	Eliminate off-grid sites.
!		Restructure the elimination code, add flag array.
! 2014-jun-26	Format bug fix, caught by ifort.
!
! 2016-jan-12	Minor library upgrade.  Use string_utils module.
!
! 2017-apr-04	Add ozone support.  Pass var name to QC routine.
!		Add ozone units conversion, mole/mole to ppmv, to match CMAQ.
!
! 2021-apr-27	Add site blocking list to remove problem intervals in obs data.
! 2021-nov-11	Improve off-grid site diagnostics.  Add distance to nearest
!		  grid point, and amount of data present.
!
! 2022-may-27	Add config parameter for obs maximum valid input threshold.
!
! 2023-mar-28	Fix PM2.5 QC for higher input threshold values.
! 2023-apr-06	Break out units converter into separate subroutine.
!		New time alignment standard, hourly forward averaged
!		  convention, for all obs input sources.
!		BUFR input is now offset one hour backward, to conform.
!		See time alignment notes in read_obs_series_bufr.f90.
!		Add QC change diagnostics.
! 2023-apr-08	Add lower limit for AirNow negative input values.
!		Add low/high input limits for ozone as well as PM2.5.
! 2023-apr-11	Minor improvements to diagnostics.
!
! * Remember to update the date in the module_id below.
!
! Input:   infile_template = Path template for input data set.  May include
!		leading environment var, and YYYY MM DD substitution strings.
!          varname = Source name for requested obs var, e.g. PM25 or COPOPM.
!	   start_date, end_date = First and last dates of requested time series.
!          Also see secondary input parameters below.
!
! Output:  site_ids = Site ID strings.
!	   site_lats, site_lons = Site coordinates.
!	   out_data = 2-D array for time series data.  (Time, sites).
!		Contigious hourly time series, no skipped days.
!	   units = Units string for requested variable.  This is the units
!		after automatic units conversion, NOT the units in the
!		original data set.  The returned units string is correct for
!		the returned data array.
!
! Notes:
!
! Several subsystems are incorporated in this routine:
!
! * Read raw obs time series (BUFR reader).
! * Remove site data time intervals specified in blocking list.
! * Automatic units conversion.
! * Missing value conversion.
! * Obs quality control.
!
! This routine reads time series for one selected variable, all
! sites, and all available dates within the requested time
! period.  Quality control is then performed on the fly, before
! returning QC'd time series to the calling program.
!
! Hourly time series are returned in a 2-D array, time x sites.
! Output arrays are auto-allocated for all sites that pass QC,
! and for the requested time interval.  Missing values are
! inserted for dates and times not available.
!
! Time alignment (2023 April 5):  Obs data are now returned as
! FORWARD AVERAGED time series.
!
! Array index  1 = hour  0 = 0Z to 1Z average.
! Array index  2 = hour  1 = 1Z to 2Z average.  Etc.
! Array index 24 = hour 23 = 23Z to 0Z average.
!
! Missing values in the input data set are converted to the
! caller's specified missing value.  As of the current version,
! 2014-jun-9, this missing value code must be a negative number,
! for correct operation of the qc_single_station subroutine.
!
! The obs input data unit is automatically converted to a
! standard unit which is expected by both the QC subroutine and
! the remainder of the bias correction process.  A units string
! for the converted unit is returned to the caller.
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

module read__obs_qc
contains

subroutine read_obs_qc (infile_template, varname, start_date, end_date, &
      base_year, obs_min_input, obs_max_input, vmiss, grid_lats, grid_lons, &
      site_block_list, diag, site_ids, site_lats, site_lons, out_data, units)

   use config, only : dp
   use convert__obs_units
   use grid_location
   use qc__single_site
   use read__obs_series
   use site__blocking
   use string_utils
   implicit none

   character(*), parameter :: module_id = 'read_obs_qc.f90 version 2023-apr-11'

! Input arguments.

   character(*), intent(in) :: infile_template	! template for input file paths
   character(*), intent(in) :: varname		! requested var name
   integer,      intent(in) :: start_date	! starting date index
   integer,      intent(in) :: end_date		! ending date index
   integer,      intent(in) :: base_year	! base year for date indexes
   real(dp),     intent(in) :: obs_min_input	! obs min valid input threshold
   real(dp),     intent(in) :: obs_max_input	! obs max valid input threshold
   real(dp),     intent(in) :: vmiss		! caller specified missing value
   real(dp),     intent(in) :: grid_lats(:,:)	! 2-D grid coordinates (X,Y)
   real(dp),     intent(in) :: grid_lons(:,:)	!   for screening site locations
   character(*), intent(in) :: site_block_list  ! blocking list file name
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

   character(*), intent(out), allocatable :: site_ids(:)   ! site ID strings (S)
   real(dp),     intent(out), allocatable :: site_lats(:)  ! site coords (S)
   real(dp),     intent(out), allocatable :: site_lons(:)
   real(dp),     intent(out), allocatable :: out_data(:,:) ! time series (T,S)
   character(*), intent(out)              :: units	   ! var units string

! Local variables.

   character(80) orig_units, title1, title2
   character(60) fmt1, fmt2, fmt3, fmt_vmiss
   character fdate_str*24

   integer si, si2, nsites_in, ntimes
   integer nvalid, nmiss
   integer nx, ny, noff_grid
   integer nsites_valid, nodata1, nodata2, nsites_nodata
   integer percent_valid_int

   real(dp) vmiss_in, vmin, vmax, percent_miss

   logical off_grid

   integer, allocatable :: i_corners(:), j_corners(:)	! grid indices
   logical, allocatable :: valid(:)			! track valid sites

! Dynamic arrays for site input data.

   character(len(site_ids)), allocatable :: in_ids(:)	! site ID strings (S)
   real(dp), allocatable :: in_lats(:)			! site coordinates (S)
   real(dp), allocatable :: in_lons(:)
   real(dp), allocatable :: in_data(:,:)	! time series (T,S)
   real(dp), allocatable :: distance(:)		! distance to nearest grid point

!-----------------------------------------------------------
! Read obs time series for all sites, all requested dates.
!-----------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, 'read_obs_qc: Start.'
   if (diag >= 2) print *, '  Module ID = ' // module_id
   if (diag >= 2) print *

   call fdate (fdate_str)
   print '(2a)', fdate_str, '  Call read_obs_series.'

   call read_obs_series (infile_template, varname, start_date, end_date, &
      base_year, diag, in_ids, in_lats, in_lons, in_data, vmiss_in, orig_units)

   ntimes    = size (in_data, 1)		! get time dimensions
   nsites_in = size (in_data, 2)		! get site dimension

! Show raw input statistics.

   print *, &
      '======================================================================='

   fmt_vmiss = '(a,g19.12,a,es9.2,a)'	! format for missing value diags

   if (diag >= 2) then
      vmin  = minval ( in_data, (in_data /= vmiss_in) )
      vmax  = maxval ( in_data, (in_data /= vmiss_in) )

      nmiss = count (in_data == vmiss_in)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      nodata1 = 0
      do si = 1, nsites_in
         if (all (in_data(:,si) == vmiss_in)) nodata1 = nodata1 + 1
      end do

      fmt1 = '(2(a,g0.4),1x,a)'
      fmt2 = '(a,i0,a,f0.1,a)'

      print '(2a)',    ' read_obs_qc:  Raw input summary:'
      print '(2a)',    '   Input var name            = ', trim (varname)
      print '(2a)',    '   Input data units          = ', trim (orig_units)
      print '(a,i0)',  '   nsites_in                 = ', nsites_in
      print '(a,i0)',  '   No. of sites with no data = ', nodata1
      print '(a,i0)',  '   Number of time steps      = ', ntimes
      print fmt1,      '   Min, max raw input data   = ', vmin, ', ', vmax, &
                             trim (orig_units)
      print fmt2,      '   Number of missing values  = ', nmiss, &
                             ' (', percent_miss, '%)'
      print fmt_vmiss, '   Missing value             =',  vmiss_in, &
                             ' (', vmiss_in, ' single precision)'
      print *
   end if

   call fdate (fdate_str)
   print '(3a)', fdate_str, '  read_obs_qc: Read obs complete.', &
      '  Run QC procedures.'

!-----------------------------------------------------------
! Block specified time intervals in site input data.
!-----------------------------------------------------------

! Block on raw input data, before the rest of the QC process.
! See site blocking routines for more details.
! This routine prints a summary after removing data.

   call site_blocking (site_block_list, in_ids, start_date, base_year, &
      vmiss_in, diag, in_data)

! Input statistics after site blocking.

   fmt_vmiss = '(a,g19.12,a,es9.2,a)'	! format for missing value diags

   if (diag >= 2) then
      vmin  = minval ( in_data, (in_data /= vmiss_in) )
      vmax  = maxval ( in_data, (in_data /= vmiss_in) )

      nmiss = count (in_data == vmiss_in)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      nodata2 = 0
      do si = 1, nsites_in
         if (all (in_data(:,si) == vmiss_in)) nodata2 = nodata2 + 1
      end do

      fmt1 = '(2(a,g0.4),1x,a)'
      fmt2 = '(a,i0,a,f0.1,a)'

      print *
      print '(a,i0)', ' read_obs_qc:  Summary after site blocking:'
      print '(a,i0)', '   No. of sites with no data = ', nodata2
      print fmt1,     '   Min, max obs data         = ', vmin, ', ', vmax, &
                            trim (orig_units)
      print fmt2,     '   Number of missing values  = ', nmiss, &
                            ' (', percent_miss, '%)'
   end if

!-----------------------------------------------------------
! Units conversion of obs input data.
!-----------------------------------------------------------

! Must perform any needed units conversion BEFORE calling QC routine.

   call convert_obs_units (varname, orig_units, vmiss_in, diag, in_data, units)

!-----------------------------------------------------------
! Convert to requested missing value.
!-----------------------------------------------------------

! This must also be done before calling the QC routine.
! This is required in this position because of dependencies within
! the current version of the QC subroutine.

   print *
   print *, 'read_obs_qc:  Convert to requested missing value code.'

   if (diag >= 2) then
      print fmt_vmiss, '   Input missing value       =', vmiss_in, &
                             ' (', vmiss_in, ' single precision)'
      print fmt_vmiss, '   Requested missing value   =', vmiss, &
                             ' (', vmiss,    ' single precision)'
   end if

! Same missing values, skip conversion.

   if (vmiss_in == vmiss) then
      print '(2a)', '   *** No conversion needed.'
      print '(2a)', '   *** Requested missing value is the same as in', &
         ' the original data.'

! Different missing values.  Do the conversion now.

   else
      where (in_data == vmiss_in) in_data = vmiss

! Ignore any possible missing value range conflict, for now.
! Currently the numbers for PM2.5 seem favorable.  Maybe check later.
! But still show revised statistics after missing value substitution.

      if (diag >= 2) then
         vmin  = minval ( in_data, (in_data /= vmiss) )
         vmax  = maxval ( in_data, (in_data /= vmiss) )

         nmiss = count (in_data == vmiss)
         percent_miss = (nmiss * 100.0_dp) / size (in_data)

         print '(2(a,g0.4),1x,a)', '   Min, max converted data   = ', vmin, &
                                         ', ', vmax, trim (units)
         print '(a,i0,a,f0.1,a)',  '   Number of missing values  = ', nmiss, &
                                         ' (', percent_miss, '%)'
         print *
      end if
   end if

!-----------------------------------------------------------
! Run the main quality control procedure.
!-----------------------------------------------------------

   print *
   print *, 'read_obs_qc:  Run obs quality control for each site.'

   allocate (valid(nsites_in))			! clear site valid flags
   valid(:) = .false.

   if (diag>=3) print *, '     After QC:'
   if (diag>=3) print *, '  Site ID          Min data     Max data' &
      // '    No. valid   No. missing'

! Main QC loop over each site.

   nsites_valid = 0

   do si = 1, nsites_in
      call qc_single_site (varname, in_data(:,si), obs_min_input, &
         obs_max_input, vmiss, diag, in_ids(si))

      nmiss  = count  (in_data(:,si) == vmiss)
      nvalid = ntimes - nmiss

      valid(si) = (nvalid > 0)		      ! mark sites with or without data

      if (valid(si)) nsites_valid = nsites_valid + 1	! count sites with data

! Show statistics line for each site, if selected.

      if (diag >= 3) then
         vmin = minval ( in_data(:,si), (in_data(:,si) /= vmiss) )
         vmax = maxval ( in_data(:,si), (in_data(:,si) /= vmiss) )

         if (nvalid == 0) vmin = vmiss		! fix min and max display
         if (nvalid == 0) vmax = vmiss		! if all missing

         print '(3x,a,2f13.2,2i13)', in_ids(si), vmin, vmax, nvalid, nmiss
      end if
   end do

   nsites_nodata = nsites_in - nsites_valid

! Short summary of site counts.

   if (diag >= 3) print *

   if (diag >= 2 .or. nsites_valid == 0) then
      nmiss = count (in_data == vmiss)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      print '(a,i0)',          '   Number of input sites     = ', nsites_in
      print '(a,i0)',          '   No. valid sites after QC  = ', nsites_valid
      print '(a,i0)',          '   No. of sites with no data = ', nsites_nodata
      print '(a,i0,a,f0.1,a)', '   Number of missing values  = ', nmiss, &
         ' (', percent_miss, '%)'
   end if

! Controlled halt if no valid sites after QC.

   if (nsites_valid == 0) then
      print *
      print '(2a)', '*** read_obs_series: Fatal error, no valid sites', &
         ' after QC.  Abort.'
      call exit (1)
   end if

!-----------------------------------------------------------
! Eliminate sites with off-grid coordinates.
!-----------------------------------------------------------

   if (diag>=2) print *
   print *, 'read_obs_qc:  Eliminate sites with off-grid coordinates.'

   if (diag>=3) print *, '  Call gridlocation, find site locations on', &
      ' output grid.'

! Compute grid indices of cell corners for the site locations.
! Off grid sites return -999 in i, j indices.

   allocate (i_corners(nsites_in), j_corners(nsites_in))
   allocate (distance(nsites_in))

   call gridlocation (in_lats, in_lons, grid_lats, grid_lons, in_ids, diag, &
      i_corners, j_corners, distance)

! Eliminate off-grid sites with data.
! Sites with no data were already eliminated.

   nx = size (grid_lats, 1)			! get grid dimensions
   ny = size (grid_lats, 2)

   noff_grid = 0

   do si = 1, nsites_in
      off_grid = (    (i_corners(si) < 1 .or. i_corners(si) > nx) &
                 .or. (j_corners(si) < 1 .or. j_corners(si) > ny) )

      if (off_grid .and. valid(si)) then
         valid(si) = .false.
         noff_grid = noff_grid + 1

         if (diag >= 2 .and. noff_grid == 1) then
            print *
            print *, 'Summary of eliminated off-grid sites with data.'
            print *, 'Set diag=3 or higher for details.'
            title1 = '                                     Nearest'
            title2 = 'Site ID        Latitude   Longitude  Grid point' &
                     // '  Data present'
            print '(15x,a)', trim (title1)
            print '(15x,a)', trim (title2)
         end if

         if (diag >= 2) then
            nvalid = count (in_data(:,si) /= vmiss)
            percent_valid_int = nint ((nvalid * 100.0) / ntimes)

            if (nvalid < ntimes) percent_valid_int = min (percent_valid_int, 99)
            if (nvalid > ntimes) percent_valid_int = max (percent_valid_int, 1)
            			! enforce truth for not exactly 0% and 100%

            print '(2a,f11.5,f12.5,f9.1,a,i8,a)', &
               ' *** Off-grid: ', in_ids(si), in_lats(si), in_lons(si), &
               distance(si), ' km', percent_valid_int, '%'
         end if

         in_data(:,si) = vmiss		! after diags, clear off-grid data
         				! to all missing, to fix statistics
      end if
   end do

   nsites_valid = nsites_valid - noff_grid	! adjust count of valid sites

   if (diag>=2) print *
   if (diag>=2) print '(2a,i0)', ' Number of additional sites eliminated', &
      ' for off grid = ', noff_grid

   if (diag>=2) print '(a,i0)',  ' Number of remaining valid sites = ', &
      nsites_valid

! Controlled halt if no valid sites after grid check.

   if (nsites_valid == 0) then
      print *
      print '(2a)', '*** read_obs_series: Fatal error, no valid sites', &
         ' after grid check.  Abort.'
      call exit (1)
   end if

!-----------------------------------------------------------
! Copy remaining data to output arrays.
!-----------------------------------------------------------

   if (diag>=2) print *
   if (diag>=2) print *, 'read_obs_qc:  Copy valid obs data to output arrays.'

   allocate (site_ids(nsites_valid), site_lats(nsites_valid))
   allocate (site_lons(nsites_valid), out_data(ntimes,nsites_valid))

   si2 = 0					! init output site index

   do si = 1, nsites_in
      if (valid(si)) then			! copy valid sites only
         si2 = si2 + 1				! increment output pointer
         site_ids(si2)    = in_ids(si)		! copy metadata
         site_lats(si2)   = in_lats(si)
         site_lons(si2)   = in_lons(si)
         out_data(:,si2)  = in_data(:,si)	! copy time series
      end if
   end do

   if (diag>=2) print '(a,i0)', '   Total number of sites eliminated = ', &
      (nsites_in - nsites_valid)

!-----------------------------------------------------------
! Show final QC statistics.
!-----------------------------------------------------------

   print *
   print *, 'read_obs_qc:  Final obs QC summary:'

   vmin  = minval ( out_data, (out_data /= vmiss) )
   vmax  = maxval ( out_data, (out_data /= vmiss) )

   nmiss = count (out_data == vmiss)
   percent_miss = (nmiss * 100.0_dp) / size (out_data)

   fmt1 = '(a,i0)'
   fmt2 = '(2(a,g0.4),1x,a)'
   fmt3 = '(a,i0,a,f0.1,a)'

   print '(2a)','   Var name                  = ', trim (varname)
   print '(2a)','   Output data units         = ', trim (units)
   print fmt1,  '   No. valid sites after QC  = ', nsites_valid
   print fmt2,  "   Min, max QC'ed data       = ", vmin,', ', vmax, trim (units)
   print fmt3,  '   Number of missing values  = ', nmiss,' (', percent_miss,'%)'
   print '(2a)','     after eliminating sites'
   print *

   if (diag>=3) print *, 'read_obs_qc:  QC complete.  Return to main program.'

end subroutine read_obs_qc
end module read__obs_qc
