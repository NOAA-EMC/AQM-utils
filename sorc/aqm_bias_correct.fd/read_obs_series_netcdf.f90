!------------------------------------------------------------------------------
!
! read_obs_series_netcdf.f90 -- Read time series from AirNow Netcdf data set.
!
! This is a somewhat generic routine to read assembled hourly
! time series from a set of AirNow Netcdf files.  One call reads
! time series for a single obs data variable, and all sites with
! any data for thst variable.
!
! 2023-apr-10	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from read_obs_series_bufr.f90, version 2023-mar-09.
!		For Netcdf data sets made by convert_airnow_csv.f90.
!
! * Remember to update the date in the module_id below.
!
! Input:   infile_template = path template for input data set.  May include
!	     leading environment var, and YYYY MM DD substitution strings.
!          varname = BUFR name for requested data variable.
!	   start_date, end_date = first and last dates of requested time series.
!          Also see secondary input parameters below.
!
! Output:  Note, output arrays are over-allocated on the site dimension.
!	   tids  = site ID strings.
!	   tlats, tlons = site coordinates.
!	   tdata = 2-D array for time series data (times, sites).
!	     Contigious hourly time series, no skipped days.
!	   vmiss = missing value code in data.
!	   units = units string for requested variable in the current data set.
!
! Notes:
!
! In a single call, this version reads full time series for one
! variable from multiple BUFR input files.  Hourly time series
! are returned in a 2-D array, time x sites.
!
! Output arrays are auto-allocated for all available sites, and
! for the requested time interval.  The site dimension is
! over-allocated.
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

module read__obs_series_netcdf
contains

subroutine read_obs_series_netcdf (infile_template, varname, start_date, &
      end_date, base_year, diag, nsites, tids, tlats, tlons, tdata, vmiss, &
      units, ndays_missing, ndays_error, ndays_valid, nfiles_coord_mismatch)

   use config, only : dp
   use expand__filename
   use index_to_date_mod
   use read__obs_file_netcdf
   use stdlit, only : normal
   implicit none

   character(*), parameter :: &
      module_id = 'read_obs_series_netcdf.f90 version 2023-apr-10'

! Input arguments.

   character(*), intent(in) :: infile_template	! template for input file paths
   character(*), intent(in) :: varname		! requested BUFR var name
   integer,      intent(in) :: start_date	! starting date index
   integer,      intent(in) :: end_date		! ending date index
   integer,      intent(in) :: base_year	! base year for date indexes
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

   integer,      intent(out)              :: nsites	! actual no. sites read
   character(*), intent(out), allocatable :: tids(:)	! site ID strings (S)
   real(dp),     intent(out), allocatable :: tlats(:)	! site coordinates (S)
   real(dp),     intent(out), allocatable :: tlons(:)
   real(dp),     intent(out), allocatable :: tdata(:,:)	! obs time series (T,S)
   real(dp),     intent(out)              :: vmiss	! missing value code
   character(*), intent(out)              :: units	! data var units string

   integer,      intent(out)              :: ndays_valid   ! diagnostic counters
   integer,      intent(out)              :: ndays_missing
   integer,      intent(out)              :: ndays_error
   integer,      intent(out)              :: nfiles_coord_mismatch

! External function definition.

   real distance_btw_coords

! Local program parameters.

   integer, parameter :: nsites_extra = 2000	! number of extra sites to
   						!  allocate temporary arrays

   integer, parameter :: nhours = 24		! hour dimension size

! Limit for discrepancy between coordinates from two different input
! files, for the same site ID.
! This is the actual surface distance between two points on the globe.
! Currently makes only warning messages, when exceeded.

   real, parameter :: max_coordinate_drift = 0.01   ! kilometers

   character(*), parameter :: calendar = 'gregorian'

! Local variables.

   character infile*200, date_string*10
   character file_units*60, save_file_units*60

   integer j, si1, si2
   integer nsites_in, max_sites
   integer hour, ntimes, hi, ti, ti1, ti2, day_offset
   integer year, month, day
   integer date_index, ndays, status
   integer nsites_coord_mismatch

   real dist, max_mismatch_distance
   logical ex, found, overflow_warned

! Dynamic arrays.

   character(len(tids)), allocatable :: file_ids(:)
   real(dp), allocatable :: file_lats(:,:), file_lons(:,:)
   real(dp), allocatable :: file_data(:,:)

   logical, allocatable :: coord_mismatch(:)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 1) print '(2a)', 'read_obs_series_netcdf: Start.'
   if (diag >= 2) print '(2a)', '  Module ID = ', module_id
   if (diag >= 2) print '(2a)','   Requested input variable = ', trim (varname)

   nsites = 0				! empty site arrays

   si2 = 0				! init "previous" site index
   					! needed to suppress possible compiler
                                        ! warning, not needed for lookup algo

   ndays_valid           = 0		! init statistics
   ndays_missing         = 0
   ndays_error           = 0
   nfiles_coord_mismatch = 0

   overflow_warned       = .false.

   units = 'unknown'			! default to unknown data units

!-----------------------------------------------------------
! Main loop over each input file.  One file per day.
!-----------------------------------------------------------

date_loop: &
   do date_index = start_date, end_date

      nsites_coord_mismatch = 0			! clear mismatch statistics
      max_mismatch_distance = 0			! for this file

      save_file_units       = ' '		! init units for each new file

      day_offset = date_index - start_date	! compute time insertion
      ti1 = 1 + (nhours * day_offset)		! indices for current date
      ti2 = ti1 + nhours - 1

      call index_to_date (date_index, year, month, day, base_year, calendar)
      						! get current Y M D integers

      hour = 99				! not expected in current template

      call expand_filename (infile_template, year, month, day, hour, infile)
      					! make input file name for current date

      write (date_string, "(i4, '-', i2.2, '-', i2.2)") year, month, day
      						! for diagnostics

! Check for file missing for current date.

      inquire (file=infile, exist=ex)

      if (.not. ex) then
         if (diag >= 2) &
            print '(2a)', '  File missing, skipping this date: ', trim (infile)
         ndays_missing = ndays_missing + 1	! count missing input files
         cycle date_loop
      end if

! Explicitly deallocate previous input arrays, as needed.
! Arrays change size between input files.

      if (allocated (file_data)) then
         if (diag >= 5) print *, '   Deallocate previous input arrays.'
         deallocate (file_ids, file_lats, file_lons, file_data)
      end if

! Read input file for this date.  Netcdf, one file per day.

! Note:  Transposed dimensions.  On input, dimensions of data
! and coordinates are (sites, times).  For output to caller,
! data array (tdata) will be transposed later to (times, sites).

      if (diag >= 4) print *
      if (diag >= 2) print '(2a)', '  Read file: ', trim (infile)

      call read_obs_file_netcdf (infile, varname, year, month, day, diag, &
         file_ids, file_lats, file_lons, file_data, vmiss, file_units, status)

      nsites_in = size (file_data, 1)		! (S,T) will become (T,S)

! If input file is flagged invalid, ignore returned data and skip this date.
! Subroutine prints its own diagnostics.

      if (status /= normal) then
         ndays_error = ndays_error + 1		! count days with input errors
         cycle date_loop
      end if

! First time only, allocate the main input accumulator arrays.

      if (.not. allocated (tdata)) then
         ndays  = end_date - start_date + 1
         ntimes = nhours * ndays

         max_sites = nsites_in + nsites_extra

         if (diag >= 3) print *, '  Allocate temporary obs input arrays for' &
            // ' maximum number of sites.'
         if (diag >= 3) print '(a,2(1x,i0))', '   ntimes, max_sites =', &
            ntimes, max_sites

         allocate (tids(max_sites), tlats(max_sites), tlons(max_sites))
         allocate (tdata(ntimes, max_sites))
         allocate (coord_mismatch(max_sites))

         tids(:)           = ' '	! clear main accumulators to all missing
         tlats(:)          = vmiss	! (S)
         tlons(:)          = vmiss	! (S)
         tdata(:,:)        = vmiss	! (T,S)
         coord_mismatch(:) = .false.	! (S)
      end if

!-----------------------------------------------------------
! Copy input site data into primary site arrays.
!-----------------------------------------------------------

site_loop: &
      do si1 = 1, nsites_in

! Skip all sites with no data for the target obs variable.

         if (all (file_data(si1,:) == vmiss)) cycle site_loop

! Find the site ID in the current site list.
! si1 = input site index, si2 = site index in primary list.

! This method is optimized to the assumption of similar site ordering
! between input files.  Start searching with the site following the
! previous found site.

! There is no extra penalty when this assumption breaks down.  Either
! way, there will be at most a single pass through the primary site list,
! for each input site.

! On entry each time, si2 = index of previous site, initially.
! Works correctly for startup (nsites = 0), and both known and
! unknown sites.

         found = .false.

         do j = 1, nsites
            si2 = si2 + 1			! advance to next site in list
            if (si2 > nsites) si2 = 1		! wrap around end of list

            found = (tids(si2) == file_ids(si1))
            if (found) exit			! site ID found
         end do

! Site not found.  Check for site array overflow.

         if (.not. found) then

            if (nsites >= max_sites) then

               if (.not. overflow_warned) then
                  overflow_warned = .true.
                  print '(2a)',   '*** read_obs_series_netcdf: Exceeded', &
                                       ' maximum number of sites.'
                  print '(a,i0)', '*** Current array size = ', max_sites
                  print '(2a)',   '*** Current site ID    = ', &
                                       trim (file_ids(si1))
                  print '(2a)',   '*** Update program, increase nsites_extra.'
                  print '(2a)',   '*** Soft error.  Skip this site, and', &
                                       ' continue with available sites.'
                  print *
               else
                  print '(2a)',   '*** Too many sites, skipping ', &
                                       trim (file_ids(si1))
               end if

               cycle site_loop			! on overflow, ignore all new
            end if				! sites; treat as soft error

! Insert new site into the primary site array.

            nsites    = nsites + 1		! increase array size
            si2       = nsites			! array index for new site

            tids(si2)  = file_ids(si1)		! add new site ID
         end if

! Now si2 = current site index number.
! Check for possible mismatched coordinates at every hour.

 hour_loop: &
         do hi = 1, 24				! 1-based index
            ti   = ti1 + (hi - 1)
            hour = hi -1			! zero-based

! Ignore missing coordinates from input file.

            if (   (file_lats(si1,hi) .eq. vmiss) &
              .or. (file_lons(si1,hi) .eq. vmiss) ) cycle hour_loop

! First time for each site, assign new coordinates from input file.

            if ( (tlats(si2) .eq. vmiss) .and. (tlons(si2) .eq. vmiss) ) then
               tlats(si2) = file_lats(si1,hi)
               tlons(si2) = file_lons(si1,hi)
               cycle hour_loop
            end if

! Check for mismatched coordinates at current hour.

            if (    (file_lats(si1,hi) .eq. tlats(si2)) &
              .and. (file_lons(si1,hi) .eq. tlons(si2)) ) cycle hour_loop

! Not exactly the same.  Look closer.

            dist = distance_btw_coords ( real (file_lats(si1,hi)), &
               real (file_lons(si1,hi)), real (tlats(si2)), real (tlons(si2)) )

            if (dist > max_coordinate_drift) then

               coord_mismatch(si2) = .true.	! flag each site with mismatch

               max_mismatch_distance = max (max_mismatch_distance, dist)
				! find max mismatch distance in entire run

               if (diag >= 3) then
                  print *
                  print *, '*** read_obs_series_netcdf: Warning:'
                  print *, '*** Difference in coordinates between input files' &
                                // ' exceeds limit.'
                  print '(3a,i2.2,a)', ' *** Site ID, date, time = ', &
                                tids(si2), date_string, hour, 'Z'
                  print '(a,2f12.5)', ' *** Previous coordinates     =', &
                                tlats(si2), tlons(si2)
                  print '(a,2f12.5)', ' *** Current file coordinates =', &
                                file_lats(si1,hi), file_lons(si1,hi)
                  print '(a,f12.3,a)', ' *** Surface offset =', dist, ' Km'
                  print '(a,f12.3,a)', ' *** Limit          =', &
                                max_coordinate_drift, ' Km'
                  print *, '*** The newer coordinates will be used.'
               end if

! For mismatches, always replace older coordinates with the newer ones.

               tlats(si2) = file_lats(si1,hi)
               tlons(si2) = file_lons(si1,hi)
            end if

         end do hour_loop

! Insert input data for current date into primary time series array.

         tdata(ti1:ti2,si2) = file_data(si1,:)		! (T,S) <-- (S,T)

! Remember the data units attribute for the current file, if available.

         if (save_file_units == ' ') then		! skip if already saved
            if (file_units /= ' ') save_file_units = file_units
         end if

      end do site_loop

! Report file summary in case of mismatched coordinates.

      if (nsites_coord_mismatch /= 0) then
         nfiles_coord_mismatch = nfiles_coord_mismatch + 1
         				! count files with mismatched coords
         if (diag >= 1) then
            print '(2a,i5)',     '  *** Number of sites in this file with', &
               ' mismatched coordinates =', nsites_coord_mismatch
            print '(a,f12.2,a)', '  *** Maximum surface mismatch in file =', &
               max_mismatch_distance, ' Km'
            print *
         end if
      end if

! Check for consistent data units attribute between files.

      if (save_file_units /= ' ') then
         if (units /= 'unknown') then
            if (save_file_units /= units) then
               print *
               print *, '*** read_obs_series_netcdf: Warning:'
               print *, '*** Data units attribute changed between input files.'
               print '(9a)', ' *** File = ', trim (infile)
               print '(9a)', ' *** Previous units =', trim (units)
               print '(9a)', ' *** Current units =',  trim (save_file_units)
               print '(9a)', ' *** The newer units will be used.'
            end if
         end if

         units = save_file_units		! save or update output units
      end if

      ndays_valid = ndays_valid + 1		! count valid input files
   end do date_loop

   if (diag >= 2) print *
   if (diag >= 2) print *, ' End of requested time range.'
   if (diag >= 3) print *, ' read_obs_series_netcdf: Return.'

! Return with all site data in over-allocated arrays.

end subroutine read_obs_series_netcdf
end module read__obs_series_netcdf
