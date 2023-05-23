!-----------------------------------------------------------------------------
!
! convert_airnow_csv.f90 -- Convert AirNow Hourly AQ Obs CSV files to Netcdf.
!
! This program converts all hourly AirNow CSV files for one calendar
! day, into a single Netcdf file.  All data variables and all observation
! sites are copied, with few exceptions.  Repetitive site metadata are
! combined into into a single record for each site.
!
! 2023-feb-06	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Because of occasional variations in the source data,
!		  this version saves site coordinates hourly, not daily.
!
! Remember to update the program ID below.
!
! This version will batch process a range of calendar dates, by
! specifying start and end dates on the command line.
!
! This version is for dynamic use in bias correction.  As such,
! it supports incomplete input data for any calendar date.  This
! is intended for repeated refreshing of each date, so as to keep
! the daily Netcdf file updated with the most recent hourly
! information.
!
! See comments in read_airnow_csv.f90 for detailed discussion of
! the AirNow CSV input file format.  Also see EPA's documentation,
! HourlyAQObsFactSheet.pdf.
!
! This program supports only AirNow records that are aligned on
! whole hours, such that ValidTime always ends in :00 (minutes).
! A small number of input records have unusual alignment such as
! 1:15 or 1:30.  These records are not supported, and they are
! discarded with warning messages and counts printed.  As of
! 2023 January, all such records are only for European or Asian
! sites, so of no consequence for North American bias correction.
!
!------------------------------------------------------------------------------
!
! Usage:
!
! Output directories must be created externally before running
! this program.
!
! ./convert_airnow_csv IN_TEMPLATE OUT_TEMPLATE YYYYMMDD1 YYYYMMDD2 diag=N
!
! IN_TEMPLATE is the path for input data files.
! OUT_TEMPLATE is the path for generated output files.
!
! Insert substitution strings YYYY, MM, DD, and HH for year, month,
! day, and hour.  Strings may be repeated as needed.
!
! Each path may be relative or absolute.
!
! YYYYMMDD1 and YYYYMMDD2 are the 8-digit start and end dates.
!
! Optional diag=N specifies the verbosity level.
! The default is diag=2 for output file name and summary, only.
!
! Sample commands:
!
! set INPATH  = data/airnow/csv/YYYY/YYYYMMDD/HourlyAQObs_YYYYMMDDHH.dat
! set OUTPATH = data/airnow/netcdf/YYYY/HourlyAQObs.YYYYMMDD.nc
!
! ./convert_airnow_csv $INPATH $OUTPATH 20220801 20220831 diag=3
! ./convert_airnow_csv $INPATH $OUTPATH 20220901 20220904
!
!-----------------------------------------------------------------------------
!
! Memory assessment:
!
! This version uses fixed sizes and oversizing for site and
! column dimensions.
!
! The primary advantage is code simplification by avoiding the
! need to determine the total number of unique site ID's in
! advance.  The secondary advantage is the ability to read each
! CSV file in only a single pass.
!
! All string fields are simply site metadata.  This
! enables a strategy of progressively merging such fields into
! a single daily value.  This avoids the need to maintain any
! 24-hour string accumulator arrays.
!
! Memory for single hourly file, full array method:
!
! Max string length capacity = 80      (chosen)
! Number of sites capacity   = 10000   (chosen)
! Number of text fields      = 6       (not relevant)
! Number of numeric fields   = 14      (not relevant)
! Number of fields in table  = 34
!
! Maximum non-optimized string array = 34 * 80 * 10000  =  27.2 Mb
! Maximum non-optimized double array = 34 *  8 * 10000  =   2.7 Mb
! Maximum non-optimized float array  = 34 *  4 * 10000  =   1.4 Mb
!
! Total program memory:
!
! Reader for single hourly file      = 27.2 + 2.7 + 1.4 =  31.3 Mb
! Daily value accumulators           = 27.2 + 1.4       =  28.6 Mb
! 24 hour accumulators, numeric only = 24 * (2.7 +1.4)  =  98.4 Mb
! Final sort and output              = 28.6 + 98.4      = 125.2 Mb
! Program grand total                                   = 282   Mb
!
!-----------------------------------------------------------------------------

program convert_airnow_csv

  use config, only : dp, i64
  use date_utils
  use expand__filename
  use get__airnow_field_table
  use get__command_args
  use index__sort
  use read__airnow_csv
  use stdlit, only : normal
  use write__airnow_netcdf
  implicit none

  character(*), parameter :: &
     program_id = 'convert_airnow_csv.f90 version 2023-feb-6'

!-----------------------------------------------------------
! Fixed program parameters.
!-----------------------------------------------------------

! Main array dimension.  Code for varying input files is simplified
! if this is simply oversized in advance.  Increase as needed.

  integer, parameter :: dim_sites = 10000	! number of sites capacity
  integer, parameter :: nhours    = 24		! hours per day

! Other fixed parameters.

  character(*), parameter :: calendar = 'gregorian'

  real, parameter :: vmiss = -999		! global use missing value

!-----------------------------------------------------------
! Local variables and arrays.
!-----------------------------------------------------------

  character(200) infile, outfile, in_template, out_template
  character(len_field) site_id, old, new
  character(len_table_item) field_name, field_type, varname
  character fdate_str*24, fmt1*40

  integer year, month, day, hour, base_year
  integer datenum, start_date, end_date
  integer j, fi, hi, si, si1, si2, vi, status, diag
  integer nsites, nsites_in, nsites_sorted
  integer nwrite, nskip, ndays_total
  integer ndisplay, ndata
  integer alt_time_count

  integer(i64) nconsistent

  logical change, mismatch

! Dynamic arrays.

  character(:), allocatable :: display(:)

  integer, allocatable :: inds(:)

! Dynamic input arrays.  Dims are (sites, fields).

  character(len_field), allocatable :: fields (:,:)	! string input fields
  real(dp),             allocatable :: doubles(:,:)	! numeric fields
  real,                 allocatable :: floats (:,:)

! Single value accumulators.  Dims are (sites, fields).

  character(len_field), allocatable :: fields2 (:,:)
  real,                 allocatable :: floats2 (:,:)

! 24-hour accumulators.  Dims are (sites, fields, hours).

  real(dp),             allocatable :: doubles3(:,:,:)
  real,                 allocatable :: floats3 (:,:,:)

! Sorted output arrays corresponding to the accumulators.

  character(len_field), allocatable :: fields4 (:,:)	! (sites, fields)
  real,                 allocatable :: floats4 (:,:)

  real(dp),             allocatable :: doubles5(:,:,:)	! (sites, fields, hours)
  real,                 allocatable :: floats5 (:,:,:)

! Output from get_airnow_field_table.

  character(len_table_item), allocatable :: field_names(:), field_types(:)
  character(len_table_item), allocatable :: expected(:), units(:)

  integer i_site_id, i_lat, i_lon, i_date, i_time, nfields

  integer, allocatable :: len_out(:)
  logical, allocatable :: daily(:), hourly(:), consistency(:)
  logical, allocatable :: required(:), merge_type(:)

!-----------------------------------------------------------
! Start program.
!-----------------------------------------------------------

  call fdate (fdate_str)
  print '(2a)', fdate_str, ':  convert_airnow_csv.f90: Start.'
  print '(2a)', 'Program ID = ', trim (program_id)

! Get command line parameters.

  diag = 2			! set default verbosity, unless changed

  call get_command_args (program_id, calendar, in_template, out_template, &
    start_date, end_date, base_year, diag)

! Fetch the decision table.

  call get_airnow_field_table (field_names, field_types, daily, hourly, &
    required, consistency, merge_type, len_out, expected, units, &
    i_site_id, i_lat, i_lon, i_date, i_time, nfields)

! Allocate main data arrays.

  allocate (fields (dim_sites, nfields))		! string input array
  allocate (doubles(dim_sites, nfields))		! numeric input arrays
  allocate (floats (dim_sites, nfields))

  allocate (fields2(dim_sites, nfields))		! single value
  allocate (floats2(dim_sites, nfields))		! accumulators

  allocate (doubles3(dim_sites, nfields, nhours))	! 24-hour accumulators
  allocate (floats3 (dim_sites, nfields, nhours))

  allocate (inds(dim_sites))				! sorting array

  nwrite = 0					! init output file counter

!-----------------------------------------------------------
! Main loop over each date in requested time range.
!-----------------------------------------------------------

date_loop: &
  do datenum = start_date, end_date

    print *, '--------------------------------'

! Initialize for new date.  Clear accumulators and statistics.

    fields2 (:,:)   = ' '			! clear accumulators to missing
    floats2 (:,:)   = vmiss			! to cover for skipped inputs

    doubles3(:,:,:) = vmiss
    floats3 (:,:,:) = vmiss

    nsites          = 0
    nskip           = 0
    nconsistent     = 0
    alt_time_count  = 0

    call index_to_date (datenum, year, month, day, base_year, calendar)
    						! get current date integers

!-----------------------------------------------------------
! Second loop over each hourly file for current date.
!-----------------------------------------------------------

hour_loop: &
    do hi = 1, nhours			! one-based hour index
      hour = hi - 1			! zero-based hour for time alignment

! Read single CSV file for current date and hour.
! Reader guarantees at least one input site if return status = normal.

      call expand_filename (in_template, infile, year, month, day, &
        hour=hour)			! make AirNow file name for current hour

      call read_airnow_csv (infile, year, month, day, hour, vmiss, diag, &
        fields, doubles, floats, nsites_in, status, alt_time_count)

      if (diag >= 3) print '(a,i0)', '    Return status   = ', status
      if (diag >= 3) print '(a,i0)', '    Number of sites = ', nsites

! Skip missing or invalid files.  Reader already printed a diagnostic.

      if (status /= normal) then
        nskip = nskip + 1
        cycle hour_loop
      end if

!-----------------------------------------------------------
! Merge current hourly file data into main accumulators.
!-----------------------------------------------------------

! Third loop over all sites from current input file.

rec_loop: &
      do si1 = 1, nsites_in

! Find current input site in master site list.

        site_id = fields(si1, i_site_id)
        si2     = 0

        do j = 1, nsites
          if (site_id == fields2(j, i_site_id)) then
            si2 = j
            exit
          end if
        end do

! If new site, add to master list.

        if (si2 == 0) then
          if (nsites == dim_sites) then
            print '(3a)',   '*** read_airnow_csv: Number of input sites for', &
                                 ' this date exceeds program capacity.'
            print '(3a)',   '*** File = ', trim (infile)
            print '(a,i0)', '*** Maximum number of sites = ', dim_sites
            print '(3a)',   '*** Increase dim_sites in '&
                                 ' convert_airnow_csv.f90, and recompile.'
            print '(3a)',   '*** Skipping site: ', trim (site_id)
            print *
            cycle rec_loop

          else
            nsites = nsites + 1		! safe to add new site to list
            si2 = nsites
            if (diag >= 3) print '(a,i6,2a)', '  Add new site:', si2, ', ', &
                                   trim (site_id)
          end if
        end if

! Now si2 points to either new or old site.

!-----------------------------------------------------------
! Copy input fields as directed by the decision table.
!-----------------------------------------------------------

! Part 1.  Blindly copy all float and double fields for this site
! into hourly accumulators.  The order of fields is the same in the
! input and accumulator arrays.

        doubles3(si2,:,hi) = doubles(si1,:)
        floats3 (si2,:,hi) = floats (si1,:)

! Part 2.  Copy individual single-value fields into daily accumulators.
! This merges repeat copies of metadata into single daily fields.

! Fourth loop over relevant fields for current site record.

field_loop: &
        do fi = 1, nfields
          field_type = field_types(fi)

! The consistency and merge_type switches are proxies for daily,
! i.e. single value, once per day fields.  Therefore, just use these
! two proxy switches, and don't bother to check the daily switches.
! These two should cover all relevant cases in the current version.

!-----------------------------------------------------------
! Check single value consistency for several field types.
!-----------------------------------------------------------

! Suppress warning if either old or new is a missing value.

! These checks DO NOT check for missing values in required fields.
! If needed, that should be done earlier, in the lower-level reader.

consistency1: &
          if (consistency(fi)) then
            change = .false.

            if (field_type == 'char') then
              if (fields (si1,fi) == fields2(si2,fi)) cycle field_loop
              if (fields (si1,fi) == ' ')             cycle field_loop
              if (fields2(si2,fi) /= ' ') then
                change = .true.
                old = fields2(si2,fi)
                new = fields (si1,fi)
              end if
              fields2(si2,fi) = fields(si1,fi)	   ! insert new or replace value

!!--------- Removed 2023 feb 4, because coordinates are now saved hourly.
!!          else if (field_type == 'double') then
!!            if (doubles (si1,fi) == doubles2(si2,fi)) cycle field_loop
!!            if (doubles (si1,fi) == vmiss)            cycle field_loop
!!            if (doubles2(si2,fi) /= vmiss) then
!!              change = .true.
!!              write (old, '(g0.10)') doubles2(si2,fi)
!!              write (new, '(g0.10)') doubles (si1,fi)
!!            end if
!!            doubles2(si2,fi) = doubles(si1,fi)   ! insert new or replace value

            else if (field_type == 'float') then
              if (floats (si1,fi) == floats2(si2,fi)) cycle field_loop
              if (floats (si1,fi) == vmiss)           cycle field_loop
              if (floats2(si2,fi) /= vmiss) then
                change = .true.
                write (old, '(g0.6)') floats2(si2,fi)
                write (new, '(g0.6)') floats (si1,fi)
              end if
              floats2(si2,fi) = floats(si1,fi)	   ! insert new or replace value
            end if

! Single value changed within same day.  Soft error, just print warning.

            if (change) then
              field_name = field_names(fi)
              print '(3a)', '*** read_airnow_csv: Unexpected change in site', &
                                 ' metadata.'
              print '(3a)', '*** File = ',   trim (infile)
              print '(3a)', '*** Site = ',   trim (site_id)
              print '(9a)', '*** Previous ', trim (field_name), ' = ', trim(old)
              print '(9a)', '*** New      ', trim (field_name), ' = ', trim(new)
              print *
            else
              nconsistent = nconsistent + 1
            end if

          end if consistency1

!-----------------------------------------------------------
! Perform merge consistency for two field types.
!-----------------------------------------------------------

! These merge operations are customized for specific AirNow fields.

merge1:   if (merge_type(fi)) then

! Status field:  Always upgrade blank --> inactive --> active.

            if (field_names(fi) == 'Status') then
              if (fields2(si2,fi) == 'active') cycle field_loop
              if (fields (si1,fi) == ' ')      cycle field_loop
              fields2(si2,fi) = fields(si1,fi)

! X_Measured fields:  Should be boolean.  Always upgrade vmiss --> 0 --> 1.

            else if (field_type == 'float') then
              if (floats (si1,fi) == floats2(si2,fi)) cycle field_loop
              if (floats (si1,fi) == vmiss)           cycle field_loop
              if (floats2(si2,fi) == vmiss) then
                floats2(si2,fi) = floats(si1,fi)	! assign new value
              else
                floats2(si2,fi) = max (floats(si1,fi), floats2(si2,fi))
              end if					! upgrade old value
            end if

          end if merge1

! All other field types and consistency types are no-ops in this field loop.

        end do field_loop
      end do rec_loop

! Print optional summary for hourly input file.

      if (diag >= 3) then
        fmt1 = '(1x,a,i0)'
        print '(3a)', 'Summary for file: ', trim (infile)
        print fmt1, 'Total number of consistent field checks  = ', nconsistent
        print *
      end if

    end do hour_loop

!-----------------------------------------------------------
! Sort data arrays by site ID's.
!-----------------------------------------------------------

    if (nsites == 0) then
      fmt1 = "(a,i4,'/',i2.2,'/',i2.2,'.')"
      print fmt1,   '*** read_airnow_csv: No valid data for ', year, month, day
      print '(3a)', '*** Skipping output file for this date.'
      print *
      cycle date_loop
    end if

    call index_sort (fields2(1:nsites, i_site_id), inds)

! Permute the data arrays into the sort order.  Use auto allocation.

    fields4  = fields2 (inds(1:nsites),:)	! permute daily arrays
    floats4  = floats2 (inds(1:nsites),:)

    doubles5 = doubles3(inds(1:nsites),:,:)	! permute hourly arrays
    floats5  = floats3 (inds(1:nsites),:,:)

    nsites_sorted = size (fields4, 1)
    mismatch = (nsites_sorted /= nsites)

    if ( (diag >= 3) .or. (mismatch) ) then
      print '(a,i0)', '    nsites input    = ', nsites
      print '(a,i0)', '    nsites permuted = ', size (fields4, 1)
    end if

    if (mismatch) then
      print *, '*** Dimension mismatch after sorting.'
      print *, '*** Fatal program error.  Abort.'
      call exit (1)
    end if

!-----------------------------------------------------------
! Write output file for current date.
!-----------------------------------------------------------

    print *

    call expand_filename (out_template, outfile, year, month, day)
					! make output file name for current date

    call write_airnow_netcdf (outfile, year, month, day, fields4, floats4, &
      doubles5, floats5, vmiss, program_id, diag)

    nwrite = nwrite + 1			! count files actually written

! Print summary for current date and output file.

    print *
    print '(3a)',   'Output file = ', trim (outfile)
    print '(a,i0)', 'Number of sites in output file          = ', nsites
    print '(a,i0)', 'Number of rejected HH:15, HH:30 records = ', alt_time_count
    print *

    display  = (/ 'OZONE', 'PM25 ', 'CO   ', 'NO2  ', 'PM10 ', 'SO2  ' /)
    ndisplay = size (display)

    do vi = 1, ndisplay
      varname = display(vi)
      fi = findloc (field_names(:), varname, dim=1)

! Count number of sites with any data for this variable.

      ndata = 0
      do si = 1, nsites
         if (any (floats5(si,fi,:) /= vmiss)) ndata = ndata + 1
      end do

      print '(3a,i0)', 'Number of sites with ', display(vi), &
        ' data         = ', ndata
    end do

  end do date_loop

!-----------------------------------------------------------
! Print run summary.
!-----------------------------------------------------------

  call fdate (fdate_str)

  ndays_total = end_date - start_date + 1

  fmt1 = '(a,i0)'

  print fmt1, '-------------------------------------------------------------'
  print fmt1, 'Run summary:'
  print fmt1, 'Total number of days in run period      = ', ndays_total
!!  print fmt1, 'Days skipped, bad or missing input files = ', ndays_error
  print fmt1, 'Number of new daily files written       = ', nwrite

  print *
  print '(2a)', fdate_str, ':  convert_airnow_csv.f90: Run complete.'

end program convert_airnow_csv
