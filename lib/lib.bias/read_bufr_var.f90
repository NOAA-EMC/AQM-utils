!-----------------------------------------------------------------------------
!
! read_bufr_var.f90 -- Read station time series from an EPA AIRNow BUFR file.
!
! This is a somewhat generic routine to read one variable and all
! sites from one AIRNow BUFR file per call.  This version assumes
! an hourly time series for one day.
!
! 2014-jun-05	Original version.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Adapted from decode_pm_bufrf.f, by Jerry Gorline, NOAA/NWS.
!		Convert free standing utility program to reader subroutine.
! 2014-jun-10	Adjust for clean progress display at diag level 2, etc.
!		Add data line dump at diag level 7 and higher.
! 2014-jul-15	Minor fixes in diagnostic prints.
!
! 2016-feb-22	Add diagnostic for missing hours in BUFR file.
!
! 2019-apr-25	Remove check for expected message count, clean up log file.
!		Test was unwarranted.  Number of messages is quite variable.
!
! 2020-oct-30	Restrict BUFR module access to only actual symbols in use.
!
! 2023-apr-04	Fix site ID strings to exactly nine digits, which is the
!		  EPA/Airnow standard for BUFR files.
!
! Notes:
!
! Primary inputs are a BUFR file name, and the requested BUFR
! variable name.  E.g. COPO for ozone, COPOPM for PM2.5.
!
! Primary outputs are site arrays of site ID's, coordinates,
! and 24-hour time series.
!
! All output arrays are auto-allocated for exactly the number of
! unique site ID's found in the current input file.
!
! EPA BUFR daily files are structured as messages and subsets.
! Each message contains data values for one hour of the day.
! Within one message, each subset contains one hourly data value
! for one site.
!
! Sites and hours may be ordered randomly within a BUFR file.
! This reader assembles the data into an appropriate array
! structure, with hours 0-23 in normal ascending order.
!
! The site ID's stored in the current version of AirNow BUFR
! files are standard 9-digit EPA site ID's, not 8-digit MOS2000
! site ID's.  The new 12-digit EPA site ID's are not supported
! in the current BUFR format.
!
! Site ID's are returned as decimal coded character strings,
! left justified.  Leading zeros are added to pad all site ID's
! to exactly nine digits.
!
! This version does NOT perform any units conversion on input
! data values.
!
! The returned missing value code is the actual missing value
! code from the BUFR library.
!
! This routine will not normally return with any local fortran
! file left open, even in the event of a soft error return with
! status = fail.  To minimize the chance of a conflict, a file
! unit number is opened temporarily within this routine, then
! always closed before returning.
!
! Error handling:
!
! For most graceful handling of missing input files, the caller
! should check for file missing before calling this routine.
! This current version will throw a fatal error and halt on file
! missing.
!
! The current version performs several levels of error handling.
! Soft errors on a per-number basis produce optional warning
! messages, and return missing values and no error status to the
! caller, i.e. status = normal.
!
! Any invalid file detection prints a warning message and
! status = fail to the caller.  This should be treated as a
! soft error, and the returned data array should be ignored.
!
! The single detected fatal condition (dimension exceeded) halts
! the entire program, so a return code is not relevant.
!
! Caveats:
!
! Data ordering within the BUFR files is not systematic.  Hours
! and sites may be stored in random order.
!
! The number of subsets within each message may change within the
! BUFR file.
!
! Available sites and site ordering may change between BUFR files.
!
!-----------------------------------------------------------------------------

module read__bufr_var
contains

subroutine read_bufr_var (infile, varname, typo_expect, tphr_expect, &
      max_sites, diag, year, month, day, nsites, site_ids, lats, lons, &
      idata, vmiss, status)

   use bufrlib, only : getbmiss, openbf, readmg, readsb, ufbint
   use config,  only : dp
   use stdlit,  only : normal, fail
   implicit none

! Input arguments.

   character(*), intent(in) :: infile			 ! BUFR input file name
   character(*), intent(in) :: varname			 ! requested BUFR var
   integer,      intent(in) :: typo_expect		 ! expected TYPO value
   integer,      intent(in) :: tphr_expect		 ! expected TPHR value
   integer,      intent(in) :: max_sites		 ! max number of sites
   							 !  for temporary arrays
   integer,      intent(in) :: diag			 ! verbosity level, 0-N

! Output arguments.

   integer,      intent(out)              :: year	 ! embedded date from
   integer,      intent(out)              :: month, day  !   current file
   integer,      intent(out)              :: nsites	 ! no. of sites returned
   character(*), intent(out), allocatable :: site_ids(:) ! site ID strings (S)
   real(dp),     intent(out), allocatable :: lats(:)	 ! site latitudes (S)
   real(dp),     intent(out), allocatable :: lons(:)	 ! site longitudes (S)
   real(dp),     intent(out), allocatable :: idata(:,:)	 ! data read array (H,S)
   real(dp),     intent(out)              :: vmiss	 ! missing value code
   integer,      intent(out)              :: status	 ! result status, normal
   							 !   or fail (stdlit)
! Local program parameters.

   character(*), parameter :: fmt_id = '(i9.9)'   ! standard 9-digit site ID's
   integer,      parameter :: len_id = 9	  ! BUFR site ID string length

   integer, parameter :: ibfin = 94	! BUFR file input unit no., temporary
					! per WCOSS coding guidelines

   integer, parameter :: nhours = 24	! hour dimension size

   integer, parameter :: nb1 = 20	! bufrin first dim, exceed all lists
   integer, parameter :: nb2 = 1	! bufrin second dimension, always 1

! Local variables.

   character(len_id) id_str
   character subset*8, fmt1*50
   character qc_str*12, val_str*16

   character date_codes*20, item_codes*40	! code lists for ufbint
   character(12) required_fields(10)		! must set dims for required
						! fields, in code below
   integer j, si, hour, ihour, ignore
   integer message_num, subset_num
   integer idate, iret, ncodes
   integer nsubsets, nsubsets_total
   integer nmessages, ov_count, nmissing
   integer ysave, msave, dsave

   integer hour_count(nhours)			! count only non-missing
   						! individual obs for each hour

   logical very_first_subset, found
   logical missing_fields

! Much of the metadata are stored as 8-byte doubles.
! Some need to ultimately be converted to char or int.

   real(dp) bufrin(nb1,nb2)		! raw, undecoded BUFR input data
					! nb1 no shorter than longest code list

   real(dp) yd, md, dd, hour_dbl	! raw date and time values
   real(dp) site_id			! site ID, type double before decoding
   real(dp) val				! raw data value from BUFR
   real(dp) qc_ind			! QC indicator, process as type double

   real(dp) typo_in			! TYPO file value (type of pollutant)
   real(dp) tphr_in			! TPHR file (time period or
   					!   displacement, averaging interval)

! Local over-dimensioned arrays for site data.  "t" means "temporary" array.

   real(dp) tids(max_sites)		! site ID's as raw type double
   real(dp) tlats(max_sites), tlons(max_sites)
   real(dp) tdata(nhours,max_sites)	! data values (hours, sites)

! Initialize.

   nsites = 0				! init counter, number of unique sites
   message_num = 0
   nsubsets_total = 0
   very_first_subset = .true.
   hour_count(:) = 0

   si = 0				! init "previously used" site index
   					! needed to suppress possible compiler
                                        ! warning, not needed for lookup algo

! Open BUFR input file for read access.  Open with fortran, any mode.
! Bufrlib will use its own direct access on the open unit number.

! Assume that calling program has pre-screened for file existence,
! if soft error handling is needed.

   if (diag >= 4) print '(2a)', 'read_bufr_var: Read ', trim (infile)

   open (unit=ibfin, file=infile, status='old', action='read')

! Open file access in BUFR library.

   call datelen (10)			! tell BUFRLIB: 4-digit years in dates

   if (diag >= 4) print *, ' Call openbf.'
   call openbf (ibfin, 'IN', ibfin)	! associate BUFR file with library
   					! and specify embedded DX tables

! Clear data input array to all missing.  Don't count on the input file
! having perfect coverage over all sites and all hours.

   vmiss = getbmiss ()			! get BUFR standard missing value
   tdata(:,:) = vmiss

!-----------------------------------------------------------
! Main loop over all BUFR messages in the input file.
!-----------------------------------------------------------

message_loop: &
   do
      message_num = message_num + 1		! count message number

      if (diag >= 6) print *
      if (diag >= 4) print '(a,i0)', '  read_bufr_var: Read message #', &
         message_num

      call readmg (ibfin, subset, idate, iret)	! read next BUFR message

! Ignore idate, avoid Y2147 bug.  Get Y M D H from ufbint calls below.

      if (diag >= 6) print *, '   iret = ', iret

      if (iret /= 0) exit message_loop		! end of file, exit main loop

      if ( (diag >= 5 .and. message_num == 1) .or. diag >= 6 ) &
         print '(2a)', '    Table A mnemonic for this message = ', trim (subset)

! 2014-jun-5:  We could also check for valid message type returned
! in "subset".  But there is some indication that we may need to
! handle more than one message type per variable, in the future.

!-----------------------------------------------------------
! Loop over all subsets within the current message.
!-----------------------------------------------------------

      subset_num = 0				! re-init the subset counter

subset_loop: &
      do
         subset_num = subset_num + 1		! count subset number

         if (diag >= 8) print *
         if (diag >= 6) print '(2(a,i0))', '    Read subset #', &
            message_num, '-', subset_num

         call readsb (ibfin, iret)

         if (diag >= 9) print *, '       iret = ', iret

         if (iret .ne. 0) exit subset_loop    ! end of message, advance to next

! Got a valid subset.  Now ready to fetch data and metadata fields.

         nsubsets_total = nsubsets_total + 1	! count all subsets in file

!-----------------------------------------------------------
! First time only, check file metadata.
!-----------------------------------------------------------

first_time_only: &
         if (very_first_subset) then		! first time only, per file
            very_first_subset = .false.

! Test for selected data variable.

            if (diag >= 4) print *, '   Call ufbint,' &
               // ' check for requested variable: ' // trim (varname)

! Special usage of UFBINT.  When requesting only a single field
! with UFBINT (ncodes = 1), iret indicates 1 for field present,
! 0 for field not found.  Technically this is "count of number of
! returned levels", but the one/zero interpretation is correct for
! this special usage.

            ncodes = 1			! 1 = querying one field at a time
            call ufbint (ibfin, bufrin, ncodes, nb2, iret, varname)

            if (diag >= 6) print *, '       iret = ', iret

            if (iret /= 1) then		! only valid result should be iret = 1
               print '(3a)', ' *** Requested data variable "', &
                  trim (varname), '" is not found in this file.'
               print '(3a)', ' *** Input file = ', trim (infile)
               print *, '*** UFBINT return code IRET = ', iret

               call closbf (ibfin)	! close file properly before aborting;
               status = fail		! also closes fortran unit number
               return			! return soft error status to caller
            end if

! Check for other required data fields.  One time only, per file.
! Include the caller's requested BUFR var name, such as COPO, COPOPM.
! Fields must be checked one at a time, with UFBINT.
! See field table, below. Caller's var name should also be in this table.

            required_fields = (/ 'YEAR ', 'MNTH ', 'DAYS ', 'HOUR ', 'PTID ', &
               'CLATH', 'CLONH', 'TYPO ', 'TPHR ', 'QCIND' /)

            missing_fields = .false.		! clear error flag

            do j = 1, size (required_fields)

               if (diag >= 5) print *, '     Call ufbint,' &
                  // ' check required field: ' // trim (required_fields(j))

! Test for fields present.  See above note about special usage
! of UFBINT when ncodes = 1.

               ncodes = 1		! 1 = querying one field at a time
               call ufbint (ibfin, bufrin, ncodes, nb2, iret, &
                  required_fields(j))

               if (diag >= 6) print *, '       iret = ', iret

               if (iret /= 1) then	! only valid result should be iret = 1
                  print *, '*** Missing required field, code = ' &
                     // trim (required_fields(j))
                  print *, '*** UFBINT return code IRET = ', iret
                  missing_fields = .true.	! set flag for deferred
                  cycle				! error handling
               end if

               if (required_fields(j) == 'TYPO') typo_in = bufrin(1,1)
               if (required_fields(j) == 'TPHR') tphr_in = bufrin(1,1)
						! save needed field values
            end do

! Deferred handling for missing fields.

            if (missing_fields) then		! check error flag
               print *
               print *, '*** read_bufr_var: Warning: Missing required fields.'
               print '(2a)', ' *** File = ' // trim (infile)
               print *, '*** Invalid input file, skipping.'

               call closbf (ibfin)	! close file properly before aborting;
               status = fail		! also closes fortran unit number
               return			! return soft error status to caller
            end if

! Metadata consistency checks.

            if (diag >= 4) then
               print '(a,g0.6)', '    TYPO = ', typo_in
               print '(a,g0.6)', '    TPHR = ', tphr_in
            end if

            if (typo_in /= typo_expect) then
               print *
               print '(2a)', ' *** read_bufr_var: Warning: Bad value for' &
                  // ' TYPO (type of pollutant).'
               print '(a,i0)',   ' *** Expected TYPO value = ', typo_expect
               print '(a,g0.6)', ' *** TYPO value in file  = ', typo_in
               print '(2a)', ' *** File = ' // trim (infile)
               print *, '*** Invalid input file, skipping.'

               call closbf (ibfin)	! close file properly before aborting;
               status = fail		! also closes fortran unit number
               return			! return soft error status to caller
            end if

            if (tphr_in /= tphr_expect) then
               print *
               print '(2a)', ' *** read_bufr_var: Warning: Bad value for', &
                  ' TPHR (time period or displacement, averaging interval).'
               print '(a,i0)',   ' *** Expected TPHR value = ', tphr_expect
               print '(a,g0.6)', ' *** TPHR value in file  = ', tphr_in
               print '(2a)', ' *** File = ' // trim (infile)
               print *, '*** Invalid input file, skipping.'

               call closbf (ibfin)	! close file properly before aborting;
               status = fail		! also closes fortran unit number
               return			! return soft error status to caller
            end if

         end if first_time_only

!-----------------------------------------------------------
! Table of available fields in EPA AIRNow message subsets.
!-----------------------------------------------------------

! Not all available fields are fetched or needed within this routine.

! References:  1. Jerry Gorline's original decode_pm_bufrf.f program, April 3.
! 2.  Debufr output from AIRNow files.  3. BUFRLIB documentation.

! MNEMONIC : DESCRIPTION
! ------ : ------------------------------------------------------------------
! YEAR   : YEAR
! MNTH   : MONTH
! DAYS   : DAY
! HOUR   : HOUR
! PTID   : PLATFORM TRANSMITTER ID
! CLATH  : LATITUDE (TO 5 DECIMAL PLACES)
! CLONH  : LONGITUDE (TO 5 DECIMAL PLACES)
! TYPO   : TYPE OF POLLUTANT : 0 MEANS OZONE
! TSIG   : TIME SIGNIFICANCE: 2 MEANS TIME AVERAGED
! TPHR   : AVERAGING INTERVAL (HOURS)
!	     (-1 = 1 HOUR, -8 = 8 HOURS)
! QCIND  : QUALITY CONTROL INDICATOR
!	     0 MEANS 'GOOD' ('G')
!	     2 MEANS 'DOUBTFUL' (INTERPRETATION OF 'K' : 'SUSPECT')
!	     3 MEANS 'WRONG' (INTERPRETATION OF 'B' : 'BAD')
!	     6 IS CURRENTLY 'RESERVED' (INTERPRETATION OF 'E' : 'ESTIMATED')
!	     6 IS CURRENTLY 'RESERVED' (INTERPRETATION OF 'I' : 'INTERPOLATED')
!	     10**11 MEANS 'MISSING' ('M')
! COPOPM : CONCENTRATION OF POLLUTANT (Kg PER M^3)
!	     MULTIPLY THIS QUANTITY BY 10**9 TO GET micrograms/M^3

! Fields are returned as double precision in the bufrin array.  Decode as
! needed.  Array positions correspond to code positions in given code list.

!-----------------------------------------------------------
! Start of each message, extract the embedded date.
!-----------------------------------------------------------

! Assume the date is global for the current BUFR file, unchanging
! between messages and subsets.  Should be valid assumption for AIRNow,
! not necessarily for other data sets.  Trust, but verify.

start_of_message: &
         if (subset_num == 1) then

            date_codes = 'YEAR MNTH DAYS'     ! see field table, above
            ncodes = 3			      ! number of codes in current list

            if (diag >= 5) print *, '     Call ufbint, fetch date fields: ' &
               // trim (date_codes)

            call ufbint (ibfin, bufrin, ncodes, nb2, iret, date_codes)

            if (diag >= 6) print *, '       iret = ', iret

            yd = bufrin(1,1)		! fetch numbers first as type double
            md = bufrin(2,1)
            dd = bufrin(3,1)

! Check for valid date numbers.  This also prevents overflow and program crash.

            if (yd < 1 .or. yd > 2999 .or. md < 1 .or. md > 12 &
             .or. dd < 1 .or. dd > 31) then
               print *
               print *, '*** read_bufr_var: Invalid embedded date in this file.'
               print '(a,3(2x,g0.6))',   ' *** Y, M, D = ', yd, md, dd
               print *, '*** File = ' // trim (infile)
               print '(2(a,i0))', ' *** Message #', message_num, &
                  ', subset #', subset_num
               print *, '*** UFBINT return code IRET = ', iret
               print *, '*** Invalid input file, skipping.'

               call closbf (ibfin)	! close file properly before aborting;
               status = fail		! also closes fortran unit number
               return			! return soft error status to caller
            end if

            year  = int (yd)		! now safe to convert to integers
            month = int (md)
            day   = int (dd)

            if ( (diag >= 4 .and. message_num == 1) .or. diag >= 5 ) &
               print '(a,i0.4,2(1x,i0.2))', '    Embedded date = ', &
                  year, month, day

! Check for date changing between messages.

            if (message_num == 1) then   ! first message: save the date
               ysave = year		 ! save as raw double, better diag
               msave = month
               dsave = day

            else			 ! rest of messages: check date
               if (year /= ysave .or. month /= msave .or. day /= dsave) then
                  print *
                  print *, '*** read_bufr_var: Date changes within the same' &
                     // ' input file.'
                  fmt1 = '(a,i0.2,a,i0.4,2(1x,i0.2))'
                  print fmt1, ' *** Message ', 01,          ': Y, M, D = ', &
                     ysave, msave, dsave
                  print fmt1, ' *** Message ', message_num, ': Y, M, D = ', &
                     year, month, day
                  print *, '*** File = ' // trim (infile)
                  print *, '*** Invalid input file, skipping.'

                  call closbf (ibfin)	! close file properly before aborting;
                  status = fail		! also closes fortran unit number
                  return		! return soft error status to caller
               end if
            end if

         end if start_of_message

!-----------------------------------------------------------
! Extract actual data for the current subset.
!-----------------------------------------------------------

         item_codes = 'HOUR PTID CLATH CLONH QCIND ' // varname
         				! see field table above
         ncodes = 6			! number of codes in current list

         if (diag >= 8) print *, '     Call ufbint, fetch ' &
            // trim (item_codes)

         call ufbint (ibfin, bufrin, ncodes, nb2, iret, item_codes)

         if (diag >= 9) print *, '       iret = ', iret

! Check hour number.  Avoid both subscript error and floating point error.

         hour_dbl = bufrin(1,1)

         if (hour_dbl < 0 .or. hour_dbl > 23) then
            print *
            print *, '   *** read_bufr_var: Hour number out of range 0 to 23.'
            print '(a,g0.6)',   '    *** Hour number in file = ', hour_dbl
            print '(a,f0.0)', '    *** Current site ID     = ', site_id
            print *, '   *** Data value is ignored.'

            cycle subset_loop		! ignore data, treat as soft error
         end if

! Fields are returned as double precision in the bufrin array.  Decode as
! needed.  Array positions correspond to code positions in given code list.

         if (diag >= 8) print *, '     Decode items fetched as type double.'

         hour    = int (hour_dbl)	! hour of day, integer
         site_id = bufrin(2,1)		! site ID, temporarily as type double
         qc_ind  = bufrin(5,1)		! QC indicator, keep as double
         val     = bufrin(6,1)		! data value, double

         ihour   = hour + 1		! get 1-based hour subscript for arrays

! Print data values at high diagnostic levels.

         if (diag >= 7) then
            write (id_str, fmt_id) int (site_id)    ! real to string site ID
            write (val_str, '(f16.10)', iostat=ignore) val
            write (qc_str,  '(f12.1)',  iostat=ignore ) qc_ind

            if (val_str(1:1) == '*') write (val_str, '(es16.3)') val
            if (qc_str(1:1)  == '*') write (qc_str,  '(es12.3)') qc_ind

            fmt1 = '(6x,i0.4,2(1x,i0.2),3x,i0.2,3x,a,2(1x,a))'
            print fmt1, year, month, day, hour, id_str, val_str, qc_str
         end if

!-----------------------------------------------------------
! Find the site ID in the current site list.
!-----------------------------------------------------------

! Optimized method for consistent order of sites between messages.
! This ordering was confirmed 2014-may-25, with AIRNow BUFR data set.
! Start searching with the site following the previous found site.

! On entry each time, si = index of previous site, initially.
! Works correctly for startup (nsites = 0), and both known and
! unknown sites.

         found = .false.

         do j = 1, nsites
            si = si + 1				! advance to next site in list
            if (si > nsites) si = 1		! wrap around end of list

            found = (tids(si) == site_id)
            if (found) exit			! site ID found
         end do

!-----------------------------------------------------------
! Insert current subset data into site arrays.
!-----------------------------------------------------------

! Site not found.  Check for site array overflow.

         if (.not. found) then

            if (nsites >= max_sites) then
               print *, '*** read_bufr_var: Exceeded maximum number of sites.'
               print '(a,i0)',   ' *** Current array size = ', max_sites
               print '(a,f0.0)', ' *** Current site ID    = ', site_id
               print *, '*** Update program, increase max_sites number.'

! Array overflow, fatal error.
! Comment out the next two lines, if soft error handling is preferred.

               print *, '*** Fatal error.  Abort.'
               call exit (1)

               cycle subset_loop		! if enabled: ignore data from
            end if				! this site, treat as soft error

! Add new site to the current list.

            nsites    = nsites + 1		! increase list size
            si        = nsites			! array index for new site

            tids(si)  = site_id			! save encoded site ID (double)
            tlats(si) = bufrin(3,1)		! save lat, lon coordinates
            tlons(si) = bufrin(4,1)
         end if

! Now si = current site index number.  Add data value to array.

         if (qc_ind /= 0) then		! if missing or other QC flag...
            tdata(ihour,si) = vmiss	! set to BUFR missing value
         else
            tdata(ihour,si) = val	! no problems, insert original datum
            hour_count(ihour) = hour_count(ihour) + 1	! count only non-missing
         end if					! individual obs for each hour

      end do subset_loop

!-----------------------------------------------------------
! End of message detected.
!-----------------------------------------------------------

      nsubsets = subset_num - 1

      if (diag >= 6) print '(a,i0,a,i0)', '    End of message #', message_num, &
         ' detected.'
      if (diag >= 5) print '(a,i0,a,i0)', '    Message #', message_num, &
         ', number of subsets in message = ', nsubsets

! Warning for for empty message.

      if (nsubsets < 1) then
         print '(a,2(i0,a))', '*** read_bufr_var: Warning: Subset count = ', &
            nsubsets, '; message #', message_num, ' is empty.'
      end if

   end do message_loop

!-----------------------------------------------------------
! Finished reading all messages.  End of file checks.
!-----------------------------------------------------------

   if (diag >= 4) print *, ' End of file detected.'

! Print summary.

   nmessages = message_num - 1

   if (diag >= 3) then
      fmt1 = '(4x,a,i6)'
      print fmt1, "Number of unique site ID's in this file = ", nsites
      print fmt1, 'Number of messages         in this file = ', nmessages
      print fmt1, 'Total number of subsets    in this file = ', nsubsets_total
   end if

! Check for missing hours.

   nmissing = count (hour_count(:) == 0)

   if (nmissing > 0) then
      print *, '*** read_bufr_var: Warning: Missing hours in this file.'
      print '(2a)',          ' *** File = ' // trim (infile)
      print '(a,24(1x,i0))', ' *** List of missing hours =', &
         pack ( (/ (hour, hour = 1, nhours) /), (hour_count(:) == 0) )
      print '(a,i0)',        ' *** Number of missing hours = ', nmissing
      print *, '*** Soft error, will assume available data is correct.'
      print *
   end if

! Close BUFR file properly.

   if (diag >= 4) print *, ' Read complete.  Call closbf, close input file.'

   call closbf (ibfin)			! also closes fortran unit number

!-----------------------------------------------------------
! Return all site data to caller.
!-----------------------------------------------------------

! Allocate output arrays for actual number of sites in file.

   if (diag >= 5) print *, ' read_bufr_var: Allocate output arrays.'

   allocate (site_ids(nsites), lats(nsites), lons(nsites))
   allocate (idata(nhours,nsites))

! Copy all site data.

   if (diag >= 4) print *, ' Copy all site data to output arrays.'

   lats(:) = tlats(1:nsites)		! copy coordinates
   lons(:) = tlons(1:nsites)

   idata(:,:) = tdata(:,1:nsites)	! copy data values (hours, sites)

! Site ID's are encoded as double precision numbers.
! Convert to fixed length strings using fixed width format spec.
! Include leading zeros to pad to the EPA/AirNow standard site ID
! length of 9 digits.  Format In.n will create asterisks on overflow,
! and catch invalid ID's.

! If the number of non-zero digits exceeds the available string size,
! then all asterisks will be returned.  This is not currently
! detected as an error condition.

   if (diag >= 5) print *, ' Convert site ID''s from doubles to strings.'
   if (diag >= 6) print *, '   Format string = ' // trim (fmt_id)

   do si = 1, nsites				     ! convert real to string;
      write (site_ids(si), fmt_id) int (tids(si))    ! preserve leading zeros;
   end do					     ! left justify

! Check for string overflow.  Currently, only a warning is printed.

   if (diag >= 1) then
      ov_count = count (site_ids(:)(1:1) == '*')     ! test only the first char

      if (ov_count == 0) then
         if (diag >= 4) print *, '   String overflow count = ', ov_count
      else
         print *, '*** read_bufr_var: Warning: String overflow detected in' &
            // ' site ID''s.'
         print *, '*** Number of affected site ID''s = ', ov_count
         print *, '*** May be serious problem.  Check program and BUFR files.'
      end if
   end if

! All done.  Return success to caller.

   status = normal

   if (diag >= 4) print *, ' read_bufr_var: Normal return.'

end subroutine read_bufr_var
end module read__bufr_var
