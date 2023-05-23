!------------------------------------------------------------------------------
!
! read_blocking_list.f90 -- Read site blocking list.
!
! This is a support routine for the NOAA NCO/ARL/ESRL bias correction
! program for CMAQ forecast outputs.
!
! 2021-apr-27	Original version.  By Dave Allured.
!		Adapted from read_exception_list.f90 version 2016-feb-09.
!
! 2023-apr-09	Minor update for 12-character site ID's.
!
! Summary:
!
! * Read site blocking list file as free format text.
! * Associate site ID's with the primary site list.
! * Translate start and end dates to internal date indices.
! * Return site info to caller, who will perform the actual data blocking.
!
! Input file format:
!
! Text file with site ID's and blocking dates, one block command
! per line.  Plain text, ten columns, free format.  Each command
! line blocks a single range of dates in one site's record.
!
! The start of the file may include an optional comment header,
! variable length, ending with one line of dashes.
!
! The rest of the file is block commands, one command per line.
! Each command line is ten columns:  Site ID, latitude, longitude,
! the keyword "block", and the start and end dates to block.
! The dates are three columns each; year, month, and day numbers.
! Command lines are free format, items separated by one or more
! spaces.
!
! Blank lines and comment lines starting with asterisk "*" are
! ignored.
!
! More than one time interval may be blocked for a single site.
! Use a separate block command line for each time interval to be
! blocked.  Time overlaps and duplicate intervals are processed
! independently.  Those will not be flagged as any kind of
! special condition.
!
! The start and end dates are not constrained by this routine to
! any particular valid data range.  This permits the free use of
! external blocking information that may be unrelated to the time
! range of the current bias correction scenario.  The calling
! program should be prepared to limit returned time intervals to
! the current valid data range.
!
! Coordinates in columns 2 and 3 are ignored.  They are included
! only for reference and for consistency with the primary site
! list.  The calling program must associate by site ID's only.
!
! CAUTION:  Coordinates from the blocking list file are only
! printed to the console log.  They are not checked against
! coordinates in primary input data.  If list file coordinates
! are different than those in the primary data files, there will
! be no warning of a mismatch.  This may be potentially
! misleading.
!
! If no site blocking list is desired, the file name should be
! specified as "none".
!
! This routine performs list file input, site ID lookup and
! validation, and date translation and validation.  The actual
! data blocking is performed by the calling program.
!
!------------------------------------------------------------------------------
!
! Output arrays:
!
! This routine returns a blocking table in three arrays:  site
! indices, blocking start date indices, and blocking end date
! indices.  Sites may have multiple entries in the table.
!
! Returned site indices are guaranteed to be in range of the
! input site ID list.
!
! Output date indices are the standard day numbers relative to
! January 1 of the indicated base year.  As mentioned above, the
! returned date indices are not limited to any particular valid
! data range.
!
! Output arrays will normally be auto-allocated.  They will be
! unallocated on return, when no blocking list is available for
! some reason, such as no list file specified, or file is
! completely unreadable.
!
!------------------------------------------------------------------------------
!
! Error handling:
!
! All errors are treated as soft errors and ignored, including
! fortran read errors, and file not found.  This routine will try
! to never halt the main program.  It makes a best effort to read
! and process the entire blocking list file, even in the presence
! of errors part way through the file.
!
! Detected error conditions result in warning messages.
! Sites not found and invalid dates are flagged as "N" -- not
! valid -- in the printed summary table.
!
!------------------------------------------------------------------------------

module read__blocking_list
contains

subroutine read_blocking_list (filename, site_ids, base_year, diag, &
      block_sites, block_starts, block_ends)

   use config, only : dp
   use date_utils
   implicit none

   character(*), intent(in)          :: filename	! list file name
   character(*), intent(in)          :: site_ids(:)	! primary site ID's
   integer,      intent(in)          :: base_year	! base year in scenario
   integer,      intent(in)          :: diag		! verbosity,
   							! 0 = errors only

   integer, intent(out), allocatable :: block_sites(:)	! output site indices
   integer, intent(out), allocatable :: block_starts(:)	! start date indices
   integer, intent(out), allocatable :: block_ends(:)	! end date indices

! Local variables.

   character errmes*200, line*100, command*10
   character site_flag*1, date_flag*1, fmt1*100

   character(len(site_ids)) site_id

   integer infile, ios, lnum, header_line, nlines
   integer ilist, si, nsites
   integer year(2), month(2), day(2), month_size(2)

   real(dp) lat, lon			! dummy site coordinates from list file

   logical found, all_dates_valid, command_valid
   logical valid(4)

   character(*), parameter :: calendar = 'Gregorian'

! Oversized arrays to accumulate blocking list.

   integer, allocatable :: site_ind(:), date1(:), date2(:)

!--------------------------------------------------------------------
! Open blocking list file for reading.
!--------------------------------------------------------------------

   print *
   print *, 'read_blocking_list: Read site blocking list file.'
   print *, '  Input file = ' // trim (filename)

   nsites = size (site_ids)

! Check for no blocking file specified.

   if (filename == 'none') then
      if (diag >= 2) print *, '  *** No blocking list is specified.'
      if (diag >= 2) print *
      return			! return empty list, outputs not allocated
   end if

! Open the site blocking file.

   open (newunit=infile, file=filename, status='old', action='read', &
      iostat=ios, iomsg=errmes)

   if (ios /= 0) then
      print '(a,i0)', '*** Error on file open, I/O status = ', ios
      print '(a)',    '*** ' // trim (errmes)
      print '(a)',    '*** No site blocking list will be used.'
      return			! return empty list, outputs not allocated
   end if

!--------------------------------------------------------------------
! Read file in two passes.  First pass = count command lines.
!--------------------------------------------------------------------

   lnum        = 0
   header_line = 0

count_loop: &
   do
      read (infile, '(a)', iostat=ios, iomsg=errmes) line    ! plain text mode

      if (is_iostat_end (ios)) exit count_loop   ! check for normal end of file

! Read error in text mode is unusual.  Handle same as end of file.

      if (ios /= 0) then
         print '(a,i0)', '*** read_blocking_list: Read error on line ', lnum+1
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)
         print *
         exit count_loop		! end loop on any strange I/O error
      end if

      lnum = lnum + 1				  ! count all lines in file

      if (line(1:4) == '----') then		  ! dashes = last line of header
         if (header_line == 0) header_line = lnum ! remember header line,
      end if					  ! first time only

   end do count_loop

   nlines = lnum

!--------------------------------------------------------------------
! Read file, second pass.  Process command lines.
!--------------------------------------------------------------------

! Print header for blocking list summary in console report.

   if (diag >= 2) print *
   if (diag >= 2) print *, '   Site ID     S/D valid  Latitude  Longitude' &
      // '  Command   Start date    End date'

   fmt1 ='(3x, a, 4x, a, 2x, a, 1x, 2f11.4, 2x, a7, 2(i7, 2i3))'

! Over-allocate the list storage arrays for input.

   allocate (site_ind(nlines), date1(nlines), date2(nlines))

   ilist = 0                    ! pointer into site blocking arrays
   all_dates_valid = .true.

   rewind (infile)		! go back to start of file

! Now read all blocking command lines to end of file.

line_loop: &
   do lnum = 1, nlines

! First read entire line as text, guard against short lines, etc.

      read (infile, '(a)', iostat=ios, iomsg=errmes) line

! Any read error in text mode is unusual.  Handle same as end of file.

      if (ios /= 0) then
         print '(a,i0)', '*** read_blocking_list: Read error on line ', lnum
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)
         print *
         exit line_loop		! like EOF, terminate read loop
      end if

! Ignore header lines, blank lines, and comment lines starting with asterisk.

      if (lnum <= header_line)  cycle line_loop
      if (len_trim (line) == 0) cycle line_loop
      if (line(1:1) == '*')     cycle line_loop

! Now read line buffer in free format.  All data columns must be present.
! Note, "block" command in column 4 is currently required, but not checked.

      read (line, *, iostat=ios, iomsg=errmes) site_id, lat, lon, command, &
         year(1), month(1), day(1), year(2), month(2), day(2)

      if (ios /= 0) then
         print '(a,i0)', '*** read_blocking_list: Read error on line ', lnum
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)

! Attempt to read only the site ID, to improve the error message.

         read (line, *, iostat=ios, iomsg=errmes) site_id
         if (ios == 0) then
            print '(2a)', '*** Site ID = ', trim (site_id)
         end if

         print *
         cycle line_loop	! now skip this command line with format error
      end if

! Input format is valid.  Now look up this site ID in the primary site list.

      found = .false.

      do si = 1, nsites
         found = (site_id == site_ids(si))
         if (found) exit
      end do

!--------------------------------------------------------------------
! Special processing for the start and end dates.
!--------------------------------------------------------------------

! Need to do some tricks here, to provide soft errors rather than
! the hard aborts currently found in the date_index routine.

! In particular, pre-filter dates earlier than base_year, and dates
! not in the Gregorian calendar, such as Feb. 30.  Dates earlier
! than base_year are not handled properly by date_index.  See that
! function's documentation header.  Assume that such dates earlier
! than base_year are not in use in the calling program.

! Independently validate the start and end dates.
! Allow all possible 4-digit year numbers, with exception for the end date.

      month_size(1) = days_in_month (year(1), month(1))
      month_size(2) = days_in_month (year(2), month(2))

      valid(1) = all (year (:) >= 0000) .and. all (year (:) <= 9999)
      valid(2) = all (month(:) >=    1) .and. all (month(:) <=   12)
      valid(3) = all (day  (:) >=    1) .and. all (day  (:) <= month_size(:))

      all_dates_valid = (all_dates_valid) .and. (all (valid(1:3)))

! Quietly flag end dates earlier than the base year.
! Current blocking command will be excluded from the output list.

      valid(4) = (year(2) >= base_year)

! Print site line, for blocking list summary in console report.

      site_flag = merge ('Y', 'N', found)
      date_flag = merge ('Y', 'N', all (valid(:)))

      command_valid = (found .and. all (valid(:)))

      if (diag >= 2) print fmt1, site_id, site_flag, date_flag, lat, lon, &
         command, year(1), month(1), day(1), year(2), month(2), day(2)

! After printing the user's values, do cheap tricks to handle dates
! earlier than base_year, and avoid date_index fatal errors.

      if (year(1) < base_year) then	! if start date earlier than base year:
         year(1)  = base_year		! set effective start date
         month(1) = 1			! to January 1 of start year
         day(1)   = 1
      end if

! For end year earlier then base year, avoid data blocking altogether.
! The current command line will not not relevant to the current data scenario.

      if (year(2) < base_year) cycle line_loop

! Now skip any command line with unknown site ID, or invalid dates.

      if (.not. command_valid) cycle line_loop

! Finally we have a set of dates that can be tolerated by date_index.
! Go translate to internal date indices, and add to output list.
! Add only valid block commands to the output list.

      ilist = ilist + 1				! increment output list pointer
      site_ind(ilist) = si

      date1(ilist) = date_index (year(1), month(1), day(1), base_year, calendar)
      date2(ilist) = date_index (year(2), month(2), day(2), base_year, calendar)
   end do line_loop

   close (infile)				! all done reading input file

!--------------------------------------------------------------------
! End of site blocking file.  Create final outputs.
!--------------------------------------------------------------------

   if (.not. all_dates_valid) &
      print '(a)', '*** Some dates in blocking list were invalid.  Check flags.'

   if (ilist == 0) then
      print '(a)', '*** Site blocking list is empty, no blocking will be done.'
      return			! return empty list, outputs not allocated
   end if

   if (diag >= 2) print *

   print '(a,i0)', '  Number of valid site blocking commands = ', ilist

! Auto-allocate the output arrays to the correct size.

   block_sites  = site_ind(1:ilist)
   block_starts = date1(1:ilist)
   block_ends   = date2(1:ilist)

end subroutine read_blocking_list
end module read__blocking_list
