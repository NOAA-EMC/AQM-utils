!------------------------------------------------------------------------------
!
! read_table_lines.f90 -- Read unprocessed table lines in config file.
!
! This is a helper routine for config file readers in the
! NOAA/NCEP/PSL bias correction program for CMAQ forecast outputs.
!
! 2014-apr-24	Original version.  By Dave Allured.
! 2016-jan-12	Minor library upgrade.  Use string_utils module.
! 2017-jun-30	Skip over comment lines within table.  Loop is restructured.
!
! 2022-apr-19	Add option to allow empty table.
!		Add caller's name input, for better diagnostics.
!
! This routine reads a single variable length table, as
! unprocessed text lines.  A header line starting with a
! specific string is required in all cases.
!
! Full text lines are read until end of table.  End of table
! is indicated by either a blank line, or end of file.
!
! Full lines are read with A format, to protect against special
! characters such as slash, which would break list directed (*)
! formatting.
!
! Comment lines with leading asterisk "*" are now properly
! skipped, as of version 2017-jun-30.
!
! This routine only returns if the table is read successfully.
! Known problems result in an error message and abort.
!
! This routine might return with end of file status on the
! config file, with no other indication.
!
!------------------------------------------------------------------------------

module read__table_lines
contains

subroutine read_table_lines (cf, header, allow_empty, caller, lines, nlines, &
      line_num)

   use stdlit
   use string_utils
   implicit none

   integer,      intent(in   ) :: cf		! config file unit number
   character(*), intent(in   ) :: header	! expected header line prefix
   character(*), intent(in   ) :: allow_empty	! "allow empty" or "no empty"
   character(*), intent(in   ) :: caller	! caller name for messages

   character(*), intent(  out) :: lines(:)	! text buffer for table lines
   						! pre-allocate more than needed
   integer,      intent(  out) :: nlines	! count of actual table lines
   integer,      intent(inout) :: line_num	! line number in file, updated

! Local variables.

   character(len(lines)) line, header2

   integer hlen, llen, ios, status

! Read table header line.

   call read_line (cf, line, llen, status, line_num)	! use reader, skip blank
   							! lines and comments
! Check for expected start of header line.

   header2 = header
   call lowercase (header2)			! case insensitive comparison
   call lowercase (line)

   hlen = len_trim (header2)			! check start of line only

   if (status /= normal .or. line(1:hlen) /= header2) then
      print *, '*** ' // trim (caller) // ': Missing header line for table.' &
         // '  Abort.'
      print *, '*** Expected prefix "' // trim (header) // '".'
      call exit (1)
   end if

! Read text lines into line buffer.  Table ends on blank line or EOF.

   nlines = 0				! init actual line counter

   do
      line_num = line_num + 1		 ! track line number within whole file
      read (cf, '(a)', iostat=ios) line  ! read full line of text

      if (ios /= 0) exit		! assume EOF, normal end of table
      if (line == ' ') exit		! blank line, normal end of table

      if (line(1:1) == '*') cycle	! skip comment line

      nlines = nlines + 1		! update count of actual table lines

      if (nlines > size (lines)) then
         print *, '*** ' // trim (caller) // ': Number of table lines exceeds' &
            // ' buffer.  Abort.'
         call exit (1)
      end if

      lines(nlines) = line		! copy valid line to output array
   end do

! Check for empty table.

   if ( (nlines == 0) .and. (allow_empty /= 'allow empty') ) then
      print *, '*** ' // trim (caller) // ': Empty table.  Abort.'
      call exit (1)
   end if

! All good, table size is within expected range.  Return to caller.

end subroutine read_table_lines
end module read__table_lines
