!------------------------------------------------------------------------------
!
! get_param_mdhz -- Get date/time parameter, MM DD HHz format, from text file.
!
! For use with text configuration files containing embedded
! parameter lines in fixed order.
!
! Dave Allured
!
! Rev	Date		Notes
! 3.01	2017-jun-05	Original version to read month, day, hour parameter.
!			By Dave Allured.
!
! input:	label = text label expected on next line in file
!			(left justified, equal sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!
! output:	date_time = three integers representing month, day, and hour
!		status = normal, fail, or eof status code (see globals.h)
!		line_num = updated to give true line number of current line
!
! assume:	Call message system has been initialized.
!
! messages:	The parameter line is echoed to the user via the
!		"call message" system.
!
!		If the parameter line is missing or unrecognized, or a
!		format error is found, an error message is issued.
!
! notes:	The parameter line specified by "label" must be the next
!		line read from the file.  The general format of the
!		parameter line is:
!
!		    label = value
!
!		Leading and trailing spaces are allowed around these
!		elements.  However, embedded spaces in the label must
!		exactly match those in the "label" calling argument.
!
!		This routine uses "read_line" protocol, which means that
!		all comment lines are ignored.  Comment lines are either
!		null, all spaces, or beginning with an asterisk (*).
!
!		The date/time parameter must be in the format MM DD HHZ
!		on the parameter line.  Leading zeros are not required.
!		However, the Z suffix on the hour IS required.
!
!		Month and day are checked to be within the expected
!		generic ranges for these two numbers.  The hour must
!		be only 0z through 23z.
!
!		Caller must further verify a real calendar date, if
!		needed.
!
!		A completely blank date/time parameter is permitted.
!		In this case, month, day, and hour will be returned
!		as negative integers.
!
!------------------------------------------------------------------------------

subroutine get_param_mdhz (label, date_time, filenum, status, line_num)

   use stdlit
   use string_utils
   implicit none

   character(*), intent (in   ) :: label
   integer,      intent (  out) :: date_time(3)   ! month, day, hour
   integer,      intent (in   ) :: filenum
   integer,      intent (  out) :: status
   integer,      intent (inout) :: line_num

! Local variables.

   character(20) string, token(4)
   character suffix*1

   integer i, j, ios1, ios2
   logical month_bad, day_bad, hour_bad

! First get entire parameter as one string.

   call get_param_string (label, string, filenum, status, line_num)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs were issued in these cases.

! First check for permitted blank parameter.

   if (string == ' ') then
      date_time(:) = -99		! return -99's to signal blank parameter
      status = normal
      return
   end if

! Parse out the input string into three expected substrings.
! Also test for expected number of substrings.

   read (string, *, iostat=ios1) token(1:4)	! expected fail
   read (string, *, iostat=ios2) token(1:3)	! expected pass

   if ( (ios1 == 0) .or. (ios2 /= 0) ) then
      call message ('*** Format error, expected only month, day, hour.')
      status = fail
      return
   end if

! Validate the letter Z suffix on the hour.  Case insensitive.

   j = len_trim (token(3))

   suffix = ' '
   if (j >= 1) suffix = token(3)(j:j)

   if ( (suffix /= 'z') .and. (suffix /= 'Z') ) then
      call message ('*** Date/time error, need Z suffix on hour number.')
      status = fail
      return
   end if

   token(3)(j:j) = ' '			! strip suffix before next step

! Convert substrings to integers.

   do i = 1, 3
      call string_to_intu (trim (token(i)), date_time(i))
      if (date_time(i) < 0) then
         call message ('*** Error, date or time is not numeric.')
         status = fail
         return
      end if
   end do

! Check for numbers out of range.

   month_bad = (date_time(1) < 1) .or. (date_time(1) > 12)
   day_bad   = (date_time(2) < 1) .or. (date_time(2) > 31)
   hour_bad  = (date_time(3) < 0) .or. (date_time(3) > 23)

   if (month_bad .or. day_bad .or. hour_bad) then
      call message ('*** Error: month, day, or hour number is out of range.')
      status = fail
      return
   end if

   status = normal		! normal exit, all tests passed

end subroutine get_param_mdhz
