!---------------------------------------------------------------------------
!
! get_param_int -- Get specified unsigned integer parameter from a text file.
!
! For use with text files containing embedded parameter lines.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-28	Initial version
! 2.00	2001-feb-14	Add file unit number argument
!			Remove references to "control file"
!			Convert to Fortran 90
!			Remove restrictions on spaces before and after label
! 2.01	2002-dec-31	Remove globals.h dependency; use stdlit module instead
! 2.02	2015-jul-07	Library fix.  Switch to string_utils module.
!			Add interface intents.
!
! input:	label = text label expected on next line in file
!			(left justified, equals sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!
! output:	num = integer value of parameter
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
!---------------------------------------------------------------------------

subroutine get_param_int (label, num, filenum, status, line_num)

   use stdlit
   use string_utils
   implicit none

   character(*), intent (in   ) :: label
   integer,      intent (in   ) :: filenum
   integer,      intent (  out) :: num, status
   integer,      intent (inout) :: line_num

   character num_string*30

   call get_param_string (label, num_string, filenum, status, line_num)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs were issued in these cases.

   call string_to_intu (trim (num_string), num)
   if (num < 0) then
      call message ('*** Format error in number.')
      status = fail
   end if

   return			! normal or error exit, come here with status

end subroutine get_param_int
