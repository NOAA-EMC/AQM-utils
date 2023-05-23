!---------------------------------------------------------------------------
!
! get_param_yesno -- Get specified logical parameter from a text file.
!
! For use with text files containing embedded parameter lines.
!
! Dave Allured
!
! Rev	Date		Notes
! 2.00	2001-feb-27	Initial version, adapted from get_param_int.
! 2.01	2004-jan-08	More F90 conversion.
!			Replace .h file with standard module.
!
! input:	label = text label expected on next line in file
!			(left justified, equal sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!
! output:	flag = true if value is "yes", false if vause is "no"
!		status = normal, fail, or eof status code (see globals.h)
!		line_num = updated to give true line number of current line
!
! assume:	Call message system has been initialized.
!
! messages:	The parameter line is echoed to the user via the
!		"call message" system.
!
!		If the parameter line is missing or unrecognized, or the
!		value is not "yesy" or "no", an error message is issued.
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

subroutine get_param_yesno (label, flag, filenum, status, line_num)
	
	use stdlit
	
	character label*(*)
	integer filenum, status, line_num
	logical flag
	
	character string*3
		
	call get_param_string (label, string, filenum, status, line_num)

	if (status /= normal) goto 999	! abort if eof, unrecognized,
					! or input string too long.
					! error msgs issued in these cases.

	if (string.eq.'yes') then
	   flag = .true.
	else if (string.eq.'no') then
	   flag = .false.
	else
	   call message ('*** Invalid parameter, must be yes or no.')
	   status = fail
	end if

999	return			! normal or error exit, come here with status
	
end subroutine get_param_yesno
