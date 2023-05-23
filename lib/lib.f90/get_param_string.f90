!------------------------------------------------------------------------------
!
! get_param_string -- Get specified parameter string from a text file.
!
! For use with text files containing embedded parameter lines.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-28	Initial version
! 1.01	2000-mar-28	Fix bug causing 1-char parameter value to fail
! 1.02	2000-sep-29	Fix syntax bug "found = false" (benign)
! 1.03	2000-dec-29	Fortran 90 compatibility
! 1.04	2001-jan-18	New calling convention for read_line
!
! 2.00	2001-feb-14	Add file unit number argument
!			Remove references to "control file"
!			Convert to Fortran 90
!			Remove restrictions on spaces before and after label
! 2.01	2001-aug-28	Remove globals.h dependency; use stdlit.h instead
! 2.02	2001-aug-31	Change from stdlit.h to stdlit module; f90 free format
!
! 3.00	2016-feb-09	Upgrade to module interface.
!			This file must now be included in a container module.
!			Add optional argument "nonblank" for required string.
!
! input:	label = text label expected on next line in file
!			(left justified, equal sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!		nonblank = optional, present & True to require non-blank string.
!
! output:	value = string value of parameter, left justified
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
!		"Value" may be a simple or complex string, or null.
!		Embedded spaces are preserved when the string is
!		returned to the caller.
!
!		This routine uses "read_line" protocol, which means that
!		all comment lines are ignored.  Comment lines are either
!		null, all spaces, or beginning with an asterisk (*).
!
!------------------------------------------------------------------------------

subroutine get_param_string (label, value, filenum, status, line_num, nonblank)

   use stdlit
   use config, only : line_dim
   implicit none

   character(*),      intent (in   ) :: label
   character(*),      intent (  out) :: value
   integer,           intent (in   ) :: filenum
   integer,           intent (  out) :: status
   integer,           intent (inout) :: line_num
   logical, optional, intent (in   ) :: nonblank

   character(line_dim) line_in, line, value1
   integer linelen, equal

! Read, echo, and parse parameter line.

   call read_line (filenum, line_in, linelen, status, line_num)

   if (status == normal) then		! if not eof...

      call message (line_in)		! always echo parameter line to display

      line = adjustl (line_in)		  ! discard leading spaces
      equal = index (line, '=', forward)  ! find equal sign after label

      if (equal == 0 .or. line(1:equal-1) /= label) then
         status = fail
      end if

   end if

! Check for eof or unrecognized label.

   if (status /= normal) then		! error: eof or unrecognized label
      call message ('*** Missing or misspelled parameter "' &
         // trim (label) // '"')
      return				! error exit
   end if

! Get value string from line.

   value1 = adjustl (line(equal+1:))	! left-justify value string
   value = value1			! copy to output string

   if (len_trim (value1) > len (value)) then	! overflow output string?
      call message ('*** Parameter string too long')
      status = fail
      return
   end if

! Optional check for non-blank string.

   if (present (nonblank)) then
      if (nonblank .eqv. .true.) then
         if (value == ' ') then
            call message ('*** Non-blank parameter value is required.')
            status = fail
            return
          end if
       end if
   end if

! Normal exit here.

end subroutine get_param_string
