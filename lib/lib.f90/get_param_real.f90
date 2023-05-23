!---------------------------------------------------------------------------
!
! get_param_real -- Get specified real number parameter from a text file.
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
! 2.01	2001-aug-31	Convert include globals.h to use stdlit
!			More f90 conformance; free format
! 2.02	2011-sep-20	Support optional leading plus sign, for parameter usage.
! 2.03	2015-jul-07	Library fix.  Switch to string_utils module.
!			Add interface intents.
! 3.02	2019-aug-01	Add double precision support.
!			Now use only generic interface in get_param_module.f90.
!
! input:	label = text label expected on next line in file
!			(left justified, equals sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!
! output:	val = real value of numeric parameter
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

subroutine get_param_real_float (label, val, filenum, status, line_num)

   use stdlit
   use string_utils
   implicit none

   character(*), intent (in   ) :: label
   integer,      intent (in   ) :: filenum
   real,         intent (  out) :: val
   integer,      intent (  out) :: status
   integer,      intent (inout) :: line_num

   character num_string*30
   integer p1
   logical val_status

   call get_param_string (label, num_string, filenum, status, line_num)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs issued in these cases.

! Remove leading plus sign, if present.

   p1 = 1				! default to start of whole string

   if (len (num_string) >= 2) then	! protect from short string
      if (num_string(1:1) == '+') then	! check for leading plus sign
         p1 = 2				! move starting point to next char
      end if
   end if

! Convert number string to float.

   call string_to_real (num_string(p1:), val, val_status)
   if (.not. val_status) then
      call message ('*** Format error in number.')
      status = fail
   end if

   return			! normal or error exit, come here with status

end subroutine get_param_real_float

!---------------------------------------------------------------------------

subroutine get_param_real_dbl (label, val, filenum, status, line_num)

   use stdlit
   use string_utils
   implicit none

   character(*),     intent (in   ) :: label
   integer,          intent (in   ) :: filenum
   double precision, intent (  out) :: val
   integer,          intent (  out) :: status
   integer,          intent (inout) :: line_num

   character num_string*30, decimal*1
   integer p1
   logical val_status

   call get_param_string (label, num_string, filenum, status, line_num)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs issued in these cases.

! Remove leading plus sign, if present.

   p1 = 1				! default to start of whole string

   if (len (num_string) >= 2) then	! protect from short string
      if (num_string(1:1) == '+') then	! check for leading plus sign
         p1 = 2				! move starting point to next char
      end if
   end if

! Convert number string to float.

   decimal = '.'

   call string_to_double_dp (num_string(p1:), decimal, val, val_status)
   if (.not. val_status) then
      call message ('*** Format error in number.')
      status = fail
   end if

   return			! normal or error exit, come here with status

end subroutine get_param_real_dbl
