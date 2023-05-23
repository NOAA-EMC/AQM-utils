!------------------------------------------------------------------------------
!
! get_param_list_double -- Read list of numbers from one line in a text file.
!
! For use with text files containing embedded parameter lines.
!
! This is an internal subroutine for get_param_module.f90.
! Do not call this routine directly.  Use the generic interface
! in the main module.
!
! 2019-aug-04	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!
! Input:	label = text label expected on next line in file
!			(left justified, equals sign not included)
!		filenum = file unit number of file to read
!		line_num = line number of previous line read from this file
!
! Output:	values  = numeric array, auto-allocated to number of items
!		n_items = number of items in list
!		status  = normal, fail, or eof status code (see globals.h)
!		line_num = updated to give true line number of current line
!
! Assume:	Call message system has been initialized.
!
! Messages:	The parameter line is echoed to the user via the
!		"call message" system.
!
!		If the parameter line is missing or unrecognized, or a
!		format error is found, an error message is issued.
!
! Notes:
!
! The parameter line specified by "label" must be the next line
! read from the file.  The general format of the parameter line is:
!
!     label = value
!
! Leading and trailing spaces are allowed around these elements.
! However, embedded spaces in the label must exactly match those
! in the "label" calling argument.
!
! This routine uses "read_line" protocol, which means that all
! comment lines are ignored.  Comment lines are either null,
! all spaces, or beginning with an asterisk (*).
!
! In this version, the output array is auto-allocated to the
! exact number of items found in the CSV parameter list.
!
! This version reads a comma-separated (CSV) list of one or more
! numbers.  Leading and trailing blanks are allowed and will be
! ignored.  However, the list must be fully populated.  Null
! fields are not allowed.
!
! Leading plus signs are also allowed, and quietly ignored.
!
! Otherwise, all list values are checked for valid numeric
! format, using the strict rules in string_to_real,
! string_to_double, etc.  Any violation returns a fatal error.
!
!------------------------------------------------------------------------------

subroutine get_param_list_double (label, values, filenum, status, line_num, &
      n_items)

   use config, only : dp, dim_token
   use stdlit
   use string_utils
   implicit none

   character(*), intent (in   )              :: label
   real(dp),     intent (out  ), allocatable :: values(:)
   integer,      intent (in   )              :: filenum
   integer,      intent (out  )              :: status
   integer,      intent (inout)              :: line_num
   integer,      intent (out  ), optional    :: n_items

! Local variables.

   character decimal*1
   character(dim_token), allocatable :: substrings(:)

   integer i, p1, n_items2
   logical val_status

! Use routine to read one parameter line, get a parsed array of substrings.

   call get_param_list_string (label, substrings, filenum, status, line_num, &
      n_items2)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs issued in these cases.
! Loop over all items in list.

   allocate (values(n_items2))		! allocate the output list

list_loop: &
   do i = 1, n_items2

! Remove leading plus sign, if present.

      if (substrings(i)(1:1) == '+') then
         p1 = 2				! start after plus sign
      else
         p1 = 1				! otherwise parse the whole string
      end if

! Convert string to number.

      decimal = '.'

      call string_to_double_dp (substrings(i)(p1:), decimal, values(i), &
         val_status)
      if (.not. val_status) then
         call message ('*** Format error in number.')
         status = fail
         exit list_loop
      end if

   end do list_loop

! Optional output.

   if (present (n_items)) n_items = n_items2

! All done.  "status" is inherited from first line, or from error.
! Both outputs "values" and "n_items" are now complete in all cases.

end subroutine get_param_list_double
