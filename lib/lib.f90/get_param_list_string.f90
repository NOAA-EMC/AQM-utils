!------------------------------------------------------------------------------
!
! get_param_list_string -- Read a list of values from one line in a text file.
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
! Output:	values  = character string array, auto-allocated to no. of items
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
! Substring rules:
!
! In this version, the output array is auto-allocated to the
! exact number of items found in the CSV parameter list.
!
! The selected delimiter in this version is the comma.
!
! Each substring in the output array is left justified, right
! padded with blanks.  Leading are not preserved, except when the
! substring is quoted.  Trailing blanks are not preserved in any
! case, due to fortran's general use of right hand blank padding.
!
! Null strings are allowed and handled properly, interpreted as
! empty substrings.  This includes empty fields at the beginning
! and end of the given list.
!
! For example, a null list is returned as a single string item
! that is all blank.  A single comma is interpreted as two null
! strings.
!
! This version properly handles quoted fields according to the
! rules in parse_delimited.f90.
!
! The string length of the list output array must be sufficient
! for the longest expected strings.  If substrings exceed the
! output string length, they will be truncated without warning.
! No checking is currently done for this condition.
!
!------------------------------------------------------------------------------

subroutine get_param_list_string (label, values, filenum, status, line_num, &
      n_items)

   use config, only : line_dim
   use parse_delimited_mod
   use stdlit
   use string_utils
   implicit none

   character(*), intent (in   )              :: label
   character(*), intent (out  ), allocatable :: values(:)
   integer,      intent (in   )              :: filenum
   integer,      intent (out  )              :: status
   integer,      intent (inout)              :: line_num
   integer,      intent (out  ), optional    :: n_items

! Local variables.

   character(line_dim) list
   character delim*1, valid_quotes*2

   integer i, p1, p2, n_fields, max_items

   integer, allocatable :: first(:), last(:)	! char pointers from parsing

! Read one parameter line, get the list initially as a single long string.

   call get_param_string (label, list, filenum, status, line_num)

   if (status /= normal) return		! abort if eof, unrecognized,
					! or input string too long.
					! error msgs issued in these cases.

! Use standard parser to find all the substrings.
! Note, this parser preserves leading blanks.  Extra trimming is needed.

   max_items = 2 + len_trim (list)	! allocate pointers, conservative,
					!   one more than maximum possible
   allocate (first(max_items), last(max_items))

   delim        = ','
   valid_quotes = '"' // "'"		! allow both single and double quotes

   call parse_delimited (list, delim, valid_quotes, first, last, n_fields)

! Allocate and copy all the substrings into the list output array.

   allocate (values(n_fields))
   values(:) = ' '			! clear to all blanks, to handle
					!   possible null substrings
   do i = 1, n_fields
      p1 = first(i)
      p2 = last (i)
      if (p2 >= p1) values(i) = adjustl (list(p1:p2))	! copy and left justify;
   end do					! truncation may happen here

! Optional output.

   if (present (n_items)) n_items = n_fields

! All done.  "status" is inherited from first line.
! Both outputs "values" and "n_items" are now complete in all cases.

end subroutine get_param_list_string
