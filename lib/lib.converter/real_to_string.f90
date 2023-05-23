!---------------------------------------------------------------------------
!
! real_to_string -- Convert real number to character string, with options.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2003-oct-21	Original version.
! 1.01	2005-sep-28	Fix nonstandard "<>" operators.
!
! 2.00	2014-sep-09	Module version.  Add double precision interface.
!
! Notes:
!
! This routine extends the Fortran "Fm.n" formatting function.
! In effect, the field width "m" is the length of the output
! string, and the number of trailing decimal places "n" is
! specified by the argument "ndigits".
!
! The input number may be positive or negative.  The output
! is to a character string for the caller's further
! processing.  Fortran round-off rules for formatted writes
! are applied when truncating digits to the right of the
! decimal point.
!
! Special options are provided for alternative decimal point
! character, and truncation of non-essential leading and
! trailing zeros and exposed right decimal point.
!
! The output string will always remain column-aligned to the
! decimal point on repeated calls (with same parameters),
! even when trimming is selected.
!
! Insufficient length of the output string will yield a
! typical Fortran error string, e.g. "*****".
!
! When trim is selected, the output string must be at least
! 3 chars longer than ndigits.  This is to make room for
! the preliminary " 0." or "-0."  Otherwise the trimming
! method will fail.
!
! "decimal" and other inputs are not protected from the
! bizarre.  You must input a period "." for normal USA
! decimal point; there is no default.  The use of space,
! control chars, digits, "decimal" string other than
! length 1, or other parameter inconsistencies may give
! strange results.
!
!---------------------------------------------------------------------------

module real__to_string

   private
   public real_to_string

   interface real_to_string
      module procedure real_to_string_single, real_to_string_double
   end interface

contains

!-----------------------------------------------------
! Double precision interface, and primary routine.
!-----------------------------------------------------

subroutine real_to_string_double (val, decimal, ndigits, trim_opt, string)
   implicit none

   double precision, intent (in ) :: val	! value to convert to string
   character(1),     intent (in ) :: decimal	! decimal point char for output
   integer,          intent (in ) :: ndigits	! max. number of digits to show
						! to right of decimal point
   logical,          intent (in ) :: trim_opt	! right trim options:
						! T = "compact number string":
						!     remove non-essential
						!     leading and trailing zeros
						!     and exposed decimal point
						! F = do not trim leading or
						!     trailing zeros, etc.
   character(*),     intent (out) :: string	! output string

! Local variables.

   character fmt1*15, len_str*5, decimal_str*5	! strings to build format spec
   integer strlen
   integer dpos, last, pre1, pre2		! char pointers within string

! Construct a Fortran format descriptor matching the output string length.

   strlen = len (string)
   dpos = strlen - ndigits			! ptr to dec point, for later

   write (len_str, '(i5)') strlen
   write (decimal_str, '(i5)') ndigits

   fmt1 = '(f' // trim (adjustl (len_str)) // '.' &
      // trim (adjustl (decimal_str)) // ')'

! Use a write statement to get the basic Fortran "Fm.n" formatting into the
! output string.  Note that the number of decimal places is initially fixed.
! Assume that the write statement does the rounding correctly.

   write (string, fmt1) val

! If selected, insert alternative decimal point character.

   if (decimal /= '.') then			! if alternative...
      if (string(dpos:dpos) == '.') then	! don't insert if error string
         string(dpos:dpos) = decimal		! overwrite the period
      end if
   end if

! If selected, blank out trailing zeros to right of the decimal point, etc.

   if (trim_opt) then

! Scan digits from the right, to blank out trailing zeros.

      last = strlen

      do while (last > dpos)
         if (string(last:last) /= '0') exit	! significant digit: stop
         string(last:last) = ' '		! otherwise blank out a trailing
         last = last - 1			! zero, and keep scanning
      end do

! Also blank out trailing decimal point, if exposed.

      if (last == dpos) then

         if (string(last:last) == decimal) then   ! don't blank if error string
            string(last:last) = ' '
         end if

! Now blank out leading zero, when -1 < val < 1.

      else		! LZ blank is excluded if decimal point was blanked.
      			! With LZ blank, no DP blank:  .1, .01, -.1
      			! With DP blank, no LZ blank:  0, 1, -1

         pre1 = dpos - 2			! examine 2 chars preceeding
         pre2 = dpos - 1			! decimal point

         if (string(pre1:pre2) == ' 0') then		! positive case
            string(pre2:pre2) = ' '

         else if (string(pre1:pre2) == '-0') then	! negative case
            string(pre1:pre2) = ' -'
         end if

      end if

   end if

! All done.  String is still column-aligned, but trimmed as requested.

end subroutine real_to_string_double

!-----------------------------------------------------
! Single precision interface.
!-----------------------------------------------------

subroutine real_to_string_single (val, decimal, ndigits, trim_opt, string)
   implicit none

   real,         intent (in ) :: val		! see docs above
   character(1), intent (in ) :: decimal
   integer,      intent (in ) :: ndigits
   logical,      intent (in ) :: trim_opt
   character(*), intent (out) :: string

! Overload on the primary routine.

   call real_to_string_double (dble (val), decimal, ndigits, trim_opt, string)

end subroutine real_to_string_single

end module real__to_string
