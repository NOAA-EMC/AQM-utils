!-----------------------------------------------------------------------------
!
! real_to_compact_string -- Format a real number with minimal characters.
!
! 2011-mar-07	Original version.  By Dave Allured.
!
! Usage:
!
! Caller specifies the minimum significant digits via the format
! prototype.  At least one digit is guaranteed before and after
! the decimal point.  The output string is always left justified.
! Leading blanks are omitted.
!
! Notes:
!
! For best results, use this routine with F0.d format prototype,
! e.g. '(F0.3)' for three significant digits after the decimal.
!
! Any other format spec that reliably produces a simple number
! string, with or without a decimal point, is handled correctly.
!
! Result strings including these variations are also handled
! correctly:
!
! * Minus sign for negative numbers.
! * Leading or trailing blanks.
! * Any exception fields with no decimal point,
!   such as NaN or all asterisks.
! * Any result NOT ending with the digit zero.
!
! Exponent format specs resulting in e.g. "3.45E+01", are NOT
! handled correctly.  They should not be used with this routine.
!
! This conservative version works with older and newer Fortran
! versions.
!
! This version is NOT speed optimized.
!
!-----------------------------------------------------------------------------

module real__to_compact_string
contains

subroutine real_to_compact_string (num, fmt_prototype, outstr)
   implicit none
   
   real,         intent (in)  :: num		! number to be formatted
   character(*), intent (in)  :: fmt_prototype	! prototype format with maximum
						!   signif. digits; eg. (f0.3)
						!   must include parentheses
   
   character(*), intent (out) :: outstr		! output string
   
   integer j, dp				! local vars

! Use write statement to convert number to initial string.

   write (outstr, fmt_prototype) num

! Omit leading blanks, if needed.

   j = len_trim (outstr)			! get initial length

   if (outstr(1:1) == ' ') then
      outstr(1:j) = adjustl (outstr(1:j))	! left justify
      j = len_trim (outstr(1:j))		! recompute the length
   end if

! Check for decimal point.

   dp = index (outstr(1:j), '.')		! if no decimal point...
   if (dp == 0) return				! all done, no further changes

! Remove excess trailing zeros.
   
   do						! scan right to left
      if (j < 3) exit				! stop if minimal length
      if (outstr(j:j) /= '0') exit		! stop on any non-zero character
      if (outstr(j-1:j-1) == '.') exit		! stop if next to decimal point
      outstr(j:j) = ' '				! clear the excess trailing zero
      j = j - 1					! move one digit to the left
   end do

! Now check for unprotected leading decimal point.
! Insert a leading zero, if needed.
! F0 makes a leading zero with some but not all compilers.

   if (dp == 1) then
      outstr(2:j+1) = outstr(1:j)		! make room for leading zero
      outstr(1:1) = '0'				! insert the leading zero
   end if

end subroutine real_to_compact_string

end module real__to_compact_string
