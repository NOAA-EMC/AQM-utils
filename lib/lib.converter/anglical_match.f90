!---------------------------------------------------------------------------
!
! anglical_match -- Compare two ASCII strings, ignore case and accent marks.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-jun-01	Initial version
! 2.00	2002-jul-25	F90 free format
!
! 3.00	2014-apr-23	Fortran module compatibility.  Add intents, etc.
!			Container module is now needed to reach "anglical".
!
! input:	two character strings of any length
!
! output:	function value = true if strings compare the same,
!			false if not
!
! Notes:
!
! The two strings need not be the same length.
! Leading spaces are significant.  Trailing spaces are NOT significant.
!
! The external function "anglical" converts all alphabetics to
! lowercase a-z, including accented ISO characters.
!
!---------------------------------------------------------------------------

logical function anglical_match (str1, str2)
   implicit none

   character(*), intent(in) :: str1, str2

   integer length, i

   anglical_match = .false.			! assume mismatch until complete

   length = len_trim (str1)
   if (length /= len_trim (str2)) return	! mismatch if nonblank lengths
						! are different
   if (length > 0) then
      do i = 1, length
         if (anglical(str1(i:i)) /= anglical(str2(i:i))) return
      end do					! mismatch if difference found
   end if

   anglical_match = .true.			! good match if all checks OK
   return

end function anglical_match
