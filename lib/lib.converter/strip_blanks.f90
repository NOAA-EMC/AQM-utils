!---------------------------------------------------------------------------
!
! strip_blanks -- Remove embedded blanks and tabs from a character string.
!
! Rev	Date		Notes
! 1.00	2010-dec-06	Initial version.  By Dave Allured.
!
! Notes:
!
! The character string is overwritten with the compacted result
! string.  Any string length may be used.
!
! Since normal Fortran strings are fixed length, it is not
! possible to remove trailing blanks.  The caller should use
! "trim" for this purpose, as needed.
!
!---------------------------------------------------------------------------

subroutine strip_blanks (string)

   use stdlit
   implicit none

   character(*), intent (inout) :: string

   character c*1
   integer length, p1, p2

   length = len_trim (string)
   p2 = 1					! init output pointer

   do p1 = 1, length				! scan input characters
      c = string(p1:p1)
      if (c /= space .and. c /= tab) then
         string(p2:p2) = string(p1:p1)		! reposition only non-blanks
         p2 = p2 + 1				! always move to left or same
      end if
   end do

   if (p2 <= length) then			! clear input residue
      string (p2:length) = space
   end if

end subroutine strip_blanks
