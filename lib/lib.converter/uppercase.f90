!---------------------------------------------------------------------------
!
! uppercase -- Convert letters in character string to all upper case.
!
! by Dave Allured
!
! Rev	Date		Notes
! 2.00	2002-jul-25	Initial version.  Adapted from lowercase.f90 v2.00.
!
! Notes:
!
! The character string is overwritten with the result string.
! Any string length may be used.
!
! Assume:
!
! In the system character set, the uppercase alphabet and the lowercase
! alphabet are each in a single continuous block of code values.
! This is true for ASCII but not for EBCDIC.
!
!---------------------------------------------------------------------------

subroutine uppercase (string)

   implicit none
   character string*(*)
   
   character c*1
   integer length, i
   
   length = len_trim (string)
   
   if (length.gt.0) then
      do i = 1, length
         c = string(i:i)
         if (c.ge.'a' .and. c.le.'z') then
            string(i:i) = char (ichar (c) - ichar ('a') + ichar ('A'))
         end if
      end do
   end if
   
   return

end subroutine uppercase
