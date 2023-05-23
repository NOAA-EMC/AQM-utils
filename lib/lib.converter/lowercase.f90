!---------------------------------------------------------------------------
!
! lowercase -- Convert letters in character string to all lowercase.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-mar-28	Initial version
! 2.00	2002-jul-25	F90 free format
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

subroutine lowercase (string)

   implicit none
   character string*(*)
   
   character c*1
   integer length, i
   
   length = len_trim (string)
   
   if (length.gt.0) then
      do i = 1, length
         c = string(i:i)
         if (c.ge.'A' .and. c.le.'Z') then
            string(i:i) = char (ichar (c) - ichar ('A') + ichar ('a'))
         end if
      end do
   end if
   
   return

end subroutine lowercase
