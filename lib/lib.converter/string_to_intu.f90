!---------------------------------------------------------------------------
!
! string_to_intu -- Convert numeric string to integer, unsigned, strict version.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-21	Initial version
! 2.00	2002-jul-25	F90 free format
!			Change to iostat method; use stdlit
!
! input:	string = character string containing only numeric digits
!
! output:	num = integer value of string
!		num = -1 if input string was null, not a valid integer,
!			or if overflow occurred
!
! notes:	The input string must be a non-null pure string of numeric
!		characters only.  Leading zeros are allowed.
!
!		No leading or trailing blanks, embedded blanks,
!		decimal point, sign character, delimiters, or other
!		non-numeric characters are allowed.
!
!---------------------------------------------------------------------------

subroutine string_to_intu (string, num)

   use stdlit
   
   implicit none
   character string*(*)
   integer num
   
   integer ios, i
	
   num = -1				! assume error return until complete
   
   if (len(string).eq.0) return		! reject a null string
   if (len(string).gt.20) return	! reject a string too long
   
   do i = 1, len(string)		! scan string for non-digits
      if (index ('0123456789', string(i:i), forward) == 0) return
   end do

   read (string, *, iostat=ios) num	! convert string to integer

   if (ios /= 0) num = -1		! reject if misc. conversion error
   					! (integer out of range?)

   return				! otherwise, return good integer

end subroutine string_to_intu
