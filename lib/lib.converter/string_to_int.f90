!---------------------------------------------------------------------------
!
! string_to_int -- Convert numeric string to signed integer, strict version.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-jul-01	Initial version
! 2.00	2002-jul-25	F90 free format; restructure routine.
!			num = undefined on error return.
!
! input:	string = character string containing only numeric digits,
!			optionally starting with a minus sign
!
! output:	num = integer value of string, or undefined if error
!		flag = true if integer is good,
!		       false if input string was not a valid integer
!			  or if overflow occurred
!
! notes:	The input string must be a non-null string consisting of an
!		optional minus sign, followed by a pure string of numeric
!		characters only.
!
!		Leading zeros are allowed in the numeric portion.
!
!		No leading or trailing blanks, embedded blanks,
!		decimal point, plus sign, delimiters, or other
!		non-numeric characters are allowed.
!
!---------------------------------------------------------------------------

subroutine string_to_int (string, num, flag)
	
   implicit none
   character string*(*)
   integer num
   logical flag

   flag = .false.				! assume error until done
   
   if (len(string) == 0) return			! reject a null string
   
   if (string(1:1) == '-') then
      call string_to_intu (string(2:), num)	! convert neg number
      if (num < 0) return
      num = -num

   else
      call string_to_intu (string, num)		! convert pos number
      if (num < 0) return
   end if
   
   flag = .true.				! normal return, good integer
   return

end subroutine string_to_int
