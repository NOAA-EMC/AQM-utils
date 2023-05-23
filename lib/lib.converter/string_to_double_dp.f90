!---------------------------------------------------------------------------
!
! string_to_double_dp -- Convert numeric string to double precision number.
!
! DP version handles non-standard decimal point.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2006-sep-15	Double precision version of string_to_real_dp v2.01.
! 1.01	2008-jun-16	Fix nonstandard "<>" operator.
!
! input:	string = character string containing only numeric digits,
!			and possible minus sign and decimal point.
!		decimal = decimal point character; must be len = 1.
!			Also must be "." for normal.
!
! output:	val = numeric value of string
!		val_status = true if conversion succeeded,
!			false if any format error or overflow occurred
!
! notes:	The input string must be a non-null pure string of numeric
!		characters, minus sign, and decimal point only.  The minus
!		sign and decimal point are optional.
!
!		Leading zeros are allowed.  Trailing blanks are allowed.
!
!		No leading blanks, embedded blanks, delimiters,
!		or other non-numeric characters are allowed.
!
!		Argument "decimal" specifies the *only* decimal point
!		allowed.  Period is *not* allowed if a different character
!		is selected.
!
!		"decimal" is not checked for the bizarre.  Use of space,
!		control chars, digits, or incorrectly defined string
!		may give unexpected results.
!
!---------------------------------------------------------------------------

subroutine string_to_double_dp (string, decimal, val, val_status)
   
   implicit none
   
   character,        intent (in   ) :: string*(*)	! args
   character,        intent (in   ) :: decimal*1
   double precision, intent (  out) :: val
   logical,          intent (  out) :: val_status
   
   integer i, strlen, ios			! local vars
   logical dp_flag				! indicates first d.p. was found
   logical copy_flag				! indicates string copy was made
   character copy*30				! string copy for d.p. overwrite
   
   val_status = .false.				! assume reject until proven

   strlen = len_trim (string)
   if (strlen == 0) return			! reject a null string
   if (strlen > 30) return			! reject a string too long
   
   dp_flag = .false.
   copy_flag = .false.
   
   do i = 1, strlen				! scan string for non-digits
      
      if (string(i:i) == decimal) then		! if decimal "point"...
         if (dp_flag) return			! reject second decimal point
         dp_flag = .true.
         
         if (decimal /= '.') then		! copy string only if needed
         					! for non-standard decimal point
            copy(1:strlen) = string(1:strlen)	! (length limited for speed)
            copy(i:i) = '.'		! overwrite custom dp. with standard dp.
            copy_flag = .true.
         end if
         
      else					! all other valid chars exc. dp.
         if (index ('0123456789-', string(i:i)) == 0) return
      end if					! reject non-digit
   
   end do
   
   if (copy_flag) then				! convert string to real
      read (copy(1:strlen), *, iostat=ios) val		! use copy if made
   else
      read (string(1:strlen), *, iostat=ios) val	! otherwise use original
   end if						! (length lim for speed)

   if (ios == 0) val_status = .true.		! good number, return normal
						! else ios /= 0: error return
end subroutine string_to_double_dp
