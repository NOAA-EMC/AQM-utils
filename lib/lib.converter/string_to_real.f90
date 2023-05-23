!---------------------------------------------------------------------------
!
! string_to_real -- Convert numeric string to real number, strict version.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-22	Initial version
! 1.01	2000-feb-28	Comment fix only
! 1.02	2001-aug-02	Convert to F90
!			Check for more than one decimal point
!			(Fortran I/O fails to check for this error)
! 1.03	2001-sep-14	Fix strlen checking bug
!			Convert to F90 free format, remove label 999, etc.
!
! input:	string = character string containing only numeric digits,
!			and possible minus sign and decimal point.
!
! output:	val = real value of string
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
!---------------------------------------------------------------------------

subroutine string_to_real (string, val, val_status)
	
   use stdlit
   
   implicit none				! args
   character string*(*)
   logical val_status
   real val
   
   integer i, strlen, ios			! local vars
   logical dp_flag
   
   val_status = .false.				! assume reject until proven

   strlen = len_trim (string)
   if (strlen == 0) return			! reject a null string
   if (strlen > 50) return			! reject a string too long
   
   dp_flag = .false.
   
   do i = 1, strlen				! scan string for non-digits
      if (string(i:i) == '.') then
         if (dp_flag) return			! reject second decimal point
         dp_flag = .true.
      else
         if (index ('0123456789-', string(i:i), forward) == 0) return
      end if					! reject non-digit
   end do
   
   read (string, *, iostat=ios) val		! convert string to real
   if (ios == 0) val_status = .true.		! good number, return normal
						! else ios /= 0: error return
end subroutine string_to_real
