!-----------------------------------------------------------------------------
!
! int_to_string -- Convert integer value to character format, LEFT JUSTIFIED.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-16	Initial version
! 1.01	2000-sep-21	Update for new calling format for left_trim v2.00
! 1.02	2001-jan-30	Fortran 90 conversion
!			Fix "-0" Fortran bug
! 1.03	2014-apr-23	Fortran compatibility.  Add intents, etc.
!
! Notes:
!
! This routine is needed because the usual "write (string,*) int"
! does not generate a left-justified string of digits.
!
! If the length of the caller's string is overflowed, the output
! becomes all asterisks.
!
!-----------------------------------------------------------------------------

subroutine int_to_string (num, outstring)
   implicit none

   integer,      intent (in ) :: num
   character(*), intent (out) :: outstring

   character*30 temp1, temp2

   if (num == 0) then		! Circumvent Sun Fortran 90 bug
      temp2 = '0'
   else
      write (temp1, *) num
      temp2 = adjustl (temp1)
   end if

   if (len_trim(temp2) > len(outstring)) then
      outstring = '******************************'
   else
      outstring = temp2
   end if

end subroutine int_to_string
