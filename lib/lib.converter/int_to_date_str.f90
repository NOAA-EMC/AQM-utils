!----------------------------------------------------------------------------
!
! int_to_date_str -- Convert year/day numbers to mm/dd/yyyy character string.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-mar-14	Initial version
! 1.01	2000-dec-29	Fortran 90 compatibility
!			Fix type declaration for dlen
! 1.02	2002-may-17	Remove leap_check.h dependency
! 2.00	2002-aug-06	F90 free format
!			Use days_in_month library routine
! 2.01	2009-dec-13	Fix ifort warning, remove unused variable.
!
! 3.00	2015-feb-06	Module fixes only.
!			Incorporate into date_utils module.  Add intents.
!
! input:	y = year
!		daynum = day number within calendar year (1 = Jan 1, etc)
!
! output:	outstring = character string containing date
!
! notes:	It's up to the caller to do the desired range checking
!		on the year and day number, and to make sure that the
!		destination string is long enough.
!
!		In case of a length error, an asterisk is inserted in
!		column 1 as a visual flag.
!
!		The number format in the output string is mm/dd/yyyy:
!		   mm, yyyy = leading zeros suppressed, spaces trimmed
!		   dd = 2 digits with leading zero (if in range)
!
!		For example:  "3/05/1999"
!
!		The entire date is left-justified within the output string.
!
!----------------------------------------------------------------------------

subroutine int_to_date_str (y, daynum, outstring)

   use string_utils
   implicit none

   integer,      intent (in ) :: y, daynum
   character(*), intent (out) :: outstring

! Local variables.

   character mm*2, dd*20, yyyy*20, temp*50
   integer m, d, days_this_month
   integer mlen, dlen

! Convert daynum to month/day.

   d = daynum
   m = 1

   do while (m < 12)
      days_this_month = days_in_month(y, m)
      if (d <= days_this_month) exit
      d = d - days_this_month
      m = m + 1
   end do

! Assemble date string, with special formatting.

   call int_to_string (m, mm)
   call int_to_string (d, dd)
   call int_to_string (y, yyyy)

   mlen = len_trim (mm)
   dlen = len_trim (dd)

   if (dlen == 1) then				! dd in range 0-9 becomes 00-09
      dd = '0' // dd
      dlen = 2
   end if

   temp = mm(1:mlen) // '/' // dd(1:dlen) // '/' // yyyy

! Check for length overflow.

   if (len_trim (temp) > len (outstring)) then
      outstring = '*' // temp			! leading asterisk if overflow
   else
      outstring = temp
   end if

   return

end subroutine int_to_date_str
