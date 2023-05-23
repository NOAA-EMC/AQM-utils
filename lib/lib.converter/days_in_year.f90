!---------------------------------------------------------------------------
!
! days_in_year -- Determine the number of days in a given year.
!
! Rev	Date		Notes
! 1.00	2000-jun-08	Initial version.  By Dave Allured.
! 1.01	2002-nov-03	F90 free format
!
! 2.00	2015-feb-08	Fully incorporate into date_utils module.
!			Add interface intents.
!
! input:	year
!
! output:	function value = 366 if leap year, 365 if not
!
! notes:	Caller should do range checking on the year, before or
!		after calling this routine.
!
!		This routine uses the leap_check function to determine
!		leap years.  See that routine for implementation notes,
!		and calendar system in use.
!
!---------------------------------------------------------------------------

integer function days_in_year (year)
   implicit none

   integer, intent (in) ::  year

   if (leap_check (year)) then
      days_in_year = 366
   else
      days_in_year = 365
   end if

end function days_in_year
