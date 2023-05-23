!---------------------------------------------------------------------------
!
! days_in_month -- Determine the number of days in a given calendar month.
!
! Rev	Date		Notes
! 1.00	2000-jul-26	Initial version.  By Dave Allured.
! 1.01	2002-nov-03	F90 free format
!
! 2.00	2015-feb-08	Fully incorporate into date_utils module.
!			Add interface intents.
!			Add minor optimization.
!
! input:	year, month
!
! output:	function value = 28 through 31
!		function value = 0 if month number was invalid.
!
! notes:	Caller should do range checking on the year and month,
!		before or after calling this routine.
!
!		This routine uses the leap_check function to determine
!		leap years.  See that routine for implementation notes,
!		and calendar system in use.
!
!---------------------------------------------------------------------------

integer function days_in_month (year, month)
   implicit none

   integer, intent (in) :: year, month

   integer, parameter :: &
      month_size(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

   if (month >= 1 .and. month <= 12) then	! valid month number?
      days_in_month = month_size (month)

      if (month == 2) then			! adjust feb. days for leap year
         if (leap_check (year)) days_in_month = 29
      end if

   else
      days_in_month = 0
   end if

end function days_in_month
