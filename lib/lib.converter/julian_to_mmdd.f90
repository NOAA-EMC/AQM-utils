!----------------------------------------------------------------------------
!
! julian_to_mmdd -- Convert julian day number to calendar month/day numbers.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2001-dec-18	Adapted from int_to_date_str v1.01
! 2.00	2015-feb-08	Fully incorporate into date_utils module.
!			Add interface intents.
!
! input:	y = year
!		daynum = julian day number within year (1 -366)
!
! output:	m = month number
!		d = day of month
!
! notes:	It's up to the caller to do the desired range checking
!		on the year and julian day number.
!
!----------------------------------------------------------------------------

subroutine julian_to_mmdd (y, daynum, m, d)
   implicit none

   integer, intent (in ) :: y, daynum
   integer, intent (out) :: m, d

   integer ndays				! local var

! Convert daynum to month/day.

   d = daynum
   m = 1

month_loop: do while (m < 12)

      ndays = days_in_month (y, m)		! function includes
						! leap year adjustment
      if (d > ndays) then
         d = d - ndays
         m = m + 1
      else
         exit month_loop
      end if

   end do month_loop

end subroutine julian_to_mmdd
