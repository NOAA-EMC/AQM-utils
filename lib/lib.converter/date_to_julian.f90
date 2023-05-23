!----------------------------------------------------------------------------
!
! date_to_julian -- Compute julian day number for a given calendar date.
!
! Rev	Date		Notes
! 1.00	2006-feb-22	Original, by Dave Allured.
! 2.00	2015-feb-08	Fully incorporate into date_utils module.
!
! input:	y, m, d = calendar year, month, day
!
! output:	daynum = julian day number within year (1 -366)
!
! notes:	It's up to the caller to do the desired range checking
!		on the calendar date integers.
!
!----------------------------------------------------------------------------

subroutine date_to_julian (y, m, d, daynum)
   implicit none

   integer, intent (in ) :: y, m, d
   integer, intent (out) :: daynum

   integer mm					! local vars

! Compute julian date from calendar date.  Add offsets for all prior months.

   daynum = d

   do mm = 1, m-1
      daynum = daynum + days_in_month (y, mm)	! function includes
   end do					! leap year adjustment

end subroutine date_to_julian
