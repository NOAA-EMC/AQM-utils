!-----------------------------------------------------------------------------
!
! index_to_date -- Convert calendar day index number to year, month, day.
!
! Rev	Date		Notes
! 1.00	2008-jan-21	Original version, by Dave Allured.
!			Complimentary function to date_index.f90.
!			Currently supports only Gregorian calendar after 1752.
! 1.01	2011-aug-16	Fix bug in leap_check function calls.
!
! 2.00	2012-aug-09	Add support for alternate calendars 360_day, 365_day,
!			  366_day, and proleptic_gregorian.
!			Split off the original routine into
!			  index_to_date_gregorian.f90, for handling
!			  Gregorian calendars only.
!			Switch from anglical_match to the more efficient
!			  lowercase method, for string comparisons.
! 2.01	2014-dec-02	Library upgrade.  Use string_utils module interface.
!
! Input:   idate = one-based date index, integer day number on the
!	      continuous calendar date line.  Positive or negative,
!	      spans multiple years.  Referenced to January 1 of the
!	      base year, which is day 1.
!	   base_year = base year for the calendar time line
!	   calendar = name of calendar system: Gregorian, etc.  Case
!	      insensitive.  Use CF calendar names with this version.
!
! Output:  year, month, day = calendar date for the given date index.
!
! Notes:
!
! This routine calculates the reverse of the date_index function.
! The integer date index is translated to its corresponding calendar
! date expressed as three integers for calendar year, month, and day.
!
! The date index is interpreted in the specified calendar system.
! The date index is the number of actual calendar days relative
! to January 1 of the specified base year.
!
! Multiple years are spanned correctly.  Leap years are always
! handled correctly in the Gregorian calendards.  Both positive
! and negative date offsets are supported.
!
! The string parameter "calendar" is required.
!
! Model calendars with fixed year lengths are handled in this
! main module, with a simple linear technique.  Refer to the
! CF model calendar definitions in chapter 4.4 of the current
! CF Conventions document:
!
!	http://cf-pcmdi.llnl.gov/
!
! The two Gregorian calendars are handled in a separate support
! routine, using a special linear approximation method.  See
! documentation in index_to_date_gregorian.f90 for a full
! description of the Gregorian calendar implementation.
!
!-----------------------------------------------------------------------------

!---------------------------------------------------------
! Module container.
!---------------------------------------------------------

module index_to_date_mod

   private				! visibility controls
   public index_to_date			! only the main routine is public

contains

!---------------------------------------------------------
! Primary date routine.  Handles fixed year calendars.
!---------------------------------------------------------

subroutine index_to_date (idate, year, month, day, base_year, calendar)

   use string_utils
   implicit none

   integer,      intent (in ) :: idate
   integer,      intent (out) :: year, month, day
   integer,      intent (in ) :: base_year
   character(*), intent (in ) :: calendar

! Local variables.

   character cal2*30
   integer ical, daynum, ndays

! Calendar data tables.

   integer, parameter :: ical_365=1, ical_366=2, ical_360=3	! table indices
   integer, parameter :: year_lens(3) = (/ 365, 366, 360 /)

! Lengths of months, 365-day and 366-day calendars only.

   integer, parameter :: days_in_month(12,2) &
      = reshape ( &
         (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
            31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31  /), &
         (/ 12, 2 /) )

! Check for supported calendar.

   cal2 = calendar
   call lowercase (cal2)

! Gregorian type calendars use special subroutine.

   if (cal2 == 'gregorian') then
      call index_to_date_gregorian (idate, year, month, day, base_year, &
         calendar, proleptic=.false.)
      return

   else if (cal2 == 'proleptic_gregorian') then
      call index_to_date_gregorian (idate, year, month, day, base_year, &
         calendar, proleptic=.true.)
      return

! Handle fixed year calendars within this routine.

   else if (cal2 == '365_day' .or. (cal2 == 'noleap')) then
      ical = ical_365

   else if (cal2 == '366_day' .or. (cal2 == 'all_leap')) then
      ical = ical_366

   else if (cal2 == '360_day') then
      ical = ical_360

   else
      print *, '*** index_to_date: Unsupported or misspelled calendar type.'
      print *, '*** idate, base_year, calendar = ', idate, base_year, &
         trim (calendar)
      print *, '*** Abort.'
      call exit (1)
   end if

! Only calendars with fixed year lengths remain.
! Compute the target year and day number within year.

   year = base_year + floor (dble (idate - 1) / year_lens(ical))
   					! use floor function for correct
                                        ! offset, in case idate is negative

   daynum = idate - (year - base_year) * year_lens(ical)
   					! compute 1-based day number within year

!!   print '(a,4i6)', 'ical, year_lens(ical) = ', ical, year_lens(ical)
!!   print '(a,4i6)', 'idate, year, daynum   = ', idate, year, daynum

! Handle the simple 360-day calendar with fixed 30-day months.

   if (ical == ical_360) then
      month = 1 + (daynum - 1) / 30
      day = daynum - (month - 1) / 30
      return

! Handle 365-day and 366-day calendars.
! Subtract out the days in each month until we find the right month.

   else
      day = daynum				! start with 1-based day of year

      do month = 1, 12
         ndays = days_in_month(month,ical)
         if (day <= ndays) return		! return at correct month
         day = day - ndays			! else, subtract preceeding days
      end do

   end if

! Algorithm failed.  Should never happen.

   print *, '*** index_to_date: Abort, algorithm failure.'
   print *, '*** idate, base_year, calendar = ', idate, base_year, &
      trim (calendar)
   print *, '*** Computed year, daynum      = ', year, daynum
   call exit (1)

end subroutine index_to_date

!----------------------------------------------------
! Included subroutines.
!----------------------------------------------------

   include 'index_to_date_gregorian.f90'

end module index_to_date_mod
