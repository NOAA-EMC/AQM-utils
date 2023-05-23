!-----------------------------------------------------------------------------
!
! index_to_date_gregorian -- Convert date index number to year, month, day.
!
! This is a private support routine for the new index_to_date module.
!
! Rev	Date		Notes
! 1.00	2008-jan-21	index_to_date:
!			Original version, by Dave Allured.
!			Complimentary function to date_index.f90.
!			Currently supports only Gregorian calendar after 1752.
! 1.01	2011-aug-16	Fix bug in leap_check function calls.
!
! 2.00	2012-jul-30	index_to_date_gregorian:
!			Original subroutine split off into separate F90 file
!			  as new component of index_to_date v2.00.
!			Add trivial support for proleptic_gregorian.
!			This particular routine is the special algorithm
!			  for Gregorian calendars only.
!			Other calendars will now be handled in the main module.
!			Most original code in this routine is preserved exactly.
!			The calendar string check is moved to the main module.
!
! 2.01	2015-feb-08	Library upgrade.  Use date_sup module interface.
!
! Input:   idate = date index, integer day number on the continuous
!		calendar date line.  Positive or negative, spans
!		multiple years.  Referenced to January 1 of the base
!		year, which is day 1.
!	   base_year = base year for the calendar time line.
!	   calendar = name of calendar system, case insensitive.
!		Should be gregorian or proleptic_gregorian.
!		Main module must check to ensure this.
!	   proleptic = calendar identifier determined by main module.
!		True = proleptic_gregorian, false = plain gregorian.
!
! Output:  year, month, day = calendar date for the given date index.
!
! Notes:
!
! This routine calculates the reverse of the date_index function.
! The integer date index is translated to its corresponding calendar
! date expressed as three integers for calendar year, month, and day.
!
! The date index is interpreted in the modern Gregorian calendar.
! The index is tied to January 1 of the specified base year.
!
! Multiple years are spanned correctly.  Leap years are always
! handled correctly.  Both positive and negative date offsets are
! supported.
!
! The string parameter "calendar" is used only for diagnostics,
! but it should always be provided by the main module.
! Currently, only the post 1752 Gregorian calendar is supported,
! unless calendar = proleptic_gregorian.
!
! Diagnostics are incomplete in this version.  For the normal
! Gregorian calendar, the base year is confirmed to be within the
! Gregorian era.  However, no such check is currently made for
! the output calendar date.
!
! Method:  Linear approximation, plus one correction step.
!
!-----------------------------------------------------------------------------
!
! Worst case analysis for Gregorian (non-rigorous):
!
! Estimating any target y, m, d by simple linear computation should
! always be accurate to within five calendar days.  Consider the
! sources of deviation from the long term average month, 30.436875
! days.  Allow one day deviation each for the 4-year and 100-year
! leap cycles.  The 400-year cycle doesn't count because it is always
! subtractive to the other two cycles.
!
! Then allow about 2.5 days deviation from the mean for February 28.
! Without a more enumerated analysis, this adds to the leap year
! deviations for a total deviation of about 4.5 days.
!
! The main conclusion is that with linear estimation, there is a
! zone of uncertainty of about 5 days either side of the actual date.
! Any algorithm can take advantage of this to minimize the amount
! of extra guessing to get to the right answer.
!
!-----------------------------------------------------------------------------
!
! Algorithm development for Gregorian calendar:
!
! Here are several algorithms that I went through before settling on
! #3.  All of these methods are types of successive approximation.
!
! 1.  Two initial guesses: First day of month, and first day of next
! month.  (Jan. 18.)  Solid algorithm, always included self checking.
! Inefficient because it called date_index at least two and sometimes
! three times.
!
! 2.  Shifted uncertainty.  Unproven, never coded.  The concept was
! to reduce the number of exceptions by folding the second half of
! the estimated y, m into the first half, thus moving the
! uncertainties at start and end of month into a single zone to be
! handled with the same code.
!
! 3.  Direct single guess followed by qualification within month,
! and exception.  Requires only one call to date_index in most cases,
! otherwise a maximum of two.
!
!-----------------------------------------------------------------------------
!
! Gregorian calendar:
!
! This version is fully implemented for the post-1752
! Gregorian calendar, which is the common calendar for US
! and worldwide business use at this time.  The 4/100/400
! leap year rule is used.
!
! Valid year values are from 1753 through present, to
! some undetermined year 2000 to 3000 years in the future
! when it's estimated a new leap year adjustment will
! be needed.
!
! 1753 is the first year that the simple Gregorian calendar
! can be considered generally reliable for US, the British
! Empire, and much of the rest of the world.
!
! The Gregorian calendar was not adopted by the American
! states (British colonial times, pre-US) until 1752.
! 1752 was an anomalous year because 11 days were excised
! from the calendar that year, and New Year's Day was
! moved to January 1.
!
! Therefore date calculations for years prior to 1753
! are dependent on the type of calendar in local use,
! and require more sophisticated routines than this.
!
! Some countries did not convert their calendars to modern
! adjusted Gregorian until as recently as 1927 (Turkey).
! Check calendar history when using pre-1928 data from
! sources outside the US and Great Britain.  See this
! website reference:
!
!	http://www.geocities.com/CapeCanaveral/Lab/7671/gregory.htm
!
! Other useful website references:
!
!	http://www.merlyn.demon.co.uk/leapyear.htm
!	http://www.merlyn.demon.co.uk/miscdate.htm
!	http://www.Crowl.org/Lawrence/time/britgreg.html
!	http://www.urbanlegends.com/legal/calendar_act.html
!	http://www2.ao.com/~regan/cal.html
!
!-----------------------------------------------------------------------------
!
! Proleptic Gregorian calendar:
!
! This is an artificial model calendar, defined simply as the
! current Gregorian calendar, extended indefinitely into the past
! and future without further irregularities.  Refer to the
! definition in chapter 4.4 of the current CF Conventions
! document:
!
!	http://cf-pcmdi.llnl.gov/
!
!-----------------------------------------------------------------------------

subroutine index_to_date_gregorian (idate, year, month, day, base_year, &
      calendar, proleptic)

   use date_index_mod				! modules
   use date_sup
   implicit none

   integer,      intent (in ) :: idate
   integer,      intent (out) :: year, month, day
   integer,      intent (in ) :: base_year
   character(*), intent (in ) :: calendar
   logical,      intent (in ) :: proleptic

! Local variables.

   integer index1, index2, moffset, mabs

! Constants for Gregorian 4/100/400 leap year rule.

   integer, parameter :: leaps_per_400_years = 100 - 4 + 1
   real, parameter :: avg_year = 365. + (leaps_per_400_years / 400.)
   real, parameter :: avg_month = avg_year / 12.

! Month lookup table for high speed algo.

   integer :: month_size(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

!! Check for supported calendar.
!! Removed in v2.00.  Calendar check is now handled in the main module.
!! Also note, this routine now also handles proleptic_gregorian.
!!
!!   if (.not. anglical_match (calendar, 'gregorian')) then
!!      print *, '*** index_to_date_gregorian: FATAL: Only Gregorian calendar' &
!!         // ' is supported.'
!!      print *, '*** idate, base_year, calendar = ', idate, base_year, &
!!         trim (calendar)
!!      call exit (1)
!!   end if

! Range check, for normal Gregorian calendar only.

   if ((.not. proleptic) .and. (base_year < 1753 .or. base_year > 3000)) then
      print *, '*** index_to_date_gregorian:'
      print *, '*** FATAL: Base year must be 1753 to 3000 with Gregorian' &
         // ' calendar.'
      print *, '*** idate, base_year, calendar = ', idate, base_year, &
         trim (calendar)
      call exit (1)
   end if

! First date estimate, by linear approximation.

! Worst case should be 5 days into preceeding or following month.
! Caution: Special integer math with mabs, moffset might be negative.

   moffset = floor ((idate - 1) / avg_month)	! # of months since ybase jan 1
   mabs = moffset + base_year * 12		! # of months since 0000 jan 1
   						! (pseudo year 0000 that is)
                                                ! mabs must always be positive

   year = mabs / 12				! first guess year and month
   month = 1 + mod (mabs, 12)

   index1 = date_index (year, month, 1, base_year, calendar)   ! first day of
   							       ! estimated month
   day = 1 + (idate - index1)			! compute actual day number

! Check first estimate by evaluating the resulting day number.

   if (day >= 1 .and. day <= month_size (month)) return
   						! fast return for most cases

   if (day == 29 .and. leap_check (year)) return  ! special case for leap years

! First estimate was wrong.  Offset by one month for second guess.

   if (day > 1) then				! check sign of error
      mabs = mabs + 1				! go one month forward
   else
      mabs = mabs - 1				! go one month back
   end if

   year = mabs / 12				! second guess year and month
   month = 1 + mod (mabs, 12)

   index2 = date_index (year, month, 1, base_year, calendar)   ! first day of
   							       ! estimated month
   day = 1 + (idate - index2)			! compute actual day number

! Check second guess by evaluating the resulting day number.

   if (day >= 1 .and. day <= month_size (month)) return

   if (day == 29 .and. leap_check (year)) return  ! special case for leap years

! Algorithm failed.  Should never happen.

   print *, '*** index_to_date_gregorian: FATAL: Failed to convert this' &
      // ' date index.'
   print *, '*** idate, base_year, calendar = ', idate, base_year, &
      trim (calendar)
   call exit (1)

end subroutine index_to_date_gregorian
