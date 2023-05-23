!-----------------------------------------------------------------------------
!
! date_index -- Compute day number on a continuous calendar time line.
!
! Rev	Date		Notes
! 1.00	2008-jan-08	Original version, by Dave Allured.
!			Currently supports only Gregorian calendar after 1752.
! 1.01	2008-jan-22	Allow hidden usage for special dates on either
!			  side of ymin and ymax, to support date
!			  approximation such as in index_to_date.f90.
!
! 2.00	2012-jul-31	Add support for proleptic_gregorian calendar.
!			Switch from anglical_match to the more efficient
!			  lowercase method, for string comparisons.
! 2.01	2014-apr-23	Rename module to date__index.
!			Retain previous module name, backward compatibility.
!			Add new function yyyymmdd_to_index to module.
!			Fix line breaking in error messages.
! 2.02	2015-feb-08	Library upgrade.  Use date_sup module interface.
!
! Input:   year, month, day = requested date; may be earlier or later
!	      or the same as the start of the specified base year
!	   base_year = base year for the calendar time line
!	   calendar = name of calendar system: Gregorian, etc.
!	      Case insensitive.
!
! Output:  Function value = one-based number of calendar days elapsed
!	      since the start of the base year.  May be positive or
!	      negative*.  Index 1 is January 1 of the base year.
!
! Notes:
!
! This routine calculates an integer date index along a continuous
! date line, for the specified calendar system.  The index is tied to
! January 1 of the specified base year.
!
! The returned index is suitable for subscripting continuous
! multi-year time series in Fortran, as well as computing relative
! calendar date offsets.
!
! Multiple years are spanned correctly.  Leap years are always
! handled correctly.  Both positive and negative date offsets are
! supported.
!
! The string parameter "calendar" is required.  Currently, only the
! post 1752 Gregorian calendar is supported.  The proleptic
! Gregorian calendar is also supported; see comments below.
!
! *This version 2.00 does not use the correct modulo math.
! Therefore, negative date indices and negative year numbers are
! not currently supported.
!
! See headers of included routines for documentation of other
! functions in this module.
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

module date__index
   implicit none

   public			! visibility:  all module-level procedures

contains

! Included module procedures.

   include 'yyyymmdd_to_index.f90'


!-----------------------------------------------------------------------------
! Primary function date_index.
!-----------------------------------------------------------------------------

integer function date_index (year, month, day, base_year, calendar)

   use date_sup
   use string_utils
   implicit none

   integer,      intent (in) :: year, month, day, base_year
   character(*), intent (in) :: calendar

! Parameters.

   integer, parameter :: ymin = 1753		! legal range for Gregorian;
   integer, parameter :: ymax = 3000		! applies to both year and ybase

! Local variables.

   character cal2*30

   integer year_leaps, base_leaps
   integer ileap, days_between_years

   logical proleptic, check1

! Month lookup tables for high speed algo.

   integer :: month_size(12,2) = reshape ( &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
         31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31  /), &
      (/ 12, 2 /) )

   integer :: month_offset(12,2) = reshape ( &
      (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &
         0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335  /), &
      (/ 12, 2 /) )

! Check for supported calendar.

   cal2 = calendar
   call lowercase (cal2)

   if (cal2 == 'gregorian') then
      proleptic = .false.

   else if (cal2 == 'proleptic_gregorian') then
      proleptic = .true.

   else
      print *, '*** date_index: FATAL: Unsupported or misspelled calendar type.'
      print '(a,4(1x,i0),1x,a)', ' *** y, m, d, base_year, calendar =', &
         year, month, day, base_year, trim (calendar)
      call exit (1)
   end if

! Range checks.

   if (.not. proleptic) then		! check years for normal gregorian only

      check1 = (year < ymin .or. year > ymax)	! general check for year range

      if (check1) check1 = &
         .not. (      (year == ymin-1 .and. month == 12 .and. day == 1) &
                 .or. (year == ymax+1 .and. month == 1  .and. day == 1) )
              			! but allow special dates outside of range

      if (check1 .or. base_year < ymin .or. base_year > ymax) then
         print '(a,i0,a,i0,a)', '*** date_index: FATAL: Years must be ', &
            ymin, ' to ', ymax, ' with Gregorian calendar.'
         print '(a,4(1x,i0),1x,a)', '*** y, m, d, base_year, calendar =', &
            year, month, day, base_year, trim (calendar)
         call exit (1)
      end if
   end if

   if (year < base_year) then		! added in v2.00
      print '(a)', '*** date_index: FATAL: Requested year is before the base' &
         // ' year.'
      print '(a)', '*** Negative date indexes are not currently supported.'
      print '(a,4(1x,i0),1x,a)', '*** y, m, d, base_year, calendar =', &
         year, month, day, base_year, trim (calendar)
      call exit (1)
   end if

   if (month < 1 .or. month > 12 .or. day < 1 .or. day > 31) then
      print '(a)', '*** date_index: FATAL: Invalid month or day number.'
      print '(a,4(1x,i0),1x,a)', '*** y, m, d, base_year, calendar =', &
         year, month, day, base_year, trim (calendar)
      call exit (1)
   end if

   ileap = 1				! leap year index for month tables
   if (leap_check (year)) ileap = 2

   if (day > month_size(month, ileap)) then
      print '(a)', '*** date_index: FATAL: Day number does not exist in the' &
         // ' given calendar month.'
      print '(a,4(1x,i0),1x,a)', '*** y, m, d, base_year, calendar =', &
         year, month, day, base_year, trim (calendar)
      call exit (1)
   end if

! Number of leap days from pseudo reference year 1.

! This implements the 4/100/400 leap year rule with fast integer math.
! Year 1 must be used as the pseudo reference to position all the
! truncations on precisely the correct Gregorian years.

! This algorithm is valid only for positive Gregorian year numbers.
! Zero and B.C. are not properly Gregorian, and need special adjustments.

   year_leaps =      (year-1)/4 -      (year-1)/100 +      (year-1)/400
   base_leaps = (base_year-1)/4 - (base_year-1)/100 + (base_year-1)/400

! Now compute the date index.

! The index becomes 1-based by including the requested day number 1 to 31.

   days_between_years = 365 * (year - base_year) + (year_leaps - base_leaps)
   date_index = days_between_years + month_offset(month, ileap) + day

end function date_index

end module date__index


!-----------------------------------------------------------------------------
! Helper module, for backward compatibility.
!-----------------------------------------------------------------------------

module date_index_mod
   use date__index			! import all
end module date_index_mod
