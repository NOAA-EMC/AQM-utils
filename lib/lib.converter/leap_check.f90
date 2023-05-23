!---------------------------------------------------------------------------
!
! leap_check -- Determine whether specified year is a leap year.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-21	Initial version
! 1.01	2000-apr-04	Add 4/100/400 leap year rule
! 1.02	2000-apr-10	Debug
! 2.00	2002-mar-05	Convert to f90
!
! input:	year
!
! output:	function value = true if leap year, false if not
!
! notes:	Caller should do range checking on the year, before or
!		after calling this routine.
!
!		This version is fully implemented for the post-1752
!		Gregorian calendar, which is the common calendar for US
!		and worldwide business use at this time.  The 4/100/400
!		leap year rule is used.
!
!		Valid year values are from 1753 through present, to
!		some undetermined year 2000 to 3000 years in the future
!		when it's estimated a new leap year adjustment will
!		be needed.
!
!		1753 is the first year that the simple Gregorian calendar
!		can be considered generally reliable for US, the British
!		Empire, and much of the rest of the world.
!
!		The Gregorian calendar was not adopted by the American
!		states (British colonial times, pre-US) until 1752.
!		1752 was an anomalous year because 11 days were excised
!		from the calendar that year, and New Year's Day was
!		moved to January 1.
!
!		Therefore date calculations for years prior to 1753
!		are dependent on the type of calendar in local use,
!		and require more sophisticated routines than this.
!
!		Some countries did not convert their calendars to modern
!		adjusted Gregorian until as recently as 1927 (Turkey).
!		Check calendar history when using pre-1928 data from
!		sources outside the US and Great Britain.  See this
!		website reference:
!
!		http://www.geocities.com/CapeCanaveral/Lab/7671/gregory.htm
!
!		Other useful website references:
!		
!		http://www.merlyn.demon.co.uk/leapyear.htm
!		http://www.merlyn.demon.co.uk/miscdate.htm
!		http://www.Crowl.org/Lawrence/time/britgreg.html
!		http://www.urbanlegends.com/legal/calendar_act.html
!		http://www2.ao.com/~regan/cal.html
!
!---------------------------------------------------------------------------

logical function leap_check (year)
	
   integer year

! Range check is disabled because it interferes with caller's error handling.
!
!   if (year.lt.1753) then	! if this traps, it is a bad bad thing.
!      print !,'!!! leap_check v1.01, FATAL: invalid before 1753,', &
!         ' year =', year
!      stop 
!   end if
   
   if (mod(year,4).ne.0) then
      leap_check = .false.		! not divisible by 4
   
   else if (mod(year,100).ne.0) then
      leap_check = .true.		! divisible by 4, unless
      
   else if (mod(year,400).ne.0) then
      leap_check = .false.		! divisible by 100, unless
      
   else
      leap_check = .true.		! divisible by 400
   end if
   
   return

end function leap_check
