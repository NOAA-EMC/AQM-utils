!-----------------------------------------------------------------------------
!
! yyyymmdd_to_index -- Convert YYYYMMDD date string to integer day number.
!
! This is a library helper function for date_index.f90.
! This function is included in the date__index module.
!
! 1.00	2014-apr-23	Original version, by Dave Allured.
!
! Input:   date_string = requested date, YYYYMMDD character string.  May
!	      be earlier or later than the start of the specified base year.
!	   base_year = base year for the calendar time line.
!	   calendar = name of calendar system: Gregorian, etc.
!	      Case insensitive.
!
! Output:  Function value = integer time line position, relative to 1 =
!	      the first day of the base year.  May be positive or negative.
!
! Notes:
!
! See additional documentation at the top of date_index.f90.
!
! This routine calculates an integer date index along a continuous
! date line, for the specified calendar system.
!
! The string parameter "calendar" is required.  See the list of
! currently supported calendars at the top of date_index.f90.
!
!-----------------------------------------------------------------------------

function yyyymmdd_to_index (date_string, base_year, calendar) result (ind)

   use string_utils
   implicit none

   character(*), intent (in) :: date_string	! YYYYMMDD format, no spaces
   integer,      intent (in) :: base_year
   character(*), intent (in) :: calendar

   integer ind					! function result, date index

   integer year, month, day			! local variables

! Check for valid number strings.

   call string_to_intu (date_string(1:4), year)
   call string_to_intu (date_string(5:6), month)
   call string_to_intu (date_string(7:8), day)

   if (min (year, month, day) < 0 .or. len_trim (date_string) /= 8) then
      print *, '*** yyyymmdd_to_index: Format error in date string.'
      print *, '*** Expected [YYYYMMDD], 8 digits, no spaces.'
      print *, '*** Received [' // trim (date_string) // ']'
      call exit (1)
   end if

! Call the primary function to do the real calendar conversion.

   ind = date_index (year, month, day, base_year, calendar)

end function yyyymmdd_to_index
