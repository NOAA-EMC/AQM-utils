!----------------------------------------------------------------------------
!
! format_date.f90 - Convert numeric date to formatted string.
!
! Rev	Date		Notes
! 1.00	2002-jul-08	int_to_ymd2_str:
!			Original version.  By Dave Allured.
!			Adapted from int_to_date_str v1.02
!
! 2.00	2010-mar-02	Make generic interface, either y,m,d or y,daynum input.
!			Improve range checking of input date.
! 2.01	2014-sep-09	Library upgrade.  Use string_utils module interface.
! 2.02	2015-feb-08	Library upgrade.  Use date_sup module interface.
!
! Alternate calls:
!
!	call format_date (y, m, d, outstring)       ! calendar date
!	call format_date (y, daynum, outstring)     ! year plus day no. in year
!
!	daynum = day number within year (1 = Jan 1, 365/366 = Dec 31, etc.)
!
! Output string format:  yyyy-mmm-dd       for example: "1999-mar-05"
!
!	The year number is always printed in full, normally four digits.
!	The month abbreviation is always three letters, lowercase.
!	The day number "dd" is always two digits, e.g. 09, 10.
!	The fields may be longer if error codes are added for bad dates.
!	The entire date is left-justified within the output string.
!
! Valid range for years:
!
!	y >= 1753 for mixed Gregorian/Julian calendar interpretation.
!	No limits for proleptic Gregorian calendar interpretation.
!
! Notes:
!
! When the day-of-year form is used, the standard Gregorian
! calendar is assumed.
!
! The actual calendar interpretation is in leap_check.f90.  As
! of 2000-apr-04, this routine uses the proleptic Gregorian
! calendar.  This is not adjusted for for the break in the real
! Gregoriar-Julian calendar, so dates prior to 1753 are not
! handled correctly.  See leap_check.f90 for more details.
!
! If the destination string is too short to hold the result, an
! asterisk is inserted in column 1 as a visual flag.
!
! If the month or day is invalid for the given year, an asterisk
! is inserted into the corresponding substring, in addition to
! the raw integer value.
!
! The code -*99-*NNN in the output string is used with the
! day-of-year interface to indicate that the given NNN day of year
! is invalid.
!
!----------------------------------------------------------------------------

module format__date

   private
   public format_date

   interface format_date
      module procedure format_date_ymd, format_date_yday
   end interface

contains

!----------------------------------------------------------------------------

subroutine format_date_ymd (y, m, d, outstring)

   use date_sup
   use string_utils
   implicit none

   integer,      intent(in ) :: y, m, d		! input calendar date
   character(*), intent(out) :: outstring	! output string

   character dd*30, mmm*30, yyyy*30, temp*70	! local vars
   integer month_size, j

   character(1), parameter :: delim = '-'

   character(*), parameter :: &
      months = 'jan feb mar apr may jun jul aug sep oct nov dec '

! Create month string, including range check.

   if (m >= 1 .and. m <= 12) then
      j = (m * 4) - 3				! point to month substring
      mmm = months(j:j+2)			! get 3-letter abbreviation
      month_size = days_in_month (y, m)

   else					! invalid month no. prints as *mnum
      mmm(1:1) = '*'
      call int_to_string (m, mmm(2:))
      month_size = 31
   end if

! Create day string, including range check.

   if (d >= 1 .and. d <= month_size) then
      call int_to_string (d + 100, dd)		! keep leading zero
      dd = dd(2:3)				! always 2 digits

   else					! invalid day no. prints as *dnum
      dd(1:1) = '*'
      call int_to_string (d, dd(2:))
   end if

! Assemble date string, with special formatting.

   call int_to_string (y, yyyy)

   temp = trim (yyyy) // delim // trim (mmm) // delim // dd

! Check for length overflow.

   if (len_trim (temp) > len (outstring)) then
      outstring = '*' // temp		! leading asterisk if overflow
   else
      outstring = temp
   end if

end subroutine format_date_ymd

!----------------------------------------------------------------------------

subroutine format_date_yday (y, daynum, outstring)

   use date_sup
   use string_utils
   implicit none

   integer,      intent(in ) :: y, daynum	! input year, and day of year
   character(*), intent(out) :: outstring	! output string

   character ddd*30, yyyy*30
   integer m, d

   if (daynum >= 1 .and. daynum <= days_in_year (y)) then   ! check day number
      call julian_to_mmdd (y, daynum, m, d)	 ! valid date, convert to YMD
      call format_date_ymd (y, m, d, outstring)  ! then just use YMD routine

   else						! invalid day number
      call int_to_string (y, yyyy)		! assemble error string
      call int_to_string (daynum, ddd)
      outstring = '*' // trim (yyyy) // '-*' // ddd
   end if

end subroutine format_date_yday

end module format__date
