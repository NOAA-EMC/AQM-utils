!----------------------------------------------------------------------------
!
! date_ymd2_to_int -- Convert date in single character string to integers.
!
! Format = yyyy-mmm-d, variable length.
! Four-digit year is required.  mmm is alpha month abbreviation.
!
! For general date spec applications.
! Not bound to input file format or external parameters.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-oct-11	date_yyyymd_to_int:
!			Close adaptation of date_str_to_int v2.04
! 1.01	2000-oct-16	date_ymd2_to_int:
!			Switch to alpha month abbreviation
! 1.02	2000-oct-17	Debug
! 2.00	2002-aug-13	F90 free format
!
! 3.00	2015-feb-06	Module fixes, F90 modern syntax.
!			Incorporate into date_utils module.  Add intents.
!
! input:	string = character string containing date
!
! output:	y = year integer with century added
!		m = month integer (1-12)
!		d = day integer (1-31)
!		daynum = day number from start of year (1-366)
!		y, m, d, daynum = all zero if input date is invalid
!			(check m or d or daynum, because 0 is valid for y)
!
! Notes:
!
!	The date string is carefully checked for formatting errors
!	including extra characters.  The resultant date must be a real
!	calendar date, including all leap year considerations.
!
!       Leading blanks are permitted in each field.  Trailing blanks are
!       permitted only following the entire date string.
!
!----------------------------------------------------------------------------

subroutine date_ymd2_to_int (string, y, m, d, daynum)

   use stdlit
   use string_utils
   implicit none

   character(*), intent (in)  :: string
   integer,      intent (out) :: y, m, d, daynum

! Parameters.  (This substitutes for date_format.h.)

   character(1), parameter :: date_delim = '-'

   character(*), parameter :: &
      months = 'jan feb mar apr may jun jul aug sep oct nov dec '
		! must be all lowercase, one space after each abbreviation

   integer, parameter :: ypos = 1, mpos = 2, dpos = 3

! Local variables.

   character mmm*4			! month abbreviation

   integer last_nb, de1, de2		! local pointers into date string
   integer yf, yl, mf, ml, df, dl
   integer first(3), last(3)	! ptrs for undetermined var-len fields

   integer ylen, mlen, dlen
   integer i, first_yy

! More date_format.h substitutions when variables are needed
! instead of constants.

   integer year_size
   integer century_base
   logical date_lead_blanks

   year_size = 4
   century_base = 1900			! (ignored when year_size=4)
   date_lead_blanks = .true.

! Variable format:  Parse string on delimiters, to get field boundaries.

   last_nb = len_trim (string)

   if (last_nb < 9) then		! check minimum string length
      goto 999

   else

      de1 = index (string, date_delim, forward)
      if (de1 < 2) goto 999		! check 1st delim & 1st field > 0 chars

      de2 = index (string(de1+1:last_nb-1), date_delim, forward)
      if (de2 < 2) goto 999		! note: de2 = offset from de1
	   				! check 2nd delim, also check
	   				!  2nd and 3rd fields > 0 chars

      first(1) = 1			! compute field bounds from delimiters
      last(1)  = de1-1
      first(2) = de1+1
      last(2)  = de1+de2-1
      first(3) = de1+de2+1
      last(3)  = last_nb

      yf = first(ypos)			! now assign field bounds based on order
      yl = last(ypos)
      mf = first(mpos)
      ml = last(mpos)
      df = first(dpos)
      dl = last(dpos)

   end if

! Pass leading blanks if allowed.

   if (date_lead_blanks) then

      do while (yf < yl .and. string(yf:yf) == ' ')
         yf = yf + 1			! don't reduce to zero chars
      end do

      do while (mf < ml .and. string(mf:mf) == ' ')
         mf = mf + 1
      end do

      do while (df < dl .and. string(df:df) == ' ')
         df = df + 1
      end do

   end if

! Check for expected number of characters.

   ylen = yl - yf + 1			! compute number of chars
   mlen = ml - mf + 1
   dlen = dl - df + 1

   if (year_size /= 0 .and. ylen /= year_size) goto 999
		! Allow any number of year digits in variable-year-size mode.

   if (mlen /= 3) goto 999
   if (dlen >  2) goto 999

! Convert year and day from strings to integers.

   ! The following calls require the strings to be unsigned integers
   ! or zero, no blanks allowed anywhere, no other special chars.
   ! -1 is returned for any string not meeting these rules.

   call string_to_intu (string(yf:yl), y)
   call string_to_intu (string(df:dl), d)

   if (y < 0) goto 999			! check year for valid format

! Convert month from abbreviation string to integer.

   mmm = string(mf:ml)			! mmm must be length 4, 3-abbreviation
   call lowercase (mmm)			!  plus trailing blank!
   m = index (months, mmm, forward)	! find in list of abbreviations
   if (mod (m, 4) /= 1) goto 999	! reject if not in list
   m = (m+3) / 4			! convert 1, 5, 9...45 to 1...12

! 2-digit years: Add in the correct century to get the complete 4-digit year.

   if ( (year_size == 2) .or. (year_size == 0 .and. y <= 99) ) then

      first_yy = mod (century_base, 100)
      if (y < first_yy) y = y + 100	! carry into next century
      y = y + (century_base - first_yy)

   end if

! Check day for valid format and range.

   if (d < 1 .or. d > days_in_month (y,m) ) goto 999

! Compute day number from start of year.

   daynum = d
   i = 1
   do while (i < m)			! add days of preceeding months
      daynum = daynum + days_in_month(y,i)
      i = i + 1
   end do

   return				! normal return, date is good

! All errors come here.

999 y = 0				! zero output signals invalid date
   m = 0				! note: check m or d, y=0 is valid
   d = 0
   daynum = 0

   return				! error return

end subroutine date_ymd2_to_int
