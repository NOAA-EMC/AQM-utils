!----------------------------------------------------------------------------
!
! parse_delimited -- Parse a line of delimited text into fields.
!
! For general application.
!
! Rev	Date		Notes
! 1.00	2001-jan-02	Original version.  By Dave Allured.
! 1.01	2001-jan-18	Remove all handling for EOL characters.
!			This is now assumed to be handled by caller.
!
! 2.00	2008-apr-24	Fortran 90 interface.  Add module definition.
!			This achieves proper bounds checking for arrays.
!
! 3.00	2011-mar-16	Add proper support for quoted fields; interface change.
!			Allow the main delimiter to be protected within quotes.
!			Support two consecutive quotes as an escape sequence,
!			  without terminating the quoted field.
!			Support multiple quote characters.
! 3.01	2011-mar-17	Simplify where null substrings are allowed.
!			Fix comments related to null substrings.
!
! Input, output:  See calling args below.
!
! Usage:
!
! Call with a line of text in "line".
! The individual fields are not actually broken out.
! Instead, two arrays containing the first and last
! character positions of each field are returned.
!
! Notes:
!
! End-of-line characters such as CR (carriage return) must be
! handled by the calling program.  An EOL character will be
! regarded here as part of the last field.
!
! For purposes of this parser, all lines are considered
! to end at the last non-space character, if any.
!
! Whenever BOL, delimiters, and/or EOL are adjacent,
! the result is null fields.  Null fields are indicated
! by last(n) < first(n).
!
! A blank or null line returns as a single null field.
!
! When the first or last fields are null strings, one of the
! returned field indices will be OUTSIDE of the range 1 to
! len_trim (line).
!
! The contents of first() and last() beyond n_fields
! are undefined.
!
! Range checking is not done.  The caller must ensure that
! output array dimensions are sufficient for any possible
! line of text.
!
! Quoting rules:
!
! * Field quoting is always optional, in this version.
!
! * Blanks before the leading quote, or after the trailing quote,
!   are allowed.
!
! * Within each field, all active quotes must be the same character.
!
! * Within each field, all quotes must be balanced.
!
! * A quoted field protects any included field delimiters.
!
! * Within a single quoted field, two consecutive quotes comprise
!   one complete escape sequence, and do not interrupt the field.
!
! * Within a single quoted field, multiple escape sequences are
!   allowed.
!
! * If any rule is violated, then all quotes are ignored, and the
!   field is re-parsed from the beginning to the first delimiter.
!
! Optimization:
!
! This version is optimized for mostly non-quoted fields, and no
! leading spaces.
!
! Best speedup is obtained by not including excess trailing
! blanks in the input line.
!
!----------------------------------------------------------------------------

module parse_delimited_mod
contains

subroutine parse_delimited (line, delim, valid_quotes, first, last, n_fields)
   
   implicit none
   character(*), intent (in ) :: line		! input text line to be parsed;
   						!   null string is allowed
   character(1), intent (in ) :: delim		! specified field delimiter
   character(*), intent (in ) :: valid_quotes   ! allowed quote characters;
   						!   null = no quote handling
						! e.g. ["] or ["'] or []
						! blanks not valid, are ignored

   integer,      intent (out) :: first(:)	! first char pos of each field
   integer,      intent (out) :: last(:)	! last char pos of each field
   integer,      intent (out) :: n_fields	! number of fields found
        					!   (always one or more)
! Local variables.

   character qch*1			! quote character for current field
   
   integer p				! char pointer within line
   integer j				! char offset when using index()
   integer eol				! pointer to last character in line
   integer fi				! field index into arrays
   
   logical valid			! T = valid quoting, F = rule violation

! Note:  This method always generates the correct output for null
! fields in any position, including start or end of line.
! A null field is indicated by last(n) < first(n).

   p = 1				! point to first character in line
   eol = len_trim (line)		! find last non-space char in line;
   					!   will be zero for blank or null lines
   fi = 0				! point to first field

!-----------------------------------------------------------------------
! Main field loop.  Do for each delimiter found, plus start of line...
!-----------------------------------------------------------------------

field_loop: &
   do
      fi = fi + 1			! save first char position in field
      first(fi) = p			! and start scanning here

      if (p > eol) exit field_loop	! all done: null field at end of line

! Scan past leading spaces in advance of a possible quoted field.
      
      do while (line(p:p) == ' ')	! scan past leading spaces, if any
         p = p + 1
         if (p > eol) exit field_loop	! all done: all blanks at end of line
      end do

! Now p points to the first non-blank character, NOT past end of line.

!-----------------------------------------------------------------------
! Special handling for QUOTED field starts here.
!-----------------------------------------------------------------------

      qch = line(p:p)			! get first non-blank character;
      					! this locks in the only allowed quote
                                        ! character for the rest of the field
quoted_field: &
      if (index (valid_quotes, qch) > 0) then	! if quote: start a quoted field
						! (okay for valid_quotes = null)

! Handle the main part of the QUOTED FIELD:  "xxx""xxxxx""xx" etc.
! Spaces are NOT allowed within the two-quote escape sequence.

         valid = .true.			! init flag to check for reversion

! At the start of each substring iteration, p points to the leading
! quote of the CURRENT SUBSTRING.

substring_loop: &
         do				! scan through 1 or more quoted substrs
            
            j = index (line(p+1:eol), qch)  ! find the trailing quote
            				    ! (okay for null string, p >= eol)
            
            if (j == 0) then		! if trailing quote is missing...
               valid = .false.		! quotes are unbalanced; 
               exit substring_loop	! revert to unquoted
            end if
            
            p = p + j + 1		  ! point to next char after 2nd quote
            if (p > eol) exit field_loop  ! all done: EOL after second quote
            
            if (line(p:p) /= qch) &	! done unless two consecutive quotes
               exit substring_loop	!   (escape sequence)
         
         end do substring_loop		! loop if two consecutive quotes

! Scan past trailing spaces following a quoted field.

         if (valid) then

            do while (line(p:p) == ' ')      ! scan past trailing spaces, if any
               p = p + 1
               if (p > eol) exit field_loop  ! all done: end of line following
            end do			     !  second quote plus blanks

! Check for valid delimiter at end of quoted field.

            if (line(p:p) == delim) then
               p = p + 1		! point to next char following delimiter
         				! this might be 1 char past end of line
               last(fi) = p - 2		! save last char position in field
               cycle field_loop		! go to next field; next will handle EOL
            end if

            ! *** If we get here, rule violation; field is partially unquoted.
            ! *** I.e. a quoted field with an unquoted suffix.
            ! *** Fall through and revert to unquoted.

         end if

! Main violations of quoting rules fall through here.  Revert to unquoted.

         p = first(fi)			! revert; re-scan the entire field
      					! (inefficient, but also infrequent)
      end if quoted_field

!-----------------------------------------------------------------------
! Handle a NON-QUOTED field, including reversions.
!-----------------------------------------------------------------------

! At this point, p is at or near start of field; may be past EOL.

      j = index (line(p:eol), delim)	! find the next delimiter
      					! (okay for null string, p > eol)
      
      if (j == 0) exit field_loop	! no delimiter, last field in line

      p = p + j				! point to next char following delimiter
         				! this might be 1 char past end of line
      last(fi) = p - 2			! save last char position in field
      
   end do field_loop			! go to next field; next will handle EOL

! Always terminate the last field on the line.
! This handles several special case exits from the main loop.

   last(fi) = eol			! last field always ends at EOL; will be
   					!   null field if delim. is last char.
   n_fields = fi			! also return number of fields

end subroutine parse_delimited

end module parse_delimited_mod
