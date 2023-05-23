!---------------------------------------------------------------------------
!
! remove_quotes -- Remove valid quote characters from a character string.
!
! Rev	Date		Notes
! 1.00	2011-feb-14	Original version.  By Dave Allured.
!
! Notes:
!
! This routine checks the input string for balanced quotes, then
! returns the de-quoted string.  The input string is overwritten
! with the de-quoted result string.
!
! Leading and trailing spaces inside the original quotes are
! preserved.  Spaces outside of the quotes are implicitly
! discarded.
!
! The strlen argument indicates the active string length for both
! input and output.  This permits the null string for input and
! output, as well as retaining possible trailing blanks.
!
! valid_quote_chars specifies one or more non-blank characters
! to accept as valid quote characters.  
!
! On any quoting problem, the original string and length are
! returned without modification.
!
! Characters outside of the input length "strlen" are ignored
! and never cleared.  For best results, clear this part of the
! input string in advance.
!
!---------------------------------------------------------------------------

subroutine remove_quotes (string, strlen, valid_quote_chars)

   use stdlit
   implicit none

   character(*), intent (inout) :: string
   integer,      intent (inout) :: strlen
   character(*), intent (in)    :: valid_quote_chars
   
   character qchar*1
   integer q1, q2, oldlen

! Validate the final quote character.

   if (strlen < 2) return		! string too short, no change

   q2 = len_trim (string(1:strlen))	! find last non-space in active string

   if (q2 < 2) return			! active string too short, no change

   qchar = string(q2:q2)		! quote char must be in the valid set
   if (index (valid_quote_chars, qchar) == 0) return

! Validate the leading quote character.

   q1 = index (string(1:q2-1), qchar)	! find first quote char

   if (q1 == 0) return			! missing first quote, no change
   
   if (q1 > 1) then
      if (string(1:q1-1) /= ' ') return  ! does not start with quote, no change
   end if
   
   if (q2 - q1 > 1) then		! if one or more chars inside quotes...
      if (index (string(q1+1:q2-1), qchar) > 0) return
   end if				! third quote is invalid, no change

! Quotes are properly balanced.  Remove quotes, return only the inside string.

   oldlen = strlen
   strlen = q2 - q1 + 1			! new length = substring within quotes

   if (strlen == 0) then
      string(1:oldlen) = ' '		! special handling, return null string

   else					! normal, 1 or more chars within quotes:
      string(1:oldlen) = string(q1+1:q2-1) ! move the inside string; clear the
   					   ! rest of the active string to blanks
   end if				   ! (inactive right side is unchanged)

end subroutine remove_quotes
