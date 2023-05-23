!---------------------------------------------------------------------------
!
! anglical_greater -- Compare two ASCII strings, ignore case and accent marks.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2001-jul-31	Initial version, adapted from anglical-match v1.00
!
! 3.00	2014-apr-23	Fortran module compatibility.  Add intents, etc.
!			Container module is now needed to reach "anglical".
!
! input:	two character strings of any length
!
! output:	function value = true if first string is lexically greater
!		    than second string, false if less than or equal
!
! notes:	The two strings need not be the same length.
!		Leading spaces are significant.  Trailing spaces are
!		NOT significant.
!
!		The implicit ASCII collating sequence applies, using
!		standard lowercase values (hex 61-7A) in place of
!		uppercase and accented alphas.  Numerics and special
!		characters are compared in their normal ASCII sequence.
!
!		If you dare to compare strings with control characters,
!		they will be compared using their actual ASCII collating
!		values.  This may give unexpected results if a control
!		character below hex 20 is compared with a space character.
!
!		The external function "anglical" converts all alphabetics
!		to lowercase a-z, including accented ISO characters.
!
!---------------------------------------------------------------------------

logical function anglical_greater (str1, str2)
	implicit none

	character(*), intent(in) :: str1, str2

	character a, b
	integer i, alen, blen, cmp_len

	alen = len_trim (str1)			! compare from start to
	blen = len_trim (str2)			! last non-blank char
	cmp_len = max (alen, blen)		! of longer string

	if (cmp_len > 0) then
	   do i = 1, cmp_len			! scan through strings...

	      if (i <= alen) then		! lowercase and pad spaces
	         a = anglical(str1(i:i))	! to right of first string
	      else
	         a = ' '
	      end if

	      if (i <= blen) then		! lowercase and pad spaces
	         b = anglical(str2(i:i))	! to right of second string
	      else
	         b = ' '
	      end if

	      if (a /= b) then			! find first non-matching
	         if (a > b) then		! pair of characters, and
	            anglical_greater = .true.	! make the comparison result
	            return
	         else
	            anglical_greater = .false.
	            return
	         end if
	      end if

	   end do
	end if

	anglical_greater = .false.		! if equal strings,
						! result = "not greater"
end function anglical_greater
