!------------------------------------------------------------------------------
!
! replace_substring -- Replace one or more substrings with substitute text.
!
! 1.00	2012-apr-26	Original version.  By Dave Allured.
!
! 2.00	2014-apr-24	Add option to replace all occurrences in one call.
!			Requires module interface via string_utils.f90.
! 2.01	2017-apr-04	Comment fix only.
!
! This routine finds one or more occurrences of a given substring
! within a main string.  If found, the occurrences are replaced
! with the substitute string.  Substrings of varying lengths are
! handled correctly.
!
! Default behavior is to replace the first (leftmost) occurrence
! only.  Multiple replacements, left to right, may be specified
! with the optional argument rep_count.
!
! Multiple replacements are made in only one pass from left to
! right through the main string.  Therefore, replacements are not
! recursive.
!
! The original main input string is overwritten.  The main string
! is not changed if the search substring is not found.
!
! Since the actual argument for the main string is overwritten, it
! must be a valid reference to a character variable in memory.
!
! The two substring arguments, "search" and "replace", must be
! exact lengths with no undesired leading or trailing spaces.
! These two actual arguments may be function results if needed,
! e.g. trim (replace).
!
! The main string variable must be long enough to contain the
! modified result string.  If it is not, then trailing characters
! will be safely truncated.  This version does not check for
! truncation.
!
!------------------------------------------------------------------------------

subroutine replace_substring (main, search, replace, rep_count)
   implicit none

   character(*),   intent (inout) :: main	! main string to be modified
   character(*),   intent (in   ) :: search	! search substring, exact length
   character(*),   intent (in   ) :: replace	! replacement substring, exact
   integer, optional, intent (in) :: rep_count	! maximum number of replacements
   						! defaults to 1
! Local variables.

   integer p1				! char start position for next search
   integer p2				! start position for current replacement
   integer p3				! start of remaining characters to keep
   integer nrep				! replacement counter
   integer nmax				! maximum number of replacements

! Handle optional count argument.

   nmax = 1				! default to only one replacement

   if (present (rep_count)) nmax = rep_count

! Main loop for each replacement.

   p1 = 1				! point to start of string
   nrep = 0				! init replacement counter

   do while (nrep < nmax)		! loop to max number of replacements

      p2 = p1 - 1 + index (main(p1:), search)	! find next occurrence
      						! of search string

      if (p2 < p1) return		! no more occurrences, all done

! Found search string.  Insert replacement into main string,
! move following text as needed.

      p3 = p2 + len (search)	  	! start of remainder to keep
      main(p2:) = replace // main(p3:)	! overwrite the substution string
					! and retain the following characters

      p1 = p2 + len (replace)	      ! start next search AFTER the replacement
      nrep = nrep + 1		      ! count this replacement
   end do

end subroutine replace_substring
