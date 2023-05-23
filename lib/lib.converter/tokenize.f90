!-----------------------------------------------------------------------------
!
! tokenize.f90 -- Parse line of text into list of names and delimiters.
!
! This is a simplistic, general-application expression parser.
!
! Its original application is for the NCEP/PSL bias correction system.
! It is used to parse formula expressions in some of the program
! configuration files.
!
! 2022-apr-16	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
! This is a low level token parser that does not apply any
! sequence rules or higher level understanding to the token
! sequence.  Higher level interpretation is left up to the caller.
!
! Caller supplies a line of text, and a list of valid delimiter
! characters.
!
! The line is broken up into a list of consecutive tokens, which
! are returned to the caller as the primary output.  The output
! array is auto-allocated.
!
! The secondary output is a list of one-character codes of token
! types.  Names are returned as the letter "n", and delimiters
! are returned as themselves.  In other words, a pattern string
! is returned.  This simplifies interpretation and validation.
!
!     Pattern example for summing:        n=n+n+n
!     Pattern example for function call:  n=n(n,n)
!
! If the input string is all spaces, then the returned token is
! a single token containing all spaces, and the returned pattern
! is also all spaces.
!
!-----------------------------------------------------------------------------
!
! Parsing rules:
!
! * Valid tokens consist of names and delimiters.  There are no
!   other possibilities.
!
! * A break between tokens is signalled by any listed delimiter,
!   or by a space character.
!
! * All delimiters except for spaces are treated as single-
!   character tokens.  Each delimiter occurrence is included in
!   the output token list, even if consecutive.
!
! * Spaces are treated specially.  They are never returned as
!   tokens.
!
! * Leading and trailing spaces are ignored when adjacent to
!   other tokens.
!
! * However, spaces between two names, with no other delimiter,
!   are treated as a break between two name tokens.
!
! * In this case, two consecutive name tokens are output, with
!   no intervening delimiter token.
!
! * No quoting is currently supported.
!
! * There is no special processing or detection for comments,
!   control characters, or other strange characters.
!
! * Every character in the fortran input string is processed
!   literally, according to the simple parsing rules above.
!
! * A consequence of these simple rules is that space and all
!   listed delimiter characters are prohibited within names.
!
!-----------------------------------------------------------------------------

module tokenize_mod
contains

subroutine tokenize (line, delimiters, tokens, pattern)
   implicit none

   character(*), intent(in)               :: line
   character(*), intent(in)               :: delimiters

   character(*), intent(out), allocatable :: tokens(:)
   character(*), intent(out)              :: pattern

! Local variables.

   character char*1
   integer p, p1, p2, line_len			! char pointers into line
   integer ti, ntokens, out_size

   integer, allocatable :: starts(:), ends(:)

   logical is_space, is_delim

! Initialize.

   line_len = len_trim (line)

   p       = 1					! point to first char in line
   ti      = 0					! init token list
   pattern = ' '				! empty pattern string to start

   allocate (starts(line_len), ends(line_len))	! max possible no. of tokens

! Outer loop -- Skip spaces, find start of next token.

outer_loop: &
   do while (p <= line_len)
      char = line(p:p)				! get next character in line

      if (char == ' ') then			! space: keep searching
         p = p + 1				!   for start of next token
         cycle outer_loop
      end if

      is_delim = (index (delimiters, char) /= 0)    ! is char a delimiter?

      if (is_delim) then			! yes, output DELIMITER token
         ti             = ti + 1
         starts(ti)     = p			! output single delimiter char.
         ends(ti)       = p
         pattern(ti:ti) = char			! output same char to pattern
         p              = p + 1			! advance to next input char
         cycle outer_loop			! go find start of next token
      end if

! Any other character is start of a NAME token.
! On entry, this is always the start of a NAME token.  No exceptions.
! This is also always a non-blank.

      ti             = ti + 1			! begin output for NAME token
      starts(ti)     = p			! remember start pointer
      pattern(ti:ti) = 'n'			! output pattern for NAME token

! Inner loop -- Find end of NAME token.

! Note -- This loop always finishes the last name token, never leaves
! an incomplete one dangling at end of line.

inner_loop: &
      do
         char     = line(p:p)			! get current character in line
         is_space = (char == ' ')		! classify
         is_delim = (index (delimiters, char) /= 0)

         if (is_space .or. is_delim) then	! end NAME token on delimiter?
            ends(ti) = p - 1			! yes, end name on PREVIOUS char
            cycle outer_loop			! DO NOT ADVANCE, and go process
						! current token breaking char.

         else if (p >= line_len) then		! final character in line?
            ends(ti) = p			! yes, end name on CURRENT char
            exit outer_loop			! and EXIT ALL scanning
         end if

         p = p + 1			! otherwise advance to next char, and
      end do inner_loop			! keep searching for end of name token

   end do outer_loop

! End of line.  All parsing is complete.

! Now convert token pointer lists to final output list.

   ntokens = ti
   out_size = max (ntokens, 1)		! handle all spaces on input

   allocate (tokens(out_size))		! allocate variable size output array

   tokens(1) = ' '			! output one space if all input spaces

   do ti = 1, ntokens
      p1         = starts(ti)		! copy each token from input line
      p2         = ends(ti)		! to output array
      tokens(ti) = line(p1:P2)
   end do

end subroutine tokenize
end module tokenize_mod
