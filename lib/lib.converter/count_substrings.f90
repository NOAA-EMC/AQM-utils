!-------------------------------------------------------------------------
!
! Count number of substrings for a list-directed internal read.
!
! 2014-feb-13	Original version.  By Dave Allured.
!
! This is a support function for simple line parsing using the
! fortran list-directed, i.e. free-format, read facility.  Use
! this functon to simplify reading strings with unknown numbers
! of elements.  The resulting item count may be used to allocate
! input arrays, etc.
!
! List-directed input rules, a simple subset, F95 and above:
!
! * Both character strings and numbers may be read.
! * Items are delimited by spaces or a comma.
! * Leading and trailing blanks around items are ignored.
! * Character string items may be unquoted, or optionally
!   enclosed within single or double quotes.
! * Quoting will preserve embedded spaces and delimiters.
! * Blank or null substrings are a problem.  The result is that
!   the positions are counted, but the corresponding variables
!   in the input list are not updated.  Therefore, pre-
!   initialization is mandatory.
! * Embedded newlines: UNKNOWN EFFECT.
!
! The full set of rules is much more involved.  Slash and
! asterisk have special meaning, avoid list-directed for these.
! See Fortran 95 standards or higher, for the definitive rules.
!
! This version handles blank strings correctly, result count = 0.
!
! This version supports only a single character variable,
! i.e. string, on input.
!
! This version uses inefficient methods, so is intended for small
! jobs only.  Run time is worse than O2, so it will bog down for
! very long lines.  For real data processing, use a real parser
! such as get_token.f90, parse_delimited.f90, gsc.f90, or
! something commercial.
!
!-------------------------------------------------------------------------

integer function count_substrings (string) result (nitems)
  implicit none

  character(*), intent (in) :: string	! line of text to parse

  integer i, j, max_items, ios		! local variables

  character item*1			! dummy read item; var length does
					! not matter

  max_items = (1 + len (string)) / 2	! maximum possible number of elements,
  					! assuming at least one delimiter
                                        ! between elements

  nitems = 0			! cheap trick to count number of substrings,
  do i = 1, max_items		! but guarantees that the count is repeatable
    read (string, *, iostat=ios) (item, j = 1, nitems+1)
    if (ios /= 0) exit
    nitems = nitems + 1
  end do

end function count_substrings
