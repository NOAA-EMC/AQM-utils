!----------------------------------------------------------------------------
!
! anglical -- Convert an ASCII alpha character to standard lowercase a-z.
!             Remove ISO-8859 diacritical marks (accent marks).
!             Convert uppercase to lower.
!
! Used for generic "anglical" string comparison.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-jun-01	Initial version
! 1.01	2000-dec-29	Fortran 90 compatibility:
!			Continuation, string constants, implicit none
! 2.00	2002-jul-17	F90 free format
!
! 3.00	2014-apr-23	Fortran module compatibility.  Add intents, etc.
!
! input:	one ASCII/ISO-8859-1 character
!
! output:	one ASCII/ISO-8859-1 character, standard alphabetic value
!
! Note:		All characters that are not alpha or alpha with accent mark
!		are returned without conversion.
!
!----------------------------------------------------------------------------

function anglical (c) result (out_c)
   implicit none

   character(1), intent(in) :: c	! input arg, single character
   character(1) out_c			! function result, single char

! Declarations

   character(*), parameter :: &
      accent='ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖØÙÚÛÜİàáâãäåçèéêëìíîïğñòóôõöøùúûüıÿ', &
      normal='aaaaaaceeeeiiiinoooooouuuuyaaaaaaceeeeiiiionoooooouuuuyy'
!             123456789 123456789 123456789 123456789 123456789 123456

   integer i

! Executables

   if (c.ge.'A' .and. c.le.'Z') then	! convert std upper to lower
      out_c = char(ichar(c) - ichar('A') + ichar('a'))

   else
      i = index(accent, c)		! convert u/l accent to std lower
      if (i.gt.0) then
         out_c = normal(i:i)
      else
         out_c = c			! return all others unchanged
      end if
   end if

end function anglical
