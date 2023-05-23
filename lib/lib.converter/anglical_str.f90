!-----------------------------------------------------------------------------
!
! anglical_str -- Convert an ASCII character string to standard lowercase a-z.
!
! Remove diacritical marks (accent marks).  Convert uppercase to lower.
! Non-alpha characters are left unchanged.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2004-feb-06	Initial version.  Adapted from anglical.f90 v2.00.
!			High efficiency.  Includes built-in lookup strings
!			to avoid iterated calls to function "anglical".
! 1.01	2006-feb-08	Minor syntax fix for XLF compiler.
! 1.02	2009-dec-29	Fix long standing string length bug in interface.
!			Present since v1.00.
!
!-----------------------------------------------------------------------------

subroutine anglical_str (str)

   implicit none
   
   character(*), intent(inout) :: str	! ASCII character string, possibly
   					! incl. uppercase and diacritical chars.
   					! Output string overwrites input string.
! Declarations.

   character (len=*), parameter :: &
      accent='ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ', &
      normal='aaaaaaceeeeiiiinoooooouuuuyaaaaaaceeeeiiiionoooooouuuuyy'
   
   integer, parameter :: lc_offset = ichar('a') - ichar('A')
   
   character c
   integer i, slen, j

! Convert one char at a time.  Overwrite only chars to be changed.
   
   slen = len_trim (str)		

   do i = 1, slen
      c = str(i:i)				! get next char to investigate
   
      if (c >= 'A' .and. c <= 'Z') then		! normal uppercase A-Z?
         str(i:i) = char(ichar(c) + lc_offset)	! yes, convert to std lowercase
	
      else
         j = index (accent, c)			! in u/l diacritical char list?
         if (j > 0) then
            str(i:i) = normal(j:j)		! yes, convert to std lowercase
         end if
      
      end if					! all other chars: no change
   end do

end subroutine anglical_str
