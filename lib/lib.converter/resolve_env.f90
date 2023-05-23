!----------------------------------------------------------------------------
!
! resolve_env -- Resolve environment variables in a path or general string.
!
! 1.00	2008-dec-17	Original hasty version.  By Dave Allured.
!			This version handles only a single environment
!			  variable at the beginning of the string.
! 1.01	2010-may-04	Bug fix for single env. var. with no trailing slash.
! 1.02	2010-dec-16	Fix compiler warning for unused future parameter.
!
! This routine substitutes environment variables in a path name
! or general string.  Any number of environment variables may
! be included.  Environment variables are specified in the usual
! Unix way as $name.
!
! Variable syntax and substituion are similar to the Unix shell, but
! simpler.  Only a few delimiters and syntax features are supported.
!
! Substitution is done recursively.  If one substitution contains
! new references to environment variables, then those will also be
! resolved.
!
! A reference to an undefined environment variable is left unchanged.
! This will result in a dollar sign ($) left in the output string.
!
! Errors result in diagnostic message and program abort.  The only
! known error condition is string capacity exceeded.  The argument
! string must be long enough to hold the fully resolved result string.
!
!----------------------------------------------------------------------------

module resolve__env
contains

subroutine resolve_env (string)
   implicit none
   
   character(*), intent (inout) :: string	! string with $var's;
   						! overwritten with result string

! Local parameters and variables.

!!   For a later version:
!!   character(*), parameter :: delimiters = '$ !*./,;[]{}"'
   				! supported delimiters to terminate the var name

   character (len=len(string)) :: varname, value, temp
   integer remain, outlen, base, p2, len2

! Initialize.

   remain = len_trim (string)			! start at full non-blank length
   						!   of input string
   outlen = len (string)			! get available string length
   
   base = 1				! set base pointer to start of string

! *** QUICK FIX -- RESOLVE ONLY A SINGLE LEADING ENVIRONMENT VARIABLE ***

! E.G.   $varname/xxx     RESOLVES TO     value/xxx

   p2 = index (string, '/')			! find first slash in string
   
   if (p2 == 0) p2 = len_trim (string) + 1	! handle no trailing slash
   
   if (string(1:1) /= '$' .or. p2 < 3) return	! skip if no leading env var
   
   varname = string(2:p2-1)		! extract varname from "$varname/"
   call getenv (trim (varname), value)	! get value for this env var

! If environment var exists, then make the substitution.

   if (value /= ' ') then
      value = adjustl (value)		! no funny business with leading spaces
      len2 = len_trim (value)		! get real length of substring
      temp = string(p2:)		! get slash plus remainder of string
      string = value			! insert substitution at beginning
      string(len2+1:) = temp		! insert remainder including slash
   end if				

! If substitution NOT made, then the original string is left intact.

end subroutine resolve_env

end module resolve__env
