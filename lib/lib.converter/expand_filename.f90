!------------------------------------------------------------------------------
!
! expand_filename.f90 -- Make a file name from time parameters and a template.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-apr-24	expand_filename.f90:
!		Original version.  By Dave Allured.
!
! 2022-apr-10	Bias correction version.
!		Change the substitution code for "cycle time" from HH to CC.
!
! 2023-jan-31	General purpose version.
!		Add new keyword arguments for hour (HH), cycle time (CC),
!		  and forecast hour (FF and FFF).
!		Include backward compatible version with four positional args
!		  including CC = cycle time.
!		Add crash proofing for field overflows.
!	`	Also do not substitute negative numbers.
!
! Notes:
!
! This routine is somewhat generalized for the bias correction
! project.  It may be used for different purposes by several
! other routines.
!
! The output variable "outname" must be long enough to contain
! both the unexpanded and expanded template strings.  No checking
! for truncation is done.
!
! In this version, string substitution is NOT recursive.  This
! is a one-pass processor.
!
!------------------------------------------------------------------------------

module expand__filename
   private				! visibility
   public expand_filename

   interface expand_filename		! generic interface for two styles
      module procedure &
         expand_filename1, &
         expand_filename2
   end interface expand_filename

contains

!------------------------------------------------------------------------------
! Original 2022 version.  Fixed args, YYYY MM DD CC.
!------------------------------------------------------------------------------

subroutine expand_filename1 (template, year, month, day, cycle_time1, outname)

   use string_utils
   implicit none

   character(*), intent(in ) :: template
   integer,      intent(in ) :: year, month, day
   integer,      intent(in ) :: cycle_time1

   character(*), intent(out) :: outname

   character ystr*4, mstr*2, dstr*2, cstr*2	! fixed-width strings
   integer er1, er2, er3, er4

! Expand environment variables.

! Note, the current version of resolve_env only handles a single
! environment variable at the beginning of the template.  Check
! with the latest version for support status.

   outname = template			! start with writable copy of template
   call resolve_env (outname)

! Make fixed-width number strings with leading zeros.

   write (ystr, '(i4.4)', iostat=er1) year
   write (mstr, '(i2.2)', iostat=er2) month
   write (dstr, '(i2.2)', iostat=er3) day
   write (cstr, '(i2.2)', iostat=er4) cycle_time1

! Replace code strings in template.
! Use repeat count 99 for multiple replacements.

   if ( (year        >= 0) .and. (er1 == 0) ) &
      call replace_substring (outname, 'YYYY', ystr, rep_count=99)

   if ( (month       >= 0) .and. (er2 == 0) ) &
      call replace_substring (outname, 'MM',   mstr, rep_count=99)

   if ( (day         >= 0) .and. (er3 == 0) ) &
      call replace_substring (outname, 'DD',   dstr, rep_count=99)

   if ( (cycle_time1 >= 0) .and. (er4 == 0) ) &
      call replace_substring (outname, 'CC',   cstr, rep_count=99)

end subroutine expand_filename1

!------------------------------------------------------------------------------
! New 2023 version.  Reordered, and keyword args for HH CC FF/FFF.
!------------------------------------------------------------------------------

subroutine expand_filename2 (template, outname, year, month, day, hour, &
      cycle_time, forecast_hour)

   use string_utils
   implicit none

   character(*),      intent (in ) :: template		! normal args
   character(*),      intent (out) :: outname
   integer,           intent (in ) :: year, month, day

   integer, optional, intent (in ) :: hour		! keyword args
   integer, optional, intent (in ) :: cycle_time
   integer, optional, intent (in ) :: forecast_hour

   character ystr*4, mstr*2, dstr*2, hstr*2	! fixed-width strings
   character cstr*2, f2str*2, f3str*3
   integer er1, er2, er3, er4, er5, er6, er7

! Expand environment variables.

! Note, the current version of resolve_env only handles a single
! environment variable at the beginning of the template.  Check
! with the latest version for support status.

   outname = template			! start with writable copy of template
   call resolve_env (outname)

! Make fixed-width number strings with leading zeros.

   write (ystr, '(i4.4)', iostat=er1) year
   write (mstr, '(i2.2)', iostat=er2) month
   write (dstr, '(i2.2)', iostat=er3) day

! Replace code strings in template.
! Use repeat count 99 for multiple replacements.

   if ( (year  >= 0) .and. (er1 == 0) ) &
      call replace_substring (outname, 'YYYY', ystr, rep_count=99)

   if ( (month >= 0) .and. (er2 == 0) ) &
      call replace_substring (outname, 'MM',   mstr, rep_count=99)

   if ( (day   >= 0) .and. (er3 == 0) ) &
      call replace_substring (outname, 'DD',   dstr, rep_count=99)

! Handle optional arguments.

   if (present (hour)) then
      write (hstr, '(i2.2)', iostat=er4) hour
      if ( (hour       >= 0) .and. (er4 == 0) ) &
         call replace_substring (outname, 'HH', hstr, rep_count=99)
   end if

   if (present (cycle_time)) then
      write (cstr, '(i2.2)', iostat=er5) cycle_time
      if ( (cycle_time >= 0) .and. (er5 == 0) ) &
         call replace_substring (outname, 'CC', cstr, rep_count=99)
   end if

   if (present (forecast_hour)) then		! FFF takes precedence over FF
      write (f3str, '(i3.3)', iostat=er6) forecast_hour
      write (f2str, '(i2.2)', iostat=er7) forecast_hour

      if ( (forecast_hour >= 0) .and. (er6 == 0) ) &
         call replace_substring (outname, 'FFF', f3str, rep_count=99)

      if ( (forecast_hour >= 0) .and. (er6 == 0) .and. (er7 == 0) ) &
         call replace_substring (outname, 'FF',  f2str, rep_count=99)
         				! also FFF error blocks FF substitution
   end if

end subroutine expand_filename2

end module expand__filename
