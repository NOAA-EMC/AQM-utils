!-----------------------------------------------------------------------------
!
! parse_time_hh_mm -- Convert time in HH:MM format to minutes.
!
! 2008-feb-12	Original version, by Dave Allured.
!		Handle standard time format with colon, HH:MMx (x = a/p).
!
! Strict version.  Leading zero prohibited except in non-suffix mode
! (suf_req = false).  Minutes must be 2 digits.  Colon required.
! No leaing or trailing blanks.  No extraneous characters.
!
! Two modes of operation:
!
! suff_req = true:   Suffix required.  1:00a to 12:59a (24 hours).
! suff_req = false:  Suffix prohibited.  0:00 to 23:59 (24 hours).
!
! In either mode, the returned time is 24 hour time, in minutes since
! 00:00.  Midnight times are handled correctly (12:00a to 12:59a).
!
! Error checking is imperfect.  Some numeric errors in HH or MM may
! cause Fortran runtime error and abort.
!
!-----------------------------------------------------------------------------

module parse_time_hh_mm_mod
contains

subroutine parse_time_hh_mm (string, suff_req, div5, mins_total, suffix, &
      status, detail)

   implicit none
   
   character*(*), intent (in ) :: string	! input time string
   logical,       intent (in ) :: suff_req	! suffix mode, see above
   logical,       intent (in ) :: div5		! true = time must end in 0 or 5
   integer,       intent (out) :: mins_total	! time out, minutes since 00:00
   character*(*), intent (out) :: suffix		! suffix a/p, suf_req = T only
   logical,       intent (out) :: status	! true = good, false = error
   character*(*), intent (out) :: detail	! errir description string

! Local variables.

   character*1 hfirst, mlast, h1_min
   
   integer ph2, pcolon, pm1, pm2, plast		! substring pointers into HH:MMx
   integer hours, mins, pm_offset, hh_max

! Configure to mode, and get the substring pointers.

   plast  = len (string)			! point to final char

   if (suff_req) then			! config for a/p suffix, 12 hour time
      pm2 = plast - 1
      h1_min = '1'
      hh_max = 12
   
   else					! config for no suffix, 24 hour time
      pm2 = plast
      h1_min = '0'
      hh_max = 23
   end if
   
   ph2    = pm2 - 3				! get substring pointers
   pcolon = pm2 - 2
   pm1    = pm2 - 1

! Check length of time string.
   
   if (ph2 < 1 .or. ph2 > 2) then		! check length of first part HH
      status = .false.
      detail = 'Invalid time, too many or too few digits.'
      return
   end if

! Character mode checks.  Validate HH:MMp time format.
   
   hfirst = string(1:1)
   
   if (hfirst < h1_min .or. hfirst > '9') then
      status = .false.
      detail = 'Hours must start with ' // h1_min // '-9.'	! e.g. 1-9.
      return
   end if
   
   if (string(pcolon:pcolon) /= ':') then
      status = .false.
      detail = 'Missing colon in time string.'
      return
   end if

! Validate AM/PM suffix, if required.
   
   suffix = string(plast:plast)			! get suffix char, a or p
   pm_offset = 0				! default for 24 hour mode
   				
   if (suff_req) then				! suffix mode only:
   
      if (suffix == 'a') then			! adjust for AM suffix (a)
         pm_offset = 0
   
      else if (suffix == 'p') then		! adjust for PM suffix (p)
         pm_offset = 12

      else
         detail = 'Invalid suffix character, must be a or p.'
         status = .false.
         return
      end if
      
   end if

! If selected, require minutes to end in 0 or 5.
   
   mlast = string(pm2:pm2)
   
   if (div5 .and. mlast /= '0' .and. mlast /= '5') then
      status = .false.
      detail = 'Minutes must end in 0 or 5.'
      return
   end if

! Validate hours and minutes.
   
   read (string(1:ph2), *) hours		! convert to integers
   read (string(pm1:pm2), *) mins
   
   if (hours < 0 .or. hours > hh_max .or. mins < 0 .or. mins > 59) then
      status = .false.
      detail = 'Invalid hours or minutes.'
      return
   end if

! Convert AM/PM times to 24 hour scale.

   if (suff_req .and. hours == 12) hours = 0	! convert 12:xx times to 0:xx
   
   hours = hours + pm_offset		! adjust for AM/PM; okay for both modes
   
   mins_total = hours*60 + mins			! final time, minutes since 0:00
   status = .true.				! normal return

end subroutine parse_time_hh_mm

end module parse_time_hh_mm_mod
