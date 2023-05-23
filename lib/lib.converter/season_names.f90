!-----------------------------------------------------------------------------
!
! season_names.f90 -- Make automatic season names for plot labels, etc.
!
! 2012-jan-06	season_names.ncl:
!		Original NCL version.  By Dave Allured, NOAA/PSD/CIRES.
!
! 2014-dec-12	season_names.f90:
!		Create fortran 90 version.
!
! This routine creates season name strings such as DJF, MAM, etc.
! for one or more calendar seasons of the specified number of
! months.  The first call format returns a single season name,
! where season = 1 indicates the season starting with January,
! 2 for February, etc.:
!
!    call season_names (season, season_length, name, opt)
!
! This alternate format returns an array of 12 season names, for
! the seasons starting respectively with January through December:
!
!    call season_names (season_length, names, opt)
!
! Any season length from 1 through 12 may be specified.  Length 1
! returns month names rather than season names.
!
! For season length 1 only, specify the optional argument opt =
! "short" to get three-letter month abbreviations such as JAN,
! FEB, MAR.  Otherwise, full month names will be returned, e.g.
! January, February, March.  The opt argument is ignored when
! season_length is 2 or more.
!
! Output name strings are uppercase or mixed case.  The length of
! caller's output string variable must be sufficient to hold the
! longest requested name, otherwise the returned strings will be
! truncated.
!
!-----------------------------------------------------------------------------

module season__names

   private
   public season_names

   interface season_names
      module procedure season_names_single, season_names_array
   end interface

contains

!-----------------------------------------------------------------------------
! Create single season name.
!-----------------------------------------------------------------------------

subroutine season_names_single (season, season_length, name, opt)
   implicit none

   integer,      intent (in )           :: season	  ! starting month 1-12
   integer,      intent (in )           :: season_length  ! no. months in season
   character(*), intent (out)           :: name		  ! output name string
   character(*), intent (in ), optional :: opt	  	  ! "short" to get
							  ! 3-letter month names

   character(len(name)) :: names(12)	! local array

   call season_names_array (season_length, names, opt)	! make all 12 names

   name = names(season)				! return the requested season

end subroutine season_names_single

!-----------------------------------------------------------------------------
! Create season names for all 12 seasons.
!-----------------------------------------------------------------------------

subroutine season_names_array (season_length, names, opt)
   implicit none

   integer,      intent (in )           :: season_length  ! no. months in season
   character(*), intent (out)           :: names(12)	  ! output name strings
   character(*), intent (in ), optional :: opt	  	  ! "short" to get
							  ! 3-letter month names
! Local variables.

   integer i
   logical short_opt

   character(*), parameter :: letters = "JFMAMJJASONDJFMAMJJASOND"
   					! 24 month letters should suffice
! Handle optional argument.

   short_opt = .false.
   if (present (opt)) then
      if (opt == "short") short_opt = .true.
   end if

! Get names for all 12 single months.

   if (season_length == 1) then

      if (short_opt) then
         names(:) = (/ "JAN", "FEB", "MAR", "APR", "MAY", "JUN", &
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"  /)
      else
         names(:) = (/ "January  ", "February ", "March    ", "April    ", &
                       "May      ", "June     ", "July     ", "August   ", &
                       "September", "October  ", "November ", "December "  /)
      end if

! Get names for all 12 seasons with two or more months.

   else
      do i = 1, 12
         names(i) = letters(i:i+season_length-1)    ! extract substrings
      end do
   end if

end subroutine season_names_array

end module season__names
