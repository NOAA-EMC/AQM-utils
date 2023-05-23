!------------------------------------------------------------------------------
!
! short_training_period.f90 - Shorten training period when recent obs are high.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! system for CMAQ forecast outputs.
!
! 2022-may-05	Original version.  By Irina Djalalova, NOAA/PSL/CIRES.
! 2022-may-25	Dave Allured:  Add subroutine wrapper.
!		Extend tuning parameters into config file.
! 2022-may-26	Dave Allured:  Simplified version, more complete blocking.
!
! 2023-mar-25	Add output to signal short training period detected.
!
! Called by analog_ensemble.f90.  This routine shortens the effective
! training period by selectively blocking early data.  The purpose is
! to improve forecast skill during current fire events, by ignoring
! pre-fire obs.
!
! Note that obs2 data is refreshed on every outer iteration.
! Therefore it is safe to block data by simply overwriting chunks
! of obs2 on each iteration.
!
!------------------------------------------------------------------------------

module short__training_period
contains

subroutine short_training_period (obs2, vmiss, iday, thresh_high, &
      ndays_short, short_detect, max_obs)

   use config, only : dp
   implicit none

   real(dp), intent(inout) :: obs2(:,:)	    ! obs training data (days, hours)
   real(dp), intent(in)    :: vmiss	    ! common missing value
   integer,  intent(in)    :: iday	    ! current iteration day number
   real(dp), intent(in)    :: thresh_high   ! obs threshold value to shorten
   integer,  intent(in)    :: ndays_short   ! no. days in short training period

   logical,  intent(out)   :: short_detect  ! true = short train period detected
   real(dp), intent(out)   :: max_obs	    ! max detected recent obs value

   integer h, nhours, iday2, iday3, ndays	! local variables

! Note, iday input argument is not used in this version.

   iday2 = iday			! squelch pedantic compiler warning for iday

! Check control parameters.  Do not block if either one is zero.

   short_detect = .false.	! initial signal if short period not enabled

   if (thresh_high == 0) return
   if (ndays_short == 0) return

! Method 2.  Block sliding sections aligned with the original obs input days.

   ndays  = size (obs2, 1)		! get dimensions
   nhours = size (obs2, 2)

   iday3  = ndays - ndays_short		! nominal start of short training period
   iday3  = max (iday3, 1)

   max_obs      = maxval (obs2(iday3:ndays-1,:))
   short_detect = (max_obs > thresh_high)

   if (short_detect) then
      do h = 1, nhours
         iday2 = (iday3 - 1) + ( (h - 1) / 24 )
         obs2(1:iday2,h) = vmiss
      end do
   end if

end subroutine short_training_period
end module short__training_period
