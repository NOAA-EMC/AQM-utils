!-----------------------------------------------------------------------------
!
! rmse.f90 -- Compare analog filter results with obs, compute RMSE.
!
! 2014-Oct-01   Original rmse function.  By Stefano Alessandrini (ste), NCAR.
!		Contained within NCAR's main_analog.F90 version 2015-dec-01.
!
! 2017-may-16	Break out from main program.
!		No calculation of analog means.
!		Use corrected forecast directly.
!		Simplify calling interface.
!
!-----------------------------------------------------------------------------

module rmse_mod
contains

function rmse (misure, forecast, vmiss)

   use config, only : dp
   implicit none

   real(dp), intent(in) :: misure(:,:)		! obs (days, hours)
   real(dp), intent(in) :: forecast(:,:)	! corrected (days, hours)
   real(dp), intent(in) :: vmiss		! code for missing value

   real(dp) rmse				! rmse function result

! Local variables.

   integer i, ii, ndays, nleadtime, nvalid

! Initialize.

   ndays     = size (misure, 1)		! get dimensions
   nleadtime = size (misure, 2)

   nvalid = 0
   rmse  = 0

! Accumulate sums for RMSE.

   do i = 1, ndays
      do ii = 1, nleadtime
         if (misure(i,ii) /= vmiss .and.forecast(i,ii) /= vmiss) then
            rmse = rmse + (misure(i,ii) - forecast(i,ii)) ** 2
            nvalid = nvalid + 1
         endif
      end do
   end do

! Calculate final RMSE.

   if (nvalid > 0) then
      rmse = rmse / nvalid
      rmse = sqrt (rmse)
   else
      rmse = huge (rmse)
!!      print*,'*** rmse: No valid obs or forecast in the test period.'
   end if

end function rmse
end module rmse_mod
