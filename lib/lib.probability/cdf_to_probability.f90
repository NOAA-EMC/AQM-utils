!-----------------------------------------------------------------------------
!
! cdf_to_probability.f90 -- Compute gridded probability from CDF function.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-jul-22	Original stand-alone version of the CDF probability routine.
!		Split off from data_input_out2.f90 version 2019-jun-10,
!		  by Irina Djalalova, NOAA/OAR/ESRL/PSD/CIRES.
!		Create stand-alone module version, by Dave Allured.
!		Convert to all double precision.
!		Make generic for both original grid types.
! 2019-jul-25	Change result arrays to pre-allocated only.
!		Improve diagnostics.
!
!-----------------------------------------------------------------------------

module cdf__to_probability
contains

subroutine cdf_to_probability (label, state, variance, lats, lons, thresh, &
      vmiss, diag, probability, zscore, cdf)

   use config, only : dp
   use xnorm__cdf
   implicit none

   character(*), intent(in)  :: label		  ! grid type for messages
   real(dp),     intent(in)  :: state(:,:)	  ! input mean state grid
   real(dp),     intent(in)  :: variance(:,:)	  ! input variance grid
   real(dp),     intent(in)  :: lats(:,:)	  ! grid coordinates
   real(dp),     intent(in)  :: lons(:,:)	  !   for diagnostics
   real(dp),     intent(in)  :: thresh		  ! probability threshold
   real(dp),     intent(in)  :: vmiss		  ! common missing value code
   integer,      intent(in)  :: diag		  ! diagnostic verbosity level

   real(dp),     intent(out) :: probability(:,:)  ! probability result arrays
   real(dp),     intent(out) :: zscore(:,:)
   real(dp),     intent(out) :: cdf(:,:)

   integer i, j, nx, ny				  ! local variables
   integer bad_count, n_positive
   real(dp) d, percent

! Get dimensions.

   nx = size (state, 1)
   ny = size (state, 2)

!! First part removed, 2019-jul-22.  Allow negative mean state values.
!! Was:  if (state(i,j) >= 0. .and. variance(i,j) > 0.) then

   bad_count = 0

   do j=1,ny
      do i=1,nx
         if (variance(i,j) > 0.) then
            zscore(i,j) = (thresh - state(i,j)) / sqrt (variance(i,j))
            if (zscore(i,j) >= 0.) then 
               d                = xnorm_cdf (zscore(i,j))
               probability(i,j) = 100. * (1.0 - d)
               cdf(i,j)         = d
            else
               d                = xnorm_cdf (-zscore(i,j))
               probability(i,j) = 100. * d
               cdf(i,j)         = 1.0 - d
            end if
         else
            if (diag >= 2) print '(a,2f9.3,2g14.5)', 'Bad ' // trim (label) &
               // ' grid point:   lat, lon, mean, variance =', &
               lats(i,j), lons(i,j), state(i,j), variance(i,j)
            zscore(i,j)         = 0
            probability(i,j)    = 0
            cdf(i,j)            = 0
            bad_count           = bad_count + 1
         end if
      end do
   end do

! Diagnostics.

   if (diag >= 2 .or. bad_count > 0) print '(6x,3a,i0)', &
      'Number of bad or missing ', trim (label), ' grid points = ', bad_count

   if (diag >= 2) then
      n_positive = count (probability > 0 .and. probability /= vmiss)
      percent = (n_positive * 100.0) / size (probability)
      print '(6x,a,i0,a,f0.1,a)', &
         'Count probability greater than zero           = ', n_positive, &
         ' (', percent, '%)'
   end if

end subroutine cdf_to_probability
end module cdf__to_probability
