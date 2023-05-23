!-----------------------------------------------------------------------------
!
! xnorm_cdf.f90 -- Cumulative distribution function.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-jul-22	Original stand-alone version of the xnorm_cdf function.
!		Split off from data_input_out2.f90 version 2019-jun-10,
!		  by Irina Djalalova, NOAA/OAR/ESRL/PSD/CIRES.
!		Create stand-alone module version, by Dave Allured.
!		Convert to all double precision.
!
!-----------------------------------------------------------------------------

module xnorm__cdf
contains

function xnorm_cdf(x) result(prob)
   use config, only : dp
   implicit none

   real(dp), intent(in) :: x		! input
   real(dp)             :: prob		! output

   integer k
   real(dp) b(5), pi, z, p, t, s

   b(1) =  0.319381530d0
   b(2) = -0.356563782d0
   b(3) =  1.781477937d0
   b(4) = -1.821255978d0
   b(5) =  1.330274429d0
   pi   =  acos (-1.d0)

   z = exp (-x**2 / 2) / sqrt (2.d0 * pi)
   p = 0.2316419d0
   t = 1. / (1. + p * x)
   s = b(5) * t

   do k = 4, 1, -1
      s = t * (b(k) + s)
   end do

   prob = 1.d0 - (z * s)

end function xnorm_cdf
end module xnorm__cdf
