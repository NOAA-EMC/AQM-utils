!------------------------------------------------------------------------------
!
! regress.f90 -- Linear regression.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSL bias correction system for CMAQ forecast outputs.
!
! 2019-aug-16	myfunct.pro:
!		Original IDL version.  By Irina Djalalova, NOAA/ESRL/PSL/CIRES.
!
! 2020-jul-27	regress.f90:
!		Translate to fortran 90.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
! 2020-jul-30	Generalize the threshold check, and prevent halting.
!		Add verbosity control.
!		Make yfit an optional output.
!
! This version does not include missing value handling.
! All input values must be valid data.
!
!------------------------------------------------------------------------------

module regress_mod
contains

subroutine regress (x, y, diag, k, b, sigma, yfit)
   use config, only : dp
   implicit none

   real(dp), intent(in)  :: x(:), y(:)	 ! input vectors to regress
   integer,  intent(in)  :: diag	 ! diagnostic verbosity level, 0-N

   real(dp), intent(out) :: k		 ! slope of best fit
   real(dp), intent(out) :: b		 ! Y intercept of best fit
   real(dp), intent(out) :: sigma	 ! std. dev. of data from the best line

   real(dp), intent(out), optional :: yfit(:)  ! best linear fit of the data

! Local variables.

   integer n
   real(dp) a11, a12, a21, a22, b1, b2
   real(dp) delta, delta1, delta2, ymean, thresh

! Make sums.

   n = size (x)

   if (diag >= 4) print *
   if (diag >= 4) print *, 'n =', n

   a11 = sum (x(:) ** 2)
   a12 = sum (x(:))
   a21 = a12
   a22 = n

   b1  = sum (x(:) * y(:))
   b2  = sum (y(:))

   if (diag >= 4) print *, 'final:::'
   if (diag >= 4) print *, 'a11, a12, a21, a22 =', a11, a12, a21, a22
   if (diag >= 4) print *, 'b1, b2 =', b1, b2

! Compute differentials.

   delta  = a11 * a22 - a12 * a21
   delta1 = b1  * a22 - b2  * a12
   delta2 = b2  * a11 - b1  * a21

   if (diag >= 4) print *, 'delta1, delta2, delta =', delta1, delta2, delta

! Detect and prevent near-singularity or divide by zero.

   ymean  = b2 / n
   thresh = ymean * 10e-6

   if (abs (delta) < thresh) then
      print *, '*** regress: No calculation allowing.'
      print *, '*** Substitute line Y = X.'
      k = 1
      b = 0

! Compute best fit linear coefficients.

   else
      k = delta1 / delta			! slope
      b = delta2 / delta			! Y intercept
   end if

   if (diag >= 4) print *, 'k, b =', k, b

   sigma = sum ( (y(:) - (k * x(:)) - b) ** 2 )

   if (present (yfit)) then		! optional, compute fitted Y data
      yfit(:) = k * x(:) + b
   end if

   if (n > 0 .and. sigma > 0) then
      sigma = sqrt (sigma / n)
   else
      sigma = 0
   end if

end subroutine regress
end module regress_mod
