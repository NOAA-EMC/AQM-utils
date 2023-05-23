!-----------------------------------------------------------------------------
!
! binomial_coefficient.f90 -- Compute the binomial coefficient of N and K,
!			      using Pascal's triangle additive method.
!
! This method is used to compute the number of unique K combinations,
! and for other mathematical applications.
!
! 2017-mar-24	binomial_coefficient.product-method.f90:
!		Original version, using product method.  By Dave Allured.
!
! 2017-mar-24	binomial_coefficient.f90:
!		Re-write using additive method, for code simplicity.
! 2017-may-12	Comment fixes only.
!
! Notes:
!
! This routine computes one binomial coefficient for N and K.
! This function is also known as N choose K.
!
! Also included is the related function N multichoose K, which
! computes the number of unique multisets.
!
! Ref. Wikipedia and other sources.  The articles "Binomial
! coefficient" and  "Multiset" are especially helpful.
!
! Limits:
!
! This initial version uses only regular 32-bit signed integers.
! The maximum value of input N is 33.  Signed integer overflow
! occurs in row 34.
!
!-----------------------------------------------------------------------------

module binomial__coefficient
contains

function binomial_coefficient (n, k) result (out)
   implicit none

   integer, intent (in) :: n		! number of elements in outer set
   integer, intent (in) :: k		! number of elements to sample
   integer :: out			! function result, binomial coefficient

   integer i				! local var

   integer row(0:n+1), prev(0:n+1)	! automatic arrays; special bounds

! Make top row of Pascal's triangle.  Must have protective zeros on
! both left and right.

! Note, the top row is considered row zero, NOT row 1, for this analysis.
! Ref. Wikipedia, etc.

   row(:) = 0
   row(1) = 1

! Compute successive rows 1 through N.  Keep only the final row N.
! Note, each row I contains I+1 non-zero elements.  Row 1 has 2 elements, etc.

   do i = 1, n
      prev(:)    = row(:)		! hold previous row to calculate next
      row(1:i+1) = prev(0:i) + prev(1:i+1)

      if (any (row(2:i) <= prev(2:i))) then
         print '(a)', '*** binomial_coefficient:  Integer overflow.  Abort.'
         print '(2(a,i0))', '*** n = ', n, ', k = ', k
         print '(2(a,i0))', '*** Overflow occurred in row N = ', i
         call exit (1)
      end if
   end do

! Take the desired end product.  N choose K is simply the K'th element,
! zero-based, in row N.  Subscript 2 for K = 1, subscript 3 for K = 2, etc.
! Again, see articles.

   out = row(k+1)

end function binomial_coefficient


!-----------------------------------------------------------------------------
!
! The function n_multichoose_k uses the bijection between multisets
! and ordinary combinations to transform multichoose into ordinary
! N choose K.  See Wikipedia:Multisets, etc.
!
!-----------------------------------------------------------------------------

function n_multichoose_k (n, k) result (out)
   implicit none
   integer, intent (in) :: n		! number of bins
   integer, intent (in) :: k		! no. of elements to spread across bins
   integer :: out			! function result, count of multisets

! Multichoose transform:
!
!       (( n ))     ( n + k - 1 )
!       ((   ))  =  (           )
!       (( k ))     (     k     )

   out = binomial_coefficient (n + k - 1, k)

end function n_multichoose_k

end module binomial__coefficient
