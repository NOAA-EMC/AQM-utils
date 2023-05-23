!-----------------------------------------------------------------------------
!
! multisets -- Generate all multisets, that is all possible combinations
! of K identical elements distributed across N distinct bins.
!
! This is equivalent to generating all combinations of N integers
! 0 to K, that sum to the fixed total K.  This is useful for generating
! weight combinations, etc.
!
! This is also called k-combinations with repetitions, or
! k-multicombinations.  Refer to "Multiset" Wikipedia article.
!
! 2017-may-16	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Renamed from prototype version, multicombinations.f90.
!		Combinations are returned in a single dynamic array of bin
!		  counts.  Columns are the bins, each row is one multiset.
!
! This routine traverses all combinations with a non-recursive
! method.  Combinations are generated in fixed order, with the
! leftmost columns varying the slowest.
!
!-----------------------------------------------------------------------------

module multisets_mod
contains

function multisets (n, k) result (sets)

   use binomial__coefficient
   implicit none

   integer, intent (in ) :: n		! number of bins
   integer, intent (in ) :: k		! number of elements to distribute
   					! i.e. the target sum of all bins

   integer, allocatable  :: sets(:,:)	! result array, counts for all multisets
   					! (sets, bins)

   integer bins(n)		     ! set of bins for walking all combinations

   integer iset, nsets, col, partial_sum

! Forward computation, number of all combinations or multisets.
! For allocating the output array.  See multiset references.

   nsets = n_multichoose_k (n, k)

! Initialize for generating all combinations.

   allocate (sets(nsets,n))	! number of multisets x number of bins per set

   bins(:) = 0			! start with (0, 0, 0, ...)
   col     = 1			! start in leftmost column
   iset    = 0

! This loop walks through all integer combinations like an odometer.
! There are some differences to optimize run time:
!
! 1.  Roll over, in other words walk back to the previous digit,
! whenever the partial sum overflows the target.
!
! 2.  The last column is always calculated, rather than searched.

gen_loop: &
   do
      partial_sum = sum (bins(1:col))

      if (bins(col) > k .or. partial_sum > k) then
         col = col - 1
         if (col < 1) exit gen_loop	! all done when first column overflows
         bins(col) = bins(col) + 1
         cycle gen_loop
      end if

      if (col < (n - 1)) then
         col = col + 1
         bins(col) = 0
         cycle gen_loop
      end if

! Now in second to last column, and target sum not exceeded.
! This makes a fully qualified valid combination.

! To optimize, the last column is always calculated rather than searched.

      bins(col+1) = k - partial_sum

! Add new combination to output array.

      iset = iset + 1
      if (iset > nsets) exit gen_loop	! abort if subscript error

      sets(iset,:) = bins(:)

!! Test mode only.  Output on 0.0 to 1.0 scale, to match previous outputs
!! from NCAR's original comp_weight subroutine in main_analog.F90.
!!
!!      print '(i8, 99f8.2)', iset, real (bins(:)) / k

! Advance to next condition following the successful combination.
! Special case for column N-1: just advance the current "digit".

      bins(col) = bins(col) + 1
   end do gen_loop

! Consistency check.  Normally silent.

   if (iset /= nsets) then
      print *
      print *, '*** multisets: Unexpected number of generated combinations.'
      print *, '*** Expected number of combinations = ', nsets
      print *, '*** Actual number generated         = ', iset
      print *, '*** Algorithm failure.  Abort.'
      call exit (1)
   end if

end function multisets
end module multisets_mod
