!-----------------------------------------------------------------------------
!
! find_runs.f90 -- Generic routine to find runs of almost-same values
!		   in 1-D data series.
!
! 2017-apr-11	Original version for bias correction.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!
! This version finds multiple runs of almost-same values.
! Missing values are not included, and will interrupt runs.
!
! The result is a logical mask of the same length as the input
! array, and True in all positions of all qualifying runs.
!
! It is possible to have overlapping runs with slightly differing
! match tolerances.  Such runs will appear as a single long run
! in the result mask.
!
!-----------------------------------------------------------------------------

module find__runs
contains

subroutine find_runs (x, vmiss, min_length, tolerance_rel, mask_out)
   use config, only : dp
   implicit none

   real(dp), intent (in) :: x(:)	     ! 1-D input data series
   real(dp), intent (in) :: vmiss	     ! missing value in data series
   integer,  intent (in) :: min_length	     ! minimum allowable run length
   real(dp), intent (in) :: tolerance_rel    ! relative tolerance to match data

   logical,  intent(out), allocatable :: mask_out(:)  ! result, true within runs

! Local variables.

   integer i, j, k, ndata
   real(dp) thresh_abs

! Initialize.

   ndata = size (x)

   allocate (mask_out(ndata))
   mask_out(:) = .false.		! init result mask, false = no runs

! Use the three-loop method (i, j, k) because it is simpler to code
! and understand.

! Loop to check EVERY data point as the possible start of a run,
! up to the last possible starting point.

iloop: &
   do i = 1, ndata - min_length + 1
      if (x(i) == vmiss) cycle iloop	! missing value disqualifies a run

! Search for a run of the specified minimum length, within the given tolerance.
! Use an absolute threshold, not relative, to protect from divide by zero.

      thresh_abs = abs (tolerance_rel * x(i))

jloop: &
      do j = i + 1, i + min_length - 1
         if (x(j) == vmiss) cycle iloop    ! missing value disqualifies a run
         if (abs (x(j) - x(i)) > thresh_abs) cycle iloop   !  disqualify if
      end do jloop			       ! mismatch within minimum length

! Found a run of minimum qualifying length.  Now find the end of the run.

kloop: &
      do k = i + min_length - 1, ndata	  ! first iteration guaranteed to match
         if (x(k) == vmiss) exit kloop	  ! missing value terminates a run
         if (abs (x(k) - x(i)) > thresh_abs) exit kloop   ! mismatch terminates
         j = k				  ! j saves the highest matching index
      end do kloop

! Now found a complete run of near-constant values.
! Indicate all values within run.

      mask_out(i:j) = .true.		! true = values within current run
   end do iloop

end subroutine find_runs
end module find__runs
