!------------------------------------------------------------------------------
!
! make_daily.f90 -- Make daily averages or 8-hour maximums for data arrays.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2020-aug-03	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!		Adapted from site_climatology.f90 version 2019-aug-09.
!
! This is a generalized support routine to compute daily averages
! or daily 8-hour maximums for probability calculations.  This
! routine is used in common for both gridded and site data.  This
! routine handles proper alignment between forecast hours and
! actual time of day in UTC.
!
! Caller specifies the position of the hourly time dimension.
! Two permutations are currently supported: (X,H,Y) and (X,Y,H).
! In this usage, H indicates the hourly dimension, and X and Y
! are two other arbitrary dimensions.
!
!------------------------------------------------------------------------------

module make__daily
contains

function make_daily (hourly, dims, vmiss, label, cycle_time, prob, diag) &
      result (daily)

   use config, only : dp
   use daily__averages
   use daily__max
   use probability_type
   implicit none

   real(dp),        intent(in) :: hourly(:,:,:)	! input data over forecast hours
   character(*),    intent(in) :: dims		! data dim spec: X,H,Y or X,Y,H
   real(dp),        intent(in) :: vmiss		! common missing value code
   character(*),    intent(in) :: label		! data name for diagnostic
   integer,         intent(in) :: cycle_time	! forecast cycle time
   type(prob_type), intent(in) :: prob		! probability config parameters
   integer,         intent(in) :: diag		! diag. verbosity level, 0-N

   real(dp),       allocatable :: daily(:,:,:) ! function result, daily averages
					       !   or maximums (X, Y, fdays)
! Local variables.

   integer nx, ny, nhours
   integer fdays_estimate, fdays_valid

! Dynamic arrays.

   integer,  allocatable :: old_dims(:)
   integer		 :: reorder(3)		! fixed size, maybe gfort bug
   integer               :: new_dims(3)		! fixed size required

   real,     allocatable :: hourly2(:,:,:)	! reordered input array
   						! (X, Y, hours)
						! note single precision

   real(dp), allocatable :: daily1(:,:,:)	! daily stats work array
   						! (X, Y, fdays)

!-----------------------------------------------------------
! Initialize.
!-----------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, 'make_daily: Start.'

! Reorder the dimensions of the input array, for input to the
! daily stat routines.

! Also convert on the fly from double to single precision,
! currently needed for input to the daily statistic routines.

   if (dims == 'X,H,Y') then
      reorder  = (/ 1,3,2 /)	    ! indices for reordering dimensions
      old_dims = shape (hourly)	    ! dim sizes in original order
      new_dims = old_dims(reorder)  ! vector subscripting, permute the dim sizes

      hourly2  = real (reshape (hourly(:,:,:), new_dims, order=reorder) )
						! (X,Y,H) <-- (X,H,Y)

! Case (X,Y,H).  No reordering needed, but convert to single precision.

   else if (dims == 'X,Y,H') then
      hourly2  = real (hourly(:,:,:))		! (X,Y,H) <-- (X,Y,H)

   else
      print *
      print *, '*** make_daily: Invalid dimension specification string.'
      print *, '*** dims = [' // trim (dims) // ']'
      print *, '*** Internal program error.  Abort.'
      call exit (1)
   end if

! Get dimensions after reordering.

   nx     = size (hourly2, 1)
   ny     = size (hourly2, 2)
   nhours = size (hourly2, 3)

   if (diag >= 3) then
      print '(a,9i6)', '      shape (hourly)  = ', shape (hourly)
      print '(a,9i6)', '      shape (hourly2) = ', shape (hourly2)
   end if

! For dimensioning, overestimate the number of forecast days,
! based on worst case scenario for defining daily windows.
! Actual number of forecast days will be determined and reported
! by the daily statistical routines.

! In available forecast hours, consider dividers at 24-hour
! intervals, on the worst possible alignments.
! For example, 1 forecast hour can not be split, so only 1 forecast day.
! 2 forecast hours can be split into two partial "days".
! 3 through 25 can also be split into two blocks.
! 26 hours can be split into 3 blocks.

   fdays_estimate = 1 + (nhours + 22) / 24

!!! Pre-allocate the result array for daily statistic results.

   allocate (daily1(nx, ny, fdays_estimate))

! Note, the daily statistic routines guarantee that the valid part
! of the daily result array (up to fdays_valid) will be filled with
! either valid data or missing values.  Therefore, pre-fill is not
! needed.

!-----------------------------------------------------------
! Calculate the specified daily averages or maximums.
!-----------------------------------------------------------

! Now calculate the specified daily statistic over two dimensions plus hours.

   if (prob%daily_statistic_type == 'daily averages') then

      if (diag >= 2) print *, '  Make daily averages for ' // trim (label) //'.'
      call daily_averages (hourly2, vmiss, cycle_time, prob, diag, daily1, &
         fdays_valid)				! (XYD) <-- (XYH)

   else if (prob%daily_statistic_type == 'daily 8-hourly maximums') then

      if (diag >= 2) print *, '  Find daily 8-hour maximums for ' &
         // trim (label) //'.'
      call daily_max      (hourly2, vmiss, cycle_time, prob, diag, daily1, &
         fdays_valid)				! (XYD) <-- (XYH)

   else
      print *
      print *, '*** make_daily:  Unknown name for requested daily statistic.'
      print *, '*** Requested statistic name = ' &
                      // trim (prob%daily_statistic_type)
      print *, '*** Check "type of daily statistic" in config file.'
      print *, '*** Abort.'
      call exit (1)
   end if

   if (diag >= 2) print '(a,i0)', &
      '    Number of valid forecast statistical days = ', fdays_valid

! Return the correctly sized final 3-D result array.

! Note that the above daily statistic routines guarantee that the
! valid part of the result array (up to fdays_valid) is be filled
! with either valid data or missing values.

   daily = daily1(:,:,1:fdays_valid)		! auto allocate

   if (diag >= 3) print *, 'make_daily: Return.'

end function make_daily
end module make__daily
