!------------------------------------------------------------------------------
!
! std_dev_grid -- Mean, variance, and std. deviation for a gridded time series.
!
! This version also includes data masking, to facilitate things like
! seasonal subsetting.
!
! 1.00	2008-dec-31	Original version.  By Dave Allured.
!			Adapted from std_dev v1.00,
!			Mean, variance, and std. deviation for gridded series.
! 1.00	2009-jan-05	Fix minor precision problem with standard deviation.
!
! Input:   3-D gridded time series, 1-D data mask, and controls.  See below.
!
! Output:  2-D grids for mean, variance, and standard deviation.
!	   In case of insufficient data, outputs are set to vmiss.
!
! Notes:
!
! Dimension T is simply the data indexing dimension.  It is called
! the time dimension herein, but it may have any definition suitable
! to the calling program.
!
! Time subsetting for random time steps is supported, by using the
! tags(T) data mask array to select time steps to be included.
! This may be used, for example, to get seasonal statistics from
! year-round (continuous) time series.
!
! This version uses the two pass algorithm to minimize roundoff error.
!
! Output arrays are allocated by this routine.  They do not need to
! be pre-allocated.  If pre-allocated, the caller's result arrays
! will safely be de-allocated, then re-allocated to the correct size.
!
!------------------------------------------------------------------------------

module std__dev_grid
   implicit none

! Special public limit parameters for debugging.
! No check is done unless these are set positive.

   integer :: std_dev_grid_min_count = -1
   integer :: std_dev_grid_max_count = -1

contains

subroutine std_dev_grid (data, tags, vmiss, thresh, mean, variance, stdev)
   implicit none

   real,    intent (in ) :: data(:,:,:)	! 3-D gridded input time series (X,Y,T)
   logical, intent (in ) :: tags(:)	! include only TRUE data pairs (T)
   real,    intent (in ) :: vmiss	! missing value in input data
   real,    intent (in ) :: thresh	! 0-1: required fraction of data present
   					! applies only to tag selected data
   
   real, allocatable, intent (out) :: mean(:,:)		! mean output grid (X,Y)
   real, allocatable, intent (out) :: variance(:,:)	! variance output grid
   real, allocatable, intent (out) :: stdev(:,:)	! std. deviation output

! Local variables.
   
   integer nx, ny, ntimes, j, required_count
   real vmissl, vmissh
   
   integer, allocatable :: ndata(:,:)
   double precision, allocatable :: sumx(:,:), sdd(:,:), dif(:,:)
   double precision, allocatable :: dmean(:,:), dvariance(:,:)

! Derived parameters.

   nx     = size (data, 1)			! get input dimensions
   ny     = size (data, 2)
   ntimes = size (data, 3)

   vmissl = vmiss - .0001 * abs (vmiss)		! test limits for missing values
   vmissh = vmiss + .0001 * abs (vmiss)
   
   required_count = ceiling (thresh * count (tags))   ! # of time steps required
   					! for each grid point for valid output
   
   required_count = max (1, required_count)	! require at least one datum
   						! at each grid point
! Consistency check.

   if (ntimes /= size (tags)) then
      print *
      write (*, '(2(a,i0))') ' *** std_dev_grid: Time dimensions for data' &
         // ' and tag arrays = ', ntimes, ', ', size (tags)
      print *, '*** std_dev_grid: Abort: Data and tag time dimensions differ.'
      stop 99
   end if

! Allocate arrays.
   
   allocate (ndata(nx,ny), sumx(nx,ny), sdd(nx,ny))	   ! work arrays
   allocate (dif(nx,ny), dmean(nx,ny), dvariance(nx,ny))
   allocate (mean(nx,ny), variance(nx,ny), stdev(nx,ny))   ! output arrays

! Compute the mean for selected time steps.

   sumx = 0.					! clear accumulator grids
   ndata = 0

   do j = 1, ntimes
      if (.not. tags(j)) cycle			! include only selected times
      where (data(:,:,j) < vmissl .or. data(:,:,j) > vmissh)
         sumx = sumx + data(:,:,j)		! accumulate by grids
         ndata = ndata + 1
      end where
   end do
   
   where (ndata > 0)				! suppress divide by zero
      dmean = sumx / ndata			! double precision for calcs
      mean = dmean				! single precision for output
   end where

! Compute variance and standard deviation.

   sdd = 0.					! clear gaccumulator grid
   
   do j = 1, ntimes
      if (.not. tags(j)) cycle
      where (data(:,:,j) < vmissl .or. data(:,:,j) > vmissh)
         dif = data(:,:,j) - dmean
         sdd = sdd + (dif * dif)		! accumulate deviations squared
      end where
   end do

   where (ndata > 1)
      dvariance = sdd / (ndata - 1)		! normal points, 2 or more data
      variance = dvariance
      stdev = sqrt (dvariance)
   
   elsewhere
      variance = 0.		 		! for degenerate points
      stdev = 0.				! with only one datum
   end where

! DEBUG:  Check data counts.  This section is completely inactive
! if debug parameters are left at their default values.

   if (std_dev_grid_min_count >= 0 .or. std_dev_grid_max_count >= 0) then
   
      if (  minval (ndata) < std_dev_grid_min_count &
       .or. maxval (ndata) > std_dev_grid_max_count) then
         print *
         print *, '*** std_dev_grid: tags array:'
      
         do j = 1, ntimes, 20
            write (*, "(i6, ' :', 20l2)") j, tags(j:min (j+19, ntimes))
         end do
      
         print *
         write (*, '(2(a,i6))') ' *** std_dev_grid: Min data count =', &
            minval (ndata), ', limit =', std_dev_grid_min_count
         write (*, '(2(a,i6))') ' *** std_dev_grid: Max data count =', &
            maxval (ndata), ', limit =', std_dev_grid_max_count
         print *
         print *, '*** std_dev_grid: Abort: Counts outside of debug limits.'
         stop 99
      end if
      
   end if

! Set missing values where data count is below threshold.

   where (ndata < required_count)	! at least one datum always required
      mean     = vmiss
      variance = vmiss
      stdev    = vmiss
   end where

end subroutine std_dev_grid

end module std__dev_grid
