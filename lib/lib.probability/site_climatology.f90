!------------------------------------------------------------------------------
!
! site_climatology.f90 -- Compute climatology fields for site data.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-may-16	dataascii_preparation_for_probability_code.pro:
!		Original IDL version.  By Irina Djalalova, NOAA/OAR/ESRL/PSD.
!
! 2019-jun-27	site_climatology.f90:
!		Convert to F90 module for bias correction.  By Dave Allured.
!		Convert data flow from intermediate files, to all in-memory.
!		Generalize the formulas for search windows and time ranges.
!		Current version computes only daily 8-hour maximums for ozone.
! 2019-jul-10	Export daily max calcs to new common routine daily_max.f90.
!		Add daily averages for PM2.5, using new daily_averages.f90.
! 2019-aug-09	Move run parameters from hard coded, to config file.
!
! Functions in this climatology module:
!
! * Start with best analogs for each site, from bias correction.
! * For ozone, compute daily mean and variance of peak 8-hour ozone,
!     by the current EPA-defined method.
! * For PM2.5, compute daily 24-hour means.
!
! Notes:
!
! Input for each site is a matrix of ten best analogs at each
! forecast hour, times the number of forecast hours.  In this
! routine, these analogs are assembled as if they are ten
! independent time series along the forecast hour dimension.
!
! The analogs in each pseudo time series are not actually
! directly correlated, but they share similar behaviors such as
! diurnal and seasonal influence.
!
!------------------------------------------------------------------------------

module site__climatology
contains

subroutine site_climatology (analogs, vmiss, cycle_time, prob, diag, &
      analog_mean, analog_variance)

   use config, only : dp
   use daily__averages
   use daily__max
   use probability_type
   use std__dev
   implicit none

! Input arguments.

   real(dp),        intent(in) :: analogs(:,:,:)  ! current best analogs
						  !   (hours, analogs, sites)
  						  ! sort order does not matter
   real(dp),        intent(in) :: vmiss		  ! common missing value code
   integer,         intent(in) :: cycle_time	  ! forecast cycle time
   type(prob_type), intent(in) :: prob		  ! probability config params
   integer,         intent(in) :: diag		  ! diag. verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: analog_mean(:,:)      ! result climos
   real(dp), intent(out), allocatable :: analog_variance(:,:)  ! (sites, fdays)

! Local variables.

   character fdate_str*24

   integer si, nsites, nanalogs, nhours
   integer fday, fdays_estimate, fdays_valid

   real(dp) stdev

! Dynamic arrays.

   integer,  allocatable :: old_dims(:)
   integer		 :: order(3)		! fixed size, maybe gfort bug
   integer               :: new_dims(3)		! fixed size required

   real,     allocatable :: analogs2(:,:,:)	! reordered input array
   						! (sites, analogs, hours)
						! note single precision

   real(dp), allocatable :: daily_stat(:,:,:)	! daily stats work array
   						! (sites, analogs, fdays)

   logical,  allocatable :: tags(:)		! tag array for std_dev

!-----------------------------------------------------------
! Initialize.
!-----------------------------------------------------------

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  site_climatology: Start.'

! Reorder the dimensions of the input array, for input to the
! daily stat routines.  Better to do this here on the small
! site data array, rather than the larger data arrays in
! gridded_climatology.f90.

! Also convert on the fly from double to single precision,
! currently needed for input to the daily stat routines.

   order    = (/ 3,2,1 /)	  ! indices for reordering dimensions
   old_dims = shape (analogs)	  ! dim sizes in original order
   new_dims = old_dims(order)	  ! vector subscripting, permute the dim sizes

   analogs2 = real (reshape (analogs(:,:,:), new_dims, order=order) )
						! (S,A,H) <-- (H,A,S)

   nsites   = size (analogs2, 1)	! get dimensions after reordering
   nanalogs = size (analogs2, 2)
   nhours   = size (analogs2, 3)

   if (diag >= 2) then
      print '(a,9(2x,i0))', '      shape (analogs)  = ', shape (analogs)
      print '(a,9(2x,i0))', '      shape (analogs2) = ', shape (analogs2)
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

! Pre-allocate the result array for daily statistic results.

! Dimension #2 serves as a fake climo days time dimension.
! See explanation under "make climatology fields" below.

   allocate (daily_stat(nsites, nanalogs, fdays_estimate))

   daily_stat(:,:,:) = vmiss		! clear to all missing, for possible
   					!   mixed forecast lengths

!-----------------------------------------------------------
! Calculate the specified daily averages or maximums.
!-----------------------------------------------------------

! Now calculate the specified daily statistic for this day's forecast cycle.

   if (prob%daily_statistic_type == 'daily averages') then

      if (diag >= 2) print *, '  Make daily averages.'
      call daily_averages (analogs2, vmiss, cycle_time, prob, diag, &
         daily_stat, fdays_valid)		! (XYF) <-- (XYH)

   else if (prob%daily_statistic_type == 'daily 8-hourly maximums') then

      if (diag >= 2) print *, '  Find daily 8-hour maximums.'
      call daily_max      (analogs2, vmiss, cycle_time, prob, diag, &
         daily_stat, fdays_valid)		! (XYF) <-- (XYH)

   else
      print *
      print *, '*** site_climatology:  Unknown name for requested' &
                      // ' daily statistic.'
      print *, '*** Requested statistic name = ' &
                      // trim (prob%daily_statistic_type)
      print *, '*** Check "type of daily statistic" in config file.'
      print *, '*** Abort.'
      call exit (1)
   end if

   if (diag >= 2) print '(a,i0)', &
      '    Number of valid forecast statistical days = ', fdays_valid

!-----------------------------------------------------------
! Now make climatology fields for derived variables.
!-----------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *,'  site_climatology: Make climatologies and variance.'
   if (diag >= 2) print *,'    Allocate multi-day climo result arrays.'

   allocate (analog_mean    (nsites, fdays_valid))
   allocate (analog_variance(nsites, fdays_valid))

   allocate (tags(nanalogs))		! markers for valid intermediates
   tags(:) = .true.			! count all analogs, even all missing

! Config parameter:  prob%analog_climo_thresh is 0-1;
! fraction of valid data required for valid climo outputs.

! Loop over each site, and each climo result day.

! The available ten analogs for each site are grouped to form a fake
! daily forecast sequence.  Compute each mean and variance over the
! analog dimension, i.e. over this fake daily forecast sequence.
! Missing values should be handled correctly.

   do si = 1, nsites
      do fday = 1, fdays_valid
         call std_dev (daily_stat(si,:,fday), tags(:), vmiss, &
            prob%analog_climo_thresh, analog_mean(si,fday), &
            analog_variance(si,fday), stdev)	! mean and variance are saved;
      end do					! stdev is not used
   end do

   if (diag >= 2) print *, 'site_climatology: Return.'

end subroutine site_climatology
end module site__climatology
