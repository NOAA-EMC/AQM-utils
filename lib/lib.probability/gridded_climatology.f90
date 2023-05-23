!------------------------------------------------------------------------------
!
! gridded_climatology.f90 -- Read CMAQ historical gridded forecasts,
!                            and compute climatology fields.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-may-16	datanetcdf_preparation_for_probability_code.pro:
!		Original IDL version.  By Irina Djalalova, NOAA/OAR/ESRL/PSD.
!
! 2019-jun-27	gridded_climatology.f90:
!		Convert to F90 module for bias correction.  By Dave Allured.
!		Convert data flow from intermediate files, to all in-memory.
!		Generalize the formulas for search windows and time ranges.
!		Current version computes only daily 8-hour maximums for ozone.
! 2019-jul-10	Export daily max calcs to new common routine daily_max.f90.
!		Add daily averages for PM2.5, using new daily_averages.f90.
! 2019-aug-06	Move run parameters from hard coded, to config file.
!
! 2022-may-22	Interface change in support routine.
!
! Functions in this climatology module:
!
! * Read block of recent CMAQ forecast grids for the target variable.
! * Calculate daily 8-hour maximums or daily averages, using external routines.
! * Compute climatological fields from the daily statistics.
! * For ozone, compute daily mean and variance of peak 8-hour ozone,
!     by the current EPA-defined method.
! * For PM2.5, compute daily 24-hour means.
!
! Notes:
!
! Input file paths are specified with a path template.
! These contain YYYY MM DD HH substitution strings that will
! resolve to desired file names for the given forecast cycle.
! Templates may include full or relative paths, and they may
! begin with an $ENV environnment variable.
!
! Forecast date indices:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
! This routine uses serial processing to avoid a very large
! array with all forecast grids in memory at the same time.
!
! In input parameters below, the climatological period is
! specified relative to the current forecast date:
!
!    ndays_climo = total number of days in the climo period.
!    ndays_end   = number of days BEFORE the forecast date,
!                    of the LAST day in the climo period.
!
! For example:
!
!    30 days total, NOT including the current forecast date:
!        ndays_climo = 30, ndays_end = 1
!
!    30 days total, INCLUDING the current forecast date:
!        ndays_climo = 30, ndays_end = 0
!
! If near the start of available forecast archives, this routine
! will quietly use shorter a climatology period, limited to
! available data.
!
!------------------------------------------------------------------------------

module gridded__climatology
contains

subroutine gridded_climatology (in_gridded_template, grid_coord_file, &
      target_var, forecast_date, cycle_time, nhours_expect, base_year, &
      calendar, vmiss, prob, diag, climo_mean, climo_variance, status)

   use config, only : dp
   use daily__averages
   use daily__max
   use expand__filename
   use index_to_date_mod
   use probability_type
   use read__gridded_vars
   use std__dev_grid
   use stdlit, only : normal
   implicit none

! Input arguments.

   character(*),    intent(in) :: in_gridded_template  ! gridded input template
   character(*),    intent(in) :: grid_coord_file  ! aux. grid coordinate file
   character(*),    intent(in) :: target_var	   ! target variable name

   integer,         intent(in) :: forecast_date	   ! current forecast date index
   integer,         intent(in) :: cycle_time	   ! forecast cycle time
   integer,         intent(in) :: nhours_expect	   ! expected no. forecast hours

   integer,         intent(in) :: base_year	   ! base year for date indexes
   character(*),    intent(in) :: calendar	   ! calendar system in use
   real(dp),        intent(in) :: vmiss		   ! common missing value code
   type(prob_type), intent(in) :: prob		   ! probability config params
   integer,         intent(in) :: diag		   ! diag. verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: climo_mean(:,:,:)      ! result grids
   real(dp), intent(out), allocatable :: climo_variance(:,:,:)  ! (X, Y, fdays)

   integer,  intent(out)          :: status	          ! normal or fail

! Local program parameter.

! Local assumption that all raw forecasts to be processed will be
! in aqm Netcdf format.
! This is a temporary fix to read the raw gridded forecast file.
! See the embedded reader selection in read_gridded_vars.f90.

   character(*), parameter :: reader_code = 'reader.aqm'

! Local variables.

   character fdate_str*24

   integer year, month, day
   integer ivar, iday, ind_day
   integer nhours, nhours_actual_max
   integer nx, ny
   integer fday, fdays_estimate, fdays_valid, fdays_max
   integer ndays_climo_avail, climo_start_day, climo_end_day
   integer nvalues, bytes_per_value, mbytes

! Dynamic arrays.

   character(60), allocatable :: units(:)

   integer,  allocatable :: nhours_actual(:)	    ! actual hours for input var

   real(dp), allocatable :: grid_lats(:,:)	    ! dummy coordinate arrays
   real(dp), allocatable :: grid_lons(:,:)	    ! only for calling reader

   real,     allocatable :: vdata(:,:,:,:)	    ! input raw forecast grids
						    !   (X, Y, [var], hours)
						    ! note single precision here

   real(dp), allocatable :: daily_stat(:,:,:,:)	    ! daily accumulator array
   						    ! (X, Y, climo days, fdays)

   real,     allocatable :: mean(:,:)		    ! result grids from std_dev
   real,     allocatable :: variance(:,:)	    !   library function (X,Y)
   real,     allocatable :: stdev(:,:)		    ! NOTE, single precision

   logical,  allocatable :: tags(:)		    ! tag array for std_dev

!-----------------------------------------------------------
! Read original forecast grids for climatology period.
!-----------------------------------------------------------

   if (diag >= 2) print *, 'gridded_climatology: Start.'

! Calculate start and end of the available climatology interval.

! Config parameters:
! prob%model_ndays_climo = number of forecast days in climo period
! prob%model_ndays_end   = climo end, number of days before current forecast
!	                   (0 = use current forecast day, 1 = do not, etc.)

   climo_end_day   = forecast_date - prob%model_ndays_end
   climo_start_day = climo_end_day - (prob%model_ndays_climo - 1)

! Limit to available training period, do not go past start of data.

   if (climo_start_day < 1) climo_start_day = 1

   ndays_climo_avail = (climo_end_day - climo_start_day) + 1

   if ( (ndays_climo_avail /= prob%model_ndays_climo) .and. (diag >= 2) ) then
      print *
      print *, '*** gridded_climatology: Climatology period is reduced.'
      print *, '*** Forecast date is too close to start of training period.'
      print '(a,i0)', &
              ' *** Requested climatology length = ', prob%model_ndays_climo
      print '(a,i0)', &
              ' *** Number of days available =     ', ndays_climo_avail
   end if

! For dimensioning, overestimate the number of forecast days,
! based on worst case scenario for defining daily windows.
! Actual number of forecast days will be determined and reported
! by the daily statistical routines.

! In the expected forecast hours, consider dividers at 24-hour
! intervals, on the worst possible alignments.
! For example, 1 forecast hour can not be split, so only 1 forecast day.
! 2 forecast hours can be split into two partial "days".
! 3 through 25 can also be split into two blocks.
! 26 hours can be split into 3 blocks.

   fdays_estimate = 1 + (nhours_expect + 22) / 24

!-----------------------------------------------------------
! Main loop over each day in climatology period.
!-----------------------------------------------------------

   fdays_max = -99			! init to find actual max forecast days

! Read target var forecast grids over climatology period, one file per day.
! Compute and save only the intermediate daily statistics,
! to reduce total array storage.

   if (diag >= 2) print *, 'Read forecast grids over climatology period for ' &
      // trim (target_var) // '.'

file_loop: &
   do iday = 1, ndays_climo_avail		! array day indices
      ind_day = iday + (climo_start_day - 1)	! date inds in training period

! Decode the date index for current date in the climatology period.

      call index_to_date (ind_day, year, month, day, base_year, calendar)
      						! get current Y M D integers

      if (diag >= 2) print "(2i9,2('-',i2.2))", iday, year, month, day

!-----------------------------------------------------------
! Read grids for current forecast cycle within period.
!-----------------------------------------------------------

! Utilize the same gridded reader used by the interpolation program.
! Forecast grids are read in the original single precision.

      if (diag >= 4) print '(3a)', '   in_gridded_template = [', &
         trim (in_gridded_template),']'

      call read_gridded_vars ( (/ target_var /), (/ reader_code /), &
         (/ in_gridded_template /), grid_coord_file, year, month, day, &
         cycle_time, nhours_expect, real (vmiss), diag, vdata, nhours_actual, &
         nhours_actual_max, units, grid_lats, grid_lons, status)
      			! read single target var into 4-D (X, Y, [var], hours)

      if (status /= normal) cycle file_loop

      nx     = size (vdata, 1)		! get dimensions
      ny     = size (vdata, 2)
      nhours = nhours_actual(1)		! actual hours may vary between files

! Allocate main accumulator array for daily statistic results,
! first time only.  Dimensions were not fully known, until now.

      if (.not. allocated (daily_stat)) then
         if (diag >= 2) then
            print *,'  Allocate daily stats accumulator array for climo period.'
            print '(5x,a,4(2x,i0))', &
               'nx, ny, ndays_climo_avail, fdays_estimate =', &
                nx, ny, ndays_climo_avail, fdays_estimate
            bytes_per_value = storage_size (daily_stat) / 8	! bit size / 8
            nvalues = nx * ny * ndays_climo_avail * fdays_estimate
            mbytes = 1 + (nvalues / 1e6 * bytes_per_value)
            print '(5x,a,i0,a)', 'Size = ', mbytes, ' mbytes'
         end if

         allocate (daily_stat(nx, ny, ndays_climo_avail, fdays_estimate))

         daily_stat(:,:,:,:) = vmiss		! clear to all missing,
      end if					!   for mixed forecast lengths

!-----------------------------------------------------------
! Calculate the specified daily averages or maximums.
!-----------------------------------------------------------

! Now calculate the specified daily statistic for this day's forecast cycle.

      ivar = 1		      ! var index for input array; only read 1 variable

      if (prob%daily_statistic_type == 'daily averages') then

         if (diag>=2 .and. iday<=3) print *, '  Make daily averages.'
         call daily_averages (vdata(:,:,ivar,1:nhours), vmiss, cycle_time, &
            prob, diag, daily_stat(:,:,iday,:), fdays_valid) ! (XYCF) <-- (XYVH)

      else if (prob%daily_statistic_type == 'daily 8-hourly maximums') then

         if (diag>=2 .and. iday<=3) print *, '  Find daily 8-hour maximums.'
         call daily_max      (vdata(:,:,ivar,1:nhours), vmiss, cycle_time, &
            prob, diag, daily_stat(:,:,iday,:), fdays_valid) ! (XYCF) <-- (XYVH)

      else
         print *
         print *, '*** gridded_climatology:  Unknown name for requested' &
                         // ' daily statistic.'
         print *, '*** Requested statistic name = ' &
                         // trim (prob%daily_statistic_type)
         print *, '*** Check "type of daily statistic" in config file.'
         print *, '*** Abort.'
         call exit (1)
      end if

! Remember the longest number of valid statistic days, to support
! mixed forecast lengths.

      fdays_max = max (fdays_valid, fdays_max)

   end do file_loop

   if (diag >= 2) print '(a,i0)', &
      '    Number of valid forecast statistical days = ', fdays_max

!-----------------------------------------------------------
! Now make climatology fields for derived variables.
!-----------------------------------------------------------

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  gridded_climatology: Make climatologies' &
                 // ' and variance.'

   if (diag >= 2) print *, '   Allocate multi-day climo result arrays.'

   allocate (climo_mean    (nx, ny, fdays_max))
   allocate (climo_variance(nx, ny, fdays_max))

   allocate (tags(ndays_climo_avail))	! markers for valid grids
   tags(:) = .true.			! count all grids, even all missing

! Config parameter:  prob%analog_climo_thresh is 0-1;
! fraction of valid data required for valid climo outputs.

! Main loop over each climo result day.

   do fday = 1, fdays_max
      call std_dev_grid (real(daily_stat(:,:,:,fday)), tags(:), real(vmiss), &
         real(prob%model_climo_thresh), mean, variance, stdev)
         		! (XY) <-- (XYCF)
         		! library routine for mean and variance
         		! 2019 Jun 27, only single precision currently available

      climo_mean    (:,:,fday) = mean    (:,:)	  ! copy to output arrays, and
      climo_variance(:,:,fday) = variance(:,:)	  ! convert to double precision
      						  ! (XYF) <-- (XY)
   end do

   if (diag >= 2) print *, 'gridded_climatology: Return.'

end subroutine gridded_climatology
end module gridded__climatology
