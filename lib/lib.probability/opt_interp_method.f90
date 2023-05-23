!------------------------------------------------------------------------------
!
! opt_interp_method.f90 -- Make probability forecasts by optimal interpolation.
!
! This routine writes the probability output file, in addition to calculations.
!
! This is one of the top level method routines of the probability
! forecast module, a main component of the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2019-may-16	Data preprocessors by Irina Djalalova, NOAA/OAR/ESRL/PSD/CIRES:
!		  dataascii_preparation_for_probability_code.pro
!		  datanetcdf_preparation_for_probability_code.pro
!
! 2019-jun-10	data_input_out2.f90:
!		Fortran wrapper for probability analyzer, by Irina Djalalova.
!		Uses Tom Hamill's optimal interpolation routine,
!		  optimal_interp_analvar.f90 version 2019 April 26.
!
! 2019-jul-02	probability.f90:
!		Original module to integrate probability forecasts into
!		  bias correction.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Merge the previous three programs.
!		Convert data flow from intermediate files, to all in-memory.
! 2019-aug-07	Various improvements through several intermediate versions.
!		Add support for multiple forecast days, and multiple
!		  probability thresholds.
!		Move run parameters from hard coded, to config file.
! 2019-dec-28	Split off all climo code into single make_climatologies.f90.
!		Also split off find_nearest_grid_points.f90.
!		No functional changes.
!
! 2020-jul-31	opt_interp_method.f90:
!		Renamed from probability.f90, to support probability
!		  method selection.  No functional changes.
!
! * Remember to update the date in the module_id below.
!
! This probability method module incorporates six major functions:
!
! * Read gridded model forecasts for a given forecast cycle.
! * Get best analogs at obs sites, from BC analog search.
! * Compute the necessary averages and climatologies.
! * Apply optimal interpolation routine to compute analysis grids.
! * Compute final probability grids using cumulative distribution function.
! * Write probability output file.
!
! Primary inputs for OI method:
!
! * Gridded raw forecast file for current and previous forecast cycle.
! * Best analogs at site locations, for current and previous forecast cycles.
! * Grid coordinates and elevations.
! * Site coordinates.
!
! Primary output:
!
! * Single data file with several computed probability forecast grids.
!
! Notes:
!
! Input and output file paths are specified with path templates.
! These contain YYYY MM DD HH substitution strings that will
! resolve to desired file names for the given forecast cycle.
! Templates may include full or relative paths, and they may
! begin with an $ENV environnment variable.
!
! Forecast date index:  Integer Gregorian date, relative to
! 1 = January 1 of base_year.  See the index_to_date library
! routine for details.
!
!------------------------------------------------------------------------------

module opt__interp_method
contains

subroutine opt_interp_method (in_gridded_template, grid_coord_file, &
      output_file_template, target_var, forecast_date, cycle_time, &
      base_year, calendar, best_analogs, site_lats, site_lons, vmiss, &
      prob, diag)

   use cdf__to_probability
   use config, only : dp
   use expand__filename
   use find__nearest_grid_points
   use index_to_date_mod
   use make__climatologies
   use optimal__interp_analvar
   use probability_type
   use read__grid_coords
   use write__probability_netcdf
   implicit none

   character(*), parameter :: &
      module_id = 'opt_interp_method.f90 version 2020-jul-31'

! Input arguments.

   character(*), intent(in) :: in_gridded_template  ! gridded input template
   character(*), intent(in) :: grid_coord_file	    ! aux. grid coordinate file
   character(*), intent(in) :: output_file_template ! output filename template
   character(*), intent(in) :: target_var	    ! target variable name

   integer,      intent(in) :: forecast_date	    ! target date index
   integer,      intent(in) :: cycle_time	    ! forecast cycle time
   integer,      intent(in) :: base_year	    ! base year for date indexes
   character(*), intent(in) :: calendar	  	    ! calendar system in use

   real(dp),     intent(in) :: best_analogs(:,:,:)  ! best analogs, forecast day
						    ! (hours, analogs, sites)
  						    ! sort order does not matter
   real(dp),        intent(in) :: site_lats(:)	    ! site coordinates
   real(dp),        intent(in) :: site_lons(:)
   real(dp),        intent(in) :: vmiss		    ! common missing value code
   type(prob_type), intent(in) :: prob		    ! probability config params
   integer,         intent(in) :: diag		    ! diag verbosity level, 0-N

! Local variables.

   character fdate_str*24, fmt1*60, fmt2*60, fmt3*60
   character outfile*200

   integer nx, ny, nsites, nsites_valid
   integer fday, ndays
   integer year, month, day
   integer ithresh, nthresh
   integer xmin, xmax, ymin, ymax
   integer nmiss_m, nmiss_v

   real rho, vdconst				! only copies, for readability

   real(dp) mmin, mmax, vmin, vmax, lmin, lmax, nmin, nmax

! Dynamic arrays.

   integer,  allocatable :: iclosest(:)		! closest grid indices for sites
   integer,  allocatable :: jclosest(:)		!   (nsites)

   integer,  allocatable :: iclosest2(:)	! packed, valid sites only
   integer,  allocatable :: jclosest2(:)	! for current forecast day

   logical,  allocatable :: site_mask(:)	! packed, valid sites only

   real(dp), allocatable :: grid_lats(:,:)	   ! grid coordinates (X, Y)
   real(dp), allocatable :: grid_lons(:,:)
   real(dp), allocatable :: grid_elevations(:,:)

   real(dp), allocatable :: climo_mean(:,:,:)	   ! climo result grids
   real(dp), allocatable :: climo_variance(:,:,:)  !   (X, Y, fdays)

   real(dp), allocatable :: analog_mean(:,:)	   ! site climos for analogs
   real(dp), allocatable :: analog_variance(:,:)   !   (sites, fdays)

   real(dp), allocatable :: analog_mean2(:)	   ! packed, valid sites only
   real(dp), allocatable :: analog_variance2(:)	   ! inperpolator input arrays
   real(dp), allocatable :: site_lats2(:)	   !   (sites)
   real(dp), allocatable :: site_lons2(:)

   real(dp), allocatable :: analog_mean3(:,:)	   ! copies of above packed
   real(dp), allocatable :: analog_variance3(:,:)  ! interpolator input arrays
   real(dp), allocatable :: site_lats3(:,:)	   ! for diagnostic outputs only
   real(dp), allocatable :: site_lons3(:,:)	   !   (sites, fdays)

   integer,  allocatable :: nsites_valid3(:)	   ! save valid length of above
   						   ! packed slices (fdays)

! 2019-jun-28: Single precision, temporary:

   real, allocatable :: analyzed_state(:,:,:)		! analyzer output
   real, allocatable :: analysis_error_variance(:,:,:)	!  arrays (X, Y, fdays)

! Probability output grids.  Map to CMAQ standard dimensions:
!   (X, Y, thresh, days) --> (X, Y, LEV, TSTEP)

   real(dp), allocatable :: background_probability(:,:,:,:) ! (X,Y,thresh,fdays)
   real(dp), allocatable :: background_zscore(:,:,:,:)
   real(dp), allocatable :: background_cdf(:,:,:,:)

   real(dp), allocatable :: analyzed_probability(:,:,:,:)   ! (X,Y,thresh,fdays)
   real(dp), allocatable :: analyzed_zscore(:,:,:,:)
   real(dp), allocatable :: analyzed_cdf(:,:,:,:)

!-------------------------------------------------------
! Initialize.
!-------------------------------------------------------

   if (diag >= 2) print *
   if (diag >= 2) print *, 'opt_interp_method: Start method routine.'
   if (diag >= 2) print *, '  Module ID = ' // module_id

! Decode the date index for the current forecast cycle.

   call index_to_date (forecast_date, year, month, day, base_year, calendar)
      						! get current Y M D integers

! Read CMAQ 2-D coordinate grids and elevations.

   if (diag >= 2) print *
   if (diag >= 2) print *, 'opt_interp_method: Read CMAQ 2-D coordinate grids' &
      // ' and elevations.'

   call read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons, &
      grid_elevations)

!-------------------------------------------------------
! Compute model and site climatologies.
!-------------------------------------------------------

! This combined climo module also does variance limiting, and prints
! array summary statistics.  See module components for more details.

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str,'  opt_interp_method: Call make_climatologies.'

   call make_climatologies (in_gridded_template, grid_coord_file, &
      target_var, forecast_date, cycle_time, base_year, calendar, &
      best_analogs, vmiss, prob, diag, climo_mean, climo_variance, &
      analog_mean, analog_variance)

! Model climo result grids,   mean and variance: (X, Y, fdays)
! Analog climo result arrays, mean and variance: (sites, fdays)

!-------------------------------------------------------
! Find nearest grid points for site locations.
!-------------------------------------------------------

   print *
   print *, 'opt_interp_method: Find nearest grid points for site locations.'

! Missing input site coordinates are handled correctly.
! Missing values in (I, J) are returned in such cases.

   call find_nearest_grid_points (site_lats, site_lons, grid_lats, &
      grid_lons, vmiss, diag, iclosest, jclosest)

!-------------------------------------------------------
! Initialize for main loop over forecast days.
!-------------------------------------------------------

! Get dimensions.

   nx      = size (grid_lats, 1)	! grid dims from CMAQ coordinate grid
   ny      = size (grid_lats, 2)

   nsites  = size (site_lats)		! number of obs sites before validation
   ndays   = size (climo_mean, 3)	! no. forecast days from climo routines
   nthresh = size (prob%thresh)		! number of requested threshold values

! Allocate fixed work arrays that span multiple forecast days.

   allocate (analyzed_state         (nx, ny, ndays))	! optimal interpolator
   allocate (analysis_error_variance(nx, ny, ndays))	! result arrays

   allocate (background_probability(nx, ny, nthresh, ndays))  ! probability
   allocate (background_zscore     (nx, ny, nthresh, ndays))  ! intermediate and
   allocate (background_cdf        (nx, ny, nthresh, ndays))  ! result arrays

   allocate (analyzed_probability  (nx, ny, nthresh, ndays))
   allocate (analyzed_zscore       (nx, ny, nthresh, ndays))
   allocate (analyzed_cdf          (nx, ny, nthresh, ndays))

! Allocate fixed arrays "3" only to save dynamic site data, for diagnostics.

   allocate (nsites_valid3(ndays))   ! this to track valid length of each slice

   allocate (site_lats3      (nsites, ndays))
   allocate (site_lons3      (nsites, ndays))
   allocate (analog_mean3    (nsites, ndays))
   allocate (analog_variance3(nsites, ndays))

! Init storage arrays to keep the unfilled areas at the ends clean.

   site_lats3       = vmiss
   site_lons3       = vmiss
   analog_mean3     = vmiss
   analog_variance3 = vmiss

!------------------------------------------------------------
! Main loop over each available probability forecast day.
!------------------------------------------------------------

fday_loop: &
   do fday = 1, ndays

      call fdate (fdate_str)
      print *
      print '(a)','------------------------------------------------------------'
      print '(2a,i0,a)', fdate_str, '  Start probability forecast day ',fday,'.'
      print '(a)','------------------------------------------------------------'
      print *

!-------------------------------------------------------
! Omit sites with invalid data.
!-------------------------------------------------------

      print *, ' Omit sites with invalid data or coordinates.'

! Create mask for sites with valid data and coordinates.
! Auto-reallocate as needed.

      site_mask =      (analog_mean    (:,fday) /= vmiss) &
                 .and. (analog_variance(:,fday) /= vmiss) &
                 .and. (iclosest       (:)      /= vmiss) &
                 .and. (jclosest       (:)      /= vmiss)

      nsites_valid = count (site_mask)

      if (diag >= 2) then
         print *
         print *, '   Number of sites in input data   = ', nsites
         print *, '   Number of valid sites remaining = ', nsites_valid
      end if

! Make packed arrays containing valid sites only.  Auto-reallocate.

      iclosest2        = pack (iclosest (:),            site_mask)
      jclosest2        = pack (jclosest (:),            site_mask)
      site_lats2       = pack (site_lats(:),            site_mask)
      site_lons2       = pack (site_lons(:),            site_mask)
      analog_mean2     = pack (analog_mean    (:,fday), site_mask)
      analog_variance2 = pack (analog_variance(:,fday), site_mask)

! Also save these OI input site arrays in fixed size arrays.
! The only purpose is to write to output file later, for diagnostics.

      nsites_valid3(fday) = nsites_valid	! remember the valid extent
      						!   for each forecast day

      site_lats3      (1:nsites_valid, fday) = site_lats2(:)
      site_lons3      (1:nsites_valid, fday) = site_lons2(:)
      analog_mean3    (1:nsites_valid, fday) = analog_mean2(:)
      analog_variance3(1:nsites_valid, fday) = analog_variance2(:)

!-------------------------------------------------------
! Input diagnostics for optimal interpolation module.
!-------------------------------------------------------

! Summary of site data for OI.

      if (diag >= 2) then
         mmin = minval (analog_mean2)
         mmax = maxval (analog_mean2)

         vmin = minval (analog_variance2)
         vmax = maxval (analog_variance2)

         lmin = minval (site_lats2)
         lmax = maxval (site_lats2)

         nmin = minval (site_lons2)
         nmax = maxval (site_lons2)

         xmin = minval (iclosest2)
         xmax = maxval (iclosest2)

         ymin = minval (jclosest2)
         ymax = maxval (jclosest2)

         fmt1 = '(4x,a,2g14.4)'
         fmt2 = '(4x,a,2i14)'
         fmt3 = '(4x,a,i14,a)'

         print *
         print *, ' opt_interp_method: Site data, OI input, summary over all' &
                      // ' valid sites:'
         print *

         print fmt2, 'Number of valid sites     =', nsites_valid
         print fmt1, 'Analog mean      min, max =', mmin, mmax
         print fmt1, 'Analog variance  min, max =', vmin, vmax

         print fmt1, 'Site latitudes   min, max =', lmin, lmax
         print fmt1, 'Site longtitudes min, max =', nmin, nmax

         print fmt2, 'iclosest         min, max =', xmin, xmax
         print fmt2, 'jclosest         min, max =', ymin, ymax
      end if

! Summary of model data for OI.

      if (diag >= 2) then
         mmin    = minval (climo_mean    (:,:,fday))
         mmax    = maxval (climo_mean    (:,:,fday))

         vmin    = minval (climo_variance(:,:,fday))
         vmax    = maxval (climo_variance(:,:,fday))

         nmiss_m = count  (climo_mean    (:,:,fday) == vmiss)
         nmiss_v = count  (climo_variance(:,:,fday) == vmiss)

         print *
         print *,' opt_interp_method: Model data, OI input, summary over all' &
                     // ' grid points:'
         print *

         print fmt1, 'Climo mean      min, max             =', mmin, mmax
         print fmt1, 'Climo variance  min, max             =', vmin, vmax

         print fmt3, 'Climo mean,     count missing values = ', nmiss_m, &
                        '  (should be zero)'
         print fmt3, 'Climo variance, count missing values = ', nmiss_v, &
                        '  (should be zero)'
      end if

!-------------------------------------------------------
! Call optimal interpolation module.
!-------------------------------------------------------

      print *
      call fdate (fdate_str)
      print '(2a,i0,a)', fdate_str, '  opt_interp_method:' &
         // ' Call optimal interpolation, forecast day ', fday, '.'

! Copy run parameters, for readability.

      rho     = prob%horizontal_length_scale_rho
      vdconst = prob%vertical_length_scale_vdconst

! Call optimal interpolation.
! Arguments nx, ny, iclosest, and jclosest must all be consistent
! with the intended X, Y dimension order.

      call optimal_interp_analvar (analog_mean2, &
         real (site_lats2), real (site_lons2), jclosest2, iclosest2, &
         real (climo_mean(:,:,fday)), real (grid_elevations), &
         real (grid_lons), real (grid_lats), real (climo_variance(:,:,fday)), &
         rho, vdconst, real (analog_variance2), nx, ny, nsites_valid, diag, &
         analyzed_state(:,:,fday), analysis_error_variance(:,:,fday))

      if (diag >= 3) then
         print *
         print *, ' opt_interp_method: Optimal interpolation module returned.'
      end if

!-------------------------------------------------------
! Compute probability grids using CDF.
!-------------------------------------------------------

      print *
      print *, ' Compute background probability for ' // trim(target_var) // '.'
      print *

      do ithresh = 1, nthresh
         print '(4x,2a,f8.3,1x,a)', trim (target_var), ', threshold =', &
            prob%thresh(ithresh), trim (prob%thresh_units)

         call cdf_to_probability ('background', climo_mean(:,:,fday), &
            climo_variance(:,:,fday), grid_lats, grid_lons, &
            prob%thresh(ithresh), vmiss, diag, &
            background_probability(:,:,ithresh,fday), &
            background_zscore(:,:,ithresh,fday), &
            background_cdf(:,:,ithresh,fday))
      end do

      print *
      print *, ' Compute analyzed probability for ' // trim (target_var) // '.'
      print *

      do ithresh = 1, nthresh
         print '(4x,2a,f8.3,1x,a)', trim (target_var), ', threshold =', &
            prob%thresh(ithresh), trim (prob%thresh_units)

         call cdf_to_probability ('analyzed', dble (analyzed_state(:,:,fday)), &
            dble (analysis_error_variance(:,:,fday)), grid_lats, grid_lons, &
            prob%thresh(ithresh), vmiss, diag, &
            analyzed_probability(:,:,ithresh,fday), &
            analyzed_zscore(:,:,ithresh,fday), analyzed_cdf(:,:,ithresh,fday))
      end do

   end do fday_loop

!-------------------------------------------------------
! Write probability output file.
!-------------------------------------------------------

   print *
   call fdate (fdate_str)
   print '(2a)', fdate_str, '  opt_interp_method: Write output file.'

! Create actual output file name for current forecast cycle.

   call expand_filename (output_file_template, year, month, day, cycle_time, &
      outfile)

! Write output file.  Data and coordinate vars will be converted from
! double to single precision, when writing to file.

! Translation of array names for output file:
!
!     analog_mean3     --> site_means
!     analog_variance3 --> site_error_variance
!
!     climo_mean       --> background_state
!     climo_variance   --> background_error_variance

   call write_probability_netcdf (outfile, target_var, grid_lats, &
      grid_lons, analyzed_probability, vmiss, prob, diag, &
      nsites_valid3, site_lats3, site_lons3, analog_mean3, &
      analog_variance3, grid_elevations, climo_mean, climo_variance, &
      background_probability, background_zscore, background_cdf, &
      dble (analyzed_state), dble (analysis_error_variance), &
      analyzed_zscore, analyzed_cdf)

   if (diag >= 2) print *, &
      'opt_interp_method: All done.  Return to main program.'

end subroutine opt_interp_method
end module opt__interp_method
