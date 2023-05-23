!------------------------------------------------------------------------------
!
! write_probability_netcdf.f90 -- Write bias corrected Netcdf output file.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-jul-23	Prototype version.  Based on output code in data_input_out2.f90
!		  version 2019-jun-10, by Irina Djalalova, NOAA/ESRL/PSD/CIRES.
! 2019-aug-05	Production version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!
! * Remember to update the date in the module_id below.
!
! This routine writes a single Netcdf file for gridded probability
! forecasts, for one chemical species, and one forecast cycle.
! Daily probability grids for the available forecasted whole days,
! and several requested probability threshold levels, are written.
!
! A variety of intermediate arrays may optionally be written to
! the same file, for testing and verification.
!
! This version writes all output grids and grid coordinates as
! single precision floats, not doubles, to match regular CMAQ
! output files.
!
! Missing values are currently supported only in the variable-
! length supplemental site arrays.  It is assumed that all other
! output arrays are fully populated.
!
! This version uses standard CMAQ names ROW and COL for the grid
! dimensions.  Custom names for this probability application are
! used for other needed dimensions.
!
! For partial CF compatibility, the grid coordinate variables are
! written as 2-D file variables, not 4-D.
!
! This version does not overwrite existing files.  Attempting
! to overwrite will result in a fatal error.
!
! Note that the site-related output arrays are written as
! specially packed 2-D arrays, to support the general case where
! the valid sites may change between forecast days.  The variable
! nsites_valid(FDAY) contains the number of valid sites for each
! forecast day.  Valid site data for each forecast day FDAY is
! packed into the sub-array (1:nsites_valid(FDAY)) for that day's
! array slice.  This applies to site coordinates as well as site
! means and variances.
!
! Error handling:
!
! Currently, all Netcdf errors in this writer are considered to
! be serious problems.  All errors result in diagnostic message,
! and program abort.  There are no soft error returns.
!
!------------------------------------------------------------------------------

module write__probability_netcdf
contains

subroutine write_probability_netcdf (outfile, target_var, grid_lats, &
    grid_lons, analyzed_probability, vmiss, prob, diag, &
    nsites_valid, site_lats, site_lons, site_means, site_error_variance, &
    grid_elevations, background_state, background_error_variance, &
    background_probability, background_zscore, background_cdf, &
    analyzed_state, analysis_error_variance, analyzed_zscore, analyzed_cdf)

  use probability_type, only : prob_type
  use netwrite3, only : dp, netcreate3, netwrite3_close, &
                        writevar, write_var_att
  implicit none

  character(*), parameter :: &
    module_id = 'write_probability_netcdf.f90 version 2019-aug-05'

! Input arguments.

  character(*),    intent(in) :: outfile	   ! name of output file
  character(*),    intent(in) :: target_var	   ! primary target var name

  real(dp),        intent(in) :: grid_lats(:,:)	   ! grid coordinates (X, Y)
  real(dp),        intent(in) :: grid_lons(:,:)
  real(dp),        intent(in) :: analyzed_probability(:,:,:,:)
  						   ! (X, Y, thresh, fday)
  						   ! main probabil. result grids
  real(dp),        intent(in) :: vmiss		   ! common missing value
  type(prob_type), intent(in) :: prob		   ! probability config params
  integer,         intent(in) :: diag		   ! diagnostic verbosity level

! Supplmental output arrays.  Only written if write_supplemental = true.

  integer,         intent(in) :: nsites_valid(:)	   ! (fdays)

  real(dp),        intent(in) :: site_lats(:,:)		   ! (sites, fdays)
  real(dp),        intent(in) :: site_lons(:,:)
  real(dp),        intent(in) :: site_means(:,:)	   ! (sites, fdays)
  real(dp),        intent(in) :: site_error_variance(:,:)

  real(dp),        intent(in) :: grid_elevations(:,:)	   ! (X, Y)

  real(dp),        intent(in) :: background_state(:,:,:)   ! (X, Y, fdays)
  real(dp),        intent(in) :: background_error_variance(:,:,:)

  real(dp),        intent(in) :: background_probability(:,:,:,:)
  real(dp),        intent(in) :: background_zscore(:,:,:,:)
  real(dp),        intent(in) :: background_cdf(:,:,:,:)   ! (X, Y,thresh,fdays)

  real(dp),        intent(in) :: analyzed_state(:,:,:)	   ! (X, Y, fdays)
  real(dp),        intent(in) :: analysis_error_variance(:,:,:)

  real(dp),        intent(in) :: analyzed_zscore(:,:,:,:)  ! (X,Y,thresh,fdays)
  real(dp),        intent(in) :: analyzed_cdf(:,:,:,:)

! Local parameters.

  real,         parameter :: no_missing = 0.0	! special code for netwrite3:
						! do not add missing value att

  character(*), parameter :: no_long  = ' '	! special codes for netwrite3:
  character(*), parameter :: no_units = ' '	! suppress these other attribs

! Reserve extra Netcdf-3 header space.  Ignored with Netcdf-4 format.
! Tune for the maximum expected amount of header info in one output file.
! Optimal is roughly 1000 extra after the whole file is written.
! Non-critical parameter.  If too small, file takes a little longer to write.

  integer, parameter :: reserve_header = 4000

! Local variables.

  character(200) history, title
  character outvar*60, varexp*100, fmt1*50

  integer ncid, fday, ndays
  real vmin, vmax

!-----------------------------------------------------------
! Create a new netcdf file, and write the initial header.
!-----------------------------------------------------------

  if (diag >= 3) print *
  if (diag >= 3) print *, 'write_probability_netcdf: Create new Netcdf file.'
  if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

  title   = 'CMAQ ' // trim (target_var) // ' probability forecasts'
  history = 'Created by ' // module_id

  call netcreate3 (outfile, title, history, reserve_header, ncid, diag=diag)
  					! history time stamp will be added

!-------------------------------------------------
! Write 1-D base coordinate variables.
!-------------------------------------------------

  if (diag >= 2) print *, '  Write 1-D base coordinate variables.'

  if (diag >= 2) print *, '    Write coordinate variable: fday'
  ndays = size (analyzed_probability, 4)
  call writevar ('fday (fday)', 'Forecast day number', 'days', &
    (/ (fday, fday = 1, ndays) /), int (no_missing))

  if (diag >= 2) print *, '    Write coordinate variable: thresh'
  call writevar ('thresh (thresh)', 'Probability threshold', &
    prob%thresh_units, real (prob%thresh), no_missing)

!-------------------------------------------------
! Write 2-D grid coordinate variables.
!-------------------------------------------------

! Make CF compliant 2-D coordinate variables with CMAQ var names.
! Dimensions are added automatically.
! Dimensions are in fortran order, not C and ncdump order.
! Convert double to single precision on the fly.

  if (diag >= 2) print *, '  Write 2-D grid coordinate variables.'

  if (diag >= 2) print *, '    Write coordinate variable: LAT'
  varexp = 'LAT (COL, ROW)'			! define dimensions
  call writevar (varexp, 'Latitude',  'degrees_north', real (grid_lats), &
    no_missing)

  if (diag >= 2) print *, '    Write coordinate variable: LON'
  varexp = 'LON (COL, ROW)'
  call writevar (varexp, 'Longitude', 'degrees_east',  real (grid_lons), &
    no_missing)

! Latitude range attribute.

  if (diag >= 3) print *, '    Write actual_range attributes for grid' &
    // ' coordinates.'

  vmin = minval (real (grid_lats))
  vmax = maxval (real (grid_lats))

  fmt1 = '(7x, 2(a, f0.4))'
  if (diag >= 2) print fmt1, 'LAT actual range = ', vmin, ', ', vmax

  call write_var_att ('LAT', 'actual_range', (/ vmin, vmax /) )

! Longitude range attribute.

  vmin = minval (real (grid_lons))
  vmax = maxval (real (grid_lons))

  if (diag >= 2) print fmt1, 'LON actual range = ', vmin, ', ', vmax

  call write_var_att ('LON', 'actual_range', (/ vmin, vmax /) )

!-------------------------------------------------
! Write main probability output grids.
!-------------------------------------------------

! Make composite file var name for the primary output grid only.

  outvar = trim (target_var) // '_probability'

! Write output array to file.  Convert double to single precision on the fly.
! Dimension names: (X, Y, thresh, fdays) --> (COL, ROW, thresh, fday)
! Dimensions are in fortran order, not C and ncdump order.

  if (diag >= 2) print *, '  Write main variable: ' // trim (outvar)

  varexp = trim (outvar) // '(COL, ROW, thresh, fday)'   ! define dimensions
  call writevar (varexp, no_long, no_units, real (analyzed_probability), &
    no_missing)

! Compute range attribute.  Convert to single precision to match data written!

  if (diag >= 3) print *, '    Write ' // trim (outvar) // ' attributes.'

  vmin = minval (real (analyzed_probability))
  vmax = maxval (real (analyzed_probability))

  fmt1 = '(7x, 2a, f0.4, a, f0.2)'
  if (diag >= 2) print fmt1, trim (outvar), ' actual range = ', vmin, ', ', vmax

  call write_var_att (outvar, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes, main variable only.

  call write_var_att (outvar, 'coordinates', 'LAT LON')     ! CF compliant
  call write_var_att (outvar, 'comment', 'OI name = analyzed_probability')

  call write_var_att (outvar, 'threshold_values', prob%thresh)  ! convenience;
  				! redundant with thresh coordinate variable
  call write_var_att (outvar, 'thresh_units', prob%thresh_units)

  call write_var_att (outvar, 'horizontal_length_scale_rho', &
                          prob%horizontal_length_scale_rho)

  call write_var_att (outvar, 'vertical_length_scale_vdconst', &
                          prob%vertical_length_scale_vdconst)

  call write_var_att (outvar, 'analog_variance_low_limit', &
                          prob%analog_variance_low_limit)

  call write_var_att (outvar, 'model_variance_low_limit', &
                          prob%model_variance_low_limit)

!-------------------------------------------------
! Write supplemental arrays, if selected.
!-------------------------------------------------

! General remarks for all remaining variables:
! Convert double to single precision on the fly.
! Same when computing actual_range, to match data written.
! Dimension names, 4-D only: (X, Y, thresh, fdays) --> (COL, ROW, thresh, fday)
! Dimensions are in fortran order, not C and ncdump order.

write_sup_arrays: &
  if (prob%write_supplemental_vars) then

    if (diag >= 2) print *, '  Write supplemental arrays, optional.'

    if (diag >= 2) print *, '    Write variable: grid_elevations'
    varexp = 'grid_elevations (COL, ROW)'
    call writevar (varexp, no_long, no_units, real(grid_elevations), no_missing)
    vmin = minval (real (grid_elevations))
    vmax = maxval (real (grid_elevations))
    call write_var_att ('grid_elevations', 'actual_range', (/ vmin, vmax /))

!-------------------------------------------------
! Write supplemental site arrays.
!-------------------------------------------------

! Site arrays are bottom packed to valid sites only, and top ends padded
! with missing values.
! Site arrays might have different valid sites for each forecast day.
! To track, add special attribute nsites_valid(fdays) to each site array.

    if (diag >= 2) print *, '    Write variable: site_lats'
    varexp = 'site_lats (site, fday)'
    call writevar (varexp, no_long, no_units, real (site_lats), real (vmiss))
    vmin = minval (real (site_lats), (site_lats /= vmiss))
    vmax = maxval (real (site_lats), (site_lats /= vmiss))
    call write_var_att ('site_lats', 'actual_range', (/ vmin, vmax /))
    call write_var_att ('site_lats', 'nsites_valid', nsites_valid(:))

    if (diag >= 2) print *, '    Write variable: site_lons'
    varexp = 'site_lons (site, fday)'
    call writevar (varexp, no_long, no_units, real (site_lons), real (vmiss))
    vmin = minval (real (site_lons), (site_lons /= vmiss))
    vmax = maxval (real (site_lons), (site_lons /= vmiss))
    call write_var_att ('site_lons', 'actual_range', (/ vmin, vmax /))
    call write_var_att ('site_lons', 'nsites_valid', nsites_valid(:))

    if (diag >= 2) print *, '    Write variable: site_means'
    varexp = 'site_means (site, fday)'
    call writevar (varexp, no_long, no_units, real (site_means), real (vmiss))
    vmin = minval (real (site_means), (site_means /= vmiss))
    vmax = maxval (real (site_means), (site_means /= vmiss))
    call write_var_att ('site_means', 'actual_range', (/ vmin, vmax /))
    call write_var_att ('site_means', 'nsites_valid', nsites_valid(:))

    if (diag >= 2) print *, '    Write variable: site_error_variance'
    varexp = 'site_error_variance (site, fday)'
    call writevar (varexp, no_long, no_units, real (site_error_variance), &
      real (vmiss))
    vmin = minval (real (site_error_variance), (site_error_variance /= vmiss))
    vmax = maxval (real (site_error_variance), (site_error_variance /= vmiss))
    call write_var_att ('site_error_variance', 'actual_range', (/ vmin, vmax /))

!-------------------------------------------------
! Write supplemental background arrays.
!-------------------------------------------------

    if (diag >= 2) print *, '    Write variable: background_state'
    varexp = 'background_state (COL, ROW, fday)'
    call writevar (varexp, no_long, no_units, real (background_state), &
      no_missing)
    vmin = minval (real (background_state))
    vmax = maxval (real (background_state))
    call write_var_att ('background_state', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: background_error_variance'
    varexp = 'background_error_variance (COL, ROW, fday)'
    call writevar (varexp, no_long, no_units, real (background_error_variance),&
      no_missing)
    vmin = minval (real (background_error_variance))
    vmax = maxval (real (background_error_variance))
    call write_var_att ('background_error_variance', 'actual_range', &
      (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: background_probability'
    varexp = 'background_probability (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (background_probability), &
      no_missing)
    vmin = minval (real (background_probability))
    vmax = maxval (real (background_probability))
    call write_var_att ('background_probability', 'actual_range', &
      (/ vmin,vmax /))

    if (diag >= 2) print *, '    Write variable: background_zscore'
    varexp = 'background_zscore (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (background_zscore), &
      no_missing)
    vmin = minval (real (background_zscore))
    vmax = maxval (real (background_zscore))
    call write_var_att ('background_zscore', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: background_cdf'
    varexp = 'background_cdf (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (background_cdf), no_missing)
    vmin = minval (real (background_cdf))
    vmax = maxval (real (background_cdf))
    call write_var_att ('background_cdf', 'actual_range', (/ vmin, vmax /))

!-------------------------------------------------
! Write supplemental analysis arrays.
!-------------------------------------------------

    if (diag >= 2) print *, '    Write variable: analyzed_state'
    varexp = 'analyzed_state (COL, ROW, fday)'
    call writevar (varexp, no_long, no_units, real (analyzed_state), no_missing)
    vmin = minval (real (analyzed_state))
    vmax = maxval (real (analyzed_state))
    call write_var_att ('analyzed_state', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: analysis_error_variance'
    varexp = 'analysis_error_variance (COL, ROW, fday)'
    call writevar (varexp, no_long, no_units, real (analysis_error_variance), &
      no_missing)
    vmin = minval (real (analysis_error_variance))
    vmax = maxval (real (analysis_error_variance))
    call write_var_att ('analysis_error_variance', 'actual_range', &
      (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: analyzed_zscore'
    varexp = 'analyzed_zscore (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (analyzed_zscore),no_missing)
    vmin = minval (real (analyzed_zscore))
    vmax = maxval (real (analyzed_zscore))
    call write_var_att ('analyzed_zscore', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: analyzed_cdf'
    varexp = 'analyzed_cdf (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (analyzed_cdf), no_missing)
    vmin = minval (real (analyzed_cdf))
    vmax = maxval (real (analyzed_cdf))
    call write_var_att ('analyzed_cdf', 'actual_range', (/ vmin, vmax /))

  end if write_sup_arrays

!-------------------------------------------------
! Close output file properly.
!-------------------------------------------------

  if (diag >= 4) print *, '  File complete, close properly.'

  call netwrite3_close			! write final atts and flush buffers

  if (diag >= 4) print *, 'write_probability_netcdf: Return.'

end subroutine write_probability_netcdf
end module write__probability_netcdf
