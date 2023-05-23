!------------------------------------------------------------------------------
!
! write_probability_netcdf_lr.f90 -- Write Netcdf probability output file.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2020-aug-04	Original version.  By Dave Allured, NOAA/ESRL/PSL/CIRES.
!		This version for the linear regression method.
!		Adapted from write_probability_netcdf.f90 version 2019-aug-05,
!		  which is the original version for the OI probability method.
!
! * Remember to update the date in the module_id below.
!
! This routine writes a single Netcdf file for gridded probability
! forecasts, for one chemical species, and one forecast cycle.
! Daily probability grids for the available forecasted whole days,
! and several requested probability threshold levels, are written.
!
! This version writes all output grids and grid coordinates as
! single precision floats, not doubles, to match regular CMAQ
! output files.
!
! Missing values are not supported.  It is assumed that all
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
! Error handling:
!
! Currently, all Netcdf errors in this writer are considered to
! be serious problems.  All errors result in diagnostic message,
! and program abort.  There are no soft error returns.
!
!------------------------------------------------------------------------------

module write__probability_netcdf_lr
contains

subroutine write_probability_netcdf_lr (outfile, target_var, grid_lats, &
    grid_lons, probability, prob, diag, grid_daily, zscore, cdf)

  use config, only : dp
  use probability_type, only : prob_type
  use netwrite3, only : netcreate3, netwrite3_close, writevar, write_var_att
  implicit none

  character(*), parameter :: &
    module_id = 'write_probability_netcdf_lr.f90 version 2020-aug-04'

! Input arguments.

  character(*),    intent(in) :: outfile	   ! name of output file
  character(*),    intent(in) :: target_var	   ! primary target var name

  real(dp),        intent(in) :: grid_lats(:,:)	   ! grid coordinates (X, Y)
  real(dp),        intent(in) :: grid_lons(:,:)
  real(dp),        intent(in) :: probability(:,:,:,:)
  						   ! (X, Y, thresh, fday)
  						   ! main probabil. result grids
  type(prob_type), intent(in) :: prob		   ! probability config params
  integer,         intent(in) :: diag		   ! diagnostic verbosity level

! Supplmental output arrays.  Only written if write_supplemental = true.

  real(dp),        intent(in) :: grid_daily(:,:,:) ! (X, Y, fdays)

  real(dp),        intent(in) :: zscore(:,:,:,:)   ! (X,Y,thresh,fdays)
  real(dp),        intent(in) :: cdf(:,:,:,:)

! Local parameters.

  real,         parameter :: no_missing = 0.0	! special code for netwrite3:
						! do not add missing value att

  character(*), parameter :: no_long  = ' '	! special codes for netwrite3:
  character(*), parameter :: no_units = ' '	! suppress these other attribs

! Reserve extra Netcdf-3 header space.  Ignored with Netcdf-4 format.
! Tune for the maximum expected amount of header info in one output file.
! Optimal is roughly 1000 extra after the whole file is written.
! Non-critical parameter.  If too small, file takes a little longer to write.

  integer, parameter :: reserve_header = 2000

! Local variables.

  character(200) history, title
  character outvar*60, varexp*100, fmt1*50

  integer ncid, fday, ndays
  real vmin, vmax

!-----------------------------------------------------------
! Create a new netcdf file, and write the initial header.
!-----------------------------------------------------------

  if (diag >= 3) print *
  if (diag >= 3) print *, 'write_probability_netcdf_lr: Create new Netcdf file.'
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
  ndays = size (probability, 4)
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
  call writevar (varexp, no_long, no_units, real (probability), &
    no_missing)

! Compute range attribute.  Convert to single precision to match data written!

  if (diag >= 3) print *, '    Write ' // trim (outvar) // ' attributes.'

  vmin = minval (real (probability))
  vmax = maxval (real (probability))

  fmt1 = '(7x, 2a, f0.4, a, f0.2)'
  if (diag >= 2) print fmt1, trim (outvar), ' actual range = ', vmin, ', ', vmax

  call write_var_att (outvar, 'actual_range', (/ vmin, vmax /) )

! Write supplemental attributes, main variable only.

  call write_var_att (outvar, 'coordinates', 'LAT LON')     ! CF compliant

  call write_var_att (outvar, 'threshold_values', prob%thresh)  ! convenience;
  				! redundant with thresh coordinate variable
  call write_var_att (outvar, 'thresh_units', prob%thresh_units)

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

    if (diag >= 2) print *, '    Write variable: grid_daily'
    varexp = 'grid_daily (COL, ROW, fday)'
    call writevar (varexp, no_long, no_units, real (grid_daily), &
      no_missing)
    vmin = minval (real (grid_daily))
    vmax = maxval (real (grid_daily))
    call write_var_att ('grid_daily', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: zscore'
    varexp = 'zscore (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (zscore),no_missing)
    vmin = minval (real (zscore))
    vmax = maxval (real (zscore))
    call write_var_att ('zscore', 'actual_range', (/ vmin, vmax /))

    if (diag >= 2) print *, '    Write variable: cdf'
    varexp = 'cdf (COL, ROW, thresh, fday)'
    call writevar (varexp, no_long, no_units, real (cdf), no_missing)
    vmin = minval (real (cdf))
    vmax = maxval (real (cdf))
    call write_var_att ('cdf', 'actual_range', (/ vmin, vmax /))

  end if write_sup_arrays

!-------------------------------------------------
! Close output file properly.
!-------------------------------------------------

  if (diag >= 4) print *, '  File complete, close properly.'

  call netwrite3_close			! write final atts and flush buffers

  if (diag >= 4) print *, 'write_probability_netcdf: Return.'

end subroutine write_probability_netcdf_lr
end module write__probability_netcdf_lr
