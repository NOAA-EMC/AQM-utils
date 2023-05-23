!------------------------------------------------------------------------------
!
! read_gridded_aqm.f90 -- Read Netcdf gridded CMAQ and MET forecast data.
!
! This is a support routine for the NCEP/PSL bias correction system
! for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
!
! 2019-may-16	Handle varying number of forecast hours.
!		Change output data array to dynamically allocated.
!		Modify tests for required and varying dimension sizes.
!		Handle unexpected required dim sizes as soft error only.
!
! 2022-mar-29	Interface change.  Read several variables in a single call.
!		This is an optimization to read each data file one time only.
!		Retain support for previous concatenated AQM format.
!		Gridded 4-D output data array is now pre-allocated by caller.
! 2022-apr-04	New read_netcdf_var interface to speed up reading gridded data.
!		Read float32 data directly.  Omit intermediate 64-bit doubles.
! 2022-may-21	Read and return the units attributes.
!
! Notes:
!
! This reader is for the Netcdf gridded archive format for CMAQ
! and MET outputs, provided by Pius Lee, January 10, 2014.
!
! This reader handles both of the following file formats, and
! similar NCEP formats.  These formats use the same NCEP standard
! layout of dimensions and general structure.
!
!   CMAQ intermediate output files   aqm.t06z.O3_pm25.ncf
!   MET  intermediate output files   sfc_met_n_PBL.t06z.ncf
!
! Each call returns gridded forecast data for one or more Netcdf
! variables, and a single forecast cycle.
!
! The returned data array must be pre-allocated by the caller,
! including an extra dimension for multiple variables.  This is
! a 4-D array with dimensions X, Y, vars, and forecast hours.
! These correspond to the original file dimensions COL, ROW, and
! TSTEP.  NCEP's vestigial LAY dimension is removed.
!
! The forecast date and cycle start time are implicit, because
! these are embedded in each Netcdf file name.  It is the
! caller's responsibility to align the input day files into a
! correct time sequence.
!
! The units attribute string for each variable is also returned.
! A blank string is returned if there is any problem reading each
! units attribute, such as missing attribute.
!
! For efficiency, this routine keeps the input file open between
! consecutive variables from the same file.  File open/close is
! actually handled by the lower-level reader, read_netcdf_var.
! Caller should group the requested variables for each input
! file, before calling this reader.
!
! Caller should pre-fill the master output array with caller's
! missing values, before calling this reader.
!
! Missing values and missing value attributes are not expected in
! input files, and are not supported in this version.  If present,
! they will be treated as ordinary data values and will not
! receive the correct special handling.
!
!------------------------------------------------------------------------------

module read__gridded_aqm
contains

subroutine read_gridded_aqm (infile, varnames, var_select, nhours_expect, &
      diag, vdata, nhours_actual, units, status)

   use netcdf
   use netcdf_sup
   use read__netcdf_var
   use stdlit, only : normal, fail
   implicit none

   character(*), parameter :: &
      module_id = 'read_gridded_aqm.f90 version 2022-may-21'

   character(*), intent(in ) :: infile	 	 ! input file name
   character(*), intent(in ) :: varnames(:)	 ! requested var names (vars)
   logical,      intent(in ) :: var_select(:)	 ! mask for selected variables
   integer,      intent(in ) :: nhours_expect	 ! expected hours, warning only
   integer,      intent(in ) :: diag		 ! verbosity level, 0-N

   real,         intent(out) :: vdata(:,:,:,:)   ! gridded output array
		                                 !   (X, Y, vars, hours)
   integer,      intent(out) :: nhours_actual(:) ! actual hours read (vars)
   character(*), intent(out) :: units(:)	 ! units attributes (vars)
   integer,      intent(out) :: status           ! result status, normal or fail

! Local variables.

   character(len(varnames)) varname
   character(len(units)) units_in
   character fmt1*50

   integer vi, nvars, nx, ny, nlay
   integer nhours, nhours_file, nhours_copy
   integer dim4_in(4), dim3_expected(3)
   integer ncid, varid, nc_status

   real vmin, vmax

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Use single precision for efficiency, to match file data type.

   real, allocatable :: indata(:,:,:,:)		! (COL, ROW, LAY, TSTEP)

! Initialize.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'read_gridded_aqm: Start.'
   if (diag >= 3) print *, 'Module ID = ' // module_id

   nx     = size (vdata, 1)		   ! get dimensions
   ny     = size (vdata, 2)
   nvars  = size (vdata, 3)
   nhours = size (vdata, 4)

   where (var_select(:)) units(:) = ' '    ! default units strings to unknown

!-------------------------------------------------
! Main loop over each selected variable.
!-------------------------------------------------

var_loop: &
   do vi = 1, nvars
      if (.not. var_select(vi)) cycle	! skip vars not in current request

      varname = varnames(vi)

      if (diag >= 4) print *, '  Read var ' // trim (varname)

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.
! This routine actually keeps the current file open between
! consecutive calls for the same file, for efficiency.

      call read_netcdf_var (infile, varname, diag, indata, status, &
         nc_id=ncid, var_id=varid)

! Read errors are soft errors.
! On read error, return with fail status from lower level.

      if (status /= normal) return

! Check for dimensions required to conform, to ensure robustness.

      dim4_in       = shape (indata)	  ! input dims: COL, ROW, LAY, TSTEP
      nhours_file   = dim4_in(4)	  ! TSTEP only

      nlay          = 1			  ! expected vestigial LAY dimension
      dim3_expected = (/ nx, ny, nlay /)  ! expected first 3 dims, omit TSTEP

! Handle unexpected dim sizes as soft error.  Abort current forecast cycle only.

      if (any (dim4_in(1:3) /= dim3_expected(1:3))) then
         print *, '*** read_gridded_aqm: Incorrect var dimensions in file.'
         print *, '*** File = ' // trim (infile)
         print *, '*** Var name = ' // trim (varname)
         fmt1 = '(a,3(1x,i0),a)'
         print fmt1, &
            ' *** Expected 4-D dimensions     = (', dim3_expected(1:3), ' any)'
         print fmt1, &
            ' *** Current file var dimensions = (', dim4_in(:), ')'

         status = fail			! signal soft error, abort cycle
         return
      end if

! Check for unexpected number of forecast hours.
! Warning only.  Print and continue with variable size array.

      if (nhours_file /= nhours_expect) then
         print *,         '*** read_gridded_aqm: Warning:' &
                                // ' Unexpected number of forecast hours.'
         print *,         '*** File = ' // trim (infile)
         print *,         '*** Var name = ' // trim (varname)
         print '(a,i0)', ' *** Expected number of hours = ', nhours_expect
         print '(a,i0)', ' *** Number of hours in file  = ', nhours_file
      end if

! Transfer data for this variable to the correct slice of the 4-D output array.
! Omit the vestigial LAY dimension.

! In unexpected case, omit time steps exceeding the size of the output array.
! Assume the previous warning will suffice.

      nhours_copy       = min (nhours, nhours_file)
      nhours_actual(vi) = nhours_copy		! output hours actually returned

      vdata(:,:,vi,1:nhours_copy) = indata(:,:,1,1:nhours_copy)
				! (X, Y, VAR, HOUR) <-- (COL, ROW, LAY, TSTEP)

! Also read the units attribute, if available.
! Any attribute error is silent and returns the defaulted blank string.

      nc_status = nf90_get_att_trim (ncid, varid, 'units', units_in)

      if (nc_status == nf90_noerr) units(vi) = units_in

! Diagnostics.

      if (diag >= 3) then
         vmin = minval (vdata(:,:,vi,1:nhours_copy))
         vmax = maxval (vdata(:,:,vi,1:nhours_copy))
         print '(2(a,f0.5),a,i4,4x,a)', '   Min, max data = ', vmin, ', ', &
            vmax, ', nhours_copy =', nhours_copy, trim (varname)
      end if

   end do var_loop

   if (diag >= 3) print *, 'read_gridded_aqm:  All variables read.  Return.'

end subroutine read_gridded_aqm
end module read__gridded_aqm
