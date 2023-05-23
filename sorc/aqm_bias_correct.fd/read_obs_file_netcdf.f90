!------------------------------------------------------------------------------
!
! read_obs_file_netcdf.f90 -- Read obs data from a single AirNow Netcdf file.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! system for CMAQ forecast outputs.
!
! 2023-apr-10	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
! Notes:
!
! This routine reads a specified obs data variable for one
! calendar date, from one 24-hour AirNow Netcdf file made by
! convert_airnow_netcdf.f90.
!
! Supplemental data are also read, including site ID's, lat and
! lon site coordinates, time coordinates, and the data variable's
! units attribute.
!
! Output arrays are auto-allocated.  Data are returned for all
! 24 hours, and all sites within the current file.  Caller is
! responsible for removing sites with no datas for the target
! obs variable, indicated by all missing data values.
!
! Lat and lon in AirNow sometimes change over time.  Therefore,
! lat and lon are returned as hourly values.  Caller should
! screen for changes.
!
! This version assumes fixed dimensions and variable names as
! created by convert_airnow_netcdf.f90.  Var names are copied
! verbatim from the original AirNow CSV source files, with the
! exceptions of date and time.
!
! Convert_airnow_netcdf.f90 sorts each Netcdf file by site ID.
! This program preserves this original sort order of sites.
!
! Error handling:
!
! This version is set up to never halt, and always return soft
! errors in case of missing or defective input files.  Some
! consistency checking is performed on the input file.
!
! Return codes (stdlit.f90):
!
! status = normal:  Success or warnings only.
! status = fail:    Soft error, invalid file.
!
!------------------------------------------------------------------------------

module read__obs_file_netcdf
contains

subroutine read_obs_file_netcdf (infile, varname_obs, year, month, day, diag, &
      file_ids, file_lats, file_lons, file_data, vmiss, file_units, status)

   use config, only : dp
   use netcdf
   use netcdf_sup
   use read__netcdf_var
   use stdlit, only : normal, fail
   implicit none

! Input arguments.

   character(*), intent(in) :: infile		! input file name
   character(*), intent(in) :: varname_obs	! requested obs var name in file
   integer,      intent(in) :: year, month, day	! expected date in file
   integer,      intent(in) :: diag		! diag verbosity level, 0-N

! Output arguments.

   character(*), intent(out),allocatable :: file_ids(:)    ! site ID strings (S)
   real(dp),     intent(out),allocatable :: file_lats(:,:) ! site coords (S,T)
   real(dp),     intent(out),allocatable :: file_lons(:,:)
   real(dp),     intent(out),allocatable :: file_data(:,:) ! time series (S,T)
   real(dp),     intent(out)             :: vmiss	   ! missing value code
   character(*), intent(out)             :: file_units	   ! data units string
   integer,      intent(out)             :: status	   ! result status,
   							   !   normal or fail
! Local variables.

   character(60) varname, time_units, expected_units
   character(40) fmt1, fmt2

   integer ncid, varid, time_id, nc_status
   integer ti, ntimes, nsites
   integer nmiss_obs, nmiss_lat, nmiss_lon

   real(dp) vmin, vmax, lat_min, lat_max, lon_min, lon_max
   real(dp), allocatable :: time_coords(:), expected_times(:)

!-------------------------------------------------
! Read obs and metadata variables.
!-------------------------------------------------

   if (diag >= 3) print *, 'read_obs_file_netcdf: Start.'

! Read obs data variable, site ID's, and coordinates.
! Use generic Netcdf reader.  Input array is auto-allocated.

! If any expected variable is missing, or other unexpected netcdf
! errors, the generic reader will print a reasonable error message.

   varname = varname_obs
   call read_netcdf_var (infile, varname, diag, file_data, status, &	! (S,T)
      nc_id=ncid, var_id=varid)

   if (status == normal) then
      varname = 'AQSID'					!
      call read_netcdf_var (infile, varname, diag, file_ids, status)	! (S)
   end if

   if (status == normal) then
      varname = 'Latitude'
      call read_netcdf_var (infile, varname, diag, file_lats, status)	! (S,T)
   end if

   if (status == normal) then
      varname = 'Longitude'
      call read_netcdf_var (infile, varname, diag, file_lons, status)	! (S,T)
   end if

   if (status == normal) then
      varname = 'time'
      call read_netcdf_var (infile, varname, diag, time_coords, &	! (T)
         status, var_id=time_id)
   end if

! Check for read error.

   if (status /= normal) then
      print *, '*** read_obs_file_netcdf: Read error, skipping file.'
      print *, '*** Missing varible, or other file defect.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Expected variable = ' // trim (varname)
      print *
      return
   end if

! Check dimensions.

   nsites = size (file_data, 1)
   ntimes = size (file_data, 2)

   if (ntimes /= 24) then
      print '(9a)',   '*** read_obs_file_netcdf: Invalid time dimension,', &
                           ' skipping file.'
      print '(9a)',   '*** File = ', trim (infile)
      print '(a,i0)', '*** Number of time steps = ', ntimes
      print '(9a)  ', '*** Expected = 24'
      print *
      status = fail
      return
   end if

   if (nsites < 1) then
      print '(9a)',   '*** read_obs_file_netcdf: Empty file, skipping.'
      print '(9a)',   '*** File = ', trim (infile)
      print '(a,i0)', '*** Number of sites = ', nsites
      print *
      status = fail
      return
   end if

!-------------------------------------------------
! Read attributes of target obs variable.
!-------------------------------------------------

   nc_status = nf90_get_att (ncid, varid, 'missing_value', vmiss)

   if (nc_status /= nf90_noerr) then
      print *, '*** read_obs_file_netcdf: Error reading missing_value', &
                    ' attribute.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = ' // trim (varname_obs)
      print *, '*** Netcdf error number = ', nc_status
      print *, '*** ', trim (nf90_strerror (nc_status))
      print *, '*** Soft error, skipping file.'
      print *
      status = fail
      return
   end if

   nc_status = nf90_get_att_trim (ncid, varid, 'units', file_units)

   if (nc_status /= nf90_noerr) then
      print *, '*** read_obs_file_netcdf: Error reading data units attribute.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = ' // trim (varname_obs)
      print *, '*** Netcdf error number = ', nc_status
      print *, '*** ', trim (nf90_strerror (nc_status))
      print *
      file_units = ' '		! soft error: print warning; return blank units
   end if			! keep going, do not skip file for this error

!-------------------------------------------------
! Read the time units attribute.
!-------------------------------------------------

   nc_status = nf90_get_att_trim (ncid, time_id, 'units', time_units)

   if (nc_status /= nf90_noerr) then
      print *, '*** read_obs_file_netcdf: Error reading time units attribute.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = time'
      print *, '*** Netcdf error number = ', nc_status
      print *, '*** ' // trim (nf90_strerror (nc_status))
      print *, '*** Soft error, skipping file.'
      print *
      status = fail
      return
   end if

! Validate the time units attribute.

   write (expected_units, "(a,i4,2('-',i0),a)") 'hours since ', year, month, &
      day, ' 0:0:0'

   if (time_units /= expected_units) then
      print *, '*** read_obs_file_netcdf: Invalid time units attribute.'
      print *, '*** File = ' // trim (infile)
      print *, '*** Var name = time'
      print *, '*** Time units attribute = [' // trim (time_units) // ']'
      print *, '*** Expected units =       [' // trim (expected_units) // ']'
      print *, '*** Soft error, skipping file.'
      print *
      status = fail
      return
   end if

!-------------------------------------------------
! Validate the time coordinates.
!-------------------------------------------------

   expected_times = (/ (ti, ti = 0, ntimes-1) /)	! real <-- integer

   if (any (time_coords /= expected_times)) then
      fmt1 = '(a, 99(1x,i0))'
      print '(9a)', '*** read_obs_file_netcdf: Invalid time coordinates.'
      print '(9a)', '*** File = ' // trim (infile)
      print '(9a)', '*** Var name = time'
      print fmt1,   '*** Time values =', time_coords(:)
      print fmt1,   '*** Expected    =', expected_times(:)
      print '(9a)', '*** Soft error, skipping file.'
      print *
      status = fail
      return
   end if

!-------------------------------------------------
! Diagnostics, if requested.
!-------------------------------------------------

! Assume same missing_value attribute between data var and coordinate vars.

   if (diag >= 3) then
      vmin    = minval (file_data, (file_data /= vmiss))
      vmax    = maxval (file_data, (file_data /= vmiss))

      lat_min = minval (file_lats, (file_lats /= vmiss))
      lat_max = maxval (file_lats, (file_lats /= vmiss))

      lon_min = minval (file_lons, (file_lons /= vmiss))
      lon_max = maxval (file_lons, (file_lons /= vmiss))

      nmiss_obs = count (file_data == vmiss)
      nmiss_lat = count (file_lats == vmiss)
      nmiss_lon = count (file_lons == vmiss)

      fmt1 = '(3x, 2(a, i0))'
      fmt2 = '(3x, 2(a, f0.3), 2a)'

      print fmt1, 'nsites, ntimes           = ', nsites, ' ', ntimes
      
      print fmt1, 'Count ' // trim(varname_obs) // ' missing      = ', nmiss_obs
      print fmt1, 'Count latitudes  missing = ', nmiss_lat
      print fmt1, 'Count longitudes missing = ', nmiss_lon

      print fmt2, 'Min, max ' // trim(varname_obs) // '           = ', vmin, &
                    ', ', vmax, ' ', trim (file_units)
      print fmt2, 'Min, max latitudes       = ', lat_min, ', ', lat_max
      print fmt2, 'Min, max longitudes      = ', lon_min, ', ', lon_max
      print *
   end if

   if (diag >= 4) print *, 'read_obs_file_netcdf: Return.'

end subroutine read_obs_file_netcdf
end module read__obs_file_netcdf
