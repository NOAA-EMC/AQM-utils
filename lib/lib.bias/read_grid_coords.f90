!------------------------------------------------------------------------------
!
! read_grid_coords.f90 -- Read 2-D grid coordinates from CMAQ coordinate file.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/ESRL/CIRES/PSD.
! 2019-jun-18	Optionally read terrain elevation from same CMAQ file.
! 2022-jun-01	Add support for RRFS file format.
!		Automatically adapt to known variations in var names and rank.
!		Convert longitudes 0-360 to -180/+180, as needed for CMAQ.
!
! Notes:
!
! In this version, any error reading the coordinate file will
! cause program abort.  The coordinate file is fundamental.
!
! Coordinate var names and dimensionality (rank) are currently
! hard coded for two known CMAQ file formats.
!
! 2-D and 4-D variations in dimensionality (rank) are handled
! automatically.  The older vestigial dimensions TSTEP and LAY
! are safely ignored.
!
!------------------------------------------------------------------------------

module read__grid_coords

contains

subroutine read_grid_coords (grid_coord_file, diag, grid_lats, grid_lons, &
      elevations)

   use config, only : dp
   use netcdf_sup
   use read__netcdf_var
   use stdlit, only : normal
   implicit none

! Input arguments.

   character(*), intent(in) :: grid_coord_file		! name of coord file
   integer,      intent(in) :: diag			! verbosity level, 0-N

! Output arguments.

   real(dp), intent(out), allocatable :: grid_lats(:,:)  ! grid coordinates
   real(dp), intent(out), allocatable :: grid_lons(:,:)  ! (X, Y) or (COL, ROW)

   real(dp), intent(out), allocatable, optional :: elevations(:,:)

! Local variables.

   character(60) varname, varname_lat, varname_lon, varname_elev
   integer nx, ny, status, rank
   logical file_missing, var_missing, elev

! 4-D input array to conform to early CMAQ file format.

   real(dp), allocatable :: data_4d(:,:,:,:)	  ! (COL, ROW, LAY, TSTEP)

   if (diag >= 1) print *, 'read_grid_coords: Start.'

! Query the input file to decide between one of two known formats.
! Decide based on a single var name.

   call getnc_dim_sizes (grid_coord_file, 'LAT', diag, rank, status, &
      file_missing, var_missing)

! We only care about two things at this point; whether the file
! is missing, and whether the requested var name is found.
! These determine the file format and the next course of action.

   if ( (.not. file_missing) .and. (.not. var_missing) ) then
      varname_lat  = 'LAT'	  ! known format for aqm.t12z.grdcro2d.ncf,
      varname_lon  = 'LON'	  ! CMAQ, 2014 through 2022
      varname_elev = 'HT'
      rank         =  4
   else
      varname_lat  = 'lat'	  ! known format for aqm.tCCz.chem_sfc_fFFF.nc,
      varname_lon  = 'lon'	  ! RRFS/online, 2022
      varname_elev = 'topography' ! topography currently unknown, 2022 June 1
      rank         =  2
   end if

   if (diag >= 2) print '(99a)', '   Var names            = ', &
      trim (varname_lat), ', ', trim (varname_lon)

! Read latitude grid into conforming array.
! Use generic Netcdf reader.  Input array is auto-allocated.
! For 2-D, read output array directly.  For 4-D, use intermediate array.

! If coord file was missing in query above, or other unexpected netcdf
! error, this call will print a reasonable error message.

   varname = varname_lat

   if (rank == 2) then
      call read_netcdf_var (grid_coord_file, varname, diag, grid_lats, status)
   else					! assume rank 4
      call read_netcdf_var (grid_coord_file, varname, diag, data_4d,   status)
      if (status == normal) grid_lats = data_4d(:,:,1,1)	! auto allocate
   end if				! (X, Y) <-- (COL, ROW, LAY, TSTEP)

! Read longitude grid.

   if (status == normal) then
      varname = varname_lon
      if (rank == 2) then
         call read_netcdf_var(grid_coord_file, varname, diag, grid_lons, status)
      else				! assume rank 4
         call read_netcdf_var(grid_coord_file, varname, diag, data_4d,   status)
         if (status == normal) grid_lons = data_4d(:,:,1,1)
      end if
   end if

! Read elevations grid, optional.

   elev = (present (elevations))

   if ( (status == normal) .and. elev ) then
      varname = varname_elev
      if (rank == 2) then
         call read_netcdf_var(grid_coord_file, varname, diag, elevations,status)
      else				! assume rank 4
         call read_netcdf_var(grid_coord_file, varname, diag, data_4d,   status)
         if (status == normal) elevations = data_4d(:,:,1,1)
      end if
   end if

! Check for read error.

   if (status /= normal) then
      print *, '*** read_grid_coords: Abort, error reading grid coordinate' &
         // ' file.'
      print *, '*** File = ' // trim (grid_coord_file)
      print *, '*** Var name = ' // trim (varname)
      call exit (1)
   end if

! Convert longitudes 0-360 to -180/+180, as needed.

   if (any (grid_lons > 180)) then
      if (diag >= 2) then
         print *, '  Convert longitudes from 0-360 to -180/+180.'
         print *, '    Before conversion:'
         print '(2(a,f0.3))', '   Min, max longitudes  = ', &
            minval (grid_lons), ', ', maxval (grid_lons)
      end if

      where (grid_lons > 180) grid_lons = grid_lons - 360

      if (diag >= 2) then
         print *
         print *, '    After conversion:'
      end if
   end if

! Diagnostics, if requested.

   if (diag >= 2) then
      nx = size (grid_lats, 1)
      ny = size (grid_lats, 2)

      print '(2(a,i0))',   '   Grid size (lat, lon) = ', ny, ', ', nx
      print '(2(a,f0.3))', '   Min, max latitudes   = ', &
         minval (grid_lats), ', ', maxval (grid_lats)
      print '(2(a,f0.3))', '   Min, max longitudes  = ', &
         minval (grid_lons), ', ', maxval (grid_lons)
      if (elev) print '(2(a,f0.3))', '   Min, max elevations  = ', &
         minval (elevations), ', ', maxval (elevations)
   end if

   if (diag >= 3) print *, 'read_grid_coords: Return.'

end subroutine read_grid_coords

end module read__grid_coords
