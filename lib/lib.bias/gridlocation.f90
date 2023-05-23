!------------------------------------------------------------------------------
!
! gridlocation.f90 -- Find AIRNow site locations on CMAQ grid.
!
! 2014-feb-20	gridlocation.f:
!		Original Fortran 77 version for CMAQ bias correction project.
!		Translated from IDL by Irina Djalalova, NOAA/ESRL/PSD3.
! 2014-apr-03	Dave Allured:  Prevent floating point error, invalid input
!		  to ACOS function, from roundoff error.
!
! 2014-apr-07	gridlocation.f90:
!		Convert to fortran 90.  By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Stand-alone demo version for testing and development.
!		Major speed improvement.  Add distance method to narrow down
!		  each point search to the nearest four grid cells, before
!		  using the more expensive contour integral method.
!
! 2014-apr-13	gridlocation.f90:
!		Convert from stand-alone demo, to subroutine module.
!		Remove all demo file I/O.
! 2014-may-06	Handle sites too close to north and east grid edges.
!		Change fprint hard coded verbosity control, to diag input arg.
!		Adjust diagnostics.
! 2014-may-12	Adjust verbosity for 2 = sparse progress display.
! 2014-jun-22	Add diagnostics for sites out of bounds.
!		Add one easy search optimization in search_four_cells.
!
! 2021-nov-11	Improve diagnostics.  Add site ID and nearest grid point
!		  distance, to off-grid messages.
!		Reduce verbosity for off-grid.  Covered by caller's summary.
!
! This program translates a list of lat, lon site coordinates into
! the lower left corner I, J indices of the containing cells on a
! given grid with two-dimensional coordinates.
!
! INPUTS:
! List of lat, lon site locations.
! Two-dimensional lat and lon coordinate arrays for current grid.
!
! OUTPUTS:
! List of I, J lower left corner indices for the corresponding grid cells.
!
!------------------------------------------------------------------------------

!---------------------------------------------------------------
! Container module for gridlocation routine and subroutines.
!---------------------------------------------------------------

module grid_location
   implicit none

   private				! all support routines are private
   public gridlocation			! only the main routine is visible

! Module parameters.

   integer, parameter :: dp = kind (1d0)	! default double precision

contains

!---------------------------------------------------------------
! Gridlocation main subroutine.
!---------------------------------------------------------------

subroutine gridlocation (site_lats, site_lons, grid_lats, grid_lons, &
      site_ids, diag, i_corners, j_corners, distance)
   implicit none

   real(dp),     intent(in)  :: site_lats(:)	! station coordinates (S)
   real(dp),     intent(in)  :: site_lons(:)
   real(dp),     intent(in)  :: grid_lats(:,:)	! grid coordinates (X,Y)
   real(dp),     intent(in)  :: grid_lons(:,:)
   character(*), intent(in)  :: site_ids(:)	! site ID strings (S)
   integer,      intent(in)  :: diag		! 0=quiet, 1=progress, 2=calcs

   integer,      intent(out) :: i_corners(:)	! output I, J corner indices (S)
   integer,      intent(out) :: j_corners(:)	!   I=X, J=Y
   real(dp),     intent(out) :: distance(:)	! distance to nearest grid point

! Local variables.

   integer i_nearest(size(site_lats))		! automatic arrays
   integer j_nearest(size(site_lats))

! Find nearest grid points, using the fast distance method.
! This reduces the search domain for the next step.

   if (diag >= 3) print *, 'gridlocation: Find nearest grid points.'

   call grid_search_distance (site_lats, site_lons, grid_lats, grid_lons, &
     i_nearest, j_nearest, distance)

! Now find the adjacent cells containing the site locations, using
! the contour integral method.  Get the lower left corners of the cells.

   if (diag >= 3) print *, 'gridlocation: Find containing grid cells,' &
     // ' using contour integrals.'

   call grid_search_contour (site_lats, site_lons, grid_lats, grid_lons, &
     i_nearest, j_nearest, distance, site_ids, diag, i_corners, j_corners)

   if (diag >= 4) print *, 'gridlocation: Return.'

end subroutine gridlocation

!---------------------------------------------------------------
! Find site locations on grid, using fast distance method.
!---------------------------------------------------------------

subroutine grid_search_distance (siteLAT, siteLON, XLAT, XLON, i_nearest, &
     j_nearest, distance)
   implicit none

   real(dp), intent (in ) :: siteLAT(:)		! lat, lon site coordinates (S)
   real(dp), intent (in ) :: siteLON(:)
   real(dp), intent (in ) :: XLAT(:,:)		! lat, lon grid coords (X,Y)
   real(dp), intent (in ) :: XLON(:,:)

   integer,  intent (out) :: i_nearest(:)	! I, J grid indices (S)
   integer,  intent (out) :: j_nearest(:)
   real(dp), intent (out) :: distance(:)	! distance to nearest grid point

   real distance_btw_coords		! external function

! Local variables.

   integer si
   integer inds(2)				! individual (I, J) indices

! Loop over all sites.

   do si = 1, size (siteLAT)

! Compute the simple planar distance from current site to all grid
! points.  Then get the (I,J) indices of the nearest grid point.
! To facilitate optimization, do all in the same statement.

! Ignore possible duplicate hits, should not make any significant
! difference in final process results.

      inds = minloc (sqrt (   (siteLAT(si) - XLAT(:,:)) ** 2 &
                            + (siteLON(si) - XLON(:,:)) ** 2   ) )

! Save resulting I, J indices and nearest point distance, for the current site.

      i_nearest(si) = inds(1)			! I = X index
      j_nearest(si) = inds(2)			! J = Y index

      distance(si) = distance_btw_coords ( &	! km - actual surface distance
         real (siteLAT(si)), real (siteLON(si)), &
         real (XLAT(inds(1),inds(2))), real (XLON(inds(1),inds(2))) )
   end do

end subroutine grid_search_distance

!---------------------------------------------------------------
! Find the grid cell containing each site location, using the
! contour integral method.
!---------------------------------------------------------------

! Output is the I, J indices of the lower left corners of containing cells.

! This routine is optimized by previously finding the closest grid
! points, using the fast distance method.

subroutine grid_search_contour (siteLAT, siteLON, XLAT, XLON, i_nearest, &
      j_nearest, distance, site_ids, diag, i_corners, j_corners)
   implicit none

   real(dp),     intent(in)  :: siteLAT(:)	! lat, lon site coordinates (S)
   real(dp),     intent(in)  :: siteLON(:)
   real(dp),     intent(in)  :: XLAT(:,:)	! lat, lon grid coords (X,Y)
   real(dp),     intent(in)  :: XLON(:,:)
   integer,      intent(in)  :: i_nearest(:)	! I, J nearest grid points (S)
   integer,      intent(in)  :: j_nearest(:)
   real(dp),     intent(in)  :: distance(:)	! distance to nearest grid point
   						!   for diagnostic only
   character(*), intent(in)  :: site_ids(:)	! site ID strings (S)
   integer,      intent(in)  :: diag		! 0=quiet, 1=progress, 2=calcs

   integer,      intent(out) :: i_corners(:)	! output I, J lower left
   integer,      intent(out) :: j_corners(:)	! corner indices (S); I:X, J:Y

! Local variables.

   integer isite, nx, ny
   integer i0, j0, icenter, jcenter

   real(dp) angle, xlat4(4), xlon4(4)

! Initialize.

   nx = size (xlat, 1)			! get grid dimensions
   ny = size (xlat, 2)

   i_corners(:) = -999			! clear output indices to all "missing"
   j_corners(:) = -999

! Main loop over all sites.

site_loop: &
   do isite = 1, size (siteLAT)

     if (diag >= 5) then
       print *, 'isite =', isite, '  x =', siteLON(isite), &
         '  y =', siteLAT(isite)
     end if

! Start with the indices of the nearest grid point to the current site.

     icenter = i_nearest(isite)
     jcenter = j_nearest(isite)

! Step away from the four grid edges, to prevent subscript errors.
! Sites next to one or two edges should still remain within the
! adjusted 2 x 2 search area, unless they are completely off grid.

     if (icenter == 1 ) icenter = icenter + 1
     if (jcenter == 1 ) jcenter = jcenter + 1

     if (icenter == nx) icenter = icenter - 1
     if (jcenter == ny) jcenter = jcenter - 1

! Search the adjacent four cells for the one containing the current site.
! I0 and J0 now represent lower left cell corners.

search_four_cells: &
     DO J0 = jcenter-1, jcenter
     DO I0 = icenter-1, icenter
       angle=-999.

! Single cell corners.

       XLAT4(1)=XLAT(I0  ,J0)
       XLAT4(2)=XLAT(I0  ,J0+1)
       XLAT4(3)=XLAT(I0+1,J0+1)
       XLAT4(4)=XLAT(I0+1,J0)
       XLON4(1)=XLON(I0  ,J0)
       XLON4(2)=XLON(I0  ,J0+1)
       XLON4(3)=XLON(I0+1,J0+1)
       XLON4(4)=XLON(I0+1,J0)

! Call the contour integral function.

       angle = CONTOUR_INTEGRAL_4 (siteLON(isite), siteLAT(isite), &
         XLON4, XLAT4, isite, diag)

!!       PRINT *,'I0=',I0,' J0=',J0,' angle=',angle

! Test for point inside the cell.  If yes, save the corner indices.
! See documentation in header of contour interval function.

       IF (ABS(angle-6.28).le.0.1) THEN
         i_corners(isite) = I0			! save corner indices
         j_corners(isite) = J0			! in output array
         exit search_four_cells			! skip unnecessary searches
       END IF

!!       IF (ABS(angle).gt.1.) THEN
!!         PRINT *,'>1.:::',I0,J0,angle
!!       END IF

     ENDDO
     ENDDO search_four_cells

! Check for point out of bounds, off grid.

     if (diag >= 3 .and. i_corners(isite) == -999) then
        I0 = i_nearest(isite)
        J0 = j_nearest(isite)

        print *, '*** Warning: Site coordinates are off grid, out of bounds.'
        print '(2a,2(2x,f0.5))', ' *** Site ID, lat and lon = ', &
           site_ids(isite), siteLAT(isite), siteLON(isite)
        print '(a,2(2x,f0.5))', ' *** Nearest grid point lat and lon =', &
           XLAT(I0,J0), XLON(I0,J0)
        print '(a,2(2x,i0),a,f0.1,a)', ' *** Nearest I/J indices (X, Y) =', &
           I0, J0, ', distance = ', distance(isite), ' km'
        print *
     end if

     if (diag >= 4) print *, 'isite =', isite, &
        '  i_corner =', i_corners(isite), '  j_corner =', j_corners(isite)
     if (diag >= 5) print *

   end do site_loop

end subroutine grid_search_contour

!---------------------------------------------------------------
! Calculate contour integral from the site location over
! the four corners of the current single cell.
!---------------------------------------------------------------

! This function computes the sum of angles from which each segment
! of the contour (XLON4,XLAT4 - 4 points) is seeing from the point (x,y).

! For the point outside the cell CI=0.
! For the point inside the cell CI=2*PI~=6.28
! For the point on the cell boundary CI=PI~=3.14

function contour_integral_4 (x, y, xlon4, xlat4, isite, diag) &
     result (integral_result)
   implicit none

   real(dp), intent (in) :: x, y		! site coordinates
   real(dp), intent (in) :: xlon4(:), xlat4(:)	! coordinates of 4 cell corners
   integer,  intent (in) :: isite		! site index for diagnostics
   integer,  intent (in) :: diag		! diagnostic verbosity level

   real(dp) integral_result			! function output variable

! Local variables.

   integer it

   real(dp) alpha, angle_out, check, cosalpha
   real(dp) dr1, dr1_square, dr2, dr2_square, ds, ds_square
   real(dp) phi, px1, px2, py1, py2, rx1, rx2, ry1, ry2

! Begin function.

   angle_out=0.
   phi=0.

   IF (diag >= 6) then
     print *, '-----------------------------'
     print *, 'XLON4=',XLON4
     print *, 'XLAT4=',XLAT4
   end if

! Loop over each of four cell boundary lines.

segment_loop: &
   DO it=1,4

     IF (diag >= 6) PRINT *,'it=',it

! Single contour segment

     Px1=XLON4(it)
     Py1=XLAT4(it)

     IF (it.lt.4) THEN
       Px2=XLON4(it+1)
       Py2=XLAT4(it+1)
     ELSE
       Px2=XLON4(1)
       Py2=XLAT4(1)
     END IF

     IF (diag >= 6) PRINT *,'P1=',Px1,Py1
     IF (diag >= 6) PRINT *,'P2=',Px2,Py2

     ds=SQRT((Px2-Px1)**2.+(Py2-Py1)**2.)
     ds_square=ds*ds

     IF (diag >= 6) PRINT *,'ds=',ds,' ds_square=',ds_square

! First vector from (x,y) to one end of the segment

     Rx1=Px1-x
     Ry1=Py1-y

     IF (diag >= 6) PRINT *,'R1=',Rx1,Ry1

     dR1=SQRT(Rx1**2.+Ry1**2.)
     dR1_square=dR1*dR1

     IF (diag >= 6) PRINT *,'dR1=',dR1,' dR1_square=',dR1_square

! Second vector from (x,y) to other end of the segment

     Rx2=Px2-x
     Ry2=Py2-y

     IF (diag >= 6) PRINT *,'R2=',Rx2,Ry2

     dR2=SQRT(Rx2**2.+Ry2**2.)
     dR2_square=dR2*dR2

     IF (diag >= 6) PRINT *,'dR2=',dR2,' dR2_square=',dR2_square

! Using triangle formula: a**2=b**2+c**2-2*b*c*COSalpha

     COSalpha=(dR1_square+dR2_square-ds_square)/(2.*dR1*dR2)

! Protect against floating point error, ACOS input slightly greater
! than 1.0 in magnitude.  Let unexpected coarser errors get trapped.

     if (abs (COSalpha) > 1.0) then
       if (abs (COSalpha) < 1.00000001d0) then
         print '(a,i5,f26.19)', &
           ' *** Fix COSalpha, magnitude greater than 1:', isite, COSalpha
         COSalpha = sign (1d0, COSalpha)
       end if
     end if

! Check the sign of alpha:
! be sure that the point (x,y) is left from vector |P1P2|
! for that calculate the vector product of |P1P2| and |-R1|

     check=(Px2-Px1)*(y-Py1)-(Py2-Py1)*(x-Px1)

     IF (check.gt.0.) THEN
       alpha=ACOS(COSalpha)
     ELSE
       alpha=-ACOS(COSalpha)
     END IF

     angle_out=angle_out+alpha

     IF (diag >= 5) print *,'alpha=',alpha,' angle_out=',angle_out

   end do segment_loop

   integral_result = abs (angle_out)

end function contour_integral_4

end module grid_location
