!------------------------------------------------------------------------------
!
! find_nearest_grid_points.f90 -- Find nearest grid points for site locations.
!
! This is a support routine for the probability forecast module of the
! NOAA NCO/ARL/PSD bias correction system for CMAQ forecast outputs.
!
! 2019-may-16	Data preprocessor by Irina Djalalova, NOAA/OAR/ESRL/PSD/CIRES:
!		  dataascii_preparation_for_probability_code.pro
!		Prototype routine to calculate recent site climatologies
!		  for forecast days.
!
! 2019-aug-07	probability.f90:
!		Release version, original module to integrate probability
!		  forecasts into bias correction.
!
! 2019-dec-27	find_nearest_grid_points.f90:
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Nearest grid points routine split off from probability.f90
!		  version 2019-aug-07.
!		No functional changes.
!
! Primary inputs:
!
! * Site coordinates.
! * 2-D grid coordinates.
!
! Outputs:
!
! * Arrays of I, J grid indices for each site.
!
! Missing input site coordinates are handled correctly.
! Missing values in (I, J) are returned in such cases.
!
!------------------------------------------------------------------------------

module find__nearest_grid_points
contains

subroutine find_nearest_grid_points (site_lats, site_lons, grid_lats, &
      grid_lons, vmiss, diag, iclosest, jclosest)

   use config, only : dp
   implicit none

! Input arguments.

   real(dp), intent(in) :: site_lats(:)		! site coordinates (nsites)
   real(dp), intent(in) :: site_lons(:)
   real(dp), intent(in) :: grid_lats(:,:)	! grid coordinates (X, Y)
   real(dp), intent(in) :: grid_lons(:,:)

   real(dp), intent(in) :: vmiss		! common missing value code
   integer,  intent(in) :: diag			! diag. verbosity level, 0-N

! Output arguments.

   integer, intent(out), allocatable :: iclosest(:)   ! closest grid indices
   integer, intent(out), allocatable :: jclosest(:)   !   for sites (nsites)

! Local variables.

   integer ix, jy, nx, ny
   integer isite, nsites
   integer xmin, xmax, ymin, ymax, nmiss

   real(dp) dist, closest_dist

!-------------------------------------------------------
! Find nearest grid points for site locations.
!-------------------------------------------------------

! Irina's method.  Simple quadratic distances, not great circle formula.
! Sufficient for this application.
! Possible missing coordinates are handled correctly.

   nx     = size (grid_lats, 1)		! get grid dimensions
   ny     = size (grid_lats, 2)

   nsites = size (site_lats)
   allocate (iclosest(nsites), jclosest(nsites))

   iclosest(:) = vmiss			! init to missing values
   jclosest(:) = vmiss			! note: i = X index, j = Y index,
   					!   as used elsewhere in BC code
   do isite = 1, nsites
      if ((site_lats(isite)) == vmiss .or. (site_lons(isite) == vmiss)) cycle
					! skip sites with missing coordinates;
					! I and J are left as missing values.

      closest_dist = huge (closest_dist)   ! begin search with huge value

      do ix = 1, nx
         do jy = 1, ny
            dist = sqrt ( ((grid_lats(ix,jy) - site_lats(isite)) ** 2) &
                        + ((grid_lons(ix,jy) - site_lons(isite)) ** 2) )
            if (dist < closest_dist) then	! if closer...
               closest_dist    = dist	! save current closest distance
               iclosest(isite) = ix	! save current grid point indices
               jclosest(isite) = jy
            end if
         end do
      end do
   end do

! Diagnostic.

   if (diag >= 2) then
      xmin = minval (iclosest, (iclosest /= vmiss))
      xmax = maxval (iclosest, (iclosest /= vmiss))

      ymin = minval (jclosest, (jclosest /= vmiss))
      ymax = maxval (jclosest, (jclosest /= vmiss))

      nmiss = count ( (iclosest == vmiss) .or. (jclosest == vmiss) )

      print '(4x,a,3i6)', 'Iclosest min, max, size =', xmin, xmax, nx
      print '(4x,a,3i6)', 'Jclosest min, max, size =', ymin, ymax, ny
      print '(4x,a,i0)',  'Number of sites with missing coordinates = ', nmiss
   end if

end subroutine find_nearest_grid_points
end module find__nearest_grid_points
