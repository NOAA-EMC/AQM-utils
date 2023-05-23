!------------------------------------------------------------------------------
!
! haversine_km.f90 -- Great circle distance -- Adapted from Matlab.
!
! 2019-apr-26	Two support routines embedded in this verion of
!		  optimal_interp_analvar.f90, by Tom Hamill.
!
! 2019-jun-20	haversine_km.f90:
!		Dave Allured:  Break out from optimal_interp_analvar.f90.
!		Make separate fortran 90 module.
!		Convert reals to double precision, except for calling args.
!		In-line the trivial subroutine, to_radian.  Runs a bit faster.
!
!------------------------------------------------------------------------------

module haversine__km
contains

subroutine haversine_km(deglat1,deglon1,deglat2,deglon2,dist)
implicit none

real, intent(in)  :: deglat1, deglon1, deglat2, deglon2
real, intent(out) :: dist

double precision a, c2, dlat, dlon, lat1, lat2

double precision, parameter :: radius = 6372.8
double precision, parameter :: deg_to_radian = atan(1D0)/45.
    ! exploit intrinsic atan to generate pi/180 runtime constant

dlat = (deglat2-deglat1) * deg_to_radian
dlon = (deglon2-deglon1) * deg_to_radian
lat1 = deglat1           * deg_to_radian
lat2 = deglat2           * deg_to_radian

a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
c2 = 2*asin(sqrt(a))
dist = radius*c2

end subroutine haversine_km
end module haversine__km
