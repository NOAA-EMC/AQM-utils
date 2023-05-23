!----------------------------------------------------------------------------
!
! distance_btw_coords.f90 -- Surface distance between two lat/lon points.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2004-aug-12	Adapted from distance_btw_stations.f90, v1.00.
!
! Input:  lat1, lon1, lat2, lon2 = two coordinate pairs, decimal degrees
!
! The great circle distance in kilometers is computed.  But the algorithm
! is simplified, an imprecise estimate.
!
! The distance algorithm is borrowed from Russ Bigley and Jon Eischeid,
! as found in /home/bigley/co_sno/line_reg.corr.dd.bin.f90, 2004-may-5.
!
! Good coordinates are assumed.  No range checking is done.
!
!----------------------------------------------------------------------------


real function distance_btw_coords (lat1, lon1, lat2, lon2) &
   result (dist)
   
   implicit none
   
   real, intent(in) :: lat1, lon1, lat2, lon2			! calling args
   
   double precision XLT1,XLG1,XLT2,XLG2,PIE,ADJX	! local vars

   XLT1 = lat1				! convert single to double precision
   XLG1 = lon1
   XLT2 = lat2
   XLG2 = lon2
   
   PIE = .0174532925
   
   ADJX=COS(((XLT1+XLT2)/2.)*PIE)*111.17774734
   dist = SQRT(((XLG2-XLG1)*ADJX)**2 + ((XLT2-XLT1)*111.17774734)**2)

end function distance_btw_coords
