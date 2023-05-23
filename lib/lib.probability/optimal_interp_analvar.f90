!-----------------------------------------------------------------------------
!
! optimal_interp_analvar.f90 -- Optimum interpolation, point data to grid.
!
! 2019-apr-26	Version from Tom Hamill, tom.hamill@noaa.gov, (303) 497-3060.
!
! 2019-jun-20	Dave Allured:  Fortran 90 upgrades.
!		Add modules for all subroutines.
!		Break off attached support routines into separate module.
!		Add missing variable declarations and intents.
! 2019-jul-29	Bug fix from Tom Hamill.  Fix undefined array problem.
! 2019-aug-07	Clean up test and diagnostic messages.
!
! * Remember to update the date in the module_id below.
!
!-----------------------------------------------------------------------------
!
! If calling this routine from python, it needs to be compiled with:
!
! OpenMP version:
! f2py2 -I/usr/local/gfortran/include -L/usr/local/gfortran/lib \
!       --opt='-O4' --opt='-fopenmp' -lgomp -c -m optimal_interp_analvar \
!       haversine_km.f90 lubksb.f90 ludcmp.f90 jacobi.f90 \
!       optimal_interp_analvar.f90
!
! Serial version:
! f2py2 --opt='-O4' -c -m optimal_interp_analvar \
!       haversine_km.f90 lubksb.f90 ludcmp.f90 jacobi.f90 \
!       optimal_interp_analvar.f90
!
!-----------------------------------------------------------------------------
!
! optimal_interp_analvar:   Optimum interpolation (OI),
!    here expressed using the more widespread notation of Kalman filters,
!    see Ide et al., J. Met. Soc. Japan, 1997, 25, p. 181.
!
!    The equations we solve are:
!
!    x_a = x_b + K(y-Hx_b)
!
!    and
!
!    Pa = (I - KH)Pf  (we will solve for variances on diagonal only)
!
!    where K = Pf HT (HPfHT + R)^(-1)
!
!    so  Pa = Pf - [PfHT(HPfHT + R)^(-1/2)] [...]T
!
!    Diagonal entries of this matrix are readily calculated from the
!    dot product of the rows of [PfHT(HPfHT + R)^(-1/2)] with itself.
!
!    In these equations:
!    * x_a is the analyzed state vector,
!    * x_b is the background, or first guess state vector (on a grid).
!    * y is the vector of observations, and
!    * Hx_b is the first guess state interpolated to the observation
!      locations (H is the interpolation operator - in this instance
!      we are simply taking the state at the nearest grid point in
!      the first guess).
!    * K is the "Kalman Gain" matrix that spreads the influence of the
!      "observation increment" (y-Hx_b) to the model grid points.
!    * K = the Kalman gain matrix, K = Pf HT (HPfHT + R)^(-1), where:
!      * HT is H-transpose,
!      * Pf is the background-error covariances, here assumed to have
!        spatially dependent error variances but with covariances static
!        and based on vertical and horizontal distance between grid pts.
!      * R is the observation-error covariance matrix (assumed diagonal,
!        errors between observation sites uncorrelated).
!    * Pa is the analysis-error covariance matrix.
!
!    The data assimilation formalism is used to generate an "analysis"
!    of ozone (or other species), along with an estimate of the
!    analysis variance of ozone, given a climatological gridded
!    background forecast of that ozone and "observations" of ozone and
!    its uncertainty derived from an analog procedure. For this
!    application, we also input lat/lon/elevation data, for our model
!    of background-error covariances around station locations is a
!    "Matern" function with the effective distance a combination of
!    horizontal distance and vertical difference.
!
!    For more, see Hamill, T. M., and M. Scheuerer, 2019: Benchmarking
!    the background forecast in rapidly updated surface temperature
!    analyses.  Part 2: gridded benchmark.  Mon. Wea. Rev., submitted.
!
!    NOTE:  When implemented in the NWS, they may have other numerical
!    linear algebra libraries that they prefer to use rather than the
!    Numerical Recipes ones used here.  And theirs might be faster.
!
!-----------------------------------------------------------------------------

module optimal__interp_analvar
contains

SUBROUTINE optimal_interp_analvar (station_data, &
    lats_stations, lons_stations, iclosest, jclosest, &
    background_state, gridded_terrain_height, &
    grid_lons, grid_lats, background_error_variance, &
    rho, vdconst, obs_error_variance, ny, nx, nstations, diag, &
    analyzed_state, analysis_error_variance)

use haversine__km
use jacobi_mod
use lubksb_mod
use ludcmp_mod

!!use omp_lib
implicit none

character(*), parameter :: &
   module_id = 'optimal_interp_analvar.f90 version 2019-aug-07'

integer, parameter :: dp = kind (1d0)	! default double precision

! Calling arguments.

INTEGER, INTENT(IN) :: nstations, ny, nx  ! # obs sites, output grid dimensions

REAL(dp), INTENT(IN), DIMENSION(nstations) :: &
                 station_data ! the ozone mean determined from analog approach
REAL, INTENT(IN), DIMENSION(nstations) :: lats_stations, lons_stations
INTEGER, INTENT(IN), DIMENSION(nstations) :: &
                 iclosest, jclosest		! station grid point indices
REAL, INTENT(IN), DIMENSION(ny, nx) :: grid_lons, grid_lats   ! gridded lat/lon
REAL, INTENT(IN), DIMENSION(ny, nx) :: background_error_variance
REAL, INTENT(IN), DIMENSION(ny, nx) :: background_state
REAL, INTENT(IN) :: rho 	     ! horizontal correlation length scale (km)
REAL, INTENT(IN) :: vdconst	     ! vertical correlation length scale (m)
REAL, INTENT(IN), DIMENSION(ny,nx) :: gridded_terrain_height  ! in meters
REAL, INTENT(IN), DIMENSION(nstations) :: &
                 obs_error_variance	      ! ozone "obs" err variance
integer, intent(in) :: diag		      ! diagnostic verbosity level, 0-N

REAL, INTENT(OUT), DIMENSION(ny, nx) :: analyzed_state, analysis_error_variance

! ---- compiler directives to make a fortran-callable procedure from python

! f2py intent(in) nstations, ny, nx
! f2py intent(in) station_data, lats_stations, lons_stations
! f2py intent(in) iclosest, jclosest
! f2py intent(in) gridded_terrain_height, grid_lons, grid_lats
! f2py intent(in) rho, vdconst, diag
! f2py intent(in) gridded_terrain_height, background_state
! f2py intent(in) background_error_variance, obs_error_variance
! f2py intent(out) analyzed_state, analysis_error_variance
! f2py depend(nstations) station_data, obs_error_variance
! f2py depend(nstations) lats_stations, lons_stations, iclosest, jclosest
! f2py depend(ny, nx) grid_lons, grid_lats
! f2py depend(ny, nx) background_state, background_error_variance
! f2py depend(ny, nx) analyzed_state, analysis_error_variance
! f2py depend(ny, nx) gridded_terrain_height

! Local variables and arrays.

REAL distance_to_stn(nstations)			! physical distance from grid
						! point to station in miles
REAL glat, glon					! gridded lat, lon
REAL olat, olon					! observed lat, lon

REAL distance
REAL(dp) HPfHT(nstations,nstations)		! background forecast error
						! covariance between stations
REAL(dp) HPfHT_plusR(nstations,nstations)	! includes obs error variance
						! on diagonal
REAL(dp) Astore(nstations,nstations)
REAL(dp) eigenvectors(nstations,nstations)	! of HPfHT_plusR
REAL(dp) eigenvalues(nstations)			! of HPfHT_plusR
INTEGER indx(nstations)			     ! used in ludcmp, L-U decomposition
REAL(dp) dsign					! used in ludcmp routine
REAL(dp), DIMENSION(nstations) :: HPfHT_plusR_inv_times_yminusHx
REAL(dp), DIMENSION(nstations, nstations) :: HPfHT_plusR_inv_sqrt
REAL(dp), DIMENSION(nstations,ny,nx) :: PfHT, PfHT_HPfHT_plusR_inv_sqrt
REAL background_at_observations(nstations)

integer ix, jy, ilocn, jlocn
integer ioloc, iploc, joloc, jploc
integer istat, istn, nrot

real(dp) horiz_distance2, vert_distance2
real     plat, plon
real(dp) t0, t1, td, tera, terb, term

! Begin interpolation routine.

if (diag >= 2) print *
if (diag >= 2) print *, 'optimal_interp_analvar:  Start.'
if (diag >= 2) print *, '  Module ID = ' // module_id

if (diag >= 2) print *
if (diag >= 2) print *,'SITE DATA:'

if (diag >= 3) print *,'shape(station_data)         =', shape(station_data)
if (diag >= 2) print *, 'Min, max station_data         =', &
                 minval(station_data), maxval(station_data)

if (diag >= 3) print *,'shape(lats_stations)        =', shape(lats_stations)
if (diag >= 2) print *, 'Min, max lats_stations        =', &
                 minval(lats_stations), maxval(lats_stations)

if (diag >= 3) print *,'shape(lons_stations)        =', shape(lons_stations)
if (diag >= 2) print *, 'Min, max lons_stations        =', &
                 minval(lons_stations), maxval(lons_stations)

if (diag >= 3) print *,'shape(iclosest)             =', shape(iclosest)
if (diag >= 2) print *, 'Min, max iclosest             =', &
                 minval(iclosest), maxval(iclosest)

if (diag >= 3) print *,'shape(jclosest)             =', shape(jclosest)
if (diag >= 2) print *, 'Min, max jclosest             =', &
                 minval(jclosest), maxval(jclosest)

if (diag >= 3) print *,'shape(obs_error_variance)   =',shape(obs_error_variance)
if (diag >= 2) print *, 'Min, max obs_error_variance   =', &
                 minval(obs_error_variance), maxval(obs_error_variance)

if (diag >= 2) print *
if (diag >= 2) print *,'GRID DATA:'

if (diag >= 3) print *,'shape(grid_lats)            =', shape(grid_lats)
if (diag >= 2) print *, 'Min, max grid_lats            =', &
                 minval(grid_lats), maxval(grid_lats)

if (diag >= 3) print *,'shape(grid_lons)            =', shape(grid_lons)
if (diag >= 2) print *, 'Min, max grid_lons            =', &
                 minval(grid_lons), maxval(grid_lons)

if (diag >= 3) print *,'shape(gridded_terrain_height) =', &
                        shape(gridded_terrain_height)
if (diag >= 2) print *, 'Min, max gridded_terrain_height =', &
                 minval(gridded_terrain_height), maxval(gridded_terrain_height)

if (diag >= 3) print *,'shape(background_state)     =', shape(background_state)
if (diag >= 2) print *, 'Min, max background_state     =', &
                 minval(background_state), maxval(background_state)

if (diag >= 3) print *,'shape(background_error_variance) =', &
                        shape(background_error_variance)
if (diag >= 2) print *, 'Min, max background_error_variance =', &
            minval(background_error_variance), maxval(background_error_variance)

! ------------------------------------------------------------------
! --- Part 1:  Compute inter-station distances in km.
!     Also populate the (HPfHT+R) matrix that's part of the
!     Kalman gain which is used later.
! ------------------------------------------------------------------

CALL cpu_time(t0)
if (diag >= 2) print *
if (diag >= 2) &
   PRINT *,'Start of part 1, before populating (HPfHT + R) time = 0.0'
if (diag >= 2) print *,'  Compute inter-station distances and (HPfHT+R) matrix.'

station_loop_1: &
DO jlocn = 1, nstations

    ! ---- Grid coordinates of current obs

    ioloc = iclosest(jlocn)
    joloc = jclosest(jlocn)
    olat = lats_stations(jlocn)
    olon = lons_stations(jlocn)
    if ( (diag >= 4) .and. (jlocn < 10) ) then
        print *,'jlocn=',jlocn,'( i,j )=',joloc,ioloc
        print *,'lats_stations=',lats_stations(jlocn)
        print *,'lons_stations=',lons_stations(jlocn)
        print *,'grid_lats=',grid_lats(joloc,ioloc)
        print *,'grid_lons=',grid_lons(joloc,ioloc)
    end if
    background_at_observations(jlocn) = background_state(joloc,ioloc)

    ! ---- loop thru remaining stations

station_loop_2: &
    DO ilocn = jlocn, nstations

        iploc = iclosest(ilocn)
        jploc = jclosest(ilocn)
        plat = lats_stations(ilocn)
        plon = lons_stations(ilocn)

        IF (ilocn .eq. jlocn) THEN
            HPfHT(jlocn,ilocn) = background_error_variance(joloc,ioloc)
            HPfHT_plusR(jlocn,ilocn) = HPfHT(jlocn,ilocn) &
                                     + obs_error_variance(ilocn)

        ELSE

            ! ---- compute great-circle distance in miles

            CALL haversine_km(plat, plon, olat, olon, distance)

            ! ---- determine the horizontal weight based on distance.
            !      Set the coefficients for the covariance model, and
            !      determine these parameters at both ilocn and jlocn and
	    !      average to make sure we have covariance matrix symmetry.
            !      set covariances as Gaussian function of horiz dist,
            !      vert dist, coastal proximity difference.
            !      Correlation fn * variance, here arbitrarily set to 50 C**2

            !td = ABS(station_height(ilocn)-station_height(jlocn))
            td = ABS(gridded_terrain_height(joloc,ioloc) &
                   - gridded_terrain_height(jploc,iploc))

            vert_distance2 = td**2 / vdconst**2
            horiz_distance2 = distance**2 / rho**2

            term = SQRT(horiz_distance2 + vert_distance2)

            HPfHT(jlocn,ilocn) = SQRT(background_error_variance(jploc,iploc)) &
                * SQRT(background_error_variance(joloc,ioloc)) * exp(-term)
            HPfHT(ilocn,jlocn) = HPfHT(jlocn,ilocn)
            HPfHT_plusR(jlocn,ilocn) = HPfHT(jlocn,ilocn)
                ! because obs errors are uncorrelated
            HPfHT_plusR(ilocn,jlocn) = HPfHT(jlocn,ilocn)
                ! so off-diagonal elements of R are 0.0

      ENDIF ! ilocn .eq. jlocn

   END DO station_loop_2
END DO station_loop_1

! ----------------------------------------------------------------------------
! --- Part 2: Generate the eigenvalues and eigenvectors of (HPfHT + R), which
!             are then used to set the square root, (HPfHT + R)^(-1/2)
! ----------------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)','Start of part 2, accumulated time = ',t1-t0
if (diag >= 2) &
   PRINT *,'  Calculate eigenvalues and eigenvectors of (HPfHT + R)'

Astore = HPfHT_plusR
CALL jacobi (Astore, nstations, nstations, eigenvalues, eigenvectors, nrot)
				! a Numerical Recipes routine

if (diag >= 3) print *,'shape(eigenvalues)          =', shape(eigenvalues)
if (diag >= 2) print *, 'Min, max eigenvalues          =', &
                 minval(eigenvalues), maxval(eigenvalues)

if (diag >= 3) print *,'shape(eigenvectors)         =', shape(eigenvectors)
if (diag >= 2) print *, 'Min, max eigenvectors         =', &
                 minval(eigenvectors), maxval(eigenvectors)

DO istn = 1, nstations
    HPfHT_plusR_inv_sqrt(:,istn) = eigenvectors(:,istn) &
                                   * ( 1.0/SQRT(eigenvalues(istn)) )
END DO

! ----------------------------------------------------------------------------
! --- Part 3: calculate PfHT, the cross covariance matrix of the background
!             states and the background interpolated to the observation
!             locations.
! ----------------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)','Start of part 3, accumulated time = ',t1-t0
if (diag >= 2) print *, '  Calculate PfHT, cross covariance matrix.'

DO jy = 1, ny
    DO ix = 1, nx
        glat = grid_lats(jy,ix)
        glon = grid_lons(jy,ix)
        DO istn = 1, nstations
            olat = lats_stations(istn)
            olon = lons_stations(istn)
            ioloc = iclosest(istn)
            joloc = jclosest(istn)
            ! --- compute horizontal distance in km
            CALL haversine_km(glat, glon, olat, olon, distance_to_stn(istn))
            ! --- get vertical distance
            tera = gridded_terrain_height(joloc,ioloc)
            terb = gridded_terrain_height(jy,ix)
            ! --- set covariances as Matern function of horiz dist, vert dist
            td = (ABS(tera-terb))
            vert_distance2 = td**2 / vdconst**2
            horiz_distance2 = distance_to_stn(istn)**2 / rho**2
            term = SQRT(horiz_distance2 + vert_distance2)
            PfHT(istn,jy,ix) = SQRT(background_error_variance(joloc,ioloc)) * &
                SQRT(background_error_variance(jy,ix)) * exp(-term)
        END DO
    END DO
END DO

! ----------------------------------------------------------------------------
! --- Part 4: calculate PfHT (HPfHT+R)^(-1/2), which will be used to calculate
!             the analysis error variance.  Use OpenMP to parallelize the loop
!             calculation
!
!             This is the slowest part of the calculation.   If you only care
!             about CONUS points, you could speed it up ~ 30% by including
!             a CONUS mask and only doing computations if the point was inside
!             the CONUS.
!
!             I tried to get this working with OpenMP but had problems.
!             Use of OpenMP seemed to make other parts of the code slower,
!             and it segfaulted in the parallelized loop below.
! ----------------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)','Start of part 4, accumulated time = ',t1-t0
if (diag >= 2) print *, '  Calculate PfHT (HPfHT+R)^(-1/2).'
if (diag >= 2) print *

if (diag >= 2) print *, 'Min, max PfHT                 =', &
                 minval (PfHT), maxval (PfHT)
if (diag >= 2) print *, 'Min, max HPfHT_plusR_inv_sqrt =', &
                 minval (HPfHT_plusR_inv_sqrt), maxval (HPfHT_plusR_inv_sqrt)
if (diag >= 2) print *

!!CALL OMP_set_num_threads(8)

DO ix = 1, nx
    IF ( (diag >= 2) .and. (MOD(ix,50) .eq. 0) ) then
        CALL cpu_time(t1)
        PRINT '(2(a,i0),a,f0.2)', '  processing ix = ', ix, ' of ', nx, &
           ', time = ', t1-t0
    END IF
    DO jy = 1, ny
        !!$OMP PARALLEL DO DEFAULT(Private) SHARED(PfHT, PfHT_HPfHT_plusR_inv_sqrt, HPfHT_plusR_inv_sqrt)
        DO istn = 1, nstations
            PfHT_HPfHT_plusR_inv_sqrt(istn,jy,ix) = &
                DOT_PRODUCT(PfHT(:,jy,ix),HPfHT_plusR_inv_sqrt(:,istn))
        END DO
        !!$OMP END PARALLEL DO
    END DO
END DO

! ---------------------------------------------------------------------------
! ---- Part 5:  now via L-U decomposition, perform what is needed to compute
!      the inverse of the matrix  (HPH^T + R)^(-1) (y-Hx).
!      ludcmp.f is from the Numerical Recipes in Fortran text.
! ---------------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)', 'Start of part 5, accumulated time = ',t1-t0
if (diag >= 2) PRINT *, '  Perform L-U decomposition, subroutine ludcmp.'

Astore = HPfHT_plusR
indx(:) = 0
CALL ludcmp(Astore, nstations, nstations, indx, dsign, istat)

! ---------------------------------------------------------------------------
! ---- Part 6:  calculate (HPHT+R)^(-1) (y-Hx).   Lubksb also from
!      Numerical Recipes text.
! ---------------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)','Start of part 6, accumulated time = ',t1-t0
if (diag >= 2) print *, '  Calculate (HPHT+R)^(-1) (y-Hx), subroutine lubksb.'
if (diag >= 2) print *

if (diag >= 2) print *,'BEFORE lubksb:::'
if (diag >= 2) print *, 'Min, max PfHT_HPfHT_plusR_inv_sqrt  =', &
          minval (PfHT_HPfHT_plusR_inv_sqrt), maxval (PfHT_HPfHT_plusR_inv_sqrt)

HPfHT_plusR_inv_times_yminusHx = station_data - background_at_observations

if (diag >= 3) print *, 'shape(HPfHT_plusR_inv_times_yminusHx) =', &
                         shape(HPfHT_plusR_inv_times_yminusHx)
if (diag >= 2) print *, 'Min, max HPfHT_plusR_inv_times_yminusHx =', &
                           minval(HPfHT_plusR_inv_times_yminusHx), &
                           maxval(HPfHT_plusR_inv_times_yminusHx)

if (diag >= 3) print *, 'shape(station_data)        =',shape(station_data)
if (diag >= 2) print *, 'Min, max station_data               =', &
                 minval(station_data), maxval(station_data)

if (diag >= 3) print *, 'shape(background_at_observations) =', &
                         shape(background_at_observations)
if (diag >= 2) print *, 'Min, max background_at_observations =', &
                           minval(background_at_observations), &
                           maxval(background_at_observations)

CALL lubksb(Astore, nstations, nstations, indx, &
    HPfHT_plusR_inv_times_yminusHx)

if (diag >= 2) print *
if (diag >= 2) print *,'AFTER lubksb:::'
if (diag >= 3) print *,'shape(HPfHT_plusR_inv_times_yminusHx) =', &
                        shape(HPfHT_plusR_inv_times_yminusHx)
if (diag >= 2) print *, 'Min, max HPfHT_plusR_inv_times_yminusHx =', &
                           minval(HPfHT_plusR_inv_times_yminusHx), &
                           maxval(HPfHT_plusR_inv_times_yminusHx)

! ------------------------------------------------------------------
! ---- Part 7: Finally, (a) calculate each analyzed state component via
!              xb + PfHT(HPfHT+R)^(-1)(y-Hx).   Also produce an estimate
!              of the analysis-error variance by diag(Pa) = diag(Pf) -
!              diag( [PfHT (HPfHT+R)^(-1/2)] [PfHT (HPfHT+R)^(-1/2)]T)
!              which can be accomplished by taking a simple dot product
!              of rows of the matrix with itself.
! ------------------------------------------------------------------

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)','Start of part 7, time = ',t1-t0
if (diag >= 2) &
   print *, '  Calculate analyzed state components and anaysis error variance.'

if (diag >= 3) then
   print *
   print *, 'shape(analyzed_state)                 =', shape(analyzed_state)
   print *, 'shape(PfHT)                           =', shape(PfHT)
   print *, 'shape(HPfHT_plusR_inv_times_yminusHx) =', &
             shape(HPfHT_plusR_inv_times_yminusHx)
   print *, 'shape(analysis_error_variance)        =', &
             shape(analysis_error_variance)
   print *, 'shape(background_error_variance)      =', &
             shape(background_error_variance)
   print *, 'shape(PfHT_HPfHT_plusR_inv_sqrt)      =', &
             shape(PfHT_HPfHT_plusR_inv_sqrt)
end if

DO jy = 1, ny
!!    CALL cpu_time(t1)			!! fast loop, timing is not needed
!!    IF (MOD(jy,50) .eq. 0) &
!!        PRINT '(2(a,i0),a,f0.2)','  jy = ',jy,' of ',ny, ', time =', t1-t0

    DO ix = 1, nx
!        if (jy == 50 .and. ix == 250)  then
!          print *,'ix=',ix,' jy=',jy
!          print *,'analyzed_state(jy,ix)=',background_state(jy,ix),' + ', &
!          REAL(DOT_PRODUCT(PfHT(:,jy,ix),HPfHT_plusR_inv_times_yminusHx))
!          print *,'analysis_error_variance(jy,ix)=', &
!          background_error_variance(jy,ix),' - ', &
!          REAL(DOT_PRODUCT(PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix),&
!          PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix)))
!            do ii=1,nstations
!                print *,'ii=',ii,' PfHT(ii,jy,ix)=',PfHT(ii,jy,ix), &
!'PfHT_plusR_inv_times_yminusHx(ii)=',HPfHT_plusR_inv_times_yminusHx(ii)
!            end do
!          print *,'ix=',ix,' jy=',jy
!          print *,'analyzed_state(jy,ix)=',background_state(jy,ix),' + ', &
!          REAL(DOT_PRODUCT(PfHT(:,jy,ix),HPfHT_plusR_inv_times_yminusHx))
!          print *,'analysis_error_variance(jy,ix)=', &
!          background_error_variance(jy,ix),' - ', &
!          REAL(DOT_PRODUCT(PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix),&
!          PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix)))
!       end if
!!!        CALL cpu_time(t1)
        analyzed_state(jy,ix) = background_state(jy,ix) + &
            REAL(DOT_PRODUCT(PfHT(:,jy,ix),HPfHT_plusR_inv_times_yminusHx))
        analysis_error_variance(jy,ix) = background_error_variance(jy,ix) - &
            REAL(DOT_PRODUCT(PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix),&
            PfHT_HPfHT_plusR_inv_sqrt(:,jy,ix)))
    END DO ! ix
END DO ! jy

if (diag >= 2) print *
if (diag >= 2) print *, 'After part 7:'

if (diag >= 3) print *, 'shape(analyzed_state)            =', &
                         shape(analyzed_state)
if (diag >= 2) print *, 'Min, max analyzed_state          =', &
                 minval(analyzed_state), maxval(analyzed_state)

if (diag >= 3) print *, 'shape(analysis_error_variance)   =', &
                         shape(analysis_error_variance)
if (diag >= 2) print *, 'Min, max analysis_error_variance =', &
                          minval (analysis_error_variance), &
                          maxval (analysis_error_variance)

CALL cpu_time(t1)
if (diag >= 2) print *
if (diag >= 2) PRINT '(a,f0.2)', &
                 '  ... at end of data assimilation, accumulated time = ',t1-t0

if (diag >= 2) print *, 'optimal_interp_analvar:  Done. Return to caller.'

END SUBROUTINE optimal_interp_analvar
end module optimal__interp_analvar
