!------------------------------------------------------------------------------
!
! kfan_method.f90 -- Kalman/analog filter for single site forecast time series.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction system for CMAQ forecast outputs.
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2016
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Original Matlab version written by:
! Luca Delle Monache - lucadm@ucar.edu  (April 2010)
! Updated by:
! Luca Delle Monache - lucadm@ucar.edu  (June 2011)
! Badrinath Nagarajan - badri@ucar.edu  (June 2011)
!
! 2016-feb-04	kfan_method.f90:
!		Top level control for KFAN method for single site time series.
!		Break out from main_analog.f90, PSD version 2016-feb-04.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2016-feb-15	Use simplified x_analog subroutine, instead of kf_analog.
!
! 2017-apr-29	Interface changes for new method functions.
!		Move some parameters into parameter structure.
! 2017-may-12	New protocol, method name embedded in filter routine.
!
! 2019-jun-18	Add output for best analog obs values, current forecast only.
! 2020-may-11	Minor, parameter name change.
!
! 2023-mar-25	Auto switch to AnEnMean, when short training period detected.
!
!------------------------------------------------------------------------------

module kfan__method
contains

function kfan_method (filter_method, obs, pred, vmiss, apar, fpar, kpar, &
    pred_weights, isite, site_id, diag, kfan_result, best_analogs_obs) &
    result (return_status)

  use config, only : dp
  use find__analog, only : fpar_type
  use analog__ensemble
  use kf__luca
  implicit none

  character(*),    intent(in ) :: filter_method		! selected method name
  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! kf_analog parameters
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  type(kpar_type), intent(in ) :: kpar			! Kalman filter params
  real(dp),        intent(in ) :: pred_weights(:)       ! V - predictor weights
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: kfan_result(:,:)	! DH - bias corr. result
  real(dp),        intent(out) :: best_analogs_obs(:,:)	! HA - best analogs

  logical return_status		! function return status: true = processed,
  				! false = method name not matched

! Local variables.

  type (apar_type) :: apar2
  type (fpar_type) :: fpar2
  type (kpar_type) :: kpar2

  character fdate_str*24
  integer ndays, nhours
  logical short_detect

  real(dp), allocatable :: anenmean_result(:,:)	  ! DH
  real(dp), allocatable :: analog_in_an(:,:,:)	  ! DHA, nearest analogs found

  real(dp), allocatable :: obs_flat(:)		  ! reshaping temp arrays
  real(dp), allocatable :: ensan_flat(:)
  real(dp), allocatable :: result_1d(:)

! Unused result array from kf_analog.  See subroutine docs.

  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs

!-----------------------------------------------------------
! Initialize.
!-----------------------------------------------------------

! Check requested filter method name.

  return_status = (filter_method == 'KFAN')	! case sensitive

  if (.not. return_status) return		! name not recognized; return
						!   without any processing
! Start KFAN method.

  if (diag >= 3) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  kfan_method, site', isite, ': Start.'
  end if

! Conservative.  Make writeable copies of parameter structures,
! to avoid parallel conflicts.
!
! * 2016-feb-4:  Possible compiler bugs in parallel support for structures.
! * Assume lower levels might modify any of these structures.

  apar2 = apar
  fpar2 = fpar
  kpar2 = kpar

! Allocate result arrays for analog_ensemble.

  ndays  = size (pred, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (pred, 2)		! number of hours in each forecast cycle

  allocate (anenmean_result(ndays, nhours))			! DH
  allocate (ianalog(ndays, nhours, apar2%num_analogs))		! DHA
  allocate (analog_in_an(ndays, nhours, apar2%num_analogs))	! DHA

!-----------------------------------------------------------
! KF/AN part 1.  Compute multiple analog forecasts.
!-----------------------------------------------------------

  apar2%start_stat = 1			! make analogs for all available dates

  call analog_ensemble (obs, pred, pred_weights, vmiss, apar2, fpar2, isite, &
    site_id, diag, anenmean_result, Ianalog, analog_in_an, short_detect)
						! last four are outputs

! Also return best analogs for the current forecast cycle only.
! This is independent from KFAN step 2.

  best_analogs_obs(:,:) = analog_in_an(ndays,:,:)	! HA <-- DHA

! Support dynamic short training period, skip Kalman filter when
! short training period is active.
! Return AnEnMean result rather than KFAN, for current site only.

k_disable: &
  if (short_detect) then
    kfan_result = anenmean_result(:,:)			! (DH) <-- (DH)

!------------------------------------------------------------------------------
! KF/AN part 2.  Apply Kalman filter to AnEnMean result (analog ensemble mean).
!------------------------------------------------------------------------------

! Normal training period, use full KFAN..
! Convert 2-D inputs to 1-D.
! Dimensions must be transposed for kf_luca, to match Matlab.

  else
    obs_flat   = (/ transpose (obs) /)		! (1-D) <-- (HD) <-- (DH)
    ensan_flat = (/ transpose (anenmean_result) /)

    kpar2%update = nhours			! array stride for KF/AN

    call kf_luca (obs_flat, ensan_flat, vmiss, kpar2, diag, result_1d)

    kfan_result = transpose (reshape (result_1d, (/ nhours, ndays /) ))
						! (DH) <-- (HD) <-- (1-D)

    if (diag >= 4) then
      print '(a,99(1x,i0))', '  kfan_result dims = ', shape (kfan_result)
      print '(a,99(1x,i0))', '  kf_luca 1-D size = ', size (result_1d)
      print '(a,99(1x,i0))', '  ndays, nhours    = ', ndays, nhours
    end if
  end if k_disable

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  kfan_method, site', isite, ': Return.'
  end if

end function kfan_method
end module kfan__method
