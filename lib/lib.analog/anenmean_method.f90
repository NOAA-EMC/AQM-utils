!------------------------------------------------------------------------------
!
! anenmean_method.f90 -- Analog ensemble mean filter for one site time series.
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
! 2016-feb-04	anenmean_method.f90:
!		Top level control for AnEnMean method, single site time series.
!		Break out from main_analog.f90, PSD version 2016-feb-01.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
! 2016-feb-15	Use simplified x_analog subroutine, instead of kf_analog.
!
! 2017-apr-29	Interface changes for new method functions.
!		Move some parameters into parameter structure.
! 2017-may-12	New protocol, method name embedded in filter routine.
! 2017-may-25	apar%start_stat and apar%forecast_last_hour must now
!		  be provided by all callers.
!
! 2019-jun-18	Add output for best analog obs values, current forecast only.
! 2020-may-11	Minor, parameter name change.
!
! 2023-mar-25	Minor interface change to subroutine.
!
!------------------------------------------------------------------------------

module anenmean__method
contains

function anenmean_method (filter_method, obs, pred, vmiss, apar, fpar, &
    pred_weights, isite, site_id, diag, anenmean_result, best_analogs_obs) &
    result (return_status)

  use config, only : dp
  use find__analog, only : fpar_type
  use analog__ensemble
  implicit none

  character(*),    intent(in ) :: filter_method		! selected method name
  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! kf_analog parameters
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  real(dp),        intent(in ) :: pred_weights(:)       ! V - predictor weights
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: anenmean_result(:,:)	! DH - bias corr. result
  real(dp),        intent(out) :: best_analogs_obs(:,:)	! HA - best analogs

  logical return_status		! function return status: true = processed,
  				! false = method name not matched

! Local variables.

  type (apar_type) :: apar2
  type (fpar_type) :: fpar2

  character fdate_str*24
  integer ndays, nhours
  logical short_detect

! Unused result arrays from analog_ensemble.  See subroutine docs.

  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs
  real(dp), allocatable :: analog_in_an(:,:,:)	! nearest analogs found

! Check requested filter method name.

  return_status = (filter_method == 'AnEnMean')   ! case sensitive

  if (.not. return_status) return		  ! name not recognized; return
						  !   without any processing
! Start AnEnMean method.

  if (diag >= 3) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  anenmean_method, site', isite, ': Start.'
  end if

! Conservative.  Make writeable copies of parameter structures,
! to avoid parallel conflicts.
!
! * 2016-feb-4:  Possible compiler bugs in parallel support for structures.
! * Assume lower levels might modify any of these structures.

  apar2 = apar
  fpar2 = fpar

! Allocate dummy result arrays for single site.

  ndays  = size (pred, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (pred, 2)		! number of hours in each forecast cycle

  allocate (ianalog(ndays, nhours, apar2%num_analogs))		! DHA
  allocate (analog_in_an(ndays, nhours, apar2%num_analogs))	! DHA

! Apply Analog Ensemble filter.

  call analog_ensemble (obs, pred, pred_weights, vmiss, apar2, fpar2, isite, &
    site_id, diag, anenmean_result, Ianalog, analog_in_an, short_detect)
					! last four are outputs

! Also return best analogs for the current forecast cycle only.

  best_analogs_obs(:,:) = analog_in_an(ndays,:,:)	! HA <-- DHA

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  anenmean_method, site', isite, ': Return.'
  end if

end function anenmean_method
end module anenmean__method
