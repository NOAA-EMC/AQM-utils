!-----------------------------------------------------------------------------
!
! filter_dispatch.f90 -- Dispatch to a specified filter method, for one site.
!
! This is a support routine for the NOAA/NCAR bias correction
! system for CMAQ forecast outputs.
!
! 2017-apr-29	Original version.  By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Adapted from main_analog.f90 and method subroutines.
! 2017-may-12	New protocol, method name embedded in each filter routine.
! 2017-may-18	Comment fixes only.
!
! 2019-jun-18	Add output for best analog obs values, current forecast only.
!
!-----------------------------------------------------------------------------

module filter__dispatch
contains

subroutine filter_dispatch (filter_method, obs, pred, vmiss, apar, fpar, &
    kpar, pred_weights, isite, site_id, diag, filter_result, best_analogs_obs)

  use analog__ensemble,  only : apar_type
  use anenmean__method
  use config,            only : dp
  use find__analog,      only : fpar_type
  use kfan__method
  use kf__luca,          only : kpar_type
  implicit none

  character(*),    intent(in ) :: filter_method	     ! selected filter method
  real(dp),        intent(in ) :: obs(:,:)	     ! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)	     ! DHV - predictor variables
  real(dp),        intent(in ) :: vmiss		     ! common missing value
  type(apar_type), intent(in ) :: apar		     ! analog_ensemble params
  type(fpar_type), intent(in ) :: fpar		     ! find_analog parameters
  type(kpar_type), intent(in ) :: kpar		     ! Kalman filter parameters
  real(dp),        intent(in ) :: pred_weights(:)    ! V - predictor weights
  integer,         intent(in ) :: isite		     ! current site index no.
  character(*),    intent(in ) :: site_id 	     ! current site ID
  integer,         intent(in ) :: diag		     ! verbosity, 0=errors only

  real(dp),        intent(out) :: filter_result(:,:)    ! DH - bias corr. result
  real(dp),        intent(out) :: best_analogs_obs(:,:)	! HA - best analogs

  character fdate_str*24		! local variable

! Start dispatch.

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  filter_dispatch, site', isite, ': Start.'
  end if

! Call the filter method routine for the current site.
! These routines must be parallel compatibile.
! Arguments isite and site_id are for diagnostics only.

! Note, 2014-feb-17, Dave A:  Removed all methods that were in the
! original Matlab code, except for the target KF/AN code.  To recon-
! struct other methods, refer to main_analog_code.m version 2013-jul-9.

! 2017-may-12:  Method names are now embedded inside each method
! function.  Each function returns true if processed, silently
! returns false if method name not matched.  Functions may respond
! to more than one name, as needed.

dispatch: do			! one-trip structure, for dispatching

    if (anenmean_method (filter_method, obs, pred, vmiss, apar, fpar, &
      pred_weights, isite, site_id, diag, filter_result, best_analogs_obs)) &
      exit dispatch

    if (kfan_method (filter_method, obs, pred, vmiss, apar, fpar, kpar, &
      pred_weights, isite, site_id, diag, filter_result, best_analogs_obs)) &
      exit dispatch

! Trap invalid method.

    print *
    print *, '*** filter_dispatch: Unknown filter method name.  Abort.'
    print *, '*** Filter method name = "' // trim (filter_method) // '"'
    call exit (1)

  end do dispatch

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  filter_dispatch, site', isite, ': Return.'
  end if

end subroutine filter_dispatch
end module filter__dispatch
