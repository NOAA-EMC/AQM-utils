!-----------------------------------------------------------------------------
!
! weight_control.f90 -- Main loop for weight combinations, when selected.
!
! This routine handles normal analog filter and optimal weight
! generation for one site at a time.
!
! This is a support routine for the NOAA/NCAR bias correction
! system for CMAQ forecast outputs.
!
! 2017-apr-29	Original stub version.  Just drops through to filter_dispatch.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Adapted from main_analog.f90 and method subroutines.
! 2017-may-17	Add search loop for optimal weights.
! 2017-may-18	Exclude sites with no obs data.  Improves run time.
! 2017-may-25	Add weight generation parameters for forecast start day,
!		  and forecast last hour.
!		Minor bug fix for equal weights.
! 2017-may-31	Split off original weight set generation into separate
!		  routine, compute_weight_sets.f90.
!		Routine will return equal weights as guaranteed first set.
! 2017-jun-06	Add support for weight generation by subsets.
!
! 2019-jun-18	Add output for best analog obs values, current forecast only.
! 2019-sep-08	Documentation fixes only.
!		Remove invalid comment changes from 2019-jun-18.
!		Rename variable "filter_result" to more accurate "corrected".
!
! 2020-apr-17	Add diagnostics for weight generation.
! 2020-may-05	Parameter change to "number of days in test period".
! 2020-may-11	Prevent use of future obs when computing site weights.
!
!-----------------------------------------------------------------------------

module weight__control		! standard visibility
  implicit none

! Parameter structure for predictor weight generation.

  type wpar_type
    logical gen_weights		! true = generate weights, false = normal oper.
    integer nweights		! number of weight increments to distribute
    				!   across all combinations
                                !   e.g. 10 = increments of 0.1
                                ! note, each weight combination will sum to 1.0
    integer ndays_test_period	! number of days in weight gen. test period
    integer forecast_last_hour	! last hour of test forecasts for weight gen.
    integer subset1		! first & last weight set numbers to generate;
    integer subset2		! no subset if subset1 is neg. (make all sets)
  end type wpar_type

contains

subroutine weight_control (filter_method, obs, pred, vmiss, apar, fpar, kpar, &
    wpar, pred_weights, bias_thresh_low, bias_thresh_high, isite, site_id, &
    diag, corrected, best_analogs_obs, new_weights, new_rmse)

  use analog__ensemble,  only : apar_type
  use compute__weight_sets
  use config,            only : dp
  use find__analog,      only : fpar_type
  use filter__dispatch
  use kf__luca,          only : kpar_type
  use multisets_mod
  use rmse_mod
  implicit none

! Input arguments.

  character(*),    intent(in ) :: filter_method	     ! selected filter method
  real(dp),        intent(in ) :: obs(:,:)	     ! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)	     ! DHV - predictor variables
  real(dp),        intent(in ) :: vmiss		     ! common missing value
  type(apar_type), intent(in ) :: apar		     ! analog_ensemble params
  type(fpar_type), intent(in ) :: fpar		     ! find_analog parameters
  type(kpar_type), intent(in ) :: kpar		     ! Kalman filter parameters
  type(wpar_type), intent(in ) :: wpar		     ! weight generation params
  real(dp),        intent(in ) :: pred_weights(:)    ! V - predictor weights
  real(dp),        intent(in ) :: bias_thresh_low    ! bias limits for
  real(dp),        intent(in ) :: bias_thresh_high   !   found analogs
  integer,         intent(in ) :: isite		     ! current site index no.
  character(*),    intent(in ) :: site_id 	     ! current site ID
  integer,         intent(in ) :: diag		     ! verbosity, 0=errors only

! Output arguments.
! Valid only in normal mode: corrected, best_analogs_obs
! Valid only in weight generation mode: new_weights, new_rmse

  real(dp),    intent(out) :: corrected(:,:)	     ! DH - bias corr. filter
  						     !   result at current site
  real(dp),    intent(out) :: best_analogs_obs(:,:)  ! HA - best analogs
  real(dp),    intent(out) :: new_weights(:)	     ! V - generated weights
  real(dp),    intent(out) :: new_rmse		     ! best RMSE for gen weights

! Local variables.

  character fdate_str*24, fmt_line*80, fmt2*80, flag*1

  integer nvars, nsets, nsets_run, ndays, nhours
  integer iday1, iday2, hour1, hour2, fvar
  integer iset, iset1, iset2, iset_min
  integer obs_valid, obs_missing, pred_valid, pred_missing

  real(dp) rmse2, rmse_min, equal_weights_value
  real(dp) obs_min, obs_max, pred_min, pred_max

  type (apar_type) apar2			! modifiable copy of structure

  real(dp), allocatable :: weight_sets(:,:)	! SV - all weight combinations

  logical, allocatable :: obs_mask(:,:), pred_mask(:,:)  ! for diagnostics only

! Start weight control.

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  weight_control, site', isite, ': Start.'
  end if

! Make writeable copy of parameter structure, to avoid parallel conflicts.

  apar2 = apar

! Transfer site-specific parameters into current site parameter structure.
! Done here specifically to avoid parallel conflicts in analog_control.

  apar2%bias_thresh_low  = bias_thresh_low
  apar2%bias_thresh_high = bias_thresh_high

! Get dimensions, etc.

  ndays  = size (pred, 1)		! number of forecast cycles in data
  					! training period is 1 to ndays-1
					! last day is current forecast cycle
  nhours = size (pred, 2)		! number of forecast hours
  nvars  = size (pred, 3)		! number of predictor variables

  fvar   = apar2%fvar			! forecast var index, short name

! Ensure initialized memory for possibly unused output variables.

  corrected(:,:) = pred (:,:,fvar)	   ! default for no obs = no bias corr.

  equal_weights_value = 1 / dble (nvars)   ! equal weights, normalized
  new_weights(:) = equal_weights_value	   ! default for no obs = equal weights
  new_rmse       = 999			   ! and fake high value RMSE

!---------------------------------------------------------------
! Weight generation mode.  Search loop for optimal weights.
!---------------------------------------------------------------

gen_mode: &
  if (wpar%gen_weights) then

! Set bounds for forecast days and hours to test for weight generation.
! The test period is a given number of days at the end of the training period.

    iday1 = ndays - wpar%ndays_test_period    ! start day of test period
    iday1 = max (1, iday1)		! limit to available training period
    iday2 = ndays - 1			! end on last day of training period

    hour1 = 1				! start on first forecast hour
    hour2 = wpar%forecast_last_hour	! end on user selected end hour

    apar2%start_stat         = iday1	! pass bound params to filter routines
    apar2%forecast_last_hour = hour2

    apar2%block_future_obs    = .true.	! block use of retrospective future obs

! Skip site with no obs data within selected time ranges.
! Outputs are the defaults set above.

no_data: &
    if (all (obs(iday1:iday2, hour1:hour2) == vmiss)) then

      if (diag >= 2) print '(a,i6,a)', ' *** Skipping site', isite, &
                       ', no obs data'

! Site has data.  Generate all possible weight combinations.
! All combinations are normalized within routine compute_weight_sets.

    else
      weight_sets = compute_weight_sets (nvars, wpar%nweights)
      nsets       = size (weight_sets, 1)

      if (diag >= 3) print *

      if (diag >= 2 .and. isite == 1) print '(a,i2,a,i9)', '   site', isite, &
        ', total number of weight combinations =', nsets

! If selected, print some detail site diagnostics for RMSE inputs.

      if (diag >= 2) then
        obs_mask  =        (obs (iday1:iday2, hour1:hour2)        /= vmiss )
        obs_min   = minval (obs (iday1:iday2, hour1:hour2),       obs_mask )
        obs_max   = maxval (obs (iday1:iday2, hour1:hour2),       obs_mask )

        pred_mask =        (pred(iday1:iday2, hour1:hour2, fvar)  /= vmiss )
        pred_min  = minval (pred(iday1:iday2, hour1:hour2, fvar), pred_mask)
        pred_max  = maxval (pred(iday1:iday2, hour1:hour2, fvar), pred_mask)

        obs_valid    = count (obs_mask )
        pred_valid   = count (pred_mask)

        obs_missing  = size (obs_mask ) -  obs_valid
        pred_missing = size (pred_mask) - pred_valid

        fmt2 = "(i8, '  Test ', a5, ' min, max, #valid, #missing ='," &
               // " 2f11.7, 2i7)"

        print fmt2, isite, '  Obs',  obs_min,  obs_max,  obs_valid,  obs_missing
        print fmt2, isite, 'Model', pred_min, pred_max, pred_valid, pred_missing
      end if

! If selected, confine weight set indices to currently selected range.

      if (wpar%subset1 < 0) then		! case: subset not selected
        iset1 = 1
        iset2 = nsets
      else
        iset1 = wpar%subset1			! case: subset is specified
        iset2 = min (wpar%subset2, nsets)
      end if

      if ( (iset1 < 1) .or. (iset1 > iset2) ) then
        print *
        print *, '*** weight_control: Invalid start or end set numbers' &
          // ' for weight generation.'
        print '(2(a,i0))', '*** Requested set range = ', wpar%subset1, &
          ' to ', wpar%subset2
        print '(2(a,i0))', '*** Available set range = 1 to ', nsets
        print *, '*** Abort.  Weights not generated.'
        call exit (1)
      end if

      nsets_run = iset2 - iset1 + 1

      if (diag >= 2 .and. isite == 1 .and. wpar%subset1 >= 0) then
        print '(a,i2,a,i9)',      '   site', isite, &
          ', number of weight sets for this run  =', nsets_run
        print '(a,i2,a,i9,a,i9)', '   site', isite, &
          ', selected subset for this run        =', iset1, ' to ', iset2
      end if

! More setup for main weight loop.

      iset_min = 0		! init memory, will always be overwritten
      rmse_min = 0

      fmt_line = '(2i8, f12.6, a1, 99f7.3)'

! Display RMSE for uncorrected forecasts, same time interval, if requested.

      if (diag >= 2) then
        rmse2 = rmse ( obs(iday1:iday2, hour1:hour2), &
                      pred(iday1:iday2, hour1:hour2, fvar), vmiss)
        iset = 0
        flag = ' '
        print '(2i8,f12.6,a1,a)', isite, iset, rmse2, flag, '  uncorrected RMSE'
      end if

! Main loop to test all weight combinations.

set_loop: &
      do iset = iset1, iset2

! Run analog filter for current weight set.

        call filter_dispatch (filter_method, obs, pred, vmiss, apar2, fpar, &
          kpar, weight_sets(iset,:), isite, site_id, diag, corrected, &
          best_analogs_obs)

! Compute RMSE for current weights, on the bias corrected filter result.

        rmse2 = rmse (obs      (iday1:iday2, hour1:hour2), &
                      corrected(iday1:iday2, hour1:hour2), vmiss)

! Remember the weight set with the lowest RMSE.

        if ( (iset == iset1) .or. (rmse2 < rmse_min) ) then
          iset_min = iset		! always save on first weight set
          rmse_min = rmse2
          flag = '*'			! visual indicator
        else
          flag = ' '
        end if

        if ( (diag >= 2 .and. flag == '*') .or. (diag >= 4) ) &
          print fmt_line, isite, iset, rmse2, flag, weight_sets(iset,:)

      end do set_loop

! Output the best set of weights.

      new_weights = weight_sets(iset_min,:)
      new_rmse    = rmse_min			! RMSE for best weight set

      if (diag >= 2) print fmt_line, isite, iset_min, new_rmse, ':', &
        new_weights(:)

! In weight generation mode, primary filter results are invalid.
! Erase these primary results to prevent misinterpretation.

      corrected(:,:)        = vmiss
      best_analogs_obs(:,:) = vmiss

    end if no_data

!---------------------------------------------------------------
! Normal mode.  Call analog filter once, with previous weights.
!---------------------------------------------------------------

  else

! Set up for bias correction in normal production mode.

    apar2%start_stat = ndays	! bias correct only last day in training period
    				!   (may be changed within selected method)

    apar2%forecast_last_hour = nhours	! bias correct over all forecast hours

    apar2%block_future_obs = .false.	! do not block use of retro. future obs

! Run the selected analog filter.

    call filter_dispatch (filter_method, obs, pred, vmiss, apar2, fpar, kpar, &
      pred_weights, isite, site_id, diag, corrected, best_analogs_obs)

  end if gen_mode

! All modes exit here.

  if (diag >= 4) then
    call fdate (fdate_str)
    print '(2a,i5,a)', fdate_str, '  weight_control, site', isite, ': Return.'
  end if

end subroutine weight_control
end module weight__control
