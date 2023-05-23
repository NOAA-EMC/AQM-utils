!------------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Produces forecast based on Kalman filtering prediction in analog space.
!
! 2012-apr-20	kf_analog.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used unchanged by Djalalova and Wilczak, NOAA/OAR/ESRL/PSD3,
!		  for development of CMAQ bias correction method.
!
! 2014-mar-25	kf_analog.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/OAR/ESRL/PSD/CIRES.
!		Remove unused ensemble dimension, omit bug and simplify.
!		Remove var dimension from only the obs(...) input array.
!		This removes an unnatural co-indexing dependency between
!		  obs and ens arrays, via apar%fvar.
!
! 2015-oct-29	Add site-specific bias threshold filter for individual analogs.
!		Fix mis-labeled error messages.
!		Minor optimization of infrequent diagnostic.
!
! 2016-jan-26	Parallel support.
!		Replace thread unsafe threshold filter with inline version.
!		Move site exception list reader up to main program.
! 2016-feb-04	Restore some return arrays, for better code compatibility.
!		Adjust comments to indicate KFAS and ANEnMean results.
!
! 2016-feb-15	x_analog.f90:
!		Simplified version of kf_analog.f90.
!		Remove unused KFAS calculations and output (so-called KFAN).
!		But keep AnEnMean calculations identical.
!
! 2017-mar-27	analog_ensemble.f90:
!		Interface changes.  Add method name protocol.
!		Add predictor weights.
!		Rename some variables for clarity.
! 2017-may-25	Add parameter apar%forecast_last_hour to limit range of
!		  computed forecasts.
!
! 2018-jan-19	Add Irina's new bias formula for present forecast only.
!		Add parameter apar%bias_formula.
!
! 2020-may-14	When computing site weights, prevent the use of future obs
!		  that would not be available at initialization time for
!		  each test day's forecast.
!		Add parameters for site weighting.
! 2020-may-26	Change parameter name from bias_formula to analog_mean.
!		Add new analog mean formula, "mean obs".
! 2020-nov-13	Remove kludgy adjustment for fpar2%trend following
!		  improvements in find_analog.f90.
!
! 2022-may-26	Selectively shorten training period for recent high obs values.
!
! 2023-mar-25	Modify number of analogs when short training period is selected.
!
! * Remember to update the date in the module_id below.
!
! *** To do:
! *** After initial proving, convert reallocates to static arrays,
!     for efficiency.
!
!------------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module analog__ensemble		! standard visibility

  use config, only : dp
  implicit none

! Parameter structure for analog_ensemble.

  type apar_type

    integer   fvar		  ! array index for target var to be corrected
    integer   start_stat	  ! start day of forecasts needed to compute
    integer   forecast_last_hour  ! last forecast hour needed to compute
    integer   num_analogs	  ! Number of best analogs to use for AN
    integer   min_num_analogs	  ! Minimum number to enable bias correction
    character weight_type*14	  ! Type of analog weighting:  equal weights,
				  !   inverse metric, or linear
    integer   skipMissingAnalogs  ! Applies to both ANKF and AN
				  ! 1: Always use num_analog analogs,
				  ! even if some of the best ones are missing
    logical   block_future_obs	  ! F = normal, T = block retrospec. future obs
    real(dp)  bias_thresh_low	  ! bias exclusion thresholds for analogs
    real(dp)  bias_thresh_high
    character analog_mean*18	  ! selected analog mean formula, reserved names

    real(dp)  short_train_thresh  ! obs threshold to shorten training period
    integer   short_train_ndays   ! length of shortened training period
    integer   num_analogs_short	  ! Number of best analogs if short train. per.
    integer   min_num_analogs_short  ! Minimum num. analogs if short train. per.

  end type apar_type

contains

!-----------------------------------------------------------
! Analog filter function.
!-----------------------------------------------------------

subroutine analog_ensemble (obs, pred, pred_weights, vmiss, apar, fpar, &
    isite, site_id, diag, ensan, Ianalog, analog_in_an, short_detect)

  use config, only : dp
  use find__analog
  use ieee_arithmetic
  use kf__luca
  use short__training_period
  implicit none

  character(*), parameter :: &
    module_id = 'analog_ensemble.f90 version 2023-mar-25'

  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: pred_weights(:)	! V   - var weights
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! analog_ensemble params
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  integer,         intent(in ) :: isite			! current site index no.
  character(*),    intent(in ) :: site_id 		! current site ID
  integer,         intent(in ) :: diag			! verbosity, 0=errs only

  real(dp),        intent(out) :: ensan(:,:)		! DH  - bias corr result
  							!   actually is AnEnMean
  integer,         intent(out) :: Ianalog(:,:,:)	! DHA - found indicies
  real(dp),        intent(out) :: analog_in_an(:,:,:)	! DHA - found analogs
  logical,         intent(out) :: short_detect		! signal short tr. per.

! Local variables.

!!  character filename*40		! debug only

  integer a1, a2, d, h, i, i2, ilast, fvar
  integer nday, nhour, nvar, dim_analog
  integer iday_obs_source
  integer winLower, winUpper, nanalogs
  integer num_available, num_selected, ct
  integer num_analogs_select, min_num_analogs_select
  integer nan_count, count_missing

  real(dp) max_obs, mean_ens, mean_obs, bias

  type(fpar_type) fpar2			! second copy for local modification

! Dynamic arrays.

  integer,  allocatable :: inds(:)	! sorted indices from find_analog
  integer,  allocatable :: isequence(:), Ian(:)

  real(dp), allocatable :: obs_flat(:), pred_flat(:), obs2(:,:)
  real(dp), allocatable :: obstemp(:), predtemp(:)
  real(dp), allocatable :: an_weights(:), test_bias(:)

  real(dp), allocatable :: metric(:)	! returned metrics for selected analogs

  logical,  allocatable :: mask2(:), analog_mask(:)

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 5) print '(a,i5,a)', ' *** analog_ensemble, site', isite, &
                       ': Start.'

  if (diag >= 2 .and. isite == 1) &
    print '(4x,3a,i0)', 'Module ID = ', module_id, ', isite = ', isite

  nday  = size (pred, 1)		! get array dimensions
  nhour = size (pred, 2)		! first two must match obs
  nvar  = size (pred, 3)

  dim_analog = size (Ianalog, 3)

  if (diag >= 6) print '(a,99(1x,i0))', &
    '  nday, nhour, nvar, dim_analog =', nday, nhour, nvar, dim_analog

  if (diag >= 6) then
    print *, 'apar ='
    print *, apar
!!    print *, 'fpar ='
!!    print *, fpar			! print not supported in gfortran 4.8.2
!!    print *, 'kpar ='
!!    print *, kpar			! print not supported in gfortran 4.8.2
  end if

  fvar  = apar%fvar			! short name for readability

  fpar2 = fpar				! local copy of find_analog parameters
  					! for loop control of time values

  ensan(:,:)          = vmiss		! clear result arrays
  analog_in_an(:,:,:) = vmiss
  Ianalog(:,:,:)      = 0

!---------------------------------------------------------------
! Main nested loop over each forecast day, and forecast hour.
!---------------------------------------------------------------

day_loop: &
  do d = apar%start_stat, nday		! only loop over target forecast days
  					! to be corrected; start of array is
					! the training period

! When computing site weights, block obs data that would be in the
! future and not available at this day's forecast initialization time.

! iday_obs_source = day index in current (reshaped) obs array,
! of obs data at test forecast initialization time, or later.
! The position changes for different forecast hours.

    obs2 = obs(:,:)			! (D,H) copy obs, preserve original,
    					! perform blocking only on the copy
    if (apar%block_future_obs) then
      do h = 1, nhour
        iday_obs_source = d - ( (h - 1) / 24 )
        obs2(iday_obs_source:nday,h) = vmiss
      end do
    end if

! Selectively shorten the training period by blocking obs, when recent
! high obs values are detected.

    call short_training_period (obs2, vmiss, d, apar%short_train_thresh, &
      apar%short_train_ndays, short_detect, max_obs)

    if (short_detect) then
      if (diag >= 3 .and. d == apar%start_stat) &
        print '(4x,a,i6,f9.2)', 'Short training period, isite =', isite, &
          ', recent max obs =', max_obs
    end if

! Short training period modifies some other analog filter controls.
! In current version, these mods uniformly affect ALL FORECAST HOURS
! at the current site.

    if (short_detect) then			! short training period
      num_analogs_select     = apar%num_analogs_short
      min_num_analogs_select = apar%min_num_analogs_short

    else					! normal training period
      num_analogs_select     = apar%num_analogs
      min_num_analogs_select = apar%min_num_analogs
    end if

!-----------------------------------------------------
! Main loop over each forecast hour.
!-----------------------------------------------------

hour_loop: &
    do h = 1, apar%forecast_last_hour	! only loop over forecast hours needed

      if (diag >= 6) print '(2(a,i0))', 'analog_ensemble: D = ', d, ', H = ', h

      fpar2%fday  = d			! override target time parameters
      fpar2%fhour = h			! only one target hour at a time,
      					! for find_analog

! Only continue if predictions for ALL variables are available.  If
! any variables are missing values, then default to the original target
! variable prediction for the current hour.  I.e., no bias correction.

      if (any (pred(d, h, :) == vmiss)) then		! (D,H,V)
        if (diag >= 6) print '(3(a,i0))', &
          '*** Missing forecast values, day ', d, ', hour', h, &
          ', skipping bias correction.'

        ensan(d, h) = pred(d, h, fvar)
        cycle hour_loop
      end if

!! Fix for problem with trend for the first and last hours.
!! This part was REMOVED 2020 NOV 13.  Trend handling was corrected
!! and simplified within find_analog.f90.

!-----------------------------------------------------
! Find all analogs for the current forecast hour.
!-----------------------------------------------------

      if (diag >= 7) print '(a,99(1x,i0))', '  shape (pred) = ', shape (pred)

! Find flattened indicies of best analogs.
! Returned indices are sorted, with best matches at the END.

! Note, analogs are based only on model data in "pred", NOT obs.

      call find_analog (pred, pred_weights, vmiss, fpar2, fvar, diag, inds, &
            metric, winLower, winUpper)

!!      open (10, file='ianalog2')
!!      write (10, '(i10)') inds

!!      open (10, file='metric2')
!!      write (10, '(f10.2)') metric

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/ianalog.', d, '.', h
!!      open (10, file=filename)
!!      write (10, '(i10)') inds

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/metric.',  d, '.', h
!!      open (10, file=filename)
!!      write (10, '(f10.2)') metric

!! print *, '*** DEBUG STOP, ANALOG_ENSEMBLE'
!! stop

!! if (h == 14) then
!!   print '(a,i0,a,i0)', '*** DEBUG STOP, analog_ensemble, day ',d, ', hour ',h
!!   stop
!! end if

! Flatten the obs and pred arrays in the current window.
! Reallocate on assignment, as needed.

!!      len_flat = (d - 1) * (winUpper - winLower + 1)
!!      allocate (obs_flat (len_flat))		! instead, auto reallocate...
!!      allocate (pred_flat(len_flat))

      obs_flat  = (/ obs2(1:d-1, winLower:winUpper)       /)	! 2-D to 1-D
      pred_flat = (/ pred(1:d-1, winLower:winUpper, fvar) /)

! Get the selected analog values.
! Append the current (d,h) prediction value as the final element.

      obstemp  = (/ obs_flat (inds), vmiss            /)
      predtemp = (/ pred_flat(inds), pred(d, h, fvar) /)

      nanalogs = size (obstemp)

!-----------------------------------------------------------
! Remove individual analogs that exceed a site-specific
! threshold for bias value.
!-----------------------------------------------------------

! Enable all analogs for current site without both specified thresholds.

bias_threshold_filter: &
      if (  apar%bias_thresh_low  == vmiss &
       .or. apar%bias_thresh_high == vmiss) then

        analog_mask = spread (.true., 1, nanalogs)

! Compute individual test bias values for site with specified thresholds.

      else
        test_bias = obstemp(:) - predtemp(:)

! Apply thresholds and create mask.
! TRUE = analog enabled, FALSE = disabled (bias outside the given thresholds).

        analog_mask =    (test_bias(:) > apar%bias_thresh_low)  &
                   .and. (test_bias(:) < apar%bias_thresh_high)

        if (diag >= 4) then			! threshold diagnostic
          print '(2a,3i6)', &
          '  site, nday, nhour, no. of threshold disabled analogs = ', &
          site_id, nday, nhour, count (analog_mask(:) .eqv. .false.)
        end if

      end if bias_threshold_filter

!-------------------------------------------------
! AN ensemble + bias computation.
!-------------------------------------------------

      isequence = (/ (i, i = 1, nanalogs) /)	! make consecutive indices

! If selected, ignore missing analogs such that we have num_analogs_select
! analogs, when possible.

      if (apar%skipMissingAnalogs == 1) then

!        I = intersect(find(obstemp ~= vmiss), find(predtemp ~= vmiss))
!        I = I(max(1,length(I)-apar%num_an+1):end)

        mask2 = (obstemp /= vmiss .and. predtemp /= vmiss)

        mask2 = mask2 .and. analog_mask		! also exclude threshold rejects

        Ian = pack (isequence, mask2)		! indices of non-missing analogs

        num_available = size (Ian)
        num_selected = min (num_available, num_analogs_select)
					! limit to requested number of analogs

        i2 = num_available - num_selected + 1		! reduce index array
        if (i2 > 1) Ian = Ian(i2:)			! to size limit

        if (diag >= 7) print *, 'size (Ian) = ', size (Ian)
!!        print '(10i5)', Ian

! Otherwise, allow fewer than apar%num_an analogs, i.e. when some are missing.
! DEFERRED 2014-mar-11.  This option was not in current use.

      else
        print *,           '*** analog_ensemble: Abort.'
        print '(a,i0,a)', ' *** Option apar%skipMissingAnalogs = ', &
          apar%skipMissingAnalogs, ' is not currently supported.'
        call exit (1)

!!        I = length(predtemp)-apar%num_an:length(predtemp)-1
!!        I = intersect(I, find(obstemp ~= vmiss))
!!        I = intersect(I, find(predtemp ~= vmiss))
      end if

      ct = size (Ian)				! number of selected analogs

! Determine type of weighting.

      if (apar%weight_type == 'equal weights') then
        an_weights = (/ (1, i = 1, ct) /)	! trick for array of ones

      else if (apar%weight_type == 'inverse metric') then
        an_weights = 1 / (metric(Ian) ** 2)

! The weights should not be nans, because the metric for a value
! where the obs and pred are not missing should be a valid number

!!        assert(isempty(find(isnan(metric(end-ct+1:end)), 1)))

        nan_count = count (ieee_is_nan (an_weights(:)))

        if (nan_count /= 0) then
          print *, '*** analog_ensemble: NaNs detected when computing' &
            // ' weights for metrics.'
          print *, '*** Number of weights = ', ct
          print *, '*** NaN count         = ', nan_count
          call exit (1)
        end if

      else if (apar%weight_type == 'linear') then
        an_weights = (/ (i, i = 1, ct) /)	! 1 through N

      else
        print *, '*** analog_ensemble: Abort, weighting scheme not supported.'
        print '(a,i0)', ' *** Option apar%weight_type = ', apar%weight_type
        call exit (1)
      end if

      an_weights = an_weights / sum (an_weights)	! Normalize weights

      if (diag >= 7) print *, 'an_weights = '
      if (diag >= 7) print '(10f8.5)', an_weights

! Compute weighted mean arrays.

      if (diag >= 7) print *, 'Compute weighted mean arrays:'
      if (diag >= 7) print *, 'Ian ='
      if (diag >= 7) print '(10i6)', Ian
      if (diag >= 7) print *, 'size (Ian)        = ', size (Ian)
      if (diag >= 7) print *, 'size (an_weights) = ', size (an_weights)
      if (diag >= 7) print *, 'size (predtemp)   = ', size (predtemp)

      mean_ens = sum (predtemp(Ian) * an_weights)   ! Compute weighted ens mean

      if (diag >= 7) print *, 'size (obstemp)    = ', size (obstemp)

      mean_obs = sum (obstemp(Ian)  * an_weights)   ! Compute weighted obs mean

! Complain about missing values, but do not stop.

      if (diag >= 6) then

         if (diag >= 7) print *, 'count missing:'

         count_missing = count (obstemp(Ian) == vmiss)

         if (count_missing /= 0) then
           print *
           print '(2a,i0,2a,i0,a)', '*** analog_ensemble: obstemp(Ian)', &
                 ' contains ', count_missing, ' missing values,', &
                 ' out of a total of ', size (obstemp(Ian)), ' values.'
           print '(10f8.2)', obstemp(Ian)
         end if

      end if

!------------------------------------------------------------------
! Bias correction.  Use analogs if we have a sufficient number.
!------------------------------------------------------------------

! Thomas's method removed, 2014-mar-12.  See original Matlab, to recover.

!!    %if (ct >= 3 && metric(end) <= par%metric_thres)  % hybrid AN-ANKF, v1
!!    %if (ct >= 3 && pred(d, h, par%fvar, e) <= 7) %  hybrid AN-ANKF, v2

enough_analogs: &
      if (ct >= min_num_analogs_select) then

        bias = mean_obs - mean_ens
        ilast = size (predtemp)

        if      (apar%analog_mean == 'mean obs') then
          ensan(d, h) = mean_obs		! use only mean of best obs

        else if (apar%analog_mean == 'forecast plus bias') then
          ensan(d, h) = predtemp(ilast) + bias	! use present forecast only

        else if (apar%analog_mean == 'mean plus bias') then
          ensan(d, h) = ( (mean_ens * ct + predtemp(ilast)) / (ct + 1) ) + bias
			! orig. analog mean formula, full spec in config file is
			! "mean (forecast plus model predictions) plus bias"
        else
          print *, '*** analog_ensemble: Abort, unknown code for analog mean' &
                        // ' formula.'
          print *, '*** Option apar%analog_mean = ' // trim (apar%analog_mean)
          call exit (1)
        end if

!!        %ensan(d, h) = predtemp(end) + bias;	    ! Thomas Palined...and me...
!!        %ensan(d, h) = mean_obs;		    ! The more elegant...
!!        %analog_in_an(d, h, 1:ct) = obstemp(Ian);  ! Save the analogs used
!!						     ! for AN (until 9 Jan 12)

        a1 = num_analogs_select - ct + 1	! also return the analogs used
        a2 = num_analogs_select
        analog_in_an(d, h, a1:a2) = obstemp(Ian)

! Otherwise default to prediction.

      else
        ilast = size (predtemp)
        ensan(d, h) = predtemp(ilast)
!!        %ensan(d, h) = kfan(d, h);		! for the hybrid method
      end if enough_analogs

    end do hour_loop
  end do day_loop

  if (diag >= 6) print *, '*** analog_ensemble: Return.'

end subroutine analog_ensemble
end module analog__ensemble
