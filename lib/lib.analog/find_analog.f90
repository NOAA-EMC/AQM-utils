!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Lazboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Author: Thomas Nipen
! Date: May 4, 2009
! Description: Takes input data and finds analogous conditions
!              to the conditions at index.
!
! 2012-apr-20	findAnalog_v1.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used by Djalalova and Wilczak, NOAA/ESRL/PSD3, for
!		  development of CMAQ bias correction method.
!
! 2014-mar-25	find_analog_v1.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Change function to subroutine, for better interface checking.
!		Remove var_set parameter, compute results over all variables.
!		Remove inactive useWspdAsAnalog kludge, dangerous indexing.
! 2014-jun-24	Apply bug fix for wind direction metric.
!		Found by Stefano Alessandrini, NCAR.  See e-mail, 2014 June 6.
!		Fix name conflict, change sort module name.
!		Switch is_circular to type logical.
! 2014-jul-15	Add diagnostics.
!
! 2017-mar-28	DRA: Add predictor weighting, per 2014-oct-01 code update
!		  by Stefano Alessandrini (ste), NCAR.
! 2017-jun-29	DRA: Fix metric problem when a predictor is all zeros
!		  or constant values within current search window.
!
! 2020-nov-15	find_analog.f90:
!		Fix support for search window wider than current forecast hour.
!		Rework and simplify all of the stencil and window logic.
!		Add support for asymmetric search window bounds.
!		Replace old compiler bug workarounds with modern array code.
!
! This version requires F2003 automatic reallocation on assignment.
!
! Inputs:
!       data		<day> x <hour> x <vars>
!			(<day> same as run or forecast cycle)
!       index		< |time|
!       fpar%vmiss	Missing value code
!       fpar%trend	Gives the relative weighting of trend values
!			Must be an odd number length, where the middle number
!			refers to the value, for example:
!			[ T-2 T-1 T T+1 T+2]
!       fpar%window(2)	lower and upper bounds for analog search window, as
!       		  offsets from current hour, e.g. 0 and 0, or -2 and 2
!       fpar%fhour	Forecast hour
!       fpar%fday	Forecast day
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module find__analog		! standard visibility

  use config, only : dp
  implicit none

! Parameter structure for find_analog.

  type fpar_type

    integer  window(2)		! lower and upper bounds for search window
				!   as offsets from current hour, e.g. -2 and 2
    integer  fhour		! Forecast hour
    integer  fday		! Forecast day
    integer  useRealTrends	! 1: Use trend, 0: use neighbours
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2
    real(dp) lowerMetric	! Lower bound for allowed metric value
				! (see findAnalog)

    real(dp), allocatable :: trend(:)	! Relative weighting of the trend values
				! Must be an odd number length, where the middle
				! number refers to the value, for example:
				!   [ T-2 T-1 T T+1 T+2]

    logical, allocatable :: is_circular(:)	! indicate circular,
    						! T or F for each analog var
  end type fpar_type

contains

!-----------------------------------------------------------
! find_analog search routine.
!-----------------------------------------------------------

subroutine find_analog (data, pred_weights, vmiss, fpar, fvar, diag, indices, &
    metrics, winLower, winUpper)

  use index__sort_descending
  use stdev_TNcirc
  use wind__dir_error
  implicit none

  real(dp),        intent(in ) :: data(:,:,:)	    ! (days, hours, vars)
  real(dp),        intent(in ) :: pred_weights(:)   ! predictor weights (vars)
  real(dp),        intent(in ) :: vmiss		    ! common missing value
  type(fpar_type), intent(in ) :: fpar		    ! find_analog parameters
  integer,         intent(in ) :: fvar		    ! var index of forecast var
  integer,         intent(in ) :: diag		    ! verbosity, 0=errors only

  integer,  allocatable, intent(out) :: indices(:)  ! 1-D indices within window
  real(dp), allocatable, intent(out) :: metrics(:)  ! 1-D metrics within window
  integer,  intent(out) :: winLower, winUpper	    ! hour range, found window

! Local variables.

  integer nday, nhour, nvars, noffsets, nanalogs
  integer v, w, i, win_size, dim_num

  integer distance1, distance2			! stencil calculation
  integer offset1, trendSize
  integer i1, i2, icenter
  integer wlimit1, wlimit2			! search window calculation

  integer hour1, hour2, w1, w2			! hour bounds, stencil & window
  integer fhour					! index of current forecast hour
  integer fday					! index of current forecast day

  integer prev_day				! index of previous day,same as:
  integer ndays_prev				! number of preceeding days

  integer, allocatable :: Ioffsets(:), Ihours(:), Whours(:)

  real(dp), allocatable :: local_trend(:)
  real(dp), allocatable :: stddata(:,:), varstd(:,:), summed(:,:)
  real(dp), allocatable :: normalized(:,:), metric(:,:)
  real(dp), allocatable :: analogDifference(:,:,:)
  real(dp), allocatable :: atrend(:,:,:), ftrend(:,:,:)
  real(dp), allocatable :: angular_err(:,:), metric_1d(:)

! Automatic arrays

  real(dp) varstd_1d(size(data,3))		! nvars

!--------------------------------------------
! Initialize.
!--------------------------------------------

  if (diag >= 5) print *
  if (diag >= 4) print *, '*** find_analog: Start.'

  nday  = size (data, 1)		! get array dimensions
  nhour = size (data, 2)
  nvars = size (data, 3)

  fhour = fpar%fhour			! shorter names for common parameters
  fday  = fpar%fday

  prev_day   = fday - 1			! day before forecast day
  ndays_prev = prev_day			! number of days before this forecast
  					! (same number, different usages)

!--------------------------------------------
! 1.  Determine the STENCIL size.
!--------------------------------------------

! The stencil is a sliding window over forecast hours, for the
! purpose of containing the TREND CALCULATION INTERVAL at each
! position.  The stencil is CENTERED on each analog in the search.
!
! For consistency, the stencil must remain UNIFORM SIZE for all
! analogs for the CURRENT TARGET FORECAST HOUR.  However, for
! target hours near the start or end of the full range of forecast
! hours, the trend interval must be SYMMETRICALLY REDUCED to fit
! within the available forecast hours.  The uniform stencil size
! is also correspondingly reduced.

! Offset from REQUESTED trend CENTER to trend start:
! offset for [ABC] = 1, for [ABCDE] = 2, etc.

! Note, this calculation is SAFE for invalid trend configs with
! an even number of elements.  This is where the LAST ELEMENT
! of an even numbered config will be ignored, because everything
! that follows will be derived from this offset calculation.
! This "offset1" is interpreted as one half of the whole symmetric
! trend interval.

  offset1 = (size (fpar%trend) - 1) / 2

! Reduce the offset when the current forecast hour is near either
! end of the forecast hour range.  The full range is 1 to nhour.
! This has the effect of narrowing the stencil when near the ends
! of the range.

  distance1 = fhour - 1
  distance2 = nhour - fhour
  trendSize = minval ((/ offset1, distance1, distance2 /))
  					! misnomer, trendSize is only 1/2 trend
                                	! size, minus the center position

  if (diag >= 6) print *, '  trendSize = ', trendSize

! For convenience later, get the reduced trend weights array
! for the current target forecast hour.

  icenter = 1 + offset1			! this method always makes a symmetric
  i1      = icenter - trendSize		! weight array, centered on the middle
  i2      = icenter + trendSize		! of the original weights array

  local_trend = fpar%trend(i1:i2)    ! matches stencil for CURRENT forecast hour

!--------------------------------------------
! 2.  Determine the ANALOG SEARCH WINDOW.
!--------------------------------------------

! The search window for the current target forecast hour is limited
! to available forecast hours.  The window is further limited by the
! stencil size.  All analogs must support the same stencil as the
! target forecast hour.  The SIDES of the stencil may extend OUTSIDE
! the current search window at each search position.  However, both
! SIDES of the stencil must remain fully inside available hours
! within the FULL FORECAST HOUR RANGE.
!
! This has the effect of narrowing the SEARCH WINDOW and reducing
! the number of analogs searched, for target forecast hours near the
! first and last forecast hours.
!
! The boundaries winLower and winUpper are absolute indicies.
! Boundary parameters fpar%window(1) and (2) are requested offsets
! down and up from the target forecast hour.  Window(1) and (2) may
! differ; i.e. ASYMMETRIC search windows are supported if desired.

  wlimit1 = 1     + trendSize			    ! lower and upper limits
  wlimit2 = nhour - trendSize			    ! with current stencil size

  winLower = max (wlimit1, fhour + fpar%window(1))  ! window(1) is 0 or negative
  winUpper = min (wlimit2, fhour + fpar%window(2))  ! window(2) is 0 or positive

!-----------------------------------------------------------
! Compute standard deviations over all of current window.
!-----------------------------------------------------------

! For each variable, subset to current stencil only.
! Collapse (days x hours) to 1-D for computing standard deviations.
! NEW 2014-mar-2: Order of (D,H) SHOULD NOT MATTER for stddata.

  win_size   = winUpper - winLower + 1		! number of hours in WINDOW
  nanalogs   = ndays_prev * win_size		! size of collapsed dimension
  						! should be same as size(metric)
  allocate (stddata(nanalogs, nvars))

  do v = 1, nvars				! XV <-- DHV, where X = D' * H'
    stddata(:, v) = (/ data(1:prev_day, winLower:winUpper, v) /)
  end do					! collapse 2-D to 1-D, any order

!!  stddata = reshape (permute (data(1:par.fday-1,winLower:winUpper,:), &
!!     [2 1 3]), [(par.fday-1)*(winUpper-winLower + 1) nvar]);    ! (Matlab)
!!				! permute should not have made any difference
!!				! in computing std dev of single vectors

! Compute standard deviation of each variable, so that the metric
! can be weighted by the reciprocal of standard deviation.
! Special Thomas Nipen routine, angular standard devs computed differently.

  if (diag >= 6) print *, '  Compute standard deviations.'

  call stdevTNcirc (stddata, vmiss, fpar%is_circular, varstd_1d)   ! V <-- XV

! Replicate standard deviations into 2-D array.

  varstd = spread (varstd_1d, 1, ndays_prev)	! DV <-- V

!!  varstd = repmat (stdTNcirc (stddata, par.vmiss, par.var_set, &
!!              par.isCircular), par.fday-1, 1);		! (Matlab)

    if (diag >= 7) then
      print '(a,99i8)', '*** find_analog loop setup:'
      print '(a,99i8)', 'fday, fhour            = ', fday, fhour
      print '(a,99i8)', 'trendSize              = ', trendSize
      print '(a,99i8)', 'win_size               = ', win_size
      print '(a,99i8)', 'nanalogs               = ', nanalogs
      print '(a,99i8)', 'winLower, winUpper     = ', winLower, winUpper
      print *
    end if

  if (diag >= 6) print *, '  Allocate metric arrays.'

  allocate (metric(ndays_prev, winLower:winUpper))   ! alloc only extent needed
  metric = vmiss				     ! clear metric array

!--------------------------------------------
! Find analogs.
!--------------------------------------------

! Loop over hours in the SEARCH WINDOW.

hour_loop: &
  do w = winLower, winUpper		! w = center position of current analog

! Stencil indices for forecast and current analog.
! Note that stencils are already constrained properly above, for all cases.

    hour1 = fhour - trendSize		! hour indices for forecast stencil
    hour2 = fhour + trendSize

    w1 = w - trendSize			! hour indices for search stencil
    w2 = w + trendSize

    if (diag >= 8) then
      print '(a,99i8)', 'nhour                  = ', nhour
      print '(a,99i8)', 'trendSize              = ', trendSize
      print '(a,99i8)', 'win_size               = ', win_size
      print '(a,99i8)', 'nanalogs               = ', nanalogs
      print '(a,99i8)', 'w                      = ', w
      print '(a,99i8)', 'winLower, winUpper     = ', winLower, winUpper
      print '(a,99i8)', 'hour1, hour2           = ', hour1, hour2
      print '(a,99i8)', 'w1, w2                 = ', w1, w2
      print '(a,99i8)', 'fpar%useRealTrends     = ', fpar%useRealTrends
    end if

    if (diag >= 5) print '(99i5)', fday, fhour, w, w1, w2, hour1, hour2, &
      winLower, winUpper, win_size, nanalogs, &
      trendSize, size (local_trend), int (100 * local_trend(:))

!--------------------------------------------
! Compute metric (linear).
!--------------------------------------------

!! Loop over days.  (See original Matlab to recover.)
!!
!!    for d = 1:par.fday-1
!!       for v = 1:nvar
!!          % Find the indicies of the stencil where the prediction
!!          % is not missing.
!!          Ioffsets = find(data(par.fday, hour1:hour2, v) ~= par.vmiss);
!!          analogDifference = abs(data(par.fday, hour1 + (Ioffsets-1), v) &
!!            - data(d, (w - trendLower) + (Ioffsets-1), v));
!!          summed(v)        = sum(analogDifference, 2);
!!       end
!!       metric(d, w)        = sum(summed./varstd);

!--------------------------------------------
! Compute metric (quadratic).
!--------------------------------------------

! Find the hour indicies of the stencil where the PREDICTION (fvar) is
! not missing.  The prediction is on the *current* forecast day (fday).

    Ioffsets = pack ( (/ (i, i = 1, size (local_trend)) /), &
                      (data(fday, hour1:hour2, fvar) /= vmiss) )

    noffsets = size (Ioffsets)

!-------------------------------------------------------------
! We have prediction values for all trend values.
!-------------------------------------------------------------

have_all_values: &
    if (noffsets == size (local_trend)) then

!-------------------------------------------------------------------------
! Metric based on difference in prediction and difference in the trend.
!-------------------------------------------------------------------------

use_real: &
      if (fpar%useRealTrends == 1) then    ! 1 = use trend, 0 = use neighbors

! Add the middle error.

        analogDifference = (spread (data(fday, fhour:fhour, :), 1, ndays_prev) &
                            - data(1:prev_day, w:w, :) ) ** 2	! DHV

! Now add the trend error.

        ftrend = spread (data(fday, hour1:hour1, :) &
                 - data(fday, hour2:hour2, :), 1, ndays_prev)	! DHV

        atrend = data(1:prev_day, hour1:hour1, :) &
                 - data(1:prev_day, hour2:hour2, :)		! DHV

        analogDifference = analogDifference + (ftrend - atrend) ** 2   ! DHV

!-------------------------------------------------------------------------
! Metric based on difference in prediction and difference in
! neighbouring values.
!-------------------------------------------------------------------------

      else
        allocate (analogDifference(ndays_prev, noffsets, nvars))

        do v = 1, nvars

! Use cyclic statistics when using circular variables.

          if (fpar%is_circular(v)) then

            call wind_dir_error (spread (data(fday, hour1:hour2, v), 1, &
              ndays_prev), data(1:prev_day, w1:w2, v), angular_err)  ! DH <-- DH

            analogDifference(:,:,v) = ( angular_err &
              * spread (local_trend, 1, ndays_prev) ) ** 2	! DH <-- DH

! Use original for normal variables.

          else
            analogDifference(:,:,v) = &
              ( ( spread (data(fday, hour1:hour2, v), 1, ndays_prev) &
                  - data(1:prev_day, w1:w2, v) ) &
                * spread (local_trend, 1, ndays_prev) ) ** 2
          end if

        end do

      end if use_real

!-------------------------------------------------------------
! Here when some of the PREDICTION values needed for trend
! for the CURRENT stencil are missing.  Can not compute
! PREDICTION trend, so revert to simpler analog calculation.
!-------------------------------------------------------------

    else

      Ihours = hour1 + (Ioffsets(:) - 1)    ! hour indices for forecast stencil
      Whours = w1    + (Ioffsets(:) - 1)    ! hour indices for search stencil

      analogDifference = &
        ( spread (data(fday, Ihours(:), :), 1, ndays_prev) &
          - data(1:prev_day, Whours(:), :) ) ** 2	! raw diffs, DHV

    end if have_all_values

!--------------------------------------------
! Compute final metrics.
!--------------------------------------------

! Average over all valid hours in current stencil.

    summed = sqrt (sum (analogDifference, 2) / noffsets)   ! DV <-- DHV

! Normalize each variable by its standard deviation.

! Also handle zero standard deviation.  This happens when a
! predictor has all constant values within the current search
! window.  A particular case is solar radiation at night time,
! which of course is all zeros.

! All constant values within the window means this predictor can not
! make a meaningful contribution to the metric-based analog search.
! Therefore, simply zero all corresponding normalized values,
! and the metrics will be based only on the other predictors.

    if (diag >= 7) then
      print '(a,99i8)', '   Varstd zero     count = ', count (varstd == 0)
      print '(a,99i8)', '   Varstd negative count = ', count (varstd <  0)
    end if

    allocate (normalized(size(summed,1), size(summed,2)))

    where     (varstd(:,:) >  0)	! protect from divide by zero
      normalized = summed(:,:) / varstd(:,:)

    elsewhere (varstd(:,:) == 0)	! handle all constant predictor values
      normalized = 0

    elsewhere				! flag invalid metrics
      normalized = vmiss
    end where

! Weighted sum of contributions from all predictors, for each analog date.
! No metric on days with any missing.
! Metric was initially filled with missing values.

    if (diag >= 7) then
      print *, '  shape (normalized)           = ', shape (normalized)
      print *, '  shape (pred_weights)         = ', shape (pred_weights)
      print *, '  shape (metric(1:prev_day,w)) = ', shape (metric(1:prev_day,w))
    end if

    dim_num = 2					! sum over the var dimension

    where (all (normalized /= vmiss, dim_num)) &	! no sum if any missing
      metric(1:prev_day, w) = matmul (normalized(:,:), pred_weights(:))
      							! D <-- DV

! End of hour loop.  Clean up for varying stencil size.

    deallocate (analogDifference, normalized)

  end do hour_loop

! Handle invalid metrics.

  if (diag >= 6) then
    print *
    print *, '  *** Check for invalid metrics:'
    print *, '  fhour                 = ', fhour
    print *, '  Missing metrics count = ', count (metric(:,:) == vmiss)
    print *, '  Metric array size     = ', size  (metric)
    print *, '  shape (metric)        = ', shape (metric)
    print *
  end if

  if (any (metric(:,:) == vmiss)) then

    if (all (metric(:,:) == vmiss)) then
      metric(:,:) = 1			! default metric when all invalid
      					! is this too strong for invalid?
    else
      print *, '*** find_analog: Either no metrics or all metrics must be' &
        // ' invalid.'
      print '(a,i0,a,i0)', ' *** Forecast day, hour = ', fday, fhour
      call exit (1)
    end if

  end if

!--------------------------------------------
! Sort analogs in window, by the metric.
!--------------------------------------------

! Sort the data within the window, which includes only the
! appropriate hours and days.

! Bound metric to the lowest allowable.

  metric = max (metric, fpar%lowerMetric)

! Flatten the 2-D window array, then sort all analogs by metric.
! Analogs now become scrambled between the day and hour dimensions.

!!  [metrics(:), indices(:)] = sort(submetric(:), 1, 'descend');   ! (Matlab)

  metric_1d = (/ metric /)			! flatten 2-D to 1-D

! For valid comparison testing, use special sort routine to match
! Matlab behavior for ties.  Sort DESCENDING by values, but ties sort
! ASCENDING by their original position indices.

  allocate (indices(nanalogs))
  call index_sort_descending (metric_1d, indices)   ! get sorted indices

  metrics = metric_1d(indices(:))		! also return list of metrics
						! in same descending order

  if (diag >= 6) print *, '*** find_analog: Return.'

end subroutine find_analog
end module find__analog
