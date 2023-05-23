!-----------------------------------------------------------------------------
!
! netwrite3.time -- Time dimension routines for netwrite3.f90.
!
! 3.08	2009-feb-16	Original time dimension include file for netwrite3.
!			Adapted from netwrite2.f90 v2.43.
!			Both fixed and unlimited time dimensions are supported.
! 3.10	2010-feb-26	Access udunits via new F90 module, rather than includes.
! 3.12	2010-may-05	Prevent multiple utopens with utisinit_f, stop warnings.
! 3.15	2011-oct-04	Two bug fixes in time_step.
!			Change one hard coded reference to 'time' var name,
!			  fix support for altername names for time var.
!			Add call to netwrite3_sync before writing time data.
!			Was previously hidden by preceeding writevar calls.
! 3.17	2013-jul-18	Switch from custom utisinit_f to standard utisopen func.
!
! Contents of this include file:
!
!    add_time_dimension	-- Add time dimension to Netcdf file.
!    time_step          -- Write Netcdf time coordinates for each time step.
!
! Restrictions in current version:
!
!    Only one time dimension per file is currently supported, because
!    of state information.
!
!    Time axis:  Gregorian calendar only.
!
!    The current version does not check for missing or non-monotonic
!    time coordinates.  It is the caller's responsibility to ensure
!    correct time coordinates.
!
!-----------------------------------------------------------------------------


!------------------------------------------------------------------------------
!
! add_time_dimension -- Add time dimension to Netcdf file.
!
! Usage:
!
! Call this routine after netcreate3, to add a time dimension and
! standard time coordinate variable to the netcdf file.  Then call
! write_time_coord for each time step, to record the correct date
! and time.
!
! These routines should be called only if a standard calendar time
! dimension is needed for any data variables to be included in the
! Netcdf file.
!
! Notes:
!
! Both fixed and unlimited time dimensions are supported.  Either is
! selected by the value of the ntimes input argument.  See below.
!
!------------------------------------------------------------------------------

subroutine add_time_dimension (varname, ntimes, units, calendar, varid)

   use udunits
   implicit none

   character(*), intent (in ) :: varname	! name of new time dimension
   integer,      intent (in ) :: ntimes		! length of time dimension, or
   						!   NF90_UNLIMITED = unlimited
   character(*), intent (in ) :: units		! time units string:  e.g.
   						!   'days since 1800-1-1 0:0:0'
   character(*), intent (in ) :: calendar	! calendar attribute; must be
   						!   'gregorian' in this version.
   integer,      intent (out) :: varid		! returned netcdf variable id

! Local variables.

   character(max_name) varexp
   integer dim_size(1)

   double precision, parameter :: no_missing = 0.  ! suppress missing_value att.

! Consistency check.

   if (calendar /= 'gregorian') then
      print *, '*** netwrite3: Unsupported calendar type: ', trim (calendar)
      print *, '*** netwrite3: Only gregorian calendar is supported.'
      call exit (1)
   end if

! Init udunits package and time state information.

   if (.not. utisopen () ) then			! if not already opened...
      call utcheck (utopen (' '))		! init udunits package
   end if

   time_unit_ptr = utmake ()			! create object for time base
						! unit, and save pointer

   call utcheck (utdec (units, time_unit_ptr))	! convert time base unit string
   						! to udunits time object

   time_actual_range(1) =  huge (time_actual_range)   ! init min, max time
   time_actual_range(2) = -huge (time_actual_range)   ! value accumulators

! Add the time dimension and coordinate variable.

   varexp = trim (varname) // '(' // trim (varname) // ')'
   dim_size(1) = ntimes			! no. of time steps, or nf90_unlimited

   call var_create_dbl (varexp, dim_size, 'Time', units, no_missing, varid)
   					! define dim. and coord var. together
                                        ! time var ID is retuned to caller

   call write_var_att_str (varname, 'calendar', calendar)	! add attribute

end subroutine add_time_dimension


!----------------------------------------------------------------------------
!
! time_step -- Write Netcdf time coordinates for each time step.
!
! Usage:
!
! Call this routine for each time step, in ascending order by time,
! starting at 1, to compute and record the correct time coordinate
! value.
!
! The caller is responsible for maintaining and incrementing the time
! index value.  This is also the Netcdf record number when the time
! dimension is unlimited.
!
! It is also the caller's responsibility to write the actual_range
! attribute, if desired.  This routine updates the public module
! variable time_actual_range(2).  After all time coordinates have
! been written, this variable may be accessed for the min/max values
! of the time coordinates.
!
! Notes:
!
! Though unusual, random record order for time writes is supported.
! In any case, it is the caller's responsibility to ensure that the
! final time coordinates are complete and monotonically ascending
! with each time step.  No checking is currently done.
!
! Irregular time spacing is supported, if needed.  This routine does
! not currently check the spacing of time coordinates between time
! steps.
!
!------------------------------------------------------------------------------

subroutine time_step (varid, time_index, year, month, day, hour, minute, second)

   use netcdf
   use udunits
   implicit none

   integer, intent (in) :: varid		! Netcdf id for time variable
   integer, intent (in) :: time_index		! Netcdf time step index, 1-N
   integer, intent (in) :: year			! calendar date and time:
   integer, intent (in) :: month		! use Gregorian dates;
   integer, intent (in) :: day			! set unused month or day to 1;
   integer, intent (in) :: hour			! set unused time vals to zero.
   integer, intent (in) :: minute
   real,    intent (in) :: second

   double precision time_coord			! local variable

! Convert specified date and time to UDUnits double precision time value.

   call utcheck (uticaltime (year, month, day, hour, minute, second, &
      time_unit_ptr, time_coord) )

! Write time coordinate value to Netcdf file at the specified time index.

   call netwrite3_sync				! switch to data mode, as needed
   call check (nf90_put_var (fid, varid, time_coord, start=(/ time_index /)))

! Accumulate min, max time coordinates for use at end of file.

   time_actual_range(1) = min (time_coord, time_actual_range(1))
   time_actual_range(2) = max (time_coord, time_actual_range(2))

end subroutine time_step
