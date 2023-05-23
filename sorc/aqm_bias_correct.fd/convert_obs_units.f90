!------------------------------------------------------------------------------
!
! convert_obs_units.f90 -- Convert obs input data units to forecast units.
!
! This is a support routine for the NOAA/NCEP/PSL bias correction
! system for CMAQ forecast outputs.
!
! 2023-apr-04	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Split off from read_obs_qc.f90 version 2023 April 4.
!		Add var names and data units for new AirNow CSV/Netcdf data.
!
! The obs input data unit is automatically converted to a
! standard unit which is expected by both the QC subroutine and
! the remainder of the bias correction process.
!
! The output units string is the data units after automatic units
! conversion, NOT the units in the original data set.  The
! returned units string is correct for the returned data array.
!
!------------------------------------------------------------------------------

module convert__obs_units
contains

subroutine convert_obs_units (obs_varname, orig_units, vmiss, diag, in_data, &
      out_units)

   use config, only : dp
   use string_utils
   implicit none

   character(*), parameter :: &
      module_id = 'convert_obs_units.f90 version 2023-apr-04'

! Input arguments.

   character(*), intent(in)    :: obs_varname	! obs source var name
   character(*), intent(in)    :: orig_units	! obs source data units
   real(dp),     intent(in)    :: vmiss		! missing value in obs data
   integer,      intent(in)    :: diag		! diag verbosity level, 0-N

! Output arguments.

   real(dp),     intent(inout) :: in_data(:,:)	! obs input data, overwritten
   character(*), intent(out)   :: out_units	! data units after conversion

! Local variables.

   character(80) orig_units2, units_needed

   integer nmiss

   real(dp) vmin, vmax, percent_miss
   real(dp) multiplier

! Initialize.

   if (diag >= 2) print *
   if (diag >= 2) print *, 'convert_obs_units:  Check for units conversion.'
   if (diag >= 2) print *, '  Module ID = ' // module_id
   if (diag >= 2) print *

   orig_units2 = orig_units			! setup for case insensitive
   call lowercase (orig_units2)

! Adaptive method.  First determine the needed units, if any.
! This is a cheap table lookup.  Could be made into formal table, if needed.

   units_needed = 'any'

   if (obs_varname == 'COPO'  ) units_needed = 'ppmv'
   if (obs_varname == 'OZONE' ) units_needed = 'ppmv'

   if (obs_varname == 'COPOPM') units_needed = 'microgram/m^3'
   if (obs_varname == 'PM25'  ) units_needed = 'microgram/m^3'

   if (diag >= 2) then
      print *, '  Obs var name               = ', trim (obs_varname)
      print *, '  Input data units           = ', trim (orig_units)
      print *, '  Units needed               = ', trim (units_needed)
      print *
   end if

! Then check for available units conversions.
! This is another cheap table.  Could also be made into a formal table.

   multiplier = -77777			! default to "no conversion" magic num.

   if (units_needed == 'microgram/m^3') then
      if (orig_units2 == 'ug/m3') then
         multiplier = 1				! UG/M3 same as micrograms/m^3
      else if (any (orig_units2 == (/ 'kg/(m**3)', 'kg m-3   ' /) )) then
         multiplier = 1.0e9_dp			! kilograms to micrograms
      end if

   else if (units_needed == 'ppmv') then
      if (orig_units2 == 'ppb') then
         multiplier = 1.0e-3_dp			! PPB to ppmv
      else if (orig_units2 == 'mole/mole') then
         multiplier = 1.0e6_dp			! MOLE/MOLE to ppmv
      end if
   end if

! Diagnostics for no conversion.

   if (multiplier == -77777) then		! if no conversion ...
      out_units = orig_units			! keep original units

      if (units_needed == 'any') then
         print *, '  No units conversion is needed.  Keep original units.'
      else
         print *, '*** convert_obs_units: Warning: Needed units conversion' &
            // ' is not available.'
         print *, '*** Keeping original units: ', trim (out_units)
         print *, '*** Results may not be accurate.'
         print *, '*** Check input data, or upgrade this program.'
      end if

! If valid, perform the units conversion now.

! Ignore any possible missing value range conflict, for now.
! Currently the numbers for PM2.5 seem favorable.  Maybe check later.

   else
      out_units = units_needed			! set the output units

      if (multiplier == 1) then
         print '(9a)','   Units are equivalent.  No units conversion is needed.'
         print '(9a)','      Output units = ', trim (out_units)
      else
         print '(9a)', ' convert_obs_units:  Apply units conversion, ', &
                       trim (orig_units), ' to ', trim (out_units)
         if (diag >= 2) &
            print '(a,es11.4)', '   Multiplier                 =', multiplier
         where (in_data /= vmiss) in_data = in_data(:,:) * multiplier
      end if
   end if

! Show unit converted statistics.

   if ( (diag >= 2) .and. (multiplier /= -77777) .and. (multiplier /= 1) ) then
      vmin  = minval ( in_data, (in_data /= vmiss) )
      vmax  = maxval ( in_data, (in_data /= vmiss) )

      nmiss = count (in_data == vmiss)
      percent_miss = (nmiss * 100.0_dp) / size (in_data)

      print '(2(a,g0.4),2a)', '   Min, max converted data    = ', vmin, &
            ', ', vmax, ' ', trim (out_units)
      print '(a,i0,a,f0.1,a)','   Number of missing values   = ', nmiss, &
            ' (', percent_miss, '%)'
   end if

   if (diag>=3) print *, 'convert_obs_units:  Return.'

end subroutine convert_obs_units
end module convert__obs_units
