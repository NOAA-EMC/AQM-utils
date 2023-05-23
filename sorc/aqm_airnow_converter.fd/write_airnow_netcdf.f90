!------------------------------------------------------------------------------
!
! write_airnow_netcdf.f90 -- Write Netcdf files for convert_airnow_csv.f90.
!
! This is a support routine for convert_airnow_csv.f90, part of the
! NOAA/NCEP/PSL bias correction system for CMAQ forecast outputs.
!
! 2023-feb-05	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from write_interp_netcdf.f90 version 2022-may-23.
!
! This routine writes a single Netcdf file with the data from 24
! hourly files for one date in the AirNow Hourly AQ Obs data set.
!
! Input for this version is several general-purpose arrays
! containing multiple daily and hourly variables.  Output
! variables are controlled by the field decision table.
!
! This version overwrites existing files.  The intention is to
! use this routine to repeatedly refresh the Netcdf data set,
! thereby tracking source data updates.
!
! This version does not generate any unlimited dimensions,
! or any dimensions that are suitable for simple cross-file
! concatenation.
!
! Error handling:
!
! Currently all Netcdf errors in this writer are considered to
! be serious problems.  All errors result in diagnostic message,
! and program abort.  There are no soft error returns.
!
!------------------------------------------------------------------------------

module write__airnow_netcdf
contains

subroutine write_airnow_netcdf (outfile, year, month, day, fields4, floats4, &
      doubles5, floats5, vmiss, program_id, diag)

   use get__airnow_field_table
   use netwrite3
   implicit none

! Input arguments.

   character(*), intent(in) :: outfile		! name of output file
   integer,      intent(in) :: year, month, day	! current date
   character(*), intent(in) :: fields4 (:,:)	! daily string and float vars
   real,         intent(in) :: floats4 (:,:)	!   (sites, fields)
   real(dp),     intent(in) :: doubles5(:,:,:)	! hourly numeric vars
   real,         intent(in) :: floats5 (:,:,:)	!   (sites, fields, hours)
   real,         intent(in) :: vmiss		! missing value for output data
   character(*), intent(in) :: program_id	! writer ID for history att.
   integer,      intent(in) :: diag		! verbosity level, 0-N

! Local parameters.

   character(*), parameter :: nomiss_char = ' '	! special codes for netwrite3
   real,         parameter :: no_missing  = 0
   character(*), parameter :: no_long     = ' '

   character(*), parameter :: newline = char(10)

! Reserve extra Netcdf-3 header space.
! Tune for the maximum expected number of variables in one output file.
! Non-critical parameter.  If too small, file takes a little longer to write.

   integer, parameter :: reserve_header = 4000

! Local variables.

   character(len_table_item) varname, field_type, unit
   character(len_table_item) output_types(3)
   character history*200, title*100, doc*400
   character varexp*100, time_units*50

   integer fi, si, ti, ncid, len1
   integer nsites, nhours, nmissing

   real     xmiss_32
   real(dp) xmiss_dbl

   logical ex

   character(len=:), allocatable :: strings_out(:)  ! variable length substrings

   real, allocatable :: time_coords(:)

! Output from get_airnow_field_table.

   character(len_table_item), allocatable :: field_names(:), field_types(:)
   character(len_table_item), allocatable :: expected(:), units(:)

   integer i_site_id, i_lat, i_lon, i_date, i_time, nfields

   integer, allocatable :: len_out(:)
   logical, allocatable :: daily(:), hourly(:), consistency(:)
   logical, allocatable :: required(:), merge_type(:)

!-----------------------------------------------------------
! Initialize.
!-----------------------------------------------------------

   nsites = size (fields4, 1)		! get dimensions
   nhours = size (floats5, 3)

! Fetch the decision table.

   call get_airnow_field_table (field_names, field_types, daily, hourly, &
      required, consistency, merge_type, len_out, expected, units, &
      i_site_id, i_lat, i_lon, i_date, i_time, nfields)

! Create a new netcdf file, and write the initial header.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'write_airnow_netcdf: Create new Netcdf file.'
   if (diag >= 2) print '(2a)', ' Create file: ', trim (outfile)

   inquire (file=outfile, exist=ex)
   if (ex) print '(2a)', ' *** Overwriting previous file: ', trim (outfile)

   title   = 'AirNow Hourly AQ Obs Concentration Data'
   history = 'Created by ' // trim (program_id)

   call netcreate3 (outfile, title, history, reserve_header, ncid, &
      overwrite=.true., diag=diag)		! will add history time stamp

! Add documentation.

   doc =  'For variable names and descriptions, see AirNow documentation:' &
       // newline // 'https://docs.airnowapi.org/files' &
       // newline // 'https://docs.airnowapi.org/docs/HourlyAQObsFactSheet.pdf'

   call write_global_att_str ('comment', doc)

!-----------------------------------------------------------
! Output each active variable in the decision table.
!-----------------------------------------------------------

var_loop: &
   do fi = 1, nfields
      varname    = field_names(fi)
      field_type = field_types(fi)
      unit       = units(fi)

! Skip vars in table that will not be written.

      output_types = (/ 'char  ', 'double', 'float ' /)
      if (.not. any (field_type == output_types(:)) ) cycle var_loop

      if (diag >= 2) print '(3a)', '   Add variable: ', trim (varname)

! Determine dimensionality.

      varexp = ' '
      if (daily (fi)) varexp = trim (varname) // '(sites)'
      if (hourly(fi)) varexp = trim (varname) // '(sites, time)'

! Shrink the string length to fit the longest actual value.

      if (field_type == 'char') then
         len1 = maxval ((/ (len_trim (fields4(si,fi)), si = 1, nsites) /))
         if (diag >= 3) print '(a, a16, i0)', 'var, len = ', varname, len1
         strings_out = fields4(:,fi)(1:len1)	! realloc to minimum length
      end if

! Decide whether to include missing value attribute.

      nmissing = 0

      if ( (field_type == 'float')  .and. daily(fi)  ) &
         nmissing = count (floats4 (:,fi)   == vmiss)

      if ( (field_type == 'double') .and. hourly(fi) ) &
         nmissing = count (doubles5(:,fi,:) == vmiss)

      if ( (field_type == 'float')  .and. hourly(fi) ) &
         nmissing = count (floats5 (:,fi,:) == vmiss)

      if (nmissing > 0) then
         xmiss_32  = vmiss
         xmiss_dbl = vmiss
      else
         xmiss_32  = no_missing
         xmiss_dbl = no_missing
      end if

! Write current variable into the netcdf file (if active).

      if ( (field_type == 'char'  ) .and. daily(fi)  ) &
         call writevar (varexp, no_long, unit, strings_out,      nomiss_char)

      if ( (field_type == 'float' ) .and. daily(fi)  ) &
         call writevar (varexp, no_long, unit, floats4 (:,fi),   xmiss_32)

      if ( (field_type == 'double') .and. hourly(fi) ) &
         call writevar (varexp, no_long, unit, doubles5(:,fi,:), xmiss_dbl)

      if ( (field_type == 'float' ) .and. hourly(fi) ) &
         call writevar (varexp, no_long, unit, floats5 (:,fi,:), xmiss_32)

   end do var_loop

! Add the time coordinate variable.

   if (diag >= 2) print '(3a)', '   Add coordinate: time'

   time_coords = (/ (ti, ti = 0, nhours-1) /)	! real <-- integer

   write (time_units, "(a,i4,2('-',i0),a)") 'hours since ', year, month, day, &
      ' 0:0:0'

   call writevar ('time(time)', no_long, time_units, time_coords, no_missing)
						! output as type real
! Close output file.

   if (diag >= 4) print *, '  Output file complete.  Close file.'

   call netwrite3_close			! write final atts and flush buffers

   if (diag >= 4) print *, 'write_airnow_netcdf: Return.'

end subroutine write_airnow_netcdf
end module write__airnow_netcdf
