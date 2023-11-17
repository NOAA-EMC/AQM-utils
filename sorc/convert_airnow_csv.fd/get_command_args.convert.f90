!------------------------------------------------------------------------------
!
! Get command line arguments for convert_airnow_netcdf.f90.
!
! 2023-jan-24	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!		Adapted from get_command_args.interp.f90 version 2022-apr-11.
!
!------------------------------------------------------------------------------
!
! Usage:
!
! ./convert_airnow_csv INPATH OUTPATH YYYYMMDD1 YYYYMMDD2 diag=N
!
! INPATH and OUTPATH are strings just returned to the caller.
!
! YYYYMMDD1 and YYYYMMDD2 are 8-digit date strings.
!
! Optional diag=N specifies the verbosity level.
! The default is diag=2.
!
! In addition to the main command arguments, base_year is returned
! to the calling program.  This is to be used as the reference year
! for all date indices within the current run.  This is a system
! for simple linear date indexing on the real-world calendar.
!
! Base_year is arbitrarily chosen to be the year that includes the
! specified start date.  This ensures that all date indices within
! the current run will be small positive integers.
!
!------------------------------------------------------------------------------

module get__command_args
contains

subroutine get_command_args (prog_name, calendar, inpath, outpath, &
      start_date, end_date, base_year, diag)

   use date_utils
   use string_utils
   implicit none

   character(*), intent(in)    :: prog_name	! name of calling program
   character(*), intent(in)    :: calendar

   character(*), intent(out)   :: inpath, outpath
   integer,      intent(out)   :: start_date, end_date
   integer,      intent(out)   :: base_year
   integer,      intent(inout) :: diag		! set diag to desired default
						! value, before calling

! Local variables.

   character(20) start_date_str, end_date_str
   character(40) diag_str

   integer ios, ndays, base_year2
   logical valid

   real, parameter :: max_days = 10 * 365.25	! max allowed date interval

! Get command arguments as strings.

   call get_command_argument (1, inpath)
   call get_command_argument (2, outpath)
   call get_command_argument (3, start_date_str)
   call get_command_argument (4, end_date_str)
   call get_command_argument (5, diag_str)

! Echo arguments for log file.

   if (diag >= 1) then
      print *
      print '(2a)', 'Command line arguments:'
      print '(2a)', '  Input path  = ', trim (inpath)
      print '(2a)', '  Output path = ', trim (outpath)
      print '(2a)', '  Start date  = ', trim (start_date_str)
      print '(2a)', '  End date    = ', trim (end_date_str)
      print '(2a)', '  Diag level  = ', trim (diag_str)
   end if

! Check number of arguments.

   if (end_date_str == ' ') then
      print *, '*** get_command_args: Abort, missing command arguments.'
      print *, '*** Four arguments are needed, the fifth is optional.'
      print *, '*** Usage: ./' // trim (prog_name) &
        // ' inpath outpath start-date end-date [diag=N]'
      call exit (1)
   end if

! Set the time line reference year to the earliest specified year.

   read (start_date_str(1:4), *, iostat=ios) base_year
   if (ios /= 0) base_year = 1800	! ignore errors, will get caught next

   read (end_date_str(1:4), *, iostat=ios) base_year2
   if (ios /= 0) base_year2 = 1800

   base_year = min (base_year, base_year2)

! Validate and convert date arguments to integer date indices
! (days since start of base_year, 1-based.  1 = January 1).

   start_date = yyyymmdd_to_index (start_date_str, base_year, calendar)
   end_date   = yyyymmdd_to_index (end_date_str,   base_year, calendar)

   if (start_date > end_date) then
      print *, '*** get_command_args: Abort, date error on command line.'
      print *, '*** Start date must not be later than end date.'
      call exit (1)
   end if

! Check for unreasonable interval.  Really just checking for gross
! date error.

   ndays = end_date - start_date + 1

   if (ndays > max_days) then
      print *, '*** get_command_args: Abort, date error on command line.'
      print *, '*** Excessive interval or length of training period.'
      call exit (1)
   end if

! Process the optional "diag" keyword argument.
! If missing, leave the original "diag" value unchanged.

   if (diag_str /= ' ') then			! if blank, do not process

      call lowercase (diag_str)			! case insensitive
      valid = (diag_str(1:5) == 'diag=')	! check for proper keyword

      if (valid) call string_to_int (trim (diag_str(6:)), diag, valid)
				! suffix to integer, and check for valid number
      if (.not. valid) then
         print *, '*** get_command_args: Abort, invalid diag argument.'
         print *, '*** Must be "diag=N", where N is a signed integer.'
         call exit (1)
      end if

   end if

end subroutine get_command_args
end module get__command_args
