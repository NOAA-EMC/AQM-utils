!------------------------------------------------------------------------------
!
! netcreate3 -- Open a new netcdf file for writing, and prepare the header.
!
! 3.12	2010-may-05	Include Netcdf API version string in history attribute.
! 3.13	2010-jul-02	Switch to no-clobber mode, do not overwrite an
!			  existing file.
! 3.14	2010-aug-05	Add optional argument to override write protect.
! 3.18	2014-may-12	Suppress extra newline if user history is blank.
!			Add optional diag argument, for verbosity control.
! 3.19	2015-feb-18	Bug fix for v3.18.
!			Switch order of optional arguments, for compatibility
!			  with previous calls to this routine, e.g. netwrite4.
!
! Usage:
!
! 1.  Call netcreate3 to begin a new netcdf file.  Use the
!     reserve_header parameter to reserve sufficient header space
!     for variable definitions and attribute storage.
!
! 2.  Call writevar routines to write array variables in single calls.
!
! 3.  Call netwrite3_close to flush buffers and close file properly.
!
! Optional:
!
! 1.  Call write_var_att routines to create and update variable
!     attributes, any time after the associated variable is defined.
!
! 2.  Call write_global_att routines to create and update global
!     attributes, any time after the initial call to netcreate3.
!
! 3.  Call var_create_* routines to pre-allocate large arrays,
!     without writing data.  Data writes to these arrays are
!     deferred until later.  Use direct nf90_put_var calls to write
!     the actual data later.
!
! 4.  The user may also use direct Netcdf calls to add a record
!     dimension and write record variables.  To avoid performance
!     degradation, all writes to record variables should be done
!     only after all writevar calls for fixed variables are
!     completed.
!
! Notes:
!
! Fixed variables and attributes may be added freely at any time,
! without performance penalty, as long as enough header space was
! initially reserved with netcreate3.
!
! If insufficient space was reserved, then Netcdf will start
! rewriting the whole file for every new variable.  If existing
! variables are large, then the program will slow down dramatically.
!
! If all of the output arrays are relatively small, then none of
! the delays will be noticeable, and header allocation will be
! unimportant.
!
!------------------------------------------------------------------------------

subroutine netcreate3 (filename, title, history, reserve_header, ncid, &
      overwrite, diag)

   use netwrite3_version
   use netcdf

   implicit none

   character(*), intent (in ) :: filename	! name of new file to create
   character(*), intent (in ) :: title		! title string (global att);
   						! blank = do not write title
   character(*), intent (in ) :: history	! caller's history string; use
   						! newline chars between lines
   integer,      intent (in ) :: reserve_header	! extra header to reserve: bytes
   integer,      intent (out) :: ncid		! netcdf file ID number, for
   						! direct user calls to Netcdf
   logical, intent (in), optional :: overwrite	! T = overwrite allowed
   integer, intent (in), optional :: diag	! verbosity control, 0-N

! Local variables.

   character curr_date*24			! return string from fdate
   character*80 history1, history2
   integer status, create_mode, diag1

! Handle optional argument.

   if (present (diag)) then
      diag1 = diag
   else
      diag1 = 3					! default to medium verbosity
   end if

! Display version info.

   if (diag1 >= 4) print *, 'netcreate3: Start.'
   if (diag1 >= 3) print *, 'Writer version = ', version
   if (diag1 >= 3) print *, 'Netcdf version = ', trim (nf90_inq_libvers())

! Create initial netcdf file.

   create_mode = nf90_noclobber			! default: prevent overwrite

   if (present (overwrite)) then		! permit overwrite if selected
      if (overwrite .eqv. .true.) create_mode = nf90_clobber
   end if

   status = nf90_create (path=trim (filename), cmode=create_mode, ncid=fid)

   if (status == nf90_eexist) then
      print *
      print *, '*** netcreate3: Abort: Overwrite protect, previous file exists.'
      print *, '*** File = ', trim (filename)
      call exit (1)
   end if

   call check (status)				! handle other possible errors

   save_reserve_header = reserve_header		! must defer header allocation
   						! until first var definition;
   						! will not work on first ENDDEF
! Global attributes.

   if (title /= ' ') then
      call check (nf90_put_att (fid, nf90_global, 'title', trim (title) ))
   end if

   call fdate (curr_date)		! fetch system date and time string
   history1 = trim (curr_date) // ': Make original NetCDF file.' // newline
   history2 = 'NetCDF writer subroutine: ' // version // newline

! Suppress final newline if user's history attribute is blank.

   if (history == ' ') then
      call check (nf90_put_att (fid, nf90_global, 'history', &
         trim (history1) // trim (history2) // 'NetCDF library: ' &
         // trim (nf90_inq_libvers()) ))

! Add newline if user's history is included.

   else
      call check (nf90_put_att (fid, nf90_global, 'history', &
         trim (history1) // trim (history2) // 'NetCDF library: ' &
         // trim (nf90_inq_libvers()) // newline // trim (history) ))
   end if

! User may now proceed with adding variables and attributes

   ncid = fid					! return file ID back to user

end subroutine netcreate3
