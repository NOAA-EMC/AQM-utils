!------------------------------------------------------------------------------
!
! netwrite3.utils.f90 -- General support routines for netwrite3 module.
!
! 3.09	2009-feb-17	netwrite3.last.inc:  Split off from netwrite3.f90.
!			Module is assembled with makefile method.
!
! 3.10	2010-feb-26	netwrite3.utils.f90:  Renamed.
!			Switch to include method via -I option.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!
! netwrite3_sync -- Flush metadata to netcdf file, for simultaneous reading.
!
! Call as often as desired, as an aid to simultaneous diagnostic
! reading while the file is being created.
!
! For internal use: This routine also ensures data mode and header
! space allocation, via ensure_data_mode.
!
!------------------------------------------------------------------------------

subroutine netwrite3_sync

   use netcdf
   implicit none
   
   call ensure_data_mode			! must switch to data mode first
   call check (nf90_sync (fid))			! now sync metadata to file
   
end subroutine netwrite3_sync


!------------------------------------------------------------------------------
!
! netwrite3_close -- Close file properly.
!
!------------------------------------------------------------------------------

subroutine netwrite3_close

   use netcdf
   implicit none

   call check (nf90_close (fid))		! close netcdf file
   
end subroutine netwrite3_close


!------------------------------------------------------------------------------
!
! ensure_define_mode -- Switch Netcdf file from unknown to define mode.
!
! INTERNAL USE ONLY
!
!------------------------------------------------------------------------------

subroutine ensure_define_mode

   use netcdf
   implicit none
   
   integer status
   
   status = nf90_redef (fid)		! attempt to switch to define mode
   
   if (status /= nf90_noerr .and. status /= nf90_eindefine) then
      call check (status)		! abort if unexpected error
   end if
   
end subroutine ensure_define_mode


!------------------------------------------------------------------------------
!
! ensure_data_mode -- Switch Netcdf file from unknown to data mode.
!
! As a side effect, any pending header allocation request is handled.
!
! INTERNAL USE ONLY
!
!------------------------------------------------------------------------------

subroutine ensure_data_mode

   use netcdf
   implicit none
   
   integer status

! Switch to data mode.  Must perform extra header allocation, if pending.
   
   status = nf90_enddef (fid, h_minfree = save_reserve_header)
   				! header allocation is simultaneous with enddef
   
   if (status == nf90_noerr) then	! if header allocation was successful:
      save_reserve_header = 0		! indicate that allocation was completed
   
   else if (status /= nf90_enotindefine) then	! ignore if already in data mode
      call check (status)		! abort if unexpected error
   end if
   
end subroutine ensure_data_mode


!-----------------------------------------------------------------------------
!
! check -- Check Netcdf return status.  Print message and abort on error.
!
! INTERNAL USE ONLY
!
!-----------------------------------------------------------------------------

subroutine check (status)

   use netcdf
   implicit none
   integer, intent (in) :: status		! Netcdf result code

   if (status /= nf90_noerr) then
      write (*, '(a,i0)') ' *** netwrite3: Netcdf error number = ', status
      print *, '*** netwrite3: ', trim (nf90_strerror (status))
      call exit (1)
   end if

end subroutine check


!-----------------------------------------------------------------------------
!
! utcheck -- Check udunits return status.  Print message and abort on error.
!
! INTERNAL USE ONLY
!
!-----------------------------------------------------------------------------

subroutine utcheck (status)

   use netcdf
   implicit none
   integer, intent (in) :: status		! UDUnits result code

   if (status /= 0) then 
      write (*, '(a,i0)') '*** netwrite3: Fatal, udunits error = ', status
      call exit (1)
   end if

end subroutine utcheck
