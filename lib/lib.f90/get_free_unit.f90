!-----------------------------------------------------------------------------
!
! get_free_unit -- Get a free i/o unit number.
!
! by Dave Allured
!
! 1.00	2002-jul-22	Initial version.
!
! Input:	none
! Output:	function value = i/o unit number
!
! Caution:	This routine only finds a free unit number.  It does not
!		allocate the unit number.  The caller must reserve the unit
!		number immediately, by opening a file, before calling this
!		routine for another unit number.
!
! Tip:		To keep a unit number reserved in a complex program,
!		don't close the previous file used, or open a dummy file
!		on the same unit number.
!
!-----------------------------------------------------------------------------

integer function get_free_unit ()

   implicit none
   
   integer unitno, first, last
   logical open_flag
   
   first = 100				! define allocation range for unit #'s
   last = 99999				! stay away from reserved and
   					! carelessly used low end
   unitno = first
   
   do unitno = first, last
      inquire (unit = unitno, opened = open_flag)
      if (.not. open_flag) then
         get_free_unit = unitno
         return
      end if
   end do

   print *, '*** ABORT: Cannot find a free file number, ', &
      first, ' through ', last
   stop 1

end function get_free_unit
