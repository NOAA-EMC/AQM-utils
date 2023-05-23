!----------------------------------------------------------------------------
!
! message_nolog -- Display subroutine messages, plain non-logfile version.
!
! This is a stub version of message.f90.
!
! 1.00	2001-sep-21	Included in /home/dallured/rainsoft/bcorrect.f v1.09.
! 2.00	2005-aug-02	Add to library.  Convert to f90.
!			Remove accidental leading space in output line.
!
!----------------------------------------------------------------------------

subroutine message (string)

   implicit none
   character, intent (in ) :: string*(*)

   write (*, '(a)') trim (string)
   return

end subroutine message
