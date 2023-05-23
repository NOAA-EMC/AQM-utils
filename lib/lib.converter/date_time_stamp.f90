!----------------------------------------------------------------------------
!
! date_time_stamp -- Generate a character string with current date and time.
!
! This routine returns a string containing only characters that are
! legal for use within filenames.
!
! Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-08	Initial version
! 1.01	2000-feb-14	All lowercase in name of month
! 1.02	2000-sep-28	Fix bug in lowercase handler (benign)
!
! 2.00	2004-may-10	Convert function to subroutine.
!			F77/F90 compatibility.
!
! usage:	call date_time_stamp (out_str)
!
! input:	None.  Date is fetched from system.
!
! output:	out_str = output string
!		The string is formatted as follows:
!		   yyyy-mmmdd-hhmmss
!		See fdate comments below for field details.
!		out_str must be at least 17 characters long.
!		Otherwise the output will be truncated.
!
!----------------------------------------------------------------------------

      subroutine date_time_stamp (out_str)

      implicit none
      character out_str*(*)
	
      character str*24, blank, m1
      character mmm*3, dd*2, yyyy*4, hh*2, mm*2, ss*2
   
      blank = ' '
      call fdate (str)			! fetch system date and time string

! Format of 24-character string from system call fdate:
!
! "www mmm dd hh:mm:ss yyyy"    where all blanks are present as shown!
!  123456789012345678901234
!
! www = alpha weekday
! mmm = alpha month
! dd = day of the month
! hh = hour of day, local time, in 24-hour format (00-23)
! mm  = minutes
! ss = seconds
! yyyy = 4-digit year
   
      if (str(9:9).eq.blank) then	! force leading zero format into dd
         str(9:9) = '0'
      end if

      mmm = str(5:7)
      dd = str(9:10)
      hh = str(12:13)
      mm = str(15:16)
      ss = str(18:19)
      yyyy = str(21:24)
   
      m1 = mmm(1:1)		! force first letter of month to lowercase
      if (m1.ge.'A' .and. m1.le.'Z') then
         mmm(1:1) = char( ichar(m1) + ichar('a') - ichar('A') )
      endif

      out_str = yyyy // '-' // mmm // dd // '-' // hh // mm // ss
      return

      end subroutine date_time_stamp
