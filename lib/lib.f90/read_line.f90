!------------------------------------------------------------------------------
!
! read_line -- Read next non-comment line from a text file.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2000-feb-09	Initial version
! 1.01	2000-feb-14	Comment fixup
! 1.02	2000-feb-15	Add line_num argument
! 1.03	2000-dec-29	Fortran 90 compatibility
!			Fix standardization bug (end=)
! 1.04	2001-jan-09	Reject blank lines that contain a return character
!			  or a control-Z character
! 1.05	2001-jan-18	Add line_len return parameter
! 2.00	2001-aug-28	Use stdlit.h instead of globals.h; more F90 conversion
!			Change to iostat handling; explicit abort on i/o error
! 2.01	2001-aug-31	Use stdlit module instead of include
! 2.02	2002-jul-26	F90 free format
! 2.03	2002-nov-03	Change eof detection for platform compatibility.
!
! Comment lines are either of the following:
!    - Any line with asterisk (*) in column 1.
!    - Any line that is all blanks.
!    - Any line that is all blanks, with one carriage return or control-Z char.
!
! input:	filenum = file number (unit identifier) of an open file
!		line_num = line number of previous line read from this file,
!		    or 0 when file was just opened
!
! output:	line = text line read from file
!		line_len = actual number of chars in line, not incl. eol char.
!		    (trailing spaces included up to eol character)
!		    (returns dimension of "line" for non-terminated lines)
!		status = normal or eof status code (see stdlit)
!		line_num = updated to give true line number of current line
!
! notes		This version discards any detected end-of-line
!		character by replacing it with a space.
!		This relieves the calling program from any further
!		need for eol character handling.
!
!------------------------------------------------------------------------------

subroutine read_line (filenum, line, line_len, status, line_num)

   use stdlit
      
   implicit none
   character line*(*)
   integer filenum, line_len, status, line_num
   
   integer ios, j
   
! Scan for first line not starting with an asterisk, and not blank.
   
   read_loop: do

      line_num = line_num + 1
      read (filenum, '(a)', iostat=ios) line

      if (ios /= 0) then			! check for eof
         status = eof
         return

      else if (line(1:1) == '*') then		! reject normal comment line
         cycle read_loop
      
      else
         j = len_trim (line)		! point to last non-space in line

         ! Discard end-of-line character, if present.
         
         if (j > 0) then
            if (line(j:j) == cr .or. line(j:j) == ctrl_z) then
               line(j:j) = ' '			! trim off the eol character
               line_len = j - 1			! remember terminated line len
               j = len_trim (line(1:j))		! recompute last non-space
            else
               line_len = len (line)		! for unix lines, length = dim
            end if
         end if
      
         if (j > 0) exit read_loop		! accept the line if non-blank
      end if					! else j=0, reject blank line
      
   end do read_loop
   
   status = normal
   return				! normal exit with valid data line

end subroutine read_line
