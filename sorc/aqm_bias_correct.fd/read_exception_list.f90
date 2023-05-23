!------------------------------------------------------------------------------
!
! read_exception_list.f90 -- Read list of bias thresholds for exception sites.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias correction
! program for CMAQ forecast outputs.
!
! 2015-oct-27	Original version.  By Dave Allured.
!		Adapted from read_station_file.f90 version 2014-apr-24.
! 2015-oct-29	Add another column for separate positive and negative
!		  thresholds.
!		Support missing input file, return nsites = 0.
!
! 2016-jan-20	Restructure to one-pass input method.
!		Add array creation and alignment with primary site indexing.
!		Improve diagnostics.
! 2016-feb-09	Require "none" for no exception file.
!
! 2023-apr-09	Minor update for 12-character site ID's.
!
! Input file format:
!
! Site list with coordinates and threshold values.
! Plain text, five columns, free format.
!
! The first part is a comment header, variable length, ending
! with one line of dashes.
!
! The second part is five columns: Site ID, latitude, longitude,
! negative bias threshold value, and positive bias threshold
! value.  Free format, items separated by one or more spaces.
! Threshold values may be integers or real numbers.
!
! The coordinates in columns 2 and 3 will be ignored.  They
! are included only for reference and for consistency with the
! primary site list.  The calling program must associate by
! site ID's only.
!
! CAUTION:  Coordinates from the list file are only printed to
! the console log.  They are not checked against coordinates in
! primary input data.  If list file coordinates are different
! than those in the primary data files, there will be no warning
! of a mismatch.  This may be potentially misleading.
!
! Note:  This version assumes there are no duplicate site ID's in
! either the primary input data set, or the site exception list.
! However, duplicate site ID's in the exception file will simply
! overwrite any previous threshold values.
!
! Output is bias threshold arrays aligned with current input site
! arrays, accessed by primary site indexing.  These output arrays
! will be auto-allocated.
!
! Missing threshold values indicate no thresholds set for the
! corresponding sites.
!
!------------------------------------------------------------------------------
!
! Error handling:
!
! The current version halts the entire run on any read error in
! the site exception file, including these common errors:
!
! * Specified file not found.
! * Specified file is not a site exception file.
! * Missing header separator line "----".
! * Invalid numbers in expected columns.
!
! If no site exception list is desired, the file name must be
! specified as "none".
!
!------------------------------------------------------------------------------

module read__exception_list
contains

subroutine read_exception_list (filename, site_ids, vmiss, diag, thresh_low, &
      thresh_high)

   use config, only : dp
   implicit none

   character(*), intent(in)           :: filename	  ! input file name
   character(*), intent(in)           :: site_ids(:)	  ! primary site ID's
   real(dp),     intent(in)           :: vmiss		  ! missing value code
   integer,      intent(in)           :: diag		  ! verbosity,
   							  ! 0 = errors only

   real(dp), intent(out), allocatable :: thresh_low(:)    ! negative & positive
   real(dp), intent(out), allocatable :: thresh_high(:)   ! threshold values

   integer get_free_unit			! function def.

! Local variables.

   character header*4, errmes*200, line*100, match_str*3
   character(len(site_ids)) excep_id

   integer infile, ios, si, lnum
   integer nsites_primary, isite_list, nsites_matched

   real(dp) lat, lon			! dummy site coordinates from list file
   real(dp) thresh_low_in, thresh_high_in

   logical match(size(site_ids))	! mark sites matched to primary sites

!--------------------------------------------------------------------
! Read site exception file in one pass.
! Set threshold values only for included sites.
!--------------------------------------------------------------------

   print *
   print *, 'read_exception_list: Read site exception list for bias thresholds.'
   print *, '  Input file = ' // trim (filename)

! Initialize output arrays.

   nsites_primary = size (site_ids)
   allocate (thresh_low(nsites_primary), thresh_high(nsites_primary))

   thresh_low(:)  = vmiss		! default all sites to no thresholds,
   thresh_high(:) = vmiss		! until sites read from exception file

! Check for no exception file specified (file name is blank).

   if (filename == 'none') then
      print *, '  *** No exception file is specified.'
      print *, '  *** Threshold tests disabled, all analogs will be enabled.'
      print *
      return			! return defaults (no thresholds) for all sites
   end if

! Open the site exception file.

   infile = get_free_unit ()			! allocate a fortran unit number
   open (infile, file=filename, status='old', action='read', iostat=ios, &
      iomsg=errmes)

   if (ios /= 0) then
      print '(a)',    '*** read_exception_list: Abort.'
      print '(a,i0)', '*** Error on file open, I/O status = ', ios
      print '(a)',    '*** ' // trim (errmes)
      call exit (1)
   end if

! File open.  Begin exception list for log file.

   if (diag >= 2) print *
   if (diag >= 2) print *, '      Site ID      Valid   Latitude  Longitude' &
      // '  Low threshold  High threshold'

! Skip lines to end of header.

   lnum       = 0				! track line numbers in file
   isite_list = 0				! input site counter
   match(:)   = .false.				! init site match flags

   do
      read (infile, '(a)', iostat=ios, iomsg=errmes) header	! read start of
      lnum = lnum + 1						! header line

      if (ios /= 0) then
         print '(a)',    '*** read_exception_list: Abort.'
         print '(a,i0)', '*** Read error in header on line ', lnum
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)
         call exit (1)
      end if

      if (header(1:4) == '----') exit		! dashes = end of header
   end do

! Now read all site ID's and threshold values, to end of file.

line_loop: &
   do

! First read entire line as text, guard against short lines, etc.

      read (infile, '(a)', iostat=ios, iomsg=errmes) line
      lnum = lnum + 1

      if (is_iostat_end (ios)) exit line_loop	! check for normal end of file

      if (ios /= 0) then
         print '(a)',    '*** read_exception_list: Abort.'
         print '(a,i0)', '*** Read error 1 on line ', lnum
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)
         call exit (1)
      end if

! Now read line buffer in free format.  All data columns must be present.

      read (line, *, iostat=ios, iomsg=errmes) excep_id, lat, lon, &
         thresh_low_in, thresh_high_in

      if (ios /= 0) then
         print '(a)',    '*** read_exception_list: Abort.'
         print '(a,i0)', '*** Read error 2 on line ', lnum
         print '(a,i0)', '*** I/O status = ', ios
         print '(a)',    '*** ' // trim (errmes)
         call exit (1)
      end if

! Input line is valid.  Now find this site ID in the primary site list.

      isite_list = isite_list + 1		! count sites in list file
      				! caution, duplicate sites will double count

      match_str   = 'no'			! default to site not in list

      do si = 1, nsites_primary
         if (excep_id == site_ids(si)) then
            match_str       = "yes"
            match(si)       = .true.		! indicate matched site
            thresh_low(si)  = thresh_low_in	! insert site thresholds
            thresh_high(si) = thresh_high_in
            exit
         end if
      end do

! Print exception site line, for log file.

      if (diag >= 2) &
         print '(i5, 2x, a, 3x, a, 2f11.4, f12.1, f15.1)', isite_list, &
            excep_id, match_str, lat, lon, thresh_low_in, thresh_high_in

   end do line_loop

! End of site exception file.

   close (infile)				! all done, close input file
   						!   and release unit number

   nsites_matched = count (match(:))		! count sites matched to data
   						! duplicate matches NOT counted
   if (diag >= 2) print *

   print '(a,i0)', '  Number of sites in exception list =   ', isite_list
   print '(a,i0)', '  Number of sites matched to data   =   ', nsites_matched
   print *

end subroutine read_exception_list

end module read__exception_list
