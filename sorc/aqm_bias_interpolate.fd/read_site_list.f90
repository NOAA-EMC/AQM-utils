!------------------------------------------------------------------------------
!
! read_site_list.f90 -- Read list of site coordinates for bias correction.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-apr-24	read_station_file.f90:
!		Original version.  By Dave Allured, NOAA/PSD/CIRES.
!
! 2023-apr-09	read_site_list.f90:
!		Subroutine and labels renamed for consistency.
!		Use fortran 2008 "newunit" rather than private function.
!
! Input file format:
!
! Site list with coordinates.
! Plain text, three columns, free format.
!
! The first part is a comment header, variable length, ending with
! one line of dashes.
!
! The second part is three columns: Site ID, latitude, longitude.
! Free format, items separated by one or more spaces.
!
!------------------------------------------------------------------------------

module read__site_list
contains

subroutine read_site_list (filename, site_ids, site_lats, site_lons, nsites)

   use config, only : dp
   implicit none

   character(*), intent(in )              :: filename	   ! input file name

   character(*), intent(out), allocatable :: site_ids(:)   ! site ID strings
   real(dp),     intent(out), allocatable :: site_lats(:)  ! site latitudes
   real(dp),     intent(out), allocatable :: site_lons(:)  ! site longitudes
   integer,      intent(out)              :: nsites	   ! no. of sites read

! Local variables.

   character(len(site_ids)) min_id, max_id
   character header*4

   integer infile, ios, i, lnum, nheaders

! First pass.  Just count the number of actual site lines in the file.

   print *
   print *, 'read_site_list: Read site list.'
   print *, '  Site list file = ' // trim (filename)

   open (newunit=infile, file=filename, status='old', action='read')

   nheaders = 0					! count header lines...
   do
      read (infile, '(a)') header		! read start of header line
      nheaders = nheaders + 1
      if (header == '----') exit		! dashes = end of header
   end do

   nsites = 0					! count date lines
   do
      read (infile, *, iostat=ios)
      if (ios /= 0) exit			! end of file = end of site list
      nsites = nsites + 1
   end do

   print '(a,i0)', '   Number of sites in file      =   ', nsites

! Second pass.  Read site ID's and coordinates into arrays.

   allocate (site_ids(nsites))			! allocate output arrays
   allocate (site_lats(nsites), site_lons(nsites))

   rewind (infile)				! go back to start of file

   do i = 1, nheaders				! skip header lines
      read (infile, '(a)')
   end do

   do i = 1, nsites				! read all site lines
      read (infile, *, iostat=ios) site_ids(i), site_lats(i), site_lons(i)
      						! free format works here
      if (ios /= 0) then
         lnum = nheaders + i
         print *, '*** read_site_list: Read error on line ', lnum
         call exit (1)
      end if
   end do

   close (infile)				! all done, close input file
   						!   and release unit number
! Progress display, show statistics.

   min_id = site_ids(1)
   max_id = site_ids(1)

   do i = 2, nsites
      min_id = min (min_id, site_ids(i))
      max_id = max (max_id, site_ids(i))
   end do

   print '(4a)', '   Lowest, highest station ID''s =  ', trim (min_id), &
      ', ', trim (max_id)

   print '(2(a,f10.5))', '   Min, max latitudes           = ', &
      minval (site_lats), ',', maxval (site_lats)
   print '(2(a,f10.5))', '   Min, max longitudes          = ', &
      minval (site_lons), ',', maxval (site_lons)

end subroutine read_site_list

end module read__site_list
