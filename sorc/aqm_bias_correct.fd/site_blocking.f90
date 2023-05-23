!------------------------------------------------------------------------------
!
! site_blocking.f90 -- Remove site data for intervals in site blocking list.
!
! This is a support routine for the NOAA NCO/ARL/ESRL bias correction
! program for CMAQ forecast outputs.
!
! 2021-apr-27	Original version.  By Dave Allured.
!
! 2023-apr-09	Minor update for 12-character site ID's.
!
! Summary:
!
! * Read and validate the site blocking list file.
! * Remove site data time intervals specified in blocking list.
!
! The purpose of this particular subroutine is to apply the site
! blocking commands to remove specific time intervals from site
! input data.
!
! See the reader, read_blocking_list.f90, for details of the list
! file format, usage information, and error handling.
!
! This routine makes a best effort to read and process the entire
! blocking list file, and never halt the program, even in the
! presence of many different error conditions.
!
!------------------------------------------------------------------------------

module site__blocking
contains

subroutine site_blocking (filename, site_ids, start_date, base_year, vmiss, &
      diag, obs_data)

   use config, only : dp
   use read__blocking_list
   implicit none

   character(*), intent(in)    :: filename	 ! site blocking list file name
   character(*), intent(in)    :: site_ids(:)	 ! primary site ID's
   integer,      intent(in)    :: start_date	 ! obs starting date index
   integer,      intent(in)    :: base_year	 ! base year in data scenario
   real(dp),     intent(in)    :: vmiss		 ! current missing value
   integer,      intent(in)    :: diag		 ! verbosity, 0 = errors only

   real(dp),     intent(inout) :: obs_data(:,:)	 ! site data to be modified
						 !   (hours, sites)
! Local variables.

   integer si, t1, t2, ntimes
   integer ilist, nlist, nblock
   integer tr_day1, tr_day2, tr_day_offset
   integer nbefore, nafter, nchange
   integer nmiss1, nmiss2

! Dynamic arrays for site blocking list.

   integer, allocatable :: block_sites(:)	! site indices (list)
   integer, allocatable :: block_starts(:)	! start and end date indices
   integer, allocatable :: block_ends(:)	!   (list)

! Read the site blocking list file.

   call read_blocking_list (filename, site_ids, base_year, diag, &
      block_sites, block_starts, block_ends)

! If returned list is empty, then nothing to do here.  Just return
! quietly, with no messages beyond what the reader has already printed.

   if (.not. allocated (block_sites)) return

! Now we have a table of site indices, with start and end date indices.
! A site may have multiple entries for different blocking time intervals.
! Data are blocked by whole 24-hour calendar days, 0Z to 23Z.

! Returned date indices are referenced to January 1 of the base year.
! They must be translated to reference to the start date of the
! training period.

! Site indices are guaranteed to match the current site arrays.
! However, date indices may go outside the available time range,
! so constrain them to the available range.

! Otherwise, process the table blindly.  Do not bother to check
! for overlaps, duplications, or range-excluded intervals.

   if (diag >= 2) then
      print *
      print *, 'site_blocking: Remove specified time intervals from site data.'
      print *, 'Counts in detail table are only for given site and time' &
                 // ' interval.'
      print *
      print *, '     Site ID     Values changed  Missing before   Missing after'
   end if

   nmiss1 = count (obs_data == vmiss)		! count missing before blocking

   nlist  = size (block_sites)
   ntimes = size (obs_data, 1)
   nblock = 0

   tr_day_offset = start_date - 1	! day offset from base year January 1
					! to start day of training period
command_loop: &
   do ilist = 1, nlist
      si = block_sites(ilist)			! get site index

! Convert blocking date indices from relative to base year January 1,
! to relative to the training period start date.

      tr_day1 = block_starts(ilist) - tr_day_offset
      tr_day2 = block_ends(ilist)   - tr_day_offset

! Convert blocking start and end indices from 1-based training days,
! to 1-based hourly time series.

      t1 = 24 * tr_day1 - 23			! hour  1 of blocking start day
      t2 = 24 * tr_day2 - 0			! hour 24 of blocking end day

      t1 = max (t1, 1)				! constrain to available
      t2 = min (t2, ntimes)			! time range of input obs

      if (diag >= 4) print '(i3,1x,a,8i7)', ilist, site_ids(si), si, &
         block_starts(ilist), block_ends(ilist), tr_day1, tr_day2, t1, t2,ntimes

! Block current site's data in the specified time interval.

      if (t1 > t2) then				! skip blocking if interval
         nbefore = 0				! is completely out of range
         nafter  = 0

      else
         nbefore = count (obs_data(t1:t2,si) == vmiss)	! track values changed
         obs_data(t1:t2,si) = vmiss		! block this interval for site
         nafter  = count (obs_data(t1:t2,si) == vmiss)
         nblock  = nblock + 1			! count only the intervals
      end if					! that are actually blocked

! Print diagnostic for each blocking command.

      nchange = nafter - nbefore
      if (diag >= 2) print '(4x,a,3i16)', site_ids(si), nchange, nbefore, nafter

   end do command_loop

! Print summary, following general style of read_obs_qc.

   if (diag >= 2) then
      nmiss2 = count (obs_data == vmiss)	! count missing after blocking
      print *
      print '(a,i0)', '   Number of time intervals actually blocked = ', nblock
      print '(a,i0)', '   Number of missing values before blocking  = ', nmiss1
      print '(a,i0)', '   Number of missing values after blocking   = ', nmiss2
   end if

end subroutine site_blocking
end module site__blocking
