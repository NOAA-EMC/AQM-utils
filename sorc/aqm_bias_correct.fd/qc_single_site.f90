!234567
!------------------------------------------------------------------------------
!
! qc_single_site.f90 -- QC for AIRNow generic hourly time series, single site.
!
! 2014-jun-08	qc_single_site.f90:
!		Split off from stand-alone program quality_control.f/f90.
!		Original fortran 77 version by Irina Djalalova, NOAA/CIRES/PSD.
!		Converted to fortran 90 module by Dave Allured.
!		This initial version is calibrated for PM2.5 data only.
! 2014-jun-10	Add diag argument for verbosity control.
!		Add diagnostic for histogram index out of range.
!		Fix histogram indexing bug.
!
! 2017-mar-30	Add QC subroutine for ozone.
!		Break out PM2.5 and ozone routines into separate include files.
!		Add subroutine dispatcher, based on the var name.
!
! 2022-may-27	Add maximum value limit for PM2.5 input.
!
! 2023-apr-03	Add var names for AirNow CSV/Netcdf to embedded var table.
! 2023-apr-06	Add diagnostic to display values changed.
! 2023-apr-08	Add lower limit for AirNow negative input values.
!		Add low/high input limits for ozone as well as PM2.5.
!
! Primary input:  Raw AIRNow hourly time series of pollutant concentrations.
! for single site.  Complete days, start on hour 0, end on hour 23.
!
! Primary output:  QC'ed time series.
!
!------------------------------------------------------------------------------

!-------------------------------------------------------------
! Module header.
!-------------------------------------------------------------

module qc__single_site

   private			! visibility
   public qc_single_site	! only the main subroutine is public

contains


!-------------------------------------------------------------
! Included subroutines.  Specific routines for obs species.
!-------------------------------------------------------------

   include 'qc_site_ozone.f90'
   include 'qc_site_pm25.f90'


!-------------------------------------------------------------
! Main routine, dispatches for current obs species.
!-------------------------------------------------------------

subroutine qc_single_site (varname, y, obs_min_input, obs_max_input, vmiss, &
      diag, site_id)

   use config, only : dp
   implicit none

   character(*), intent (in   ) :: varname	 ! name of current obs species
   real(dp),     intent (inout) :: y(:)		 ! hourly time series for 1 site
   real(dp),     intent (in   ) :: obs_min_input ! obs min valid input threshold
   real(dp),     intent (in   ) :: obs_max_input ! obs max valid input threshold
   real(dp),     intent (in   ) :: vmiss	 ! missing value in time series
   integer,      intent (in   ) :: diag		 ! diag verbosity level, 0-N
   character(*), intent (in   ) :: site_id	 ! site ID for diagnostic

! Local variables.

   integer ti, ndiff
   real(dp), allocatable :: ycopy(:)

! Save a copy of input data, for diagnostic.

   ycopy = y(:)

! Dispatch to specific QC routine for the current obs species.

   if (any (varname == (/ 'OZONE', 'COPO ' /) )) then
      call qc_site_ozone (y, obs_min_input, obs_max_input, vmiss, diag, site_id)

   else if (any (varname == (/ 'PM25  ', 'COPOPM' /) )) then
      call qc_site_pm25 (y, obs_min_input, obs_max_input, vmiss, diag, site_id)

   else
      print *, '*** qc_single_site: Requested obs variable name is unknown.' &
                    // ' Abort.'
      print *, '*** Requested var name = "' // trim (varname) // '"'
      call exit (1)
   end if

! Site diagnostics for changed values.

   ndiff = count (y(:) /= ycopy(:))

   if ( (diag >= 2) .and. (ndiff /= 0) ) then
      print '(7x,2a,i5)', site_id, ':  Number of values changed = ', ndiff

      if (diag >= 3) then
         do ti = 1, size (y)
            if (y(ti) /= ycopy(ti)) then
               print '(7x,2a,i6,2g16.7)', site_id, ':  Changed:', &
                  ti, ycopy(ti), y(ti)
            end if
         end do
      end if
   end if

end subroutine qc_single_site


!-------------------------------------------------------------
! Subroutine histy.  Histogram of the integer array.
!-------------------------------------------------------------

subroutine histy (y, iymin, iymax, ibinsize, diag, site_id, yhist)
   use config, only : dp
   implicit none

   real(dp),     intent (in ) :: y(:)		! data time series
   integer,      intent (in ) :: iymin		!
   integer,      intent (in ) :: iymax		!
   integer,      intent (in ) :: ibinsize	!
   integer,      intent (in ) :: diag		! diag verbosity level, 0-N
   character(*), intent (in ) :: site_id	! site ID for diagnostic

   integer,      intent (out) :: yhist(:)	! result histogram array

! Local variables.

   character data_str*30
   integer i, j, nysize, isize

! Initialize.

   nysize = size (y)				! length of input time series

   isize=int((iymax-iymin)/ibinsize)
   if (diag>=5) PRINT *,'isize=',isize

   yhist(:)=0					! clear histogram array

! Fill histogram array.
! Note, out of range values are currently not counted in ANY bin.

   do i=1,nysize
     if (y(i).gt.0) then
       j = 1 + int (y(i) / ibinsize)

       if (j < 1 .or. j > size (yhist)) then
          if (diag >= 2) then
             write (data_str, '(g11.4)') y(i)
             print '(3a,i6,a,i11,2a)', '** ', site_id, &
                '  histo index out of range: i =', i, '  index =', j, &
                '  data =', trim (data_str)
          end if
       else
          yhist(j) = yhist(j) + 1
       end if

     end if
   enddo

   return
end subroutine histy

end module qc__single_site
