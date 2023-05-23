!-----------------------------------------------------------------------------
!
! verify_date_time.f90 -- Confirm date & time fields for read_airnow_csv.f90.
!
! 2023-feb-06	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
!-----------------------------------------------------------------------------

module verify__date_time
contains

subroutine verify_date_time (date_str, time_str, year, month, day, hour, &
      errmes, alternate)
   implicit none

   character(*), intent(in)  :: date_str, time_str
   integer,      intent(in)  :: year, month, day, hour

   character(*), intent(out) :: errmes
   logical,      intent(out) :: alternate

! Local vars.

   character(12) date_expect(2), time_expect(3)
   character(40) fmt1, fmt2

   integer year_2digit, ios

! Verify expected date and time as strings.
! AirNow is currently using 2-digit years.
! For future proof, accept either 2-digit or 4-digit years.

   year_2digit = mod (year, 100)

   fmt1 = "(i2.2, '/', i2.2, '/', i2.2)"
   fmt2 = "(i2.2, '/', i2.2, '/', i4  )"

! Use iostat to prevent crash in obscure scenarios.

   write (date_expect(1), fmt1, iostat=ios) month, day, year_2digit
   write (date_expect(2), fmt2, iostat=ios) month, day, year

! Time of day must always be HH:00, for normal status.
! AirNow contins a few HH:15 and HH:30, so detect and signal them.

   write (time_expect(1), "(i2.2, ':00')", iostat=ios) hour
   write (time_expect(2), "(i2.2, ':15')", iostat=ios) hour
   write (time_expect(3), "(i2.2, ':30')", iostat=ios) hour

! Verify.

   if (.not. any (date_str == date_expect(:))) then
      errmes    = 'Invalid date in record.'
      alternate = .false.

   else if (time_str /= time_expect(1)) then
      errmes    = 'Invalid time in record.'
      alternate = any (time_str == time_expect(2:3))

   else
      errmes    = 'no error'
      alternate = .false.
   end if

end subroutine verify_date_time
end module verify__date_time
