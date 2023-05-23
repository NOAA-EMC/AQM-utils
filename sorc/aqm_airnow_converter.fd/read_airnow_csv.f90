!-----------------------------------------------------------------------------
!
! read_airnow_csv.f90 -- Read an EPA AirNow Hourly AQ Obs CSV file.
!
! This routine reads all primary variables and all sites from one
! AirNow Hourly AQ Obs file, per call.  This version assumes that
! the file is in the published Hourly AQ Obs CSV format, and that
! it contains only one specific day and hour.
!
! 2023-feb-06	Original version.  By Dave Allured, NOAA/PSL/CIRES.
!
!-----------------------------------------------------------------------------
!
! File format:
!
! AirNow Hourly AQ Obs files are CSV format with fully quoted
! fields.  Each data line contains one obs site with metadata,
! and data values for one time step and all chemical species
! for that site.
!
! As of January 2023, columns for six chemical species are always
! present in Hourly AQ Obs:  CO, NO2, Ozone, PM10, PM2.5, and S02.
!
! The positions of the data columns move around between files.
! However, this version assumes that ALL CSV columns might
! potentially move around.  Therefore, variable column positions
! are fully supported for ALL columns.
!
! Some records contain embedded commas within the quoted fields.
! These fields are read correctly using ordinary fortran free
! formatted string reads.
!
! The character set is plain ASCII for most records.  However,
! there is occasional UTF-8 within a few non-US records.  UTF-8
! is handled transparently in this reader by ordinary string
! handling with default character type, with common fortran
! compilers including gfortran and Intel.
!
! Site ID's are variable length, up to 12 characters long.  This
! reader version left justifies the site ID's, but does NOT left
! pad them with zeros.
!
! Available sites and site ordering change between AirNow files.
! Site arrays are in original, unsorted order upon return to
! caller.
!
! Missing values are indicated by empty or blank fields within
! each data line.  They are converted to missing value codes as
! appropriate for each type of output array.
!
!-----------------------------------------------------------------------------
!
! Notes:
!
! Primary subroutine inputs are the AirNow file name, and the
! requested date and time of day.
!
! Primary outputs are site arrays for site ID's, coordinates,
! other site metadata, and air concentration data for multiple
! variables at the current time step.
!
! This version does not perform any units conversion on input
! data values.
!
! This routine will not normally return with any local fortran
! file left open, even in the event of a soft error return with
! status = fail.  To minimize the chance of conflict, a file
! unit number is opened temporarily within this routine, then
! always closed before returning.
!
! This routine uses simple methods that are robust but also
! inefficient and memory intensive.  Methods could be improved to
! reduce run time, if needed.
!
! AirNow contains a small number of records that are not aligned
! on whole hours, such as 1:15, 1:30, etc.  These alignments are
! not currently supported in output files.  Such records are
! discarded, with optional warning messages printed.
!
! This routine supports variable column positions based on the
! header line in each file.  A list of expected fields is found
! in get_airnow_field_table.f90.
!
! One-to-one correspondence between the field list and actual
! fields in each AirNow file is not required.  This routine will
! properly handle discrepancies.  Missing fields will output
! missing values, and print warnings.  Extra or unknown fields
! will be ignored, also with warnings.
!
! A memory assessment for the current file format is included in
! in the notes at the top of the main program.
!
!-----------------------------------------------------------------------------
!
! Error handling:
!
! The current version performs a number of error checks.
! Internal soft errors produce optional warning messages, and
! return missing values and no error status to the caller, i.e.
! status = normal.
!
! If the file is missing or fully invalid, a warning message is
! printed, and status = fail is returned.  The returned data
! array should be ignored in this case.
!
!-----------------------------------------------------------------------------
!
! Read method for CSV:
!
! This routine uses fortran free formatted reads to parse the CSV
! lines efficiently.  AirNow CSV files appear to contain only CSV
! fields that can be correctly parsed this way in fortran.  The
! relevant characteristics are:
!
! * Fields are always comma separated.
! * All fields are double quoted.
! * In particular, fields with relevant internal spaces
!   or commas are always protected by quotes.
!
! Free format details:
!
! * Free format will successfully read all such fields as STRINGS.
! * Free format CAN NOT read such numeric fields directly.
! * Numeric fields must be read first as strings to remove the
!   quotes, then a second time to convert strings to numbers.
!
! Missing values are represented as empty fields in AirNow CSV.
! They can be read directly as strings, but direct numeric read
! will fail.  Empty fields must be detected separately, before
! numeric conversion.
!
!-----------------------------------------------------------------------------

module read__airnow_csv

! String lengths for local and caller.
! These sizes may be accessed by the caller for dynamic allocation.
! Note, strings in interfaces always use assumed length (*).
! Line length must be large enough for the header line.

   integer, parameter :: len_field      = 80
   integer, parameter :: len_line       = 500

contains

subroutine read_airnow_csv (filename, year, month, day, hour, vmiss, diag, &
      fields, doubles, floats, nsites, status, alt_time_count)

   use config, only : dp
   use get__airnow_field_table
   use stdlit, only : normal, fail
   use verify__date_time
   implicit none

! Input arguments.

   character(*), intent(in) :: filename		! input file name
   integer,      intent(in) :: year, month	! expected date and time
   integer,      intent(in) :: day, hour
   real,         intent(in) :: vmiss		! requested missing value code
   integer,      intent(in) :: diag		! verbosity level, 0-N

! Output arguments.

   character(*), intent(out) :: fields(:,:)	! string fields  (sites, fields)
   real(dp),     intent(out) :: doubles(:,:)	! numeric fields (sites, fields)
   real,         intent(out) :: floats(:,:)
   integer,      intent(out) :: nsites		! number of sites returned
   integer,      intent(out) :: status		! normal or fail

   integer,    intent(inout) :: alt_time_count	! count reject HH:15/30 records

! Local variables.

   character(len_field) field, site_id, date_str, time_str
   character(len_field), allocatable :: fields_in(:)

   character(len_table_item) field_name
   character(len_table_item), allocatable :: field_names(:)
   character(len_table_item), allocatable :: field_names_in(:)

   character(len_table_item) field_type
   character(len_table_item), allocatable :: field_types(:)
   character(len_table_item), allocatable :: expected(:), units(:)

   character(len_line) line
   character errmes*200

   integer i, j, fi, ncommas, nfields, nfields_in
   integer i_site_id, i_lat, i_lon, i_date, i_time
   integer idata, lnum, nlines, dim_sites
   integer infile, ios
   integer alt_time_local_count

   real(dp) lat, lon

   logical not1, not2
   logical test1, test2, alternate_time

   integer, allocatable :: fptr(:), len_out(:)

   logical, allocatable :: daily(:), hourly(:)
   logical, allocatable :: consistency(:), required(:)
   logical, allocatable :: merge_type(:), in_header(:)

! Initialize.

   dim_sites = size (fields, 1)	   ! get dimension

   status = fail		   ! ensure error return until final validation
   nsites = 0

   alt_time_local_count = 0	   ! init local time reject counter

!--------------------------------------------------------------------
! Open CSV input file for reading.
!--------------------------------------------------------------------

   if (diag >= 3) print *
   if (diag >= 2) print *, 'Read file: ' // trim (filename)

   open (newunit=infile, file=filename, status='old', action='read', &
      iostat=ios, iomsg=errmes)

   if (ios /= 0) then
      print '(3a)',    '*** File not found: ', trim (filename)

! Check for unexpected error, other than typical file not found.

      not1 = (index (errmes, 'No such file'  ) == 0)	! gfortran
      not2 = (index (errmes, 'file not found') == 0)	! Intel

      if ( (diag >= 3) .or. (not1 .and. not2) ) then
         print '(a,i0)', '***   I/O status = ', ios
         print '(3a)',   '***   ', trim (errmes)
      end if

      close (infile)
      status = fail
      return				! file missing, soft error return
   end if

!--------------------------------------------------------------------
! Read header line.
!--------------------------------------------------------------------

! Header part 1.  Read full line first as plain text.  Prevent extra line read.

   read (infile, '(a)', iostat=ios, iomsg=errmes) line

   if (ios /= 0) then
      print '(a)',    '*** read_airnow_csv: Read error on header line 1.'
      print '(3a)',   '*** File = ', trim (filename)
      print '(a,i0)', '*** I/O status = ', ios
      print '(3a)',   '*** ', trim (errmes)
      print *
      close (infile)
      status = fail
      return				! bad file, soft error return
   end if

! Count number of commas in the header line.

! This method assumes that there are no protected commas inside quote
! pairs in header lines.  This is valid for AirNow Hourly AQ Obs.

   ncommas = count ((/ (line(i:i) == ',', i = 1, len_trim (line)) /))

   nfields_in = ncommas + 1

   if (diag >= 3) print '(a,i0)', '    Number of fields in header = ', nfields_in

! Header part 2.  Read the field names from the header.

   allocate (field_names_in(nfields_in))

   read (line, *, iostat=ios, iomsg=errmes) field_names_in

   if (ios /= 0) then			! rare, possible quoting error in header
      print '(a)',    '*** read_airnow_csv: Read error parsing header line.'
      print '(3a)',   '*** File   = ',   trim (filename)
      print '(3a)',   '*** Line 1 = [', trim (line), ']'
      print '(a,i0)', '*** I/O status = ', ios
      print '(3a)',   '*** ', trim (errmes)
      print *
      close (infile)
      status = fail
      return				! bad file, soft error return
   end if

!--------------------------------------------------------------------
! Cross reference the field names with the standard field table.
!--------------------------------------------------------------------

! Fetch the decision table.

   call get_airnow_field_table (field_names, field_types, daily, hourly, &
      required, consistency, merge_type, len_out, expected, units, &
      i_site_id, i_lat, i_lon, i_date, i_time, nfields)

   allocate (fptr(nfields_in))		! init cross reference pointers & flags
   allocate (in_header(nfields))

   fptr(:) = vmiss
   in_header(:) = .false.

! Look up each input field name.

! j  = index of input fields
! fi = field index within decision table.

lookup1: &
   do j = 1, nfields_in
      field_name = field_names_in(j)
      fi = findloc (field_names(:), field_name, dim=1)

      if (fi == 0) then
         print '(a)',  '*** read_airnow_csv: Unexpected field name in header.'
         print '(3a)', '*** Unexpected field name = [', trim (field_name), ']'
         print '(3a)', '*** File   = ',   trim (filename)
         print '(3a)', '*** Line 1 = [', trim (line), ']'
         print '(a)',  '*** This field will be ignored.'
         print *
         cycle lookup1		! soft error, ignore this field and keep going
      end if

      fptr(j)       = fi		! save cross reference pointer
      in_header(fi) = .true.
   end do lookup1

! Check for missing fields.

   do fi = 1, nfields
      if (.not. in_header(fi)) then
         print '(a)',  '*** read_airnow_csv: Missing field name in header.'
         print '(3a)', '*** Expected field name = [', trim (field_names(fi)),']'
         print '(3a)', '*** File   = ',   trim (filename)
         print '(3a)', '*** Line 1 = [', trim (line), ']'

         if (required(fi)) then
            print '(a)',  '*** This field is required.  Skip defective file.'
            print *
            close (infile)
            status = fail
            return			! bad file, soft error return
         end if

         print *
      end if
   end do

! Header is now fully validated.  Set up for main loop.

   allocate (fields_in(nfields))		! string input array

   fields (:,:) = ' '				! clear output arrays to missing
   doubles(:,:) = vmiss				! to cover for skipped inputs
   floats (:,:) = vmiss

   lnum  = 1					! init line number
   idata = 1					! valid output data pointer --
   						! points to next slot to fill

!--------------------------------------------------------------------
! Main loop, read all data lines.
!--------------------------------------------------------------------

line_loop: &
   do
      lnum = lnum + 1				! advance physical line number

      if (diag >= 3) print '(a,i0)', '  line = ', lnum

! Data line part 1.  Read line first as plain text, prevent extra line read.

      read (infile, '(a)', iostat=ios, iomsg=errmes) line

      if (is_iostat_end (ios)) exit line_loop	! end of file, loop complete

      if (ios /= 0) then		! otherwise unexpected read error
         print '(a,i0)', '*** read_airnow_csv: Read error on data line ', lnum
         print '(3a)',   '*** File = ',  trim (filename)
         print '(a,i0)', '*** I/O status = ', ios
         print '(3a)',   '*** ', trim (errmes)
         print *
         exit line_loop			! soft error, terminate input file
      end if				! but continue to partial outputs

! Data line part 2.  Read all CSV fields as strings, free formatted read.

      read (line, *, iostat=ios, iomsg=errmes) fields_in

! Check for short or broken data line.
! Note, partial read of short lines is not currently supported.

      if (ios /= 0) then
         print '(3a)',   '*** read_airnow_csv: Read error parsing data line,', &
                              ' skipping line.'
         print '(3a)',   '*** File = ',  trim (filename)
         print '(3a)',   '*** Line = [', trim (line), ']'
         print '(a,i0)', '*** Line number = ', lnum
         print '(a,i0)', '*** I/O status  = ', ios
         print '(3a)',   '*** ', trim (errmes)
         print *
         cycle line_loop		! skip bad line, keep reading file
      end if

! Check for unexpected array overlow.

      if (idata > dim_sites) then
         print '(3a)',  '*** read_airnow_csv: Number of sites exceeds capacity.'
         print '(3a)',  '*** Increase dim_sites in main program.'
         print '(3a)',  '*** File = ',  trim (filename)
         print '(3a)',  '*** Skipping rest of file, starting here:'
         print '(3a)',  '*** Line = [', trim (line), ']'
         print '(a,i0)','*** Line number = ', lnum
         print *
         exit line_loop			! soft error, terminate input file
      end if				! but continue to partial outputs

!--------------------------------------------------------------------
! Primary scan for all input fields in current line.
!--------------------------------------------------------------------

! Unrecognized fields and the "omit" fields are skipped.

field_loop: &
      do j = 1, nfields_in	! loop over INPUT fields, not output fields

! Permute input fields to output field positions.

         fi = fptr(j)				! get mapping to output field
         if (fi == vmiss) cycle field_loop	! skip unrecognized fields

         field      = fields_in(j)		! get current INPUT field string
         field_type = field_types(fi)
         field_name = field_names(fi)

! Type char.  Copy original input string to output field.

case_field_type: &
         if (field_type == 'char') then
            fields(idata, fi) = field

! Check for length overflow.
! Soft error, warning only.  No correction is needed.

            if (len_trim (field) > len_out(fi)) then
               print '(3a)',   '*** read_airnow_csv: ', trim (field_name), &
                                    ' string is too long, will be truncated.'
               print '(3a)',   '*** File = ',  trim (filename)
               print '(3a)',   '*** Line = [', trim (line), ']'
               print '(a,i0)', '*** Line number = ', lnum
               print '(3a)',   '*** Field = [', trim (field), ']'
               print *
            end if

! Type verify.  Copy input string, only for later verification step.

         else if (field_type == 'verify') then
            fields(idata, fi) = field

! Type double.  Convert string to number.

         else if (field_type == 'double') then
            if (field == ' ') then		! blank field, convert quietly
               doubles(idata, fi) = vmiss	!   to missing value
            else
               read (field, *, iostat=ios, iomsg=errmes) doubles(idata, fi)
               if (ios /= 0) then
                  print '(3a)',   '*** read_airnow_csv: Invalid number in ', &
                                         trim (field_name), '.'
                  print '(3a)',   '*** File = ',  trim (filename)
                  print '(3a)',   '*** Line = [', trim (line), ']'
                  print '(a,i0)', '*** Line number = ', lnum
                  print '(3a)',   '*** Field = [', trim (field), ']'
                  print '(a,i0)', '*** I/O status  = ', ios
                  print '(3a)',   '*** ', trim (errmes)
                  print *
                  doubles(idata, fi) = vmiss	! soft error, convert to missing
               end if
            end if

! Type float.  Convert string to number.

         else if (field_type == 'float') then
            if (field == ' ') then		! blank field, convert quietly
               floats(idata, fi) = vmiss	!   to missing value
            else
               read (field, *, iostat=ios, iomsg=errmes) floats(idata, fi)
               if (ios /= 0) then
                  print '(3a)',   '*** read_airnow_csv: Invalid number in ', &
                                         trim (field_name), '.'
                  print '(3a)',   '*** File = ',  trim (filename)
                  print '(3a)',   '*** Line = [', trim (line), ']'
                  print '(a,i0)', '*** Line number = ', lnum
                  print '(3a)',   '*** Field = [', trim (field), ']'
                  print '(a,i0)', '*** I/O status  = ', ios
                  print '(3a)',   '*** ', trim (errmes)
                  print *
                  floats(idata, fi) = vmiss
               end if
            end if

! Type unit.  Constant field, should always be expected value, or blank.
! If something else, then soft error only.  Print warning and continue.
! Don't bother to copy the input field.  Just output the expected value, later.

         else if (field_type == 'unit') then
            if ( (field /= ' ') .and. (field /= expected(fi)) ) then
               print '(4a)',   '*** read_airnow_csv: Unexpected unit value', &
                                    ' in ', trim (field_name), '.'
               print '(3a)',   '*** File = ',  trim (filename)
               print '(3a)',   '*** Line = [', trim (line), ']'
               print '(a,i0)', '*** Line number = ', lnum
               print '(3a)',   '*** Field    = [', trim (field), ']'
               print '(3a)',   '*** Expected = [', trim (expected(fi)), ']'
               print *
            end if

         end if case_field_type

      end do field_loop

! Primary scanning for current line is complete.

! Note, at this point all field permutations from input order
! to output order are now complete.

!--------------------------------------------------------------------
! More consistency checks.
!--------------------------------------------------------------------

! Note, all errors that "cycle line_loop" effectively skip the
! current record, by not incrementing the output pointer.

! Check for missing site ID.

      site_id = fields(idata, i_site_id)

      if (site_id == ' ') then
         print '(a)',    '*** read_airnow_csv: Missing site ID, skipping line.'
         print '(3a)',   '*** File = ',  trim (filename)
         print '(3a)',   '*** Line = [', trim (line), ']'
         print '(a,i0)', '*** Line number = ', lnum
         print *
         cycle line_loop		! skip bad line, keep reading file
      end if

! Check for duplicate site ID's within same file.

! Unlikely scenario, but check this anyway.
! If duplicate, the old record is kept, and the new record is discarded.

      if (idata > 1) then
         if (any (fields(1:idata-1, i_site_id) == site_id)) then
            print '(3a)',   '*** read_airnow_csv: Duplicate site ID, ', &
                                 ' skipping current data line.'
            print '(3a)',   '*** Site ID = [', trim (site_id), ']'
            print '(3a)',   '*** File = ',  trim (filename)
            print '(3a)',   '*** Line = [', trim (line), ']'
            print '(a,i0)', '*** Line number = ', lnum
            print *
            cycle line_loop		! skip bad line, keep reading file
         end if
      end if

! Check this record's embedded date and time for consistency.

      date_str = fields(idata, i_date)
      time_str = fields(idata, i_time)

      call verify_date_time (date_str, time_str, year, month, day, hour, &
         errmes, alternate_time)

      if (alternate_time) alt_time_local_count = alt_time_local_count + 1
      					! count HH:15 or HH:30 detected

! Print warning unless alternate HH:15 or HH:30 is the only problem.
! But print anyway, if verbosity is elevated.

      test1 = (errmes /= 'no error') .and. (.not. alternate_time)
      test2 = (errmes /= 'no error') .and. (diag >= 3)

      if (test1 .or. test2) then
         print '(3a)',   '*** read_airnow_csv: ', trim (errmes)
         print '(3a)',   '*** File = ',  trim (filename)
         print '(3a)',   '*** Line = [', trim (line), ']'
         print '(a,i0)', '*** Line number = ', lnum
         print '(3a)',   '*** ValidDate = [', trim (date_str), ']'
         print '(3a)',   '*** ValidTime = [', trim (time_str), ']'
         print '(a)',    '*** Skipping this data line.'
         print *
         cycle line_loop		! skip bad line, keep reading file
      end if

! Require valid lats and lons for a valid record.

      lat = doubles(idata, i_lat)
      lon = doubles(idata, i_lon)

      if ( (lat == vmiss) .or. (lon == vmiss) ) then
         print '(3a)',   '*** read_airnow_csv: Invalid lat/lon coordinates,', &
                              ' skipping line.'
         print '(3a)',   '*** File = ',  trim (filename)
         print '(3a)',   '*** Line = [', trim (line), ']'
         print '(a,i0)', '*** Line number = ', lnum
         print *
         cycle line_loop		! skip bad line, keep reading file
      end if

! Now all required checks have passed.
! Keep this new record by advancing the data pointer.
! Any checks that cycled around the line loop will skip this point,
! thereby discarding the current record.

      idata = idata + 1		! advance output pointer, valid records only

   end do line_loop

   close (infile)

!--------------------------------------------------------------------
! All data lines are finished.  Do final checks.
!--------------------------------------------------------------------

! Return output arrays which contain valid records up to just before
! the current data pointer.

   nsites = idata - 1		! final record count, VALID RECORDS ONLY
   nlines = lnum  - 1		! final line count, all lines unless truncated

   if ( (diag >= 3) .or. (nsites == 0) ) then
      print '(a,i0)', '  Total number of lines read = ', nlines
      print '(a,i0)', '  Number of valid records    = ', nsites
      print '(a,i0)', '  Number of rejected lines   = ', nlines - nsites - 1
      print *						! deduct header line
   end if

! Check for no valid data records.

   if (nsites == 0) then
      print '(a)', '*** read_airnow_csv: File contains no valid records.'
      print '(a)', '*** File = ' // trim (filename)
      print *
      status = fail
      return				! soft error return
   end if

! Normal return.  File contains at least one valid record.

   status = normal

! Update error details only for normal returns.

   alt_time_count = alt_time_count + alt_time_local_count

end subroutine read_airnow_csv
end module read__airnow_csv
