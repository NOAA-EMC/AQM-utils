!------------------------------------------------------------------------------
!
! read_config_file_interp.f90 -- Config file reader for interpolate.f90.
!
! This is a support routine for the NOAA NCO/ARL/PSD bias
! correction program for CMAQ forecast outputs.
!
! 2014-may-01	read_config_file.f90:
!		Original version.  By Dave Allured.
! 2016-feb-10	Update for get_param_module.
!
! 2019-may-17	read_config_file_interp.f90:
!		Change to specific subroutine name.
!		Change config file format to support derived variables.
!		Add optional derived variables to var table, using keywords.
!		Add save/nosave column to var table.
!
! 2022-apr-13	Add parameter for number of forecast hours.
!		Remove dependency on get_free_unit, use fortran 2008 newunit.
! 2022-apr-19	Change from hard coded derivatives, to formula expressions.
!
! 2023-apr-09	Rename parameter "station file" to "site list" for consistency.
!
! Notes:
!
! The configuration file is a simple text file containing file
! paths and specification tables for data variables to be
! processed.
!
! The configuration file contains comments, and is self-
! documenting.  See a typical interpolator configuration file
! for more details.
!
! The "formulas" output array will be allocated zero length if
! there are no defined formulas.
!
!------------------------------------------------------------------------------

module read__config_file_interp
contains

subroutine read_config_file_interp (config_file, site_list_file, &
      grid_coord_file, interp_file_template, nhours, varnames, reader_codes, &
      infile_templates, formulas, var_save)

   use get_param_module
   use read__table_lines
   use stdlit
   use string_utils
   implicit none

   character(*), intent(in ) :: config_file	! name of config file to read

   character(*), intent(out) :: site_list_file
   character(*), intent(out) :: grid_coord_file
   character(*), intent(out) :: interp_file_template
   integer,      intent(out) :: nhours

   character(*), intent(out), allocatable :: varnames(:)
   character(*), intent(out), allocatable :: reader_codes(:)
   character(*), intent(out), allocatable :: infile_templates(:)
   character(*), intent(out), allocatable :: formulas(:)
   logical,      intent(out), allocatable :: var_save(:)

! Local variables.

   integer, parameter :: max_table_size = 20	! max number of lines in any
   						! single table in config file;
						! at least one more than needed

   character(200) lines(max_table_size)		! line buffer for maximal table

   character(200) line, err_mes
   character(200) header_expected

   character(len(reader_codes)) keyword

   character(50), allocatable :: file_type_names(:)
   character(50), allocatable :: var_file_types(:)
   character(50), allocatable :: save_strs(:)

   character(len(reader_codes)),     allocatable :: type_reader_codes(:)
   character(len(infile_templates)), allocatable :: type_templates(:)

   character(*), parameter :: caller_name = 'read_config_file_interp'

   integer i, j, k, vi, ios, status

   integer cf				! unit number for control file
   integer lnum				! line number within control file

   integer n_file_types			! size of file type table
   integer nvars			! size of var table
   integer nformulas			! number of derivative formulas

! Open config file for input.

   print *
   print *, 'Read configuration file.'
   print *, '  File = ' // trim (config_file)

   open (newunit=cf, file=config_file, status='old', action='read', &
      iostat=ios, iomsg=err_mes)

   lnum = 0					! init line counter

   if (ios /= 0) then
      print '(a,i0,a)', '*** Config file error no. ', ios, '.  Abort.'
      print '(a)',      '*** Fortran error: ' // trim (err_mes)
      call exit (1)
   end if

!-----------------------------------------------------------
! Read initial specification lines, in the order listed.
!-----------------------------------------------------------

! Item labels are case sensitive.
! Helper routines skip over comment lines and blank lines.

read_file: &
   do					! block for escape handling only

      call get_param_string ('site list', site_list_file, cf, status, lnum, &
         nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('grid coordinate file', grid_coord_file, cf, &
         status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_string ('interp file template', interp_file_template, &
         cf, status, lnum, nonblank)
      if (status /= normal) exit read_file

      call get_param_int ('number of forecast hours', nhours, cf, status, lnum)
      if (status /= normal) exit read_file

!-----------------------------------------------------------
! Read file table.
!-----------------------------------------------------------

! Column 1 = File type name.
! Column 2 = Read subroutine selector.
! Column 3 = Filename template for gridded input files, including paths.

! First read table as raw lines of text.  Get line count.

      print *
      print *, '  Read file table:'
      header_expected = 'file table:'
      call read_table_lines (cf, header_expected, 'no empty', caller_name, &
         lines, n_file_types, lnum)

! Parse table lines.  Free format, space delimited.
! Only read the first three columns.  Anything else is comments.

! Can't use list-directed formatting (*) for item 3.  Path items
! may contain active characters such as slash, which break
! list-directed formatting.

! Instead, use this cheap parser, which delimits purely on blanks.
! This also handles empty fields correctly, unlike list-directed.

      allocate (file_type_names(n_file_types))
      allocate (type_reader_codes(n_file_types))
      allocate (type_templates(n_file_types))

      do i = 1, n_file_types
         line = adjustl (lines(i))	  ! must ensure left justified to start
         print '(5x,a)', trim (line)	  ! progress display

         j = index (line // ' ', ' ')	  ! first blank after item 1
         k = max (j-1, 1)		  ! end of item, guard against empty
         file_type_names(i) = line(1:k)	  ! get item 1
         j = min (j, len(line)-1)	  ! guard against excessively long item
         line = adjustl (line(j+1:))	  ! step past item 1

         j = index (line, ' ')		  ! first blank after item 2
         k = max (j-1, 1)		  ! end of item, guard against empty
         type_reader_codes(i) = line(1:k) ! get item 2
         line = adjustl (line(j+1:))	  ! step past item 2

         j = index (line, ' ')		  ! first blank after item 3
         k = max (j-1, 1)		  ! end of item, guard against empty
         type_templates(i) = line(1:k)	  ! get item 3
      end do

!-----------------------------------------------------------
! Read var table.
!-----------------------------------------------------------

! Column 1  = Actual var name within input file.  Case sensitive.
! Column 2  = File type name, must match file type above, col. 1 of file table.
!             Or else "derived" for derived variables.
! Column 3  = "save" to write to interpolated output file; normal analog vars.
!             "nosave" to not write; vars only for derived var calculations.
! Column 4+ = Comments.

! First read table as raw lines of text.  Get line count.

      print *
      print *, '  Read var table:'
      header_expected = 'var table:'
      call read_table_lines (cf, header_expected, 'no empty', caller_name, &
         lines, nvars, lnum)

! Parse table lines.  Free format, space delimited.
! Only read the first three columns.  Anything else is comments.

      allocate (varnames(nvars))
      allocate (var_file_types(nvars))
      allocate (save_strs(nvars))

      do vi = 1, nvars
         line = adjustl (lines(vi))	  ! must ensure left justified to start
         print '(5x,a)', trim (line)	  ! progress display

         j = index (line // ' ', ' ')	  ! first blank after item 1
         k = max (j-1, 1)		  ! end of item, guard against empty
         varnames(vi) = line(1:k)	  ! get item 1
         j = min (j, len(line)-1)	  ! guard against excessively long item
         line = adjustl (line(j+1:))	  ! step past item 1

         j = index (line, ' ')		  ! first blank after item 2
         k = max (j-1, 1)		  ! end of item, guard against empty
         var_file_types(vi) = line(1:k)	  ! get item 2
         line = adjustl (line(j+1:))	  ! step past item 2

         j = index (line, ' ')		  ! first blank after item 3
         k = max (j-1, 1)		  ! end of item, guard against empty
         save_strs(vi) = line(1:k)	  ! get item 3
      end do

!-----------------------------------------------------------
! Correlate the var file types with the file table.
!-----------------------------------------------------------

! Purpose:  This section matches file type codes in the var table,
! with file types listed in the file table.

      allocate (reader_codes(nvars))
      allocate (infile_templates(nvars))

var_loop: &
      do vi = 1, nvars

! Look up file type for current variable in the file table.

         do j = 1, n_file_types
            if (var_file_types(vi) == file_type_names(j)) then
               reader_codes(vi)     = type_reader_codes(j)
               infile_templates(vi) = type_templates(j)
               cycle var_loop
            end if
         end do

! Check for "derived" keyword in place of normal file type.

         keyword = var_file_types(vi)
         call lowercase (keyword)		! case insensitive for "derived"

         if (keyword == 'derived') then
            reader_codes(vi)     = keyword	! return "derived" lowercase
            infile_templates(vi) = "none"
            cycle var_loop
         end if

! File type for this var is not recognized.  Abort.

         print *, '*** File type "' // trim (var_file_types(vi)) &
            // '" is not listed in the file table, or "derived" keyword.'
         print *, '*** Cross referenced from "' // trim (varnames(vi)) &
            // '" in the var table.'
         status = fatal
         exit read_file

      end do var_loop

!-----------------------------------------------------------
! Translate the save/nosave column in the var table.
!-----------------------------------------------------------

! Purpose:  Validate save/nosave keywords, and convert to simple true/false.

      allocate (var_save(nvars))

      do vi = 1, nvars
         call lowercase (save_strs(vi))		   ! allow case insensitive
         var_save(vi) = (save_strs(vi) == 'save')  ! convert strs to true/false

         if ( (.not. var_save(vi)) .and. (save_strs(vi) /= 'nosave') ) then
            print *, '*** Keyword error for variable "' &
               // trim (varnames(vi)) // '" in var table.'
            print *, '*** Column 3 must be "save" or "nosave".'
            status = fatal
            exit read_file
         end if
      end do

!-----------------------------------------------------------
! Read formulas to calculate derivatives.
!-----------------------------------------------------------

! Formulas are in the form of familiar assignment statements, such as:
!
!   x = u_vector (speed, direction)
!   x = a + b + c
!
! Blank line ends the list of formulas.
! Formulas are parsed, interpreted, and associated in derivatives.f90.
! See that routine for currently supported formula types.

! Just read formulas as raw lines of text.  Get line count.

      print *
      print *, '  Read formulas for derivatives:'
      header_expected = 'Calculation of derived variables:'
      call read_table_lines (cf, header_expected, 'allow empty', caller_name, &
         lines, nformulas, lnum)

! Resize and copy formulas to output array.

      allocate (formulas(nformulas))	      ! zero size if no formulas listed

      do i = 1, nformulas			! might be zero
         print '(5x,a)', trim (lines(i))	! progress display
         formulas(i) = lines(i)
      end do

!-----------------------------------------------------------
! End read_file block, and handle block aborts.
!-----------------------------------------------------------

      exit				! normal exit for single pass block
   end do read_file

! Catch aborts from anywhere in read_file block.
! Ensure "status" is always set appropriately before coming here.

   if (status /= normal) then
      print '(a,i0)', '*** read_config_file: Abort, config file line' &
         // ' number = ', lnum
      call exit (1)
   end if

! All done, output arguments are already assigned.  Return to caller.

end subroutine read_config_file_interp
end module read__config_file_interp
