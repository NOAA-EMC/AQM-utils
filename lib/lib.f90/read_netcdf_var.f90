!------------------------------------------------------------------------------
!
! read_netcdf_var.f90 -- Generic reader for single Netcdf variables.
!
! This routine reads a whole array variable from a Netcdf file.
! Multiple dimensions are supported.
!
! 2014-may-06	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!		This version supports only numeric arrays, up to 4 dimensions.
! 2014-may-13	Bug fix.  Close the old input file before opening a new one.
!		Fix file open/close state memory, and diagnostics.
!		Adjust verbosity for 2 = sparse progress display.
! 2014-jun-18	Add reader for 2-D character variables (1-D strings).
! 2014-jun-26	Fix illegal use of shape function.
! 2014-dec-20	Add missing value support.
! 2014-dec-20	Add reader for var dimension names.
! 2014-dec-24	Add support for character variables of unknown length.
!		F2003 deferred length character variables are not sufficiently
!		  supported through gfortran 4.9.2.
!		So use F90-style specification expression, instead.
!		Include string truncation check.
!
! 2015-nov-16	Add optional return arguments for file and variable ID's,
!		  to support reading attributes, etc.
!
! 2017-mar-27	Minor.  Fix line wrapping in diagnostic print, with ifort.
!
! 2022-apr-03	Split into multiple, smaller source files, using includes.
!		Add specific interfaces for 32-bit type float output arrays.
!		Speed up 32-bit apps, avoid unnecessary 64-bit conversions.
! 2022-apr-19	Minor.  Adjust verbosity levels such that 3 prints each file
!		  name only once, and 4 prints one line for each var read.
!
! Input:   infile = Path to Netcdf input file
!	   varname = Name of requested variable.  Case sensitive.
!          diag = Verbosity control, 0-2 = errors only.  See below for more.
!
! Output:  out = Caller's output array, any desired rank.
!	   status = Simplified operation status, as defined in stdlit.f90:
!	     normal = success, fail = any of several errors.
!	   Supplemental outputs, see read_netcdf_var_1d code for details.
!
! Notes:
!
! In a single call, this routine reads the specified array
! variable from a Netcdf file.  The whole array is read.
!
! All numeric data types are read generically as type float or
! type double, depending on the type of the caller's output
! array.  The rank of the requested file variable must match the
! caller's output array.
!
! Caller must somehow ensure that the output array type has
! sufficient numeric range for the actual data requested.
! Output type double is widely used for all possible 8, 16, and
! 32-bit integers, both signed and unsigned; and all possible
! float32's and doubles.  Output type real (32-bit floats) may be
! used to improve read time for the matching float32 data type,
! or for reading smaller integer data up to about 20 bits in
! value.
!
! The output array is auto-allocated, as needed.  However, auto-
! matic reallocation is not supported.  A previously allocated
! caller's array is not deallocated.  This is done for efficiency
! and consistency checking.
!
! Subsequent reads into the same array must have the same array
! dimensions, otherwise an error is generated.  To recycle the
! same array with changing dimensions, the caller must deallocate
! the array between calls.
!
! Character variables are also supported.  They are reshaped into
! fortran strings on input, with rank reduced by one.  Trailing
! nulls are removed.  Mismatched string lengths are handled
! correctly.
!
! For efficiency, the input file is left open after reading.
! Subsequent reads from the same file will utilize the existing
! open file, until a different file name is requested.
!
! If present in the file, the missing_value or _FillValue
! attribute value is returned in the optional vmiss argument.
! If not present, the float32 or float64 value -huge() matching
! the output array type is returned, providing a safe test value
! either way.
!
! You may omit or ignore the vmiss argument if you assume that
! the input data contains all valid data, no missing values.
! Alternatively, you may use an assumed missing value strategy,
! with caution.
!
! In this version, missing value support and dimension names are
! provided for numeric variables only, not text variables.
!
! The Netcdf file ID and current variable ID are returned in
! optional arguments nc_id and var_id.  They may be used by the
! caller to access attributes, and for other purposes.
!
! The dimension names of the requested variable are also returned
! in the optional dim_names argument.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.
! The messaging level is cumulative.  0 to 2 = errors only,
! 3 = one line for each file name, 4 = one line for each var,
! 5 = more details.
!
! Error handling:
!
! This version treats most errors as soft errors.  An error
! message is printed, and "fail" status is returned to the caller.
!
! On any soft error return, all return arguments other than
! "status" are undefined and possibly unallocated.
!
! Limitations:
!
! Array subsetting is not supported.  This is to keep the
! interface simple.
!
! Reallocation (automatic dealloc/realloc) is not currently
! supported.  This version uses fortran 95 allocation semantics,
! not fortran 2003 reallocation.  The intention is allocate the
! main data array only once, then re-use it, for efficiency.
!
! Dimension reordering is not supported.  Coordinate variables
! are ignored.  If coordinate variables are needed, they should
! be read independently as separate named variables.
!
! Aside from the missing value, no other support for reading
! attributes is currently included.  Attributes may be read
! through independent Netcdf call sequences.
!
!------------------------------------------------------------------------------

!-------------------------------------------------
! Module structure and state variables.
!-------------------------------------------------

module read__netcdf_var

   use config, only : dp		! common module linkage
   use netcdf				! for all the internal routines
   use stdlit, only : normal, fail
   implicit none

   private				! visibility
   public read_netcdf_var		! only the generic routine is public

! State variables for this module, saved between public calls.

   character(200) :: previous_file = ' '	! remember open file name
   integer        :: ncidi = 0			! remember open file ID

! Internal state variables.  These are used between internal
! routines only, not between public calls.

   character alloc_msg*200
   integer varidi, rank, dimids(9), dims(9), alloc_status
   logical need_allocate

! Shared variables for convenience.  Not used between routines.

   integer vshape(9)

! Local named literals.

   character(1), parameter :: null  = char (0)		! char constants
   character(1), parameter :: blank = ' '
   logical,      parameter :: string_flag = .true.	! string var indicator

! Default missing values for matching to the output memory data types.

   real(dp), parameter :: default_vmiss_f32 = -huge(1.0)
   real(dp), parameter :: default_vmiss_dbl = -huge(1D0)

! Generic interface.

   interface read_netcdf_var
      module procedure &
         read_netcdf_var_1d, read_netcdf_var_1d_f32, &
         read_netcdf_var_2d, read_netcdf_var_2d_f32, &
         read_netcdf_var_3d, read_netcdf_var_3d_f32, &
         read_netcdf_var_4d, read_netcdf_var_4d_f32, &
         read_netcdf_text_1d
   end interface read_netcdf_var

contains

   include 'read_netcdf_var.double.f90'
   include 'read_netcdf_var.real32.f90'
   include 'read_netcdf_var.text.f90'
   include 'read_netcdf_var.setup.f90'
   include 'read_netcdf_var.alloc_check.f90'
   include 'read_netcdf_var.read_check.f90'
   include 'read_netcdf_var.check_nc.f90'

end module read__netcdf_var
