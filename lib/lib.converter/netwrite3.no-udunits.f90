!------------------------------------------------------------------------------
!
! netwrite3.f90 -- Helper routines to write Netcdf files.
!
! By Dave Allured, NOAA/OAR/ESRL/PSD/CAB, CU/CIRES/CDC.
!
! Ver	Date		Notes
! 3.00	2008-jul-03	Initial development version, with diagnostics.
!			Adapted from netwrite2.f90 v2.43.
! 3.01	2008-jul-04	Clean up comments and obsolete routines.
!			Use varname rather than var_id in support routines.
!			Fix allocation of extra header space.
!			New protocol:  Select file mode on most user entry
!			  points; leave undefined on exits.
! 3.02	2008-jul-04	Add writevar subtypes: 1D & 2D float & double, etc.
! 3.03	2008-dec-10	Add subtypes: 1D integer, 4D float.
!			Start new series of public var creators to define large
!			  arrays without writing data.  Add var_create_float.
! 3.04	2008-dec-15	Compatibility fix for XLF 8.1 compiler, it can't accept:
!			  integer, parameter :: ndims = size (shape (vdata))
!			Replace usage with automatic arrays.
!			var_create_xxx always exits define mode.
!			Streamline var_create.
!
! 3.05	2009-jan-02	Add subtype: 1D string, equivalent to 2D character.
!			Add automatic extra dimension for string lengths.
!			Blank input suppresses long_name and units attributes.
!			Fix up module data section and visibility controls.
!			"check" becomes module private, also all module vars.
!			Fix header comments.
! 3.06	2009-jan-28	Add subtypes for variable and global attributes.
! 3.07	2009-feb-05	Break up entire module into several include files.
!			Add var_create subtypes: byte, short, double.
!			Allow unlimited dimension.  Fix related var_create bug.
! 3.08	2009-feb-16	Protocol change, reverse mode protocol from v3.04.
!			Programs using direct data writes (nf90_put_var)
!			  must call netwrite3_sync first, to ensure data mode.
!			Add support for standard unlimited time dimension:
!			  Routines add_time_dimension, time_step.
! 3.09	2009-feb-17	Build configuration change only, fix dependency problem.
!			v309 is source and functional equivalent to v308.
!			File netwrite3.f90 is split, becomes makefile target
!			  made automatically.
!			Assembly is now by concatenates, rather than includes.
!			Must run local makefile whenever any segment is changed.
!
! 3.10	2010-feb-26	Switch to conventional includes and visibility controls.
!			Make single generic interface for all writevar's.
!			Add full support for 1-D to 5-D doubles, floats,
!			  integers, and shorts.
!			Access udunits via new F90 module, rather than includes.
! 3.11	2010-feb-26	Fix visibility problems; hide private module variables.
! 3.12	2010-may-05	Add generic interfaces for attribute write routines.
!			Add write_var_att subtypes: byte and short.
!			Define local kind params for i8 (byte) and i16 (short).
!			Switch some declarations to F90 standard char(*) form.
!			Include Netcdf version string in history attribute.
!			Prevent multiple utopens with utisinit_f, stop warnings.
! 3.13	2010-jul-02	Add generic interface for var_create routines.
!			Switch to no-clobber mode, do not overwrite an
!			  existing Netcdf file.
! 3.14	2010-dec-16	Add overwrite argument to netcreate3 (Aug. 5).
!			Switch to F90 preferred kind declarations.
!			Convert all non-conforming integer*1 and *2 statements.
!			Fix minor warning for gfortran -Wcharacter-truncation.
!
! 3.15	2011-oct-04	Two bug fixes in time_step.
!			Change one hard coded reference to 'time' var name,
!			  fix support for altername names for time var.
!			Add call to netwrite3_sync before writing time data.
!			Was previously hidden by preceeding writevar calls.
! 3.16	2012-feb-21	Add workaround for substring passing compiler bug
!			  that affects writevar_1d_str and nf90_put_var.
! 3.17	2013-jul-18	Switch from custom utisinit_f to standard utisopen func.
! 3.18	2014-may-12	Add verbosity control to netcreate3.
!
! 3.19	2015-feb-18	Bug fix for v3.18.
!			Switch order of optional arguments in netcreate3,
!			  for backward compatibility.
!			Add diagnostic for duplicate variable create error.
!
! 3.19.1  2015-feb-18	*** Side version without udunits.    ***
!			*** Also missing netwrite3.time.f90. ***
!			*** For bias correction project.     ***

module netwrite3_version
   character(*), parameter :: version = 'netwrite3.f90 v3.19.1'
end module netwrite3_version

! Notes:
!
! This is a general purpose Netcdf writer for Fortran 90/95.  It
! simplifies writing plain Netcdf files.  A simple data array can be
! defined and written in a single Fortran call.  A complete Netcdf
! file can be written with only three Fortran calls.
!
! Features:
!
! * Any number of array variables can be written to a single Netcdf
!   file.
!
! * Arrays of any type and dimension may be supported by adding the
!   appropriate subtype routines, as needed.
!
! * Named dimensions are automatically created and managed.  They
!   may be shared between variables without user intervention.
!
! * Variable and global attributes are easily added and updated.
!
! * The standard attributes long_name, units, and missing_value are
!   included with every added variable, with no extra calls.
!
! * The global attributes title and history are automatically
!   included.  History includes a time stamp, version info, and the
!   user's multi-line history comments.
!
! * Pre-allocation of file header space is supported for optimizing
!   write performance.
!
! * Incremental writes of large array variables are supported by
!   var_create_* routines, which pre-allocate without writing data.
!
! * Extra dimensions "len99" etc. for string length are automatically
!   added to the output file, when Fortran string arrays are written
!   to the output file.  These extra length dimensions are required
!   by classic Netcdf 3 files.
!
! * Several operational details of Netcdf are simplified and hidden
!   from the user, such as file modes, variable ID's, and the
!   h_minfree nexus.
!
! * Direct calls to the Netcdf API may be intermixed with this
!   interface, with care, for adding customized file operations.
!
! * The output format is Netcdf 3 classic (standard 32 bit offsets),
!   for greatest portability.
!
! * This interface is theoretically compatible with both the Netcdf 3
!   and Netcdf 4 API libraries.
!
! Restrictions in current version:
!
! * A limited number of data types, dimensionality, and attribute
!   types are supported in this early version.  However, it is rather
!   easy to add subtype routines as needed.
!
! * The record dimension and record variables are not supported,
!   except through custom Netcdf programming.
!
! * Coordinate variables are not specifically supported.  The user
!   must manually include the proper naming and attributes to get
!   correct time and spatial coordinate variables.
!
! * The output file is not strictly compliant with any of the Netcdf
!   formal conventions that are currently published.  However, there
!   is enough included so that output files can be readily used by
!   Fortran, NCL, Grads, and other common graphics tools and language
!   interfaces.
!
! * Most advanced Netcdf 3 and Netcdf 4 features are not currently
!   supported.
!
! Compile:  See makefile.
!
! For F90 interface, library netcdf v3.5.0 or greater is required.
!
!------------------------------------------------------------------------------

module netwrite3

!!   use udunits, only : UD_POINTER_KIND	! omit for no-udunits version
   implicit none

   public		! all module vars and procedures are public, except...

!!   private UD_POINTER_KIND			! omit for no-udunits version
   private newline, max_name, i8, i16, fid
   private save_reserve_header
!!   private time_unit_ptr			! omit for no-udunits version
   private ensure_define_mode, ensure_data_mode, check
   private var_create_x

! *** For the no-udunits version, the following actually becomes a
! *** public module routine, for the sole purpose of suppressing
! *** compiler warnings.  This is a harmless kludge.

!!   private utcheck

! Private module parameters.

   character(1), parameter :: newline = achar (10)	! linefeed:
   					! line separator for history string

   integer, parameter :: max_name = 150		! maximum length of combined
   						! var name & subscript names

! Kind parameters for non-default fortran data types.

! This custom method works for all fortran 90 compilers and up.
! It does not depend on advanced features such as the ISO_FORTRAN_ENV module.
! "Digits" is the number of *decimal digits* to obtain each data type.

   integer, parameter :: i8  = selected_int_kind (2)  ! byte:   8 bits, 2 digits
   integer, parameter :: i16 = selected_int_kind (4)  ! short: 16 bits, 4 digits

   integer, parameter :: dp = kind (1d0)	! default double precision;
   						! expect 64 bit float on
						!   common platforms
! Netcdf related state variables.

   integer fid					! save netcdf file ID

! Buffer control for requested extra header space in Netcdf file.

! This must be saved and applied AFTER the first data variable is
! defined.  If applied earlier, such as NF90_ENDDEF in netcreate3,
! then the Netcdf API will fail to remember the allocation.

   integer save_reserve_header			! header extra space, in bytes
   						! set to zero after allocation
   						! to prevent extra file rewrites
! Time dimension state variable.

!!   integer(UD_POINTER_KIND) :: time_unit_ptr	! udunits time unit object
!!   *** Omit for no-udunits version.

! Public module variables.

!!   double precision time_actual_range(2)	! time range accumulator;
!!   *** Omit for no-udunits version.
   					! valid after all time steps are written
					! valid only if time_step routine used
! Generic interfaces.

   interface var_create
      module procedure &
         var_create_byte, var_create_dbl,   var_create_float, &
         var_create_int,  var_create_short, var_create_str
   end interface var_create

   interface writevar
      module procedure &
         writevar_1d_dbl,   writevar_2d_dbl,   writevar_3d_dbl,   &
         writevar_4d_dbl,   writevar_5d_dbl,                      &
         writevar_1d_float, writevar_2d_float, writevar_3d_float, &
         writevar_4d_float, writevar_5d_float,                    &
         writevar_1d_int,   writevar_2d_int,   writevar_3d_int,   &
         writevar_4d_int,   writevar_5d_int,                      &
         writevar_1d_short, writevar_2d_short, writevar_3d_short, &
         writevar_4d_short, writevar_5d_short,                    &
         writevar_1d_str
   end interface writevar

   interface write_var_att
      module procedure &
         write_var_att_byte,  write_var_att_byte_1d,  &
         write_var_att_dbl,   write_var_att_dbl_1d,   &
         write_var_att_float, write_var_att_float_1d, &
         write_var_att_int,   write_var_att_int_1d,   &
         write_var_att_short, write_var_att_short_1d, &
         write_var_att_str
   end interface write_var_att

   interface write_global_att
      module procedure &
         write_global_att_dbl, write_global_att_float, &
         write_global_att_int, write_global_att_str
   end interface write_global_att

! Include section for module procedures.

! These includes should appear in the same directory as this
! primary module container, netwrite3.f90.

contains

   include 'netwrite3.create.f90'
   include 'netwrite3.global_atts.f90'
!!   include 'netwrite3.time.f90'		! omit for no-udunits version
   include 'netwrite3.utils.f90'
   include 'netwrite3.var_atts.f90'
   include 'netwrite3.var_create.f90'
   include 'netwrite3.writevar.f90'

end module netwrite3
