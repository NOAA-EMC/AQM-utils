!------------------------------------------------------------------------------
!
! config.f90 -- General configuration module for Fortran 90 programs.
!
! ***** THIS MODULE IS A PROTOTYPE *****
! ***** USE AS IS, OR CUSTOMIZE A COPY FOR YOUR APPLICATION *****
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2002-jul-25	Initial version, adapted from gsc globals.h.
! 1.01	2002-aug-06	Add dev_delim, path_delim, ext_delim.
! 1.02	2005-oct-14	Add dim_* names (preferred).
!			Keep backward compatibility for *_dim names.
!			Add dim_filename, fortran_eof.
! 1.03	2006-oct-25	Move Fortran error numbers to module f90_config.
!			Invoke that module and pass definitions to higher user.
!			This isolates true compiler dependencies, e.g. iostat.
! 1.04	2008-jul-15	Add newline.
! 1.05	2010-oct-26	Add a few Fortran standard kind parameters.
! 1.06	2010-dec-07	Add kind parameters for i64, dp.
!
! 2019-jul-30	Omit module f90_config, simplify build systems.
!		Programs should use F2003 standard inquiry features,
!		  such as functions is_iostat_end, is_iostat_eor.
!
! Note:	This module is used to set various system-dependent and compile-
!	dependent paramaters, such as the following.  It may also be
!	used for application-dependent global parameters.
!
!	* Default character string dimensions.
!	* Main array dimensions.
!	* System-dependent paths and constants.
!	* I/O error numbers.
!
!------------------------------------------------------------------------------

module config

! General-purpose dimensions for character strings.

   integer, parameter :: dim_token     = 100	! data items, names, etc.
   integer, parameter :: dim_filename  = 200	! full path filenames
   integer, parameter :: dim_line      = 200	! general input lines
   integer, parameter :: dim_data_line = 500	! wide data file lines

   integer, parameter :: token_dim = dim_token	! backward compatibility
   integer, parameter :: line_dim  = dim_line
   integer, parameter :: data_line_dim = dim_data_line

! File system parameters.

   character, parameter :: dev_delim  = ':'	! pathname elements
   character, parameter :: path_delim = '/'
   character, parameter :: ext_delim  = '.'

! Character literals.

   character, parameter :: newline = char (10)	! std. end of line = linefeed

! Type definitions per F90 best practices.
! Use as kind parameters, e.g. integer(i8) var.

   integer, parameter :: i8  = selected_int_kind (2)	! 8 bit integer (byte)
   						! select -99 to +99
   						! full range -128 to +127

   integer, parameter :: i16 = selected_int_kind (4)	! 16 bit integer (short)
   						! select -9999 to +9999
   						! full range -65536 to +65535

   integer, parameter :: i64 = selected_int_kind (18)	! 64 bit integer
   						! select -1e18 to +1e18
   						! full range -2^63 to +2^63-1

   integer, parameter :: dp = kind (1d0)	! default double precision
   						! expect 64 bit float;
						! not guaranteed
end module config
