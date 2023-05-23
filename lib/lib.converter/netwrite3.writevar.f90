!------------------------------------------------------------------------------
!
! writevar -- Create and write Netcdf file variables with a single call.
!
! 3.08	2009-feb-16	Protocol change.  Always ensure data mode before
!			  writing data.
! 3.11	2010-feb-26	Support generic writevar; rename original to
!			  writevar_3d_float.
!			Convert more subroutines with var_create_* to reduce
!			  code size.
!			Add full support for 1-D to 5-D doubles, floats,
!			  integers, and shorts.
! 3.16	2012-feb-21	Add workaround for substring passing bug in some
!			  compilers such as Intel fortran v11-v12.1,
!			  and Sun fortran 8.3.
!
! Usage:
!
! Call these routines after netcreate3, to write new variables
! into the Netcdf file.  Use multiple calls for several variables.
!
! Call with the generic name "writevar" for all data types and
! dimensionalities.
!
! Notes:
!
! The variable name and dimensions are all specified together in the
! string var_expr, such as this.  The order of dimensions must be the
! same as the data array in the Fortran calling program:
!
!	var_expr = 'precip(time, lat, lon)'
!
! Named dimensions in var_expr will be created as needed.  All
! variable and dimension names are CASE SENSITIVE!
!
! Everything in the var_expr follows normal Fortran syntax rules,
! except for case sensitivity.  Spaces are allowed anywhere except
! within names.
!
! Dimensions must be fully consistent with previously created
! dimensions by the same names, if any.
!
! Special array subsetting, offsetting, and reshaping must be handled
! in the calling program.  This routine handles only a normal
! rectangular hyperslab array with origin at (1, 1, 1, ..., 1).
!
! This version does not yet correctly handle scalar variables with
! no dimensions.  This should be added when needed.
!
!------------------------------------------------------------------------------
!
! Subscript ordering:
!
! When calling this routine, the order of dimensions must be the same
! as the data array in the Fortran calling program.  However, the
! subscripts in the Netcdf file will appear REVERSED from the Fortran
! ordering:
!
!    call writevar:	precip(time, lat, lon)
!
!    ncdump:		precip(lon, lat, time)
!
! The reason for this apparent reversal is to preserve the storage
! order of dimensions, which can affect performance.  We are forced
! to follow the Fortran convention when writing the file, and the
! C convention when reading the file with C-based applications like
! ncdump and NCL.
!
! Note in this example, that in both input and output, the fastest
! moving dimension is "time", following the respective C and Fortran
! conventions.
!
!------------------------------------------------------------------------------

subroutine writevar_1d_dbl (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*),     intent(in) :: varexp	! name and subscripts of array
   character(*),     intent(in) :: long_name	! long name attribute
   character(*),     intent(in) :: units	! units attribute string
   double precision, intent(in) :: vdata(:)	! data array sized to
   						!   intended dimensions
   double precision, intent(in) :: vmiss	! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_dbl (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_1d_dbl

!------------------------------------------------------------------------------

subroutine writevar_2d_dbl (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*),     intent(in) :: varexp	! name and subscripts of array
   character(*),     intent(in) :: long_name	! long name attribute
   character(*),     intent(in) :: units	! units attribute string
   double precision, intent(in) :: vdata(:,:)	! data array sized to
   						!   intended dimensions
   double precision, intent(in) :: vmiss	! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_dbl (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_2d_dbl

!------------------------------------------------------------------------------

subroutine writevar_3d_dbl (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*),     intent(in) :: varexp	! name and subscripts of array
   character(*),     intent(in) :: long_name	! long name attribute
   character(*),     intent(in) :: units	! units attribute string
   double precision, intent(in) :: vdata(:,:,:)	! data array sized to
   						!   intended dimensions
   double precision, intent(in) :: vmiss	! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_dbl (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_3d_dbl

!------------------------------------------------------------------------------

subroutine writevar_4d_dbl (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*),     intent(in) :: varexp	! name and subscripts of array
   character(*),     intent(in) :: long_name	! long name attribute
   character(*),     intent(in) :: units	! units attribute string
   double precision, intent(in) :: vdata(:,:,:,:)  ! data array sized to
   						   !   intended dimensions
   double precision, intent(in) :: vmiss	! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_dbl (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_4d_dbl

!------------------------------------------------------------------------------

subroutine writevar_5d_dbl (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*),     intent(in) :: varexp	! name and subscripts of array
   character(*),     intent(in) :: long_name	! long name attribute
   character(*),     intent(in) :: units	! units attribute string
   double precision, intent(in) :: vdata(:,:,:,:,:)  ! data array sized to
   						     !   intended dimensions
   double precision, intent(in) :: vmiss	! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_dbl (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_5d_dbl

!------------------------------------------------------------------------------

subroutine writevar_1d_float (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   real,         intent(in) :: vdata(:)		! data array sized to
   						!   intended dimensions
   real,         intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_float (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_1d_float

!------------------------------------------------------------------------------

subroutine writevar_2d_float (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   real,         intent(in) :: vdata(:,:)	! data array sized to
   						!   intended dimensions
   real,         intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_float (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_2d_float

!------------------------------------------------------------------------------

subroutine writevar_3d_float (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   real,         intent(in) :: vdata(:,:,:)	! data array sized to
   						!   intended dimensions
   real,         intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_float (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_3d_float

!------------------------------------------------------------------------------

subroutine writevar_4d_float (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   real,         intent(in) :: vdata(:,:,:,:)	! data array sized to
   						!   intended dimensions
   real,         intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_float (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_4d_float

!------------------------------------------------------------------------------

subroutine writevar_5d_float (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   real,         intent(in) :: vdata(:,:,:,:,:)	! data array sized to
   						!   intended dimensions
   real,         intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_float (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_5d_float

!------------------------------------------------------------------------------

subroutine writevar_1d_int (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer,      intent(in) :: vdata(:)		! data array sized to
   						!   intended dimensions
   integer,      intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_int (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_1d_int

!------------------------------------------------------------------------------

subroutine writevar_2d_int (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer,      intent(in) :: vdata(:,:)	! data array sized to
   						!   intended dimensions
   integer,      intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_int (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_2d_int

!------------------------------------------------------------------------------

subroutine writevar_3d_int (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer,      intent(in) :: vdata(:,:,:)	! data array sized to
   						!   intended dimensions
   integer,      intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_int (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_3d_int

!------------------------------------------------------------------------------

subroutine writevar_4d_int (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer,      intent(in) :: vdata(:,:,:,:)	! data array sized to
   						!   intended dimensions
   integer,      intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_int (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_4d_int

!------------------------------------------------------------------------------

subroutine writevar_5d_int (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer,      intent(in) :: vdata(:,:,:,:,:)	! data array sized to
   						!   intended dimensions
   integer,      intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_int (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_5d_int

!------------------------------------------------------------------------------

subroutine writevar_1d_short (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer*2,    intent(in) :: vdata(:)		! data array sized to
   						!   intended dimensions
   integer*2,    intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_short (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_1d_short

!------------------------------------------------------------------------------

subroutine writevar_2d_short (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer*2,    intent(in) :: vdata(:,:)	! data array sized to
   						!   intended dimensions
   integer*2,    intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_short (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_2d_short

!------------------------------------------------------------------------------

subroutine writevar_3d_short (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer*2,    intent(in) :: vdata(:,:,:)	! data array sized to
   						!   intended dimensions
   integer*2,    intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_short (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_3d_short

!------------------------------------------------------------------------------

subroutine writevar_4d_short (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer*2,    intent(in) :: vdata(:,:,:,:)	! data array sized to
   						!   intended dimensions
   integer*2,    intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_short (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_4d_short

!------------------------------------------------------------------------------

subroutine writevar_5d_short (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent(in) :: varexp		! name and subscripts of array
   character(*), intent(in) :: long_name	! long name attribute
   character(*), intent(in) :: units		! units attribute string
   integer*2,    intent(in) :: vdata(:,:,:,:,:)	! data array sized to
   						!   intended dimensions
   integer*2,    intent(in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
 
   call var_create_short (varexp, shape(vdata), long_name, units, vmiss, varid)
   				! define new variable & write standard attribs.

   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vdata))   ! write entire data array

end subroutine writevar_5d_short

!------------------------------------------------------------------------------

! NOTE:  For all writevar_**_str routines, the compiler must process the
! character string length equivalent to one extra dimension on the left
! side (fastest moving).  This assumption is *not* currently checked.

subroutine writevar_1d_str (varexp, long_name, units, vdata, vmiss)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varexp		! name and subscripts of array
   character(*), intent (in) :: long_name	! long name attribute
   character(*), intent (in) :: units		! units attribute string
   character(*), intent (in) :: vdata(:)	! data array sized to
   						!   intended dimensions
   character(*), intent (in) :: vmiss		! missing value attribute
   						! 0 = DO NOT ADD MISSING VALUE
   
   integer varid				! local variable
   
   character(len(vdata)), dimension (size(vdata)) :: vcopy  ! local copy buffer
 
   call var_create_str (varexp, len (vdata), shape (vdata), long_name, units, &
      vmiss, varid)		! define new variable & write standard attribs.

!!   print *, 'writevar_1d_str: shape (vdata) = ', shape (vdata)
!!   print *, 'writevar_1d_str: len   (vdata) = ', len (vdata)
!!   print *, 'writevar_1d_str: size  (vdata) = ', size (vdata)
!!   print *, 'writevar_1d_str: Call nf90_put_var.'
   
   vcopy = vdata		! make local copy of character array to work
   				! around substring passing bug in some compilers
   
   call netwrite3_sync				   ! switch to data mode, etc.
   call check (nf90_put_var (fid, varid, vcopy))   ! write entire data array

end subroutine writevar_1d_str
