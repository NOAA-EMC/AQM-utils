!------------------------------------------------------------------------------
!
! var_create_x -- Add a new variable to the Netcdf file.
!		  Support routine for the writevar* subroutines.
!
! INTERNAL USE ONLY
!
! 2009-feb-05	Fix define bug with unlimited dimension.
!		Always inquire before adding a new dimension.  If reverse,
!		  and dim is unlimited, Netcdf will throw an unexpected error.
! 2009-feb-16	Protocol change.  Do not automatically exit define mode
!		  at the end of each var_create_xxx.  Caller is responsible.
! 2010-jul-02	Name change to support the var_create generic interface.
! 2010-dec-16	Use F90 conforming declarations for bytes, shorts, and doubles.
!		Switch to F90 preferred character(*) declarations.
! 2015-feb-18	Add specific diagnostic for duplicate variable create error.
!
! Usage:
!
! var_create_* and writevar* subroutines call this generic routine
! to add a new variable to the Netcdf file.  See writevar comments
! for details about var_expr and the other common arguments.  This
! routine does *not* handle data.
!
! On entry:  The Netcdf file must be open.  File mode is not required.
! On exit:   The file is in define mode for missing_value to be added.
!
! This routine also adds new dimensions and standard attributes, as
! needed.  Dimensions must be exactly the same size as previously
! created dimensions with the same names, if any.
!
! To define one dimension as unlimited, give the special code
! NF90_UNLIMITED in place of the dimension size in vshape.
!
! Creation of the standard attributes "long_name" and "units" are
! suppressed if the corresponding input strings are blank.
!
! This version does not yet correctly handle scalar variables with
! no dimensions.  This should be added when needed.
!
!------------------------------------------------------------------------------

subroutine var_create_x (var_expr, vtype, vshape, long_name, units, var_id)

   use netcdf
   use parse_varexp_mod
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vtype		! NF90 data type code
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   integer,      intent (out) :: var_id		! return var ID to caller

! Local variables.

   character(max_name) varname, dimnames(size (vshape))

   integer ndims_data, ndims, di, status, dimsize
   integer dimids(size (vshape))

   character dstatus*3   		!*** for tests only, OK to leave defined

! Parse and validate the variable specification: variable name and dimensions.

   call parse_varexp (var_expr, varname, dimnames, ndims)
   					! prints message and aborts on error

   ndims_data = size (vshape)		! get presumed rank of data array

! This check may be preempted by parse_varexp.  Check subroutine...?

   if (ndims /= ndims_data) then	! number of dimensions must match array
      print *
      print *, '*** var_create: Abort: Number of dimensions specified', &
         ' is different than data array.'
      print *, '*** Variable = ', trim (var_expr)
      write (*, '(a, i0)') ' *** Specified dimensions = ', ndims
      write (*, '(a, i0)') ' *** Array dimensions =     ', ndims_data
      stop 99
   end if

! Add each dimension to the Netcdf file.  Verify any that are pre-existing.
! Also collect the dimension ID's for the call to nf90_def_var.

!!   print *
!!   print *, 'Add dimensions for ', trim (var_expr)	! ******* TEST ONLY
!!   write (*, '(a, 99i7)') ' vshape =', vshape		! ******* TEST ONLY

   call ensure_define_mode			! ensure define mode for mods

   do di = 1, ndims
      status = nf90_inq_dimid (fid, dimnames(di), dimids(di))
						! inquire by dim name; get ID

! If dimension not yet defined, then add this dimension..

      if (status == nf90_ebaddim) then
         dstatus = 'new'
!!	 write (*,'(a,16x,a,4i5)') ' ** nf90_def_dim call: name, di, size =', &
!!	    trim (dimnames(di)), di, vshape(di)		! ******* TEST ONLY

         call check (nf90_def_dim (fid, dimnames(di), vshape(di), dimids(di)))

!!	 write (*,'(a,4i5)') ' ** nf90_def_dim:      name, di, size, dimid, ' &
!!	    // 'status = ' // trim (dimnames(di)), di, vshape(di), dimids(di), &
!!	    status

! If dimension was previously defined, then check for consistent size.

      else
         dstatus = 'old'
         call check (status)			! catch other possible errors

         call check (nf90_inquire_dimension (fid, dimids(di), len = dimsize))
						! get previous dimension size

         if (dimsize /= vshape(di)) then	! old and new sizes must match
            print *
            print *, '*** var_create: Abort: Dimension was previously', &
               ' defined with different size.'
            print *, '*** Variable = ', trim (var_expr)
            write (*, '(a, i0)') ' *** Previous dimension size for ' &
               // trim (dimnames(di)) // ' = ', dimsize
            write (*, '(a, i0)') ' ***      New dimension size for ' &
               // trim (dimnames(di)) // ' = ', vshape(di)
            stop 99
         end if
      end if

!!    write (*, '(a,i2,3x, a9,a, i2,i7, 2x,a)') ' dim', di, dimnames(di), &
!!	 '  dimid =', dimids(di), vshape(di), dstatus	! ******* TEST ONLY
   end do

! Add the variable definition and standard attributes.

   status = nf90_def_var (ncid = fid, name = varname, xtype = vtype, &
      dimids = dimids, varid = var_id)

   if (status == nf90_enameinuse) then
      print *
      print *, '*** var_create: Abort: Can not create a variable twice with' &
         // ' the same name.'
      print *, '*** Var name = ' // trim (varname)
      call exit (1)
   end if

   call check (status)				! handle other possible errors

   if (long_name /= ' ') then			! blank suppresses attribute
      call check (nf90_put_att (fid, var_id, 'long_name', long_name))
   end if

   if (units /= ' ') then			! blank suppresses attribute
      call check (nf90_put_att (fid, var_id, 'units', units))
   end if

! Return to caller, but leave file in define mode for missing_value.

end subroutine var_create_x


!------------------------------------------------------------------------------
!
! var_create_int, _float, etc. -- Create a new data variable in a single call.
!
! Also write standard attributes, but DO NOT write data.
!
! Usage:
!
! Used both internally and externally to this module.  Most comments
! under the generic var_create apply.  Also see writevar comments for
! details about var_expr and the other common arguments.
!
! Call these public routines to define new variables in the Netcdf
! file, but defer writing data.  On exit, file is left in define mode.
!
!------------------------------------------------------------------------------

subroutine var_create_byte (var_expr, vshape, long_name, units, vmiss, varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   integer(i8),  intent (in ) :: vmiss		! missing value attribute;
   						!   0 = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Create the new variable definition in the Netcdf file, with attributes.

   call var_create_x (var_expr, nf90_byte, vshape, long_name, units, varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= 0) then				! zero suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_byte


!------------------------------------------------------------------------------

subroutine var_create_dbl (var_expr, vshape, long_name, units, vmiss, varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   real(dp),     intent (in ) :: vmiss		! missing value attribute;
   						!   0 = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Create the new variable definition in the Netcdf file, with attributes.

   call var_create_x (var_expr, nf90_double, vshape, long_name, units, varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= 0) then				! zero suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_dbl


!------------------------------------------------------------------------------

subroutine var_create_float (var_expr, vshape, long_name, units, vmiss, varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
      						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   real,         intent (in ) :: vmiss		! missing value attribute;
   						!   0 = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Create the new variable definition in the Netcdf file, with attributes.

   call var_create_x (var_expr, nf90_float, vshape, long_name, units, varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= 0) then				! zero suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_float


!------------------------------------------------------------------------------

subroutine var_create_int (var_expr, vshape, long_name, units, vmiss, varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   integer,      intent (in ) :: vmiss		! missing value attribute;
   						!   0 = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Create the new variable definition in the Netcdf file, with attributes.

   call var_create_x (var_expr, nf90_int, vshape, long_name, units, varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= 0) then				! zero suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_int


!------------------------------------------------------------------------------

subroutine var_create_short (var_expr, vshape, long_name, units, vmiss, varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   integer(i16), intent (in ) :: vmiss		! missing value attribute;
   						!   0 = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Create the new variable definition in the Netcdf file, with attributes.

   call var_create_x (var_expr, nf90_short, vshape, long_name, units, varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= 0) then				! zero suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_short


!------------------------------------------------------------------------------
!
! var_create_str:  Special handling is used to store Fortran strings
! in character*1 arrays.  An extra dimension for string length is added.
!
!------------------------------------------------------------------------------

subroutine var_create_str (var_expr, strlen, vshape, long_name, units, vmiss, &
      varid)

   use netcdf
   implicit none

   character(*), intent (in ) :: var_expr	! name and subscripts of array
   integer,      intent (in ) :: strlen		! allocated string length
   integer,      intent (in ) :: vshape(:)	! shape (list of dim. sizes);
   						! NF90_UNIMITED = unlimited dim.
   character(*), intent (in ) :: long_name	! long name attribute;
   						!   blank = do not include
   character(*), intent (in ) :: units		! units attribute string;
   						!   blank = do not include
   character(*), intent (in ) :: vmiss		! missing value attribute;
   						!   blank = do not include
   integer,      intent (out) :: varid		! netcdf id for new variable

! Local variables.

   character(max_name) var_exp_plus		! var expression with extra dim.
   character*20 dim_name			! fabricated new dimension name
   integer p1
   integer shape_plus(size (vshape) + 1)	! shape array with extra dim.

! Add the extra string length dimension to the var subscript expression.

! The extra dimension name in the Netcdf file will be "len99"
! where 99 is replaced by the actual allocated string length.

   write (dim_name, '(a,i0)') 'len', strlen	! create new dimension name
   p1 = index (var_expr, '(')			! find the open parenthesis

   var_exp_plus = var_expr(1:p1) // trim (dim_name) // ',' // var_expr(p1+1:)
   					! insert extra name in middle of string

! Add the extra string length dimension to the shape array.

   shape_plus(1) = strlen			! string length is dim #1
   shape_plus(2:) = vshape(:)			! insert the regular dim sizes

! Create the new variable definition in the Netcdf file, with attributes.

!!   print *, 'var_create_str:  extra dim name   = ', trim (dim_name)
!!   print *, 'var_create_str:  var_exp_plus     = ', trim (var_exp_plus)
!!   print *, 'var_create_str:  shape_plus array = ', shape_plus
!!   print *, 'var_create_str:  Call var_create.'

   call var_create_x (var_exp_plus, nf90_char, shape_plus, long_name, units, &
      varid)

! Add the missing_value attribute, type specific.

   if (vmiss /= ' ') then			! blank suppresses the attribute
      call check (nf90_put_att (fid, varid, 'missing_value', vmiss))
   end if					! vmiss type sets attribute type

end subroutine var_create_str
