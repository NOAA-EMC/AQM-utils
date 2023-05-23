!------------------------------------------------------------------------------
!
! write_var_att -- Create or update a variable attribute.
!
! 2009-feb-16	Add double precision subtypes.
! 2010-may-05	Add byte and short subtypes.
!		Switch declarations to F90 standard character(*) form.
!
! These are optional support routines for netwrite3.  These routines
! are used to add and update attributes on defined variables in the
! current Netcdf file.
!
! Note the several flavors of write_var_att for various data
! types.  The typing resides in the argument declarations only.
!
! Usage:
!
! Refer to general usage notes under netwrite3.
!
! These routines may be called any time after the associated variable
! has been defined, to create or rewrite the variable's attributes.
! An attribute may be rewritten as often as desired.
!
!------------------------------------------------------------------------------

subroutine write_var_att_byte (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer(i8),  intent (in) :: value
   
   integer var_id

   call ensure_define_mode				    ! ensure define mode
   call check (nf90_inq_varid (fid, varname, var_id))	    ! look up var name
   call check (nf90_put_att (fid, var_id, attname, value))  ! create or update

end subroutine write_var_att_byte

!------------------------------------------------------------------------------

subroutine write_var_att_byte_1d (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer(i8),  intent (in) :: value(:)
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_byte_1d

!------------------------------------------------------------------------------

subroutine write_var_att_dbl (varname, attname, value)

   use netcdf
   implicit none
   
   character(*),     intent (in) :: varname
   character(*),     intent (in) :: attname
   double precision, intent (in) :: value
   
   integer var_id

   call ensure_define_mode				    ! ensure define mode
   call check (nf90_inq_varid (fid, varname, var_id))	    ! look up var name
   call check (nf90_put_att (fid, var_id, attname, value))  ! create or update

end subroutine write_var_att_dbl

!------------------------------------------------------------------------------

subroutine write_var_att_dbl_1d (varname, attname, value)

   use netcdf
   implicit none
   
   character(*),     intent (in) :: varname
   character(*),     intent (in) :: attname
   double precision, intent (in) :: value(:)
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_dbl_1d

!------------------------------------------------------------------------------

subroutine write_var_att_float (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   real,         intent (in) :: value
   
   integer var_id

   call ensure_define_mode				    ! ensure define mode
   call check (nf90_inq_varid (fid, varname, var_id))	    ! look up var name
   call check (nf90_put_att (fid, var_id, attname, value))  ! create or update

end subroutine write_var_att_float

!------------------------------------------------------------------------------

subroutine write_var_att_float_1d (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   real,         intent (in) :: value(:)
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_float_1d

!------------------------------------------------------------------------------

subroutine write_var_att_int (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer,      intent (in) :: value
   
   integer var_id

   call ensure_define_mode				    ! ensure define mode
   call check (nf90_inq_varid (fid, varname, var_id))	    ! look up var name
   call check (nf90_put_att (fid, var_id, attname, value))  ! create or update

end subroutine write_var_att_int

!------------------------------------------------------------------------------

subroutine write_var_att_int_1d (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer,      intent (in) :: value(:)
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_int_1d

!------------------------------------------------------------------------------

subroutine write_var_att_short (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer(i16), intent (in) :: value
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_short

!------------------------------------------------------------------------------

subroutine write_var_att_short_1d (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   integer(i16), intent (in) :: value(:)
   
   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_short_1d

!------------------------------------------------------------------------------

subroutine write_var_att_str (varname, attname, value)

   use netcdf
   implicit none
   
   character(*), intent (in) :: varname
   character(*), intent (in) :: attname
   character(*), intent (in) :: value

   integer var_id

   call ensure_define_mode
   call check (nf90_inq_varid (fid, varname, var_id))
   call check (nf90_put_att (fid, var_id, attname, value))

end subroutine write_var_att_str
