!------------------------------------------------------------------------------
!
! write_global_att -- Create or update a global attribute.
!
! These are optional support routines for netwrite3.  These routines
! are used to add and update global attributes in the current Netcdf
! file.
!
! Note the several flavors of write_global_att for various data
! types.  The typing resides in the argument declarations only.
!
! Usage:
!
! Refer to general usage notes under netwrite3.
!
! These routines may be called any time after netcreate3, to create
! or rewrite global attributes.  An attribute may be rewritten as
! often as desired.
!
!------------------------------------------------------------------------------

subroutine write_global_att_dbl (attname, value)

   use netcdf
   implicit none
   
   character,        intent (in) :: attname*(*)
   double precision, intent (in) :: value

   call ensure_define_mode				! ensure define mode
   call check (nf90_put_att (fid, nf90_global, attname, value))
   							! create or update attr
end subroutine write_global_att_dbl


!------------------------------------------------------------------------------

subroutine write_global_att_float (attname, value)

   use netcdf
   implicit none
   
   character, intent (in) :: attname*(*)
   real,      intent (in) :: value

   call ensure_define_mode				! ensure define mode
   call check (nf90_put_att (fid, nf90_global, attname, value))
   							! create or update attr
end subroutine write_global_att_float


!------------------------------------------------------------------------------

subroutine write_global_att_int (attname, value)

   use netcdf
   implicit none
   
   character, intent (in) :: attname*(*)
   integer,   intent (in) :: value

   call ensure_define_mode				! ensure define mode
   call check (nf90_put_att (fid, nf90_global, attname, value))
   							! create or update attr
end subroutine write_global_att_int


!------------------------------------------------------------------------------

subroutine write_global_att_str (attname, value)

   use netcdf
   implicit none
   
   character, intent (in) :: attname*(*)
   character, intent (in) :: value*(*)

   call ensure_define_mode
   call check (nf90_put_att (fid, nf90_global, attname, value))

end subroutine write_global_att_str
