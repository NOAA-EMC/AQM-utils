!------------------------------------------------------------------------------
!
! get_param_module.f90 -- Container module for get_param config file routines.
!
! This container provides an explicit module interface for
! each included procedure.
!
! 3.00	2016-feb-09	Original module container.  By Dave Allured.
! 3.01	2017-jun-01	Add get_param_mdhz.f90.
!
! 2019-aug-01	Add double precision support for get_param_real.
!		Add visibility control for internal procedures.
! 2019-aug-02	Add list support: get_param_list_string, get_param_list_double.
!
!------------------------------------------------------------------------------

module get_param_module
   implicit none

! Conceal the internal module procedures.

   public
   private get_param_real_dbl, get_param_real_float
   private get_param_list_string, get_param_list_double

! Module parameter.

   logical, parameter :: nonblank = .true.	! optional argument value
   						! for get_param_string

! Module generic procedures.

   interface get_param_real
      module procedure get_param_real_dbl, get_param_real_float
   end interface get_param_real

   interface get_param_list
      module procedure get_param_list_string, get_param_list_double
   end interface get_param_list

contains

   include 'get_param_int.f90'
   include 'get_param_mdhz.f90'
   include 'get_param_real.f90'
   include 'get_param_string.f90'
   include 'get_param_yesno.f90'

   include 'get_param_list_string.f90'
   include 'get_param_list_double.f90'

end module get_param_module
