!-----------------------------------------------------------------------------
!
! date_utils.f90 -- Container module for date utility routines.
!
! This container provides an explicit module interface for
! each included procedure.
!
! 1.00	2014-dec-15	Original module container.  By Dave Allured.
!			Adapted from string_utils.f90 v1.02.
! 1.01	2015-feb-08	Add date_sup internal module to facilitate dependency
!			  sequencing.
!			Fully incorporate format_date as internal module
!			  procedure.
!
! Visibility is controlled by each individual routine.
! The default is all top-level objects are public.
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------
! Internal module for basic date support.
!-----------------------------------------------------------

! This date_sup module is intended for internal support only.
! Applications should use date_utils, not date_sup.

! This module includes basic support routines needed by later sections.
! Routines in this section are incorporated by includes.

module date_sup
implicit none
contains

   include 'date_time_stamp.f90'
   include 'date_to_julian.f90'
   include 'date_ymd2_to_int.f90'
   include 'days_in_month.f90'
   include 'days_in_year.f90'
   include 'int_to_date_str.f90'
   include 'julian_to_mmdd.f90'
   include 'leap_check.f90'

end module date_sup

!-----------------------------------------------------------
! Include section for routines with their own modules.
!-----------------------------------------------------------

   include 'date_index.f90'
   include 'format_date.f90'
   include 'index_to_date.f90'
   include 'parse_time_hh_mm.f90'
   include 'season_names.f90'

!-----------------------------------------------------------
! Primary date_utils container module.
!-----------------------------------------------------------

! Routines in this section are incorporated by module inheritance.

module date_utils

   use date_sup
   use date__index
   use format__date
   use index_to_date_mod
   use parse_time_hh_mm_mod
   use season__names

end module date_utils
