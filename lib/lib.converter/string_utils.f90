!-----------------------------------------------------------------------------
!
! string_utils.f90 -- Container module for string utility routines.
!
! This container provides an explicit module interface for
! each included procedure
!
! 1.00	2014-apr-23	Original module container.  By Dave Allured.
! 1.01	2014-sep-09	Change real_to_string from include to module.
! 1.02	2014-dec-15	Add includes for routines with their own modules.
!
! 1.03	2022-apr-16	Add parsers parse_varexp.f90 and tokenize.f90.
!
! Visibility is controlled by each individual routine.
! The default is all top-level objects are public.
!
!-----------------------------------------------------------------------------

! Include section for routines with their own modules.

   include 'parse_delimited.f90'
   include 'parse_varexp.f90'
   include 'real_to_compact_string.f90'
   include 'real_to_string.f90'
   include 'resolve_env.f90'
   include 'tokenize.f90'

! Primary container module.

module string_utils

! Incorporated by module inheritance.

   use parse_delimited_mod
   use parse_varexp_mod
   use real__to_compact_string
   use real__to_string
   use resolve__env
   use tokenize_mod

   implicit none

! Incorporated by includes.

contains

   include 'anglical.f90'
   include 'anglical_greater.f90'
   include 'anglical_match.f90'
   include 'anglical_str.f90'
   include 'count_substrings.f90'
   include 'int_to_string.f90'
   include 'lowercase.f90'
   include 'remove_quotes.f90'
   include 'replace_substring.f90'
   include 'string_to_double_dp.f90'
   include 'string_to_int.f90'
   include 'string_to_intu.f90'
   include 'string_to_real.f90'
   include 'string_to_real_dp.f90'
   include 'strip_blanks.f90'
   include 'uppercase.f90'

end module string_utils
