!-----------------------------------------------------------------------------
!
! shell-sort.bc.f90 -- Shell sort for a 1-D data list.
!
! 1.00	2007-may-03	shell-sort.f90:
!			F90 index sort implementation of Sedgewick's method.
! 1.01	2010-may-27	Add support for integer and real data lists.
!			Add generic interface.
!			Change module name to underscore convention.
! 1.02	2014-mar-06	Add support for double precision.
!
! ****	2014-mar-24	shell-sort.bc.f90:	
!			Special version for bias correction.
!			Match undocumented Matlab 8.3 behavior of "sort"
!			  function, "descend" mode, for comparison testing.
!			Primary:    DESCENDING by value.
!			Secondary:  Ties ASCENDING by original list indices.
!			Also note, the primary sortation in this version is
!			  REVERSED from the original versions 1.00 to 1.02.
!			Note subroutine name changes.
!
! ****	2014-jun-20	shell-sort.descending.f90:
!			Change file name and module name to fix conflict.
!			The original ascending shell-sort.f90 is needed
!			  elsewhere in the bias correction program.
!
! Reference:  Shell sort.
! Implementation of Robert Sedgewick, Princeton University.
! http://www.cs.princeton.edu/~rs/shell/
! (Note, 2007-may-03:  Sort code on this website is buggy.)
!
! Notes:
!
! This version leaves the original list intact, but returns a sorted
! list of pointers (integer subscripts) into the original list.  The
! items are then obtained in sorted order by stepping through the
! pointer list.
!
! When sorting strings, the character collating sequence depends
! on the compiler, platform, and config settings.  Watch for
! unexpected results when the input strings include control
! characters, 8-bit extended ascii, unicode, or other encodings.
! Tabs and spaces generally do not collate the same.  The sort
! order of this routine may differ from the "sort" shell command.
!
! For string sort, the speed could be generally improved with a
! local supplemental array of string lengths, and by comparing
! only list(p)(1:len(p)) rather than the whole string list(p).
!
!-----------------------------------------------------------------------------

module index__sort_descending
   implicit none

   private			  ! visibility control:
   public index_sort_descending   ! all private except for generic interface

   interface index_sort_descending		! generic interface...
      module procedure index_sort_double	!    note the name change
      module procedure index_sort_integer	!    on the generic!
      module procedure index_sort_real
      module procedure index_sort_string
   end interface index_sort_descending

! Sedgewick's increment sequence, chosen for good performance in general cases.

   integer :: increments(16) = (/ 1391376, 463792, 198768, 86961, &
      33936, 13776, 4592, 1968, 861, 336, 112, 48, 21, 7, 3, 1 /)

contains

!-----------------------------------------------------------------------------

subroutine index_sort_double (list, isort)
   implicit none

   double precision, intent (in ) :: list(:)	! data list to be sorted
   integer,          intent (out) :: isort(:)	! list indices, in sorted order

! Local variables.

   double precision cur
   integer h, i, j, k, p, pcur, list_size

! Fill index array with unsorted pointers into the input array.

   list_size = size (list)

   do p = 1, list_size
      isort(p) = p
   end do

! Now sort the index array on the referenced strings.

   do k = 1, size (increments)		! loop on decreasing Shell increments
      h = increments(k)

      do i = h+1, list_size		! interleaved loop over all last items
         pcur = isort(i)		! to be h-sorted
         cur = list(pcur)
         j = i

         do while (j > h)		! insertion sort for item isort(i)
            p = isort(j-h)		! into h-slice (i-h)
            if (list(p) > cur) exit	! sort items into DESCENDING order
            if (list(p) == cur) then	! but ties sort ASCENDING by indices
               if (p < pcur) exit
            end if
            isort(j) = p		! slide smaller items up one by one
            j = j - h
         end do

         isort(j) = pcur		! insert the target item into position
      end do

   end do

end subroutine index_sort_double

!-----------------------------------------------------------------------------

subroutine index_sort_integer (list, isort)
   implicit none

   integer, intent (in ) :: list(:)		! data list to be sorted
   integer, intent (out) :: isort(:)		! list indices, in sorted order

! Local variables.

   integer cur
   integer h, i, j, k, p, pcur, list_size

! Fill index array with unsorted pointers into the input array.

   list_size = size (list)

   do p = 1, list_size
      isort(p) = p
   end do

! Now sort the index array on the referenced strings.

   do k = 1, size (increments)		! loop on decreasing Shell increments
      h = increments(k)

      do i = h+1, list_size		! interleaved loop over all last items
         pcur = isort(i)		! to be h-sorted
         cur = list(pcur)
         j = i

         do while (j > h)		! insertion sort for item isort(i)
            p = isort(j-h)		! into h-slice (i-h)
            if (list(p) > cur) exit	! sort items into DESCENDING order
            if (list(p) == cur) then	! but ties sort ASCENDING by indices
               if (p < pcur) exit
            end if
            isort(j) = p		! slide smaller items up one by one
            j = j - h
         end do

         isort(j) = pcur		! insert the target item into position
      end do

   end do

end subroutine index_sort_integer

!-----------------------------------------------------------------------------

subroutine index_sort_real (list, isort)
   implicit none

   real,    intent (in ) :: list(:)		! data list to be sorted
   integer, intent (out) :: isort(:)		! list indices, in sorted order

! Local variables.

   real cur
   integer h, i, j, k, p, pcur, list_size

! Fill index array with unsorted pointers into the input array.

   list_size = size (list)

   do p = 1, list_size
      isort(p) = p
   end do

! Now sort the index array on the referenced strings.

   do k = 1, size (increments)		! loop on decreasing Shell increments
      h = increments(k)

      do i = h+1, list_size		! interleaved loop over all last items
         pcur = isort(i)		! to be h-sorted
         cur = list(pcur)
         j = i

         do while (j > h)		! insertion sort for item isort(i)
            p = isort(j-h)		! into h-slice (i-h)
            if (list(p) > cur) exit	! sort items into DESCENDING order
            if (list(p) == cur) then	! but ties sort ASCENDING by indices
               if (p < pcur) exit
            end if
            isort(j) = p		! slide smaller items up one by one
            j = j - h
         end do

         isort(j) = pcur		! insert the target item into position
      end do

   end do

end subroutine index_sort_real

!-----------------------------------------------------------------------------

subroutine index_sort_string (list, isort)
   implicit none

   character(*), intent (in ) :: list(:)	! list of strings to be sorted
   integer,      intent (out) :: isort(:)	! list indices, in sorted order

! Local variables.

   character(len(list)) :: cur
   integer h, i, j, k, p, pcur, list_size

! Fill index array with unsorted pointers into the input array.

   list_size = size (list)

   do p = 1, list_size
      isort(p) = p
   end do

! Now sort the index array on the referenced strings.

   do k = 1, size (increments)		! loop on decreasing Shell increments
      h = increments(k)

      do i = h+1, list_size		! interleaved loop over all last items
         pcur = isort(i)		! to be h-sorted
         cur = list(pcur)
         j = i

         do while (j > h)		! insertion sort for item isort(i)
            p = isort(j-h)		! into h-slice (i-h)
            if (list(p) > cur) exit	! sort items into DESCENDING order
            if (list(p) == cur) then	! but ties sort ASCENDING by indices
               if (p < pcur) exit
            end if
            isort(j) = p		! slide smaller items up one by one
            j = j - h
         end do

         isort(j) = pcur		! insert the target item into position
      end do

   end do

end subroutine index_sort_string

end module index__sort_descending
