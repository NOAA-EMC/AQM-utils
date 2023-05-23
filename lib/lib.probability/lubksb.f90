!------------------------------------------------------------------------------
!
! 2019-apr-10	lubksb.f:    Original fortran 77 version from Tom Hamill.
!		Originally from Numerical Recipes.
!
! 2019-jun-20	lubksb.f90:  Minimal conversion to fortran 90.  By Dave Allured.
!		Add module wrapper.  Declare argument intents.
!		Convert all real*8 to double precision.
!
!------------------------------------------------------------------------------

module lubksb_mod
contains

SUBROUTINE lubksb(a,n,np,indx,b)
      implicit none

      INTEGER,          intent(in)  :: n, np, indx(n)
      double precision, intent(in)  :: a(np,np)
      double precision, intent(out) :: b(n)

      INTEGER i,ii,j,ll
      double precision sum

      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return

end subroutine lubksb
end module lubksb_mod
