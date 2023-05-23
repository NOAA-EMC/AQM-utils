!------------------------------------------------------------------------------
!
! 2019-apr-10	ludcmp.f:    Original fortran 77 version from Tom Hamill.
!		Originally from Numerical Recipes.
!
! 2019-jun-20	ludcmp.f90:  Minimal conversion to fortran 90.  By Dave Allured.
!		Add module wrapper.  Declare argument intents.
!		Convert all real*8 to double precision.
!
!------------------------------------------------------------------------------

module ludcmp_mod
contains

SUBROUTINE ludcmp(a,n,np,indx,d,istat)
      implicit none

      INTEGER,          intent(in)  :: n, np
      INTEGER,          intent(out) :: indx(n), istat
      double precision, intent(out) :: d, a(np,np)

      INTEGER,          PARAMETER :: NMAX = 1500
      double precision, PARAMETER :: TINY = 1.0e-20

      INTEGER i,imax,j,k
      double precision aamax,dum,sum,vv(NMAX)

      d=1.
      istat = 0
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) then
           print *, 'singular matrix in ludcmp'
           istat = -1
           goto 20
        endif
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
20    return

end subroutine ludcmp
end module ludcmp_mod
