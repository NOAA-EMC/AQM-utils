      program test_readgrib2
!
!-----------------------------------------------------------------
! ABSTRACT: This routine is to write out a new grib2 file
!   Mar 2013     J.Wang   
!-----------------------------------------------------------------
!
      implicit none
!
      integer, parameter   :: max_bytes=20000000
      integer, parameter   :: nx=192,ny=94
!
      integer listsec0(2)
      integer listsec1(13)
      integer igds(5)
      integer igdstmpllen
      integer ipdstmpllen
      integer idrstmpllen
      integer idrsnum,ibmap,numcoord,ipdsnum,idefnum
      
      integer,dimension(100) :: igdstmpl
      integer,dimension(100) :: ipdstmpl
      integer,dimension(100) :: idrstmpl
!
      integer ideflist(1)
      real(4) coordlist(1)
!
      character(255) :: cout
      character*1 cgrib(max_bytes)
!
      logical*1 bmap(nx,ny)
!
      real(4),dimension(nx*ny) :: fld
      integer ifilw,i,j,lengrib,lonstt,lonlst,latstt,latlst,ierr
      integer yy,mm,dd,hh,mn,sc
      real(4) :: dxval
!
! code start
!-----------------------------------------------------------------
!
!-- set file unit
      ifilw=52
!
!-- get file name
      call getarg(1,cout)
!
!-- Open GRIB2 file
      call baopen(ifilw,trim(cout),ierr)
      print *,'cout=',trim(cout),'ierr=',ierr
!
!-- section 0: 
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table 0)  (7:ncep)
      listsec1(2)=4       ! Identification of orginating subcenter (ON388-Table C) (4:emc)
      listsec1(3)=11      ! GRIB master tables version number (Table 1.0)  (11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2) (0:ana 1:fcst 2:vrfy)
      listsec1(6)=2013    ! Reference time - Year (4 digits)
      listsec1(7)=07      ! Reference time - Month
      listsec1(8)=15      ! Reference time - Day
      listsec1(9)=00      ! Reference time - Hour
      listsec1(10)=00     ! Reference time - Minute
      listsec1(11)=00     ! Reference time - Second
      listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
       print*,'gribcreate status=',ierr
!
!-- section 3: grid definition section
      igds(1)=0           ! Source of grid definition (Table 3.0) (0:specified in the code)
      igds(2)=nx*ny       ! Number of grid points in the defined grid
      igds(3)=0           ! Number of octets for optional list of numbers defining number of points 
      igds(4)=0           ! Interpretation of list of numbers defining number of points 
!-- example: Gaussian Lat/lon
      igds(5)=40          ! Grid definition template number (Table 3.1) (0:Lat/lon, 30:Lambert 40:Gaussian)
      if( igds(5)==40) then
        igdstmpllen=19
        igdstmpl=0
!
!-- set up grid definition template 3.40
        igdstmpl=0
        igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
        igdstmpl(8)=nx      ! Ni . number of points along a paralell 
        igdstmpl(9)=ny      ! Nj . number of points along a meridian 
        igdstmpl(10)=0      ! Basic angle of the initial production domain 
        igdstmpl(11)=0      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments 
        latstt=88541961
        lonstt=0.
        latlst=-88541961
        lonlst=358125000
        dxval=1875000
        igdstmpl(12)=latstt ! La1 - latitude of first grid point
        igdstmpl(13)=lonstt ! Lo1 - longitude of first grid point 
        igdstmpl(14)=48     ! Resolution and component flags (Table 3.3) 
        igdstmpl(15)=latlst ! La2 - latitude of last grid point
        igdstmpl(16)=lonlst ! Lo2 - longitude of last grid point 
        igdstmpl(17)=dxval  ! Di - i direction increment
        igdstmpl(18)=ny/2   ! N - number of paralells between a pole and the equator
        igdstmpl(19)=0      ! Scanning mode (Table 3.4) (0:Points in the first row or column scan in the +i (+x) direction)
      endif 
!
      idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
      call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
      print*,'addgrid status=',ierr
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=0             ! Product Definition Template Number (Table 4.0) (0: Analysis or forecast at a horizontal level or in a horizontal layer at a point in time) 
      ipdstmpllen=15            ! pdt template length
      ipdstmpl(1)=0         ! catogory
      ipdstmpl(2)=0         ! parameter
      ipdstmpl(3)=3         ! Type of generating process (Table 4.3) (0:ana, 1:ic, 2:fcst, 3:bias-cor)
      ipdstmpl(4)=0         ! Background generating process identifier 
      ipdstmpl(5)=81        ! Analysis or forecast generating process identified (ON388TableA) 
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
      ipdstmpl(9)=0         ! Forecast time in units defined by ipdstmpl(8)
      ipdstmpl(10)=100      ! Type of first fixed surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(11)=0        ! Scale factor of first fixed surface
      ipdstmpl(12)=85000    ! Scaled value of first fixed surface
      ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(14)=0        ! Scale factor of second fixed surface
      ipdstmpl(15)=0        ! Scaled value of second fixed surface
!
      numcoord=0            ! Number of coordinate values after template 
      coordlist=0.          ! Optional list of coordinate values
!
!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table 5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=0         ! Binary scale factor
      idrstmpl(3)=2         ! Decimal scale factor
      idrstmpl(4)=16        ! Decimal scale factor
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute 
      idrstmpl(17)=2        ! Order of spatial difference (see Code Table 5.6) 
!
!-- section 6:       
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
!
!-- test data
      do j=1,ny
      do i=1,nx
        fld(i+(j-1)*nx)=270.+i*0.01+(j-1)*0.1
      enddo
      enddo
      print *,'fld=',maxval(fld(1:nx*ny)),minval(fld(1:nx*ny))
!
      call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)
      print*,'addfield status=',ierr

!-- finalize  GRIB message after all section
!-- adds the End Section ( "7777" )

      call gribend(cgrib,max_bytes,lengrib,ierr)
      print*,'gribend status=',ierr
      print*,'length of the final GRIB2 message in octets =',lengrib
!
      call wryte(ifilw, lengrib, cgrib)
!
!-- generate wind grib message:
      cgrib=''
!
!--
      call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
      print*,'gribcreate status=',ierr
      call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
      print*,'addgrid status=',ierr
!u wind
      do j=1,ny
      do i=1,nx
        fld(i+(j-1)*nx)=i*0.01+(j-1)*0.1
      enddo
      enddo
      ipdstmpl(1)=2         ! catogory (Momentum:2)
      ipdstmpl(2)=2         ! parameter (ugrd:2)
!
      idrstmpl(3)=4         ! decimal scale 
      idrstmpl(4)=14        ! Decimal scale factor
      call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)
!v wind
      do j=1,ny
      do i=1,nx
        fld(i+(j-1)*nx)=i*0.1+(j-1)*0.01
      enddo
      enddo
      ipdstmpl(1)=2         ! catogory (Momentum:2)
      ipdstmpl(2)=3         ! parameter (vgrd:3)
!
      idrstmpl(3)=4         ! decimal scale 
      idrstmpl(4)=14        ! Decimal scale factor
      call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)
      call gribend(cgrib,max_bytes,lengrib,ierr)
!
      call wryte(ifilw, lengrib, cgrib)
      print*,'after wrt cgrib2, lengrib=',lengrib
!
      return
      end

