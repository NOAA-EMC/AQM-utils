!------------------------------------------------------------------------------
!  aqm.post1_bias_correct_grib2  
!  Author:   Jianping Huang 01/08/2015
!            based on read__gridded_aqm.f90
!  Purposes: 1) convert Bias Correction files from netcdf format to grib1
!            2) convert Bias Correction files from netcdf format to grib2
!            3) calculate daily 1-hr max and 24-hr ave PM2.5
!  11/20/2022 Jianping Huang
!            modified for post-processing aqm v7.0 (ufs-aqm) hourly output files
!
!------------------------------------------------------------------------------
      program aqm_post_grib2

      use config, only : dp
      use read__netcdf_var
      use stdlit, only : normal
      use index_to_date_mod        
      use date__index
      use next__time       
!   use grib_mod

      implicit none

! Local variables.

      character outfile*200
      integer nhours,L,L2,nspcmaq
      integer dims_in4(4), dims_in3(3)
! logical fail1, fail2

! added by JP  
      character  infile*200
      character  varname*10,ymd*8,ch_cyc*2
      integer    diag, imax,jmax
      integer    icyc,iyear,imonth,iday,ihour,base_year,nt
      integer    nowdate,nowtime
      integer    ierr,ier
! for grib2 
      integer, parameter   :: max_bytes=20000000
      integer, parameter   :: nx=775,ny=488
      integer, parameter   :: ncmaq=3
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
      character*1 cgrib(max_bytes)
!
      logical*1 bmap(nx,ny)
!
      real(4),dimension(nx*ny) :: fld
      integer ifilw,i,j,lengrib,lonstt,lonlst,latstt,latlst
!      integer yy,mm,dd,hh,mn,sc
      real(4) :: dxval

      character*50 gdss(400)
      integer GRID, kgdss(200), lengds,im,jm,jf
!-------------------------------------------------------------------

    integer status

    character chtmp*3
    character grib_id*3

    character(*), parameter :: calendar  = 'gregorian'
    character*16 cmaqspec(ncmaq),varlist(ncmaq)
    real conv_ratio(ncmaq),gipds1(ncmaq),gipds2(ncmaq),&
             gipds27(ncmaq)

    logical  ave1hr

    integer indexcmaq(ncmaq),id_gribdomain  ! 
     
    data cmaqspec(1),gipds1(1),gipds2(1),gipds27(1)/'o3',14,193,1/
    data cmaqspec(2),gipds1(2),gipds2(2),gipds27(2)/'O3_8hr',14,193,8/
    data cmaqspec(3),gipds1(3),gipds2(3),gipds27(3)/'PM25_TOT',13,193,1/

    data varlist/ncmaq*'     '/

    namelist /control/varlist,infile,outfile,id_gribdomain

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

!   real(dp), allocatable :: vdata(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real, allocatable :: bc_data(:,:)	! (COL, ROW, LAY, TSTEP)

! Get command arguments as strings.
    open(7,file='aqm_post.ini')
    read(7,control)
    close(7)

!--- cmaq species

    do L=1,ncmaq
     if(varlist(L).ne.'    ') then

       do L2=1,ncmaq
         if(varlist(L).eq.cmaqspec(L2)) exit
       enddo
       if(L2.gt.ncmaq) then
        print*,'wrong varlist ', varlist(L)
        stop
       endif
        indexcmaq(L)=L2
      else
        exit
      endif
     enddo

    nspcmaq=L-1

    if(nspcmaq.lt.1) then
      print*,'no CMAQ species provided'
!       stop
    endif

   diag=5
   ave1hr=.true.
   call get_command_argument (1, ymd)
   call get_command_argument (2, ch_cyc)
!
   read(ymd(1:4),*)iyear
   read(ymd(5:6),*)imonth
   read(ymd(7:8),*)iday
   read(ch_cyc(1:2),*)icyc
   ihour=icyc

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.

   if (diag >= 3) print *
   if (diag >= 3) print *, 'aqm.post1_bias_correct: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varlist(1))


   call read_netcdf_var (infile, trim(varlist(1)), diag, indata, status)

! Read errors are soft errors.
! On read error, return with fail status from lower level.
   print*,"status=",status

!   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata)			! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )		! omit vestigial LAY dimension

   imax       = dims_in4(1)
   jmax       = dims_in4(2)
!   expect2    = shape (indata)			! expected dimensions
!   print*,"expected dimenstio=",expect2
!   expect2(3) = expect2(3) + 1			! with 1 extra hour (MET)

!   fail1 = (any (dims_in3 /= shape (vdata)))
!   fail2 = (any (dims_in3 /= expect2))

! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours = size (indata, 4)
   allocate (bc_data(imax,jmax))

! Diagnostics, if requested.

   if (diag >= 3) then
      print '(2(a,f0.3))', '   Min, max data = ', minval (indata), ', ', &
         maxval (indata)
   end if

   if (diag >= 3) print *, 'read_gridded_aqm: Return.'

!---------------------------------------------------------------------------------
!    nhours=1

      listsec1(6)=iyear    ! Reference time - Year (4 digits)
      listsec1(7)=imonth      ! Reference time - Month
      listsec1(8)=iday      ! Reference time - Day
      listsec1(9)=icyc      ! Reference time - Hour

      nowtime=(ihour+1)*10000
      do nt=1,nhours

       GRID=793
       if(GRID.eq.793) then   !For HRRR grid
         im=775
         jm=488
         jf=im*jm
       else
         call makgds(GRID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
       end if
!
      base_year=iyear
!      nowtime=ihour*10000

!-- set file unit
      ifilw=52

      write(chtmp,'(i3.3)')nt      
      write(grib_id,'(i3.3)')id_gribdomain
      call baopen(ifilw,trim(outfile)//'.f'//chtmp//'.'//grib_id//&
                       '.grib2',ierr)

      if(ierr.ne.0) then
       print*,'can not open ',trim(outfile)
       stop 2001
      endif

       cgrib=''

!
!-----Grib2 file header information

!-- section 0:
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table 0)(7:ncep)
      listsec1(2)=0       ! Identification of orginating subcenter (ON388-TableC) (4:emc)
      listsec1(3)=2       ! GRIB master tables version number (Table 1.0) (11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2) (0:ana 1:fcst 2:vrfy)
! following need to be changed !
!     listsec1(6)=iyear    ! Reference time - Year (4 digits)
!     listsec1(7)=imonth      ! Reference time - Month
!     listsec1(8)=iday      ! Reference time - Day
!     listsec1(9)=icyc      ! Reference time - Hour
!     listsec1(9)=INT(nowtime/10000)      ! Reference time - Hour
      listsec1(10)=00     ! Reference time - Minute
      listsec1(11)=00     ! Reference time - Second
      listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

!       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)

!
!-- section 3: grid definition section
      igds(1)=0           ! Source of grid definition (Table 3.0) (0:specified in the code)
      igds(2)=nx*ny       ! Number of grid points in the defined grid
      igds(3)=0           ! Number of octets for optional list of numbers defining number of points
      igds(4)=0           ! Interpretation of list of numbers defining number of points
!-- example: Gaussian Lat/lon
      igds(5)=1          ! Grid definition template number (Table 3.1) (0:Lat/lon, 30:Lambert 40:Gaussian)
      if( igds(5)==1) then
        igdstmpllen=22
        igdstmpl=0
!
!-- set up grid definition template 3.30
        igdstmpl=0
        igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
        igdstmpl(8)=nx      ! Ni . number of points along a paralell
        igdstmpl(9)=ny      ! Nj . number of points along a meridian
        igdstmpl(10)=0      ! Basic angle of the initial production domain
        igdstmpl(11)=0      ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!        latstt=0
!        lonstt=38500000
        latstt= -28500000
        lonstt= 314750000
        dxval=1875000
        igdstmpl(12)=latstt ! La1 - latitude of first grid point
        igdstmpl(13)=lonstt ! Lo1 - longitude of first grid point
        igdstmpl(14)=48
        igdstmpl(15)=28500000
        igdstmpl(16)=45250000
        igdstmpl(17)=116908
        igdstmpl(18)=116908
        igdstmpl(19)=64
        igdstmpl(20)=-40000000  !Latitude of the southern pole of projectio
        igdstmpl(21)=242000000  !Longitude of the southern pole of projection
        igdstmpl(22)=0
      endif

      idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=8             ! Product Definition Template Number (Table 4.0) (0: Analysis or forecast at a horizontal level or in a horizontal layer at a point in time)
      ipdstmpllen=29        ! pdt template length
      ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana, 1:ic, 2:fcst)
      ipdstmpl(4)=0         ! Background generating process identifier
      ipdstmpl(5)=211        ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
      ipdstmpl(9)=nt-1         ! Forecast time in units defined by ipdstmpl(8)
      ipdstmpl(10)=104      ! Type of first fixed surface (see Code table 4.5) (100:isobaric leve)
      ipdstmpl(11)=4        ! Scale factor of first fixed surface
      ipdstmpl(12)=10000    ! Scaled value of first fixed surface
      ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(14)=0        ! Scale factor of second fixed surface
      ipdstmpl(15)=0        ! Scaled value of second fixed surface
      ipdstmpl(16)=iyear        !  Year
      ipdstmpl(17)=imonth        !  Month
      ipdstmpl(18)=iday        !  Date
      ipdstmpl(19)=INT(nowtime/10000)        !  Forecast hour
      ipdstmpl(20)=0        !
      ipdstmpl(21)=0        !
      ipdstmpl(22)=1        !
      ipdstmpl(23)=0        !
      ipdstmpl(24)=0        !
      ipdstmpl(25)=2        !
      ipdstmpl(26)=1        !
!     ipdstmpl(27)=1        !
      ipdstmpl(28)=255      !
      ipdstmpl(29)=0        !

!
      numcoord=0            ! Number of coordinate values after template
      coordlist=0.          ! Optional list of coordinate values
!

!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18        ! Length of Data representation section
      idrstmpl(2)=0         ! Binary scale factor
      idrstmpl(3)=2         ! Decimal scale factor
      idrstmpl(4)=15        ! Decimal scale factor
      idrstmpl(5)=0         !
      idrstmpl(6)=1         !
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute
!     idrstmpl(10)=9047     !
      idrstmpl(11)=0        !
      idrstmpl(12)=5        !
      idrstmpl(13)=1        !
      idrstmpl(14)=1        !
!     idrstmpl(15)=12       !
      idrstmpl(16)=5        !
      idrstmpl(17)=2
      idrstmpl(18)=2
!
!-- section 6:
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)
!
   

     do L=1,nspcmaq

       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)

       call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

!       if(varlist(L).ne.'pm25') then
!         if(varlist(L).eq.'O3_8hr') then
!             o3_8hr(1:imax,1:jmax,1,nt)=o3_8hr(1:imax,1:jmax,1,nt)*1000
          
!        bc_data(:,:)=indata(:,:,1,nt)
         print*,"hjp222,L=",L,"varlist(L)=",varlist(L)
         if(varlist(L).eq.'O3_8hr'.and.nt.ge.8) then
          do i=1,imax
           do j=1,jmax
             bc_data(i,j)=sum(indata(i,j,1,nt-7:nt))/8
           enddo
          enddo
          ipdstmpl(9)=nt-8
         else
           bc_data(:,:)=indata(:,:,1,nt)
           ipdstmpl(9)=nt-1
         endif


        do j=1,ny
        do i=1,nx
         if (varlist(L).eq. 'PM25_TOT' ) then
          fld(i+(j-1)*nx)=bc_data(i,j)
         else
          fld(i+(j-1)*nx)=bc_data(i,j)
         endif
        enddo
        enddo

        ipdstmpl(1)=gipds1(indexcmaq(L))
        ipdstmpl(2)=gipds2(indexcmaq(L))
        ipdstmpl(27)=gipds27(indexcmaq(L))

       if ( varlist(L) .ne. 'O3_8hr' ) then
        call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)

        call gribend(cgrib,max_bytes,lengrib,ierr) 
        call wryte(ifilw, lengrib, cgrib)
       endif

       if ( varlist(L) .eq. 'O3_8hr' .and. nt.ge.8 ) then
        call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmpllen,fld,nx*ny,ibmap,bmap,ierr)

        call gribend(cgrib,max_bytes,lengrib,ierr)
        call wryte(ifilw, lengrib, cgrib)
       endif

       enddo  ! CMAQ species loop

       nowdate=date_index(iyear, imonth, iday, base_year, calendar)

       call next_time(nowdate,nowtime,10000)
       call index_to_date(nowdate,iyear, imonth, iday, base_year, calendar)


       end do   ! nt loop
         
  end program aqm_post_grib2

