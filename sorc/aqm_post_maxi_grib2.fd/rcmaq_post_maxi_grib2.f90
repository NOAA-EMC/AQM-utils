!------------------------------------------------------------------------------
!  aqm_post_maxi_bias_cor_grib2 
!  Author:   Jianping Huang 03/19/2015
!            based on read__gridded_aqm.f90
!            Jianping Huang for o3 bias correction 09/03/2017
!  Purposes: 1) convert Bias Correction files from netcdf format to grib2
!            2) calculate daily max and daily averaged PM2.5 
!            3) calculate daily 8hr_ave max PM2.5
!
!  Ho-Chun Huang     Oct 23 2019   eplace hardwire day2 to total_day
!  Ho-Chun Huang     NOV 26 2019   Update ipdstmpl(19) to be consistent with raw model for 
!                                  max_1hr_o3 and max_8hr_o3
!------------------------------------------------------------------------------
program aqm_post_maxi_bias_cor_grib2 

   use config, only : dp
   use read__netcdf_var
   use stdlit, only : normal
   use index_to_date_mod        
   use date__index
   use next__time

   implicit none

   						!   fail (stdlit)
! Local variables.

   character outfile*200,grib_id*3
   integer nhours,nhours8, nt,nowtime8,total_day
   integer dims_in4(4), dims_in3(3)
!   logical fail1, fail2

! added by JP  
   character  infile1*200,infile2*200,infile3*200
   character  varname*10,ymd*8,ch_cyc*2,ch_chk*2
   character  ch_chk1*2
   integer    diag, imax,jmax
   integer    icyc,iyear,imonth,iday,ihour,base_year,ichk,ichk1
   integer    nowdate,nowtime
   integer    nowdate9,nowtime9,iyear9,imonth9,iday9
   integer    ierr,mday,ier
   integer    i, j 
! for grib2 by JP
!   integer, parameter   :: max_bytes=20000000
!   integer, parameter   :: nx=393,ny=225
   integer, parameter   :: nx=775,ny=488
   integer, parameter   :: max_bytes=nx*ny*4
   integer, parameter   :: markutc=05 
   integer, parameter   :: ncmaq=4
!
   integer listsec0(2)
   integer listsec1(13)
   integer igds(5)
   integer igdstmpllen
   integer ipdstmpllen
   integer idrstmpllen
   integer idrsnum,ibmap,numcoord,ipdsnum,idefnum
   integer defnum
   integer ibin_scl, idec_scl,inumbits

   integer,dimension(100) :: igdstmpl
   integer,dimension(100) :: ipdstmpl
   integer,dimension(100) :: idrstmpl
!
   integer ideflist(1)
   real(4) coordlist(1)
!
   character*1 cgrib1(max_bytes)
   character*1 cgrib2(max_bytes)
!
   logical*1 bmap(nx,ny)
!
   real(4),dimension(nx*ny) :: fld1
   real(4),dimension(nx*ny) :: fld2
   logical*1,dimension(nx*ny) :: bmap1 

   integer ifilw1,ifilw2,lengrib,lonstt,lonlst,latstt,latlst
!    integer yy,mm,dd,hh,mn,sc
   real(4) :: dxval

   character*50 gdss(400)
   integer GRID, kgdss(200), lengds,im,jm,jf,ibmap1
!-------------------------------------------------------------------

   integer status
   
   character(*), parameter :: calendar  = 'gregorian'

   logical  ave1hr

! 4-D input array to conform to the current CMAQ and MET gridded format.
! Must be double precision for the generic Netcdf reader.

!   real(dp), allocatable :: vdata(:,:,:)	! (COL, ROW, LAY, TSTEP)
!   real(dp), allocatable :: indata1(:,:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: indata1(:,:,:,:)	! (RECORD,TIME,PFULL,COL, ROW)
   real(dp), allocatable :: indata2(:,:,:,:)	! (RECORD,TIME,PFULL,COL, ROW)
   real(dp), allocatable :: indata3(:,:,:,:)	! (RECORD,TIME,PFULL,COL, ROW)
!   real(dp), allocatable :: indata2(:,:,:,:,:)	! (COL, ROW, LAY, TSTEP)
!   real(dp), allocatable :: indata3(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
!   real(dp), allocatable :: indata(:,:,:,:)	! (COL, ROW, LAY, TSTEP)
   real, allocatable :: bc_data(:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: bc_op(:,:,:)	! (COL, ROW, LAY, TSTEP)
   real(dp), allocatable :: o3_8h_ave(:,:,:),o3_8h_max(:,:,:)
   real(dp), allocatable :: o3_1h_max(:,:,:)
   real(dp), allocatable :: pm25_24h_ave(:,:,:),pm25_1h_max(:,:,:)
   real(dp), allocatable :: work(:,:)
   logical, allocatable :: LB1(:)
!
   real(4) :: fldscl
   real(4),parameter :: SPVAL=9.9e10

   character*16 cmaqspec(ncmaq),varlist(ncmaq)
   real     gipds1(ncmaq),gipds2(ncmaq),&
            gipds27(ncmaq)

!    logical  ave1hr

   integer indexcmaq(ncmaq),id_gribdomain,L,L2,nspcmaq,max_proc  !

   data cmaqspec(1),gipds1(1),gipds2(1),gipds27(1)/'O3_1h_max',14,200,23/
   data cmaqspec(2),gipds1(2),gipds2(2),gipds27(2)/'O3_8h_max',14,201,23/
   data cmaqspec(3),gipds1(3),gipds2(3),gipds27(3)/'pm25_24h_ave',13,193,23/
   data cmaqspec(4),gipds1(4),gipds2(4),gipds27(4)/'pm25_1h_max',14,202,23/

   data varlist/ncmaq*'     '/

   namelist /control/varlist,outfile,id_gribdomain,max_proc

   open(7,file='rcmaq_max.ini')
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

! Get command arguments as strings.
   diag=5
!   id_gribdomain=148
!   outfile='aqm.t99z.max_8hr_o3_bc.148.grib2'
   ave1hr=.true.
   print*,"it is testing"
!   call get_command_argument (1, varname) 
   call get_command_argument (1, ymd)
   call get_command_argument (2, ch_cyc)
   call get_command_argument (3, ch_chk)
   call get_command_argument (4, ch_chk1)


!
   read(ymd(1:4),*)iyear
   read(ymd(5:6),*)imonth
   read(ymd(7:8),*)iday
   read(ch_cyc(1:2),*)icyc
   read(ch_chk(1:2),*)ichk
   read(ch_chk1(1:2),*)ichk1
   ihour=icyc

!   if ( icyc .le. 9 ) then
!    write(outfile(6:6),'(a1)')"0"
!    write(outfile(7:7),'(i1)')icyc
!   else
!    write(outfile(6:7),'(i2)')icyc
!   endif

   print*,"iyear=",iyear,"imonth=",imonth,"iday=",iday,"icyc=",icyc
   print*,"hjp111,ichk=",ichk,"ichk1=",ichk1
   print*,"outfile=",outfile

! Read 4-D file variable.  Use generic Netcdf reader.
! Input array is auto-allocated.
   if ( varlist(1) .eq. 'O3_1h_max' .or.  varlist(1) .eq. 'O3_8h_max' ) then
      write(varname,'(a2)')'o3'
   elseif ( varlist(1) .eq. 'pm25_24h_ave' .or. varlist(1) .eq. 'pm25_1h_max' ) then
!      write(varname,'(a4)')'pm25'
      write(varname,'(a8)')'PM25_TOT'
   endif

   if (diag >= 3) print *
   if (diag >= 3) print *, 'aqm.post1_bias_correct: Start.'
   if (diag >= 3) print *, '  Read var ' // trim (varname)

   infile1="a.nc"
   infile2="b.nc"
   infile3="c.nc"
 
   call read_netcdf_var (infile1, varname, diag, indata1, status)

   if ( icyc .eq. 6 ) then
      call read_netcdf_var (infile2, varname, diag, indata2, status)
   endif
   if ( icyc .eq. 12 ) then
      call read_netcdf_var (infile2, varname, diag, indata2, status)
      call read_netcdf_var (infile3, varname, diag, indata3, status)
   endif
    

! Read errors are soft errors.
! On read error, return with fail status from lower level.
   print*,"status=",status

!   if (status /= normal) return

! Check for conforming array dimensions.  Special test for robustness.
! This check is needed to prevent crash, in addition to the check inside
! read_netcdf_var, in case the very first file is invalid.

   dims_in4   = shape (indata1)              ! get dims of input array
   dims_in3   = dims_in4( (/ 1,2,4 /) )      ! omit vestigial LAY dimension

   imax       = dims_in4(1)
   jmax       = dims_in4(2)


! Transfer data to the output array.
! Truncate the extra hour, if needed (MET).
! Omit the vestigial LAY dimension.
! Round from double to single precision, using default rounding mode.

   nhours  = size (indata1, 4)
   
   nhours8 = nhours - 7

   allocate (bc_data(imax,jmax))
   allocate (work(imax,jmax))
   allocate (bc_op(imax,jmax,nhours))
   allocate (o3_8h_ave(imax,jmax,nhours8))
   allocate (o3_1h_max(imax,jmax,3))
   allocate (o3_8h_max(imax,jmax,3))
   allocate (pm25_24h_ave(imax,jmax,3))
   allocate (pm25_1h_max(imax,jmax,3))

   allocate (LB1(imax*jmax))

!
!  combine previoud day 12z and today 06 or 12z to create a complete data for day1 and
!  day 2 (from current day 05z to 04z of the next day)
   if  ( icyc .eq. 6 ) then
      if ( max_proc .eq. 48 ) then
         do i = 1, imax
            do j = 1, jmax
               if ( ichk .eq. 1 ) then
                  bc_op(i,j,1:2)  = indata2(i,j,1,5:6)  ! using today 00z file
               else
                  bc_op(i,j,1:2)  = indata2(i,j,1,17:18) ! using previous day 12z file
               endif
               bc_op(i,j,3:nhours) = indata1(i,j,1,1:46)   ! n_bias_cor_day *24 - 2
            enddo  
         enddo 
      else
         do i = 1, imax
            do j = 1, jmax
               if ( ichk .eq. 1 ) then
                  bc_op(i,j,1:2)  = indata2(i,j,1,5:6)  ! using today 00z file
               else
                  bc_op(i,j,1:2)  = indata2(i,j,1,17:18) ! using previous day 12z file
               endif
               bc_op(i,j,3:nhours) = indata1(i,j,1,1:70)   ! n_bias_cor_day *24 - 2
            enddo  
        enddo 
     end if
   elseif ( icyc .eq. 12 ) then
      if ( max_proc .eq. 48 ) then
         do i = 1, imax
            do j = 1, jmax
               if ( ichk .eq. 1 ) then    !! today's 00Z output is available
                  bc_op(i,j,1:2)  = indata2(i,j,1,5:6)  ! from 05z and 06z at 00z run
               else
                 bc_op(i,j,1:2)  = indata2(i,j,1,7:8) ! 17-18Z using previous day 12z file
               endif

               if ( ichk1 .eq. 1 ) then    !! today's 06Z output is available
                  bc_op(i,j,1:8)  = indata3(i,j,1,1:8)  ! from 01-06 hour at 06z run
               else
                bc_op(i,j,1:8)  = indata2(i,j,1,17:24)    ! from 19-24hr at previous day 12z run
               endif
               bc_op(i,j,9:nhours) = indata1(i,j,1,1:40) ! from 12z run  n_bias_cor_day *24 - 8
            enddo
         enddo
      else
         do i = 1, imax
            do j = 1, jmax
               if ( ichk .eq. 1 ) then    !! today's 00Z output is available
                  bc_op(i,j,1:2)  = indata2(i,j,1,5:6)      ! from 05z and 06z at 00z run
               else
                  bc_op(i,j,1:2)  = indata2(i,j,1,17:18)    ! 17-18Z using previous day 12z file
               endif
!
               if ( ichk1 .eq. 1 ) then    !! today's 06Z output is available
                  bc_op(i,j,3:8)  = indata3(i,j,1,1:6)      ! from 01-06 hour at 06z run
               else
                  bc_op(i,j,3:8)  = indata3(i,j,1,19:24)    ! from 19-24hr at previous day 12z run
               endif
               bc_op(i,j,9:nhours) = indata1(i,j,1,1:64) ! from 12z run  n_bias_cor_day *24 - 8
            enddo
         enddo
      endif
   endif
!
!-- set file unit

   ifilw1=51
   write(grib_id,'(i3.3)')id_gribdomain
   call baopen(ifilw1,trim(outfile)//'.'//grib_id//&
             '.grib2',ierr)
   if(ierr.ne.0) then
      print*,'can not open ',trim(outfile)
      stop 2001
   endif

!
!-- ------------------------------------------------------
!  calculaet 8hr_ave o3

!   if ( varlist(1) .eq. 'O3_8h_max' ) then
!    do nt=1,nhours-7
!     do i=1,imax
!       do j=1,jmax
!          o3_8h_ave(i,j,nt)=sum(bc_op(i,j,nt:nt+7))/8       ! 8hr running ave
!       enddo
!      enddo
!    enddo
!   endif

   base_year=iyear
   listsec1(6)=iyear     ! Reference time - Year (4 digits)
   listsec1(7)=imonth    ! Reference time - Month
   listsec1(8)=iday      ! Reference time - Day
   listsec1(9)=icyc      ! Reference time - Hour
     
   total_day=nhours/24
   do mday = 1, total_day 
!     if ( varlist(1) .eq. 'O3_8h_max' ) then
!       do i = 1, imax
!        do j = 1, jmax
!         o3_8h_max(i,j,mday) = maxval(o3_8h_ave(i,j,(mday-1)*24+1:mday*24))  ! daily 8hr_ave max
!        enddo
!       enddo
!      elseif ( varlist(1) .eq. 'pm25_24hr_ave' ) then
!       do i = 1, imax
!        do j = 1, jmax
!         pm25_24h_ave(i,j,mday) = sum(bc_op(i,j,(mday-1)*24+1:mday*24))/24.
!         pm25_24h_max(i,j,mday) = maxval(bc_op(i,j,(mday-1)*24+1:mday*24))  ! daily 8hr_ave max
!        enddo
!       enddo
!      endif
! Diagnostics, if requested.

!    if (diag >= 3) then
!      print '(2(a,f0.3))', '   Min, max data = ', minval (o3), ', ', &
!         maxval (o3)
!    end if

      if (diag >= 3) print *, 'read_gridded_aqm: Return.'

!-----------------------------------------------------------------------

      nowtime=(ihour+1)*10000
!      do nt=1,nhours

      GRID=793
      im=775
      jm=488
      jf=im*jm
!
      cgrib1=''
      cgrib2=''

!-----Grib2 file header information
!-- section 0:
      listsec0(1)=0       ! Discipline: table 0.0
      listsec0(2)=2       ! grib edition number
!
!-- section 1:
      listsec1(1)=7       ! Identification of orginating center (Table0)(7:ncep)
      listsec1(2)=0       ! Identification of orginating subcenter(ON388-TableC) (4:emc)
      listsec1(3)=2       ! GRIB master tables version number (Table 1.0)(11:May 2013 version)
      listsec1(4)=1       ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=1       ! Significance of reference time (Table 1.2) (0:ana 1:fcst 2:vrfy)
! following need to be changed !
!    listsec1(6)=iyear    ! Reference time - Year (4 digits)
!    listsec1(7)=imonth   ! Reference time - Month
!    listsec1(8)=iday     ! Reference time - Day
!    listsec1(9)=icyc     ! Reference time - Hour
!    listsec1(9)=INT(nowtime/10000)      ! Reference time - Hour
      listsec1(10)=0      ! Reference time - Minute
      listsec1(11)=0      ! Reference time - Second
      listsec1(12)=0      ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=1      ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

!     call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)
!     call gribcreate(cgrib2,max_bytes,listsec0,listsec1,ierr)
!
!-- section 3: grid definition section
      igds(1)=0           ! Source of grid definition (Table 3.0) (0:specified in the code)
      igds(2)=nx*ny       ! Number of grid points in the defined grid
      igds(3)=0           ! Number of octets for optional list of numbers defining number of points
      igds(4)=0           ! Interpretation of list of numbers defining number of points
!-- example: rotated Lat/lon
      igds(5)=1          ! Grid definition template number (Table 3.1) (0:Lat/lon, 30:Lambert 40:Gaussian)
      if( igds(5)==1) then
         igdstmpllen=22
         igdstmpl=0
!
!-- set up grid definition template 3.30
         igdstmpl=0
         igdstmpl(1)=6       ! Shape of the Earth (Table 3.2) (6:Shape of the Earth = 6,371,229.0 m)
         igdstmpl(8)=nx           ! Ni . number of points along a paralell
         igdstmpl(9)=ny           ! Nj . number of points along a meridian
         igdstmpl(10)=0    ! Basic angle of the initial production domain
         igdstmpl(11)=0   ! Subdivisions of basic angle used to define extreme longitudes and latitudes, and direction increments
!jp     latstt=56
         latstt=0
         latlst= -28500000
         lonlst= 314750000
         dxval=1875000
         igdstmpl(12)=latlst      ! La1 - latitude of first grid point
         igdstmpl(13)=lonlst      ! Lo1 - longitude of first grid point
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
      defnum=1              ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
!     call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)
!
!-- section 4: product definition section
      ipdstmpl=0
      ipdsnum=8             ! Product Definition Template Number (Table 4.0) (0:Analysis or forecast at a horizontal level or in a horizontal layer at a point in time)
      ipdstmpllen=29        ! pdt template length
!    ipdstmpl(1)=13          ! catogory
!    ipdstmpl(2)=193         ! parameter
      ipdstmpl(3)=2         ! Type of generating process (Table 4.3) (0:ana, 1:ic, 2:fcst)
      ipdstmpl(4)=0         ! Background generating process identifier
      ipdstmpl(5)=211       ! Analysis or forecast generating process identified (ON388TableA)
      ipdstmpl(6)=0         ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0         ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1         ! Indicator of unit of time range (Table 4.4) (0:minute, 1:hour 2:day)
!    ipdstmpl(9)=(5-icyc)+(mday-1)*24         ! Forecast time in units defined by ipdstmpl(8)
      ipdstmpl(10)=104      ! Type of first fixed surface (see Code table 4.5)(100:isobaric leve)
      ipdstmpl(11)=4        ! Scale factor of first fixed surface
      ipdstmpl(12)=10000    ! Scaled value of first fixed surface
      ipdstmpl(13)=255      ! Type of first second surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(14)=0        ! Scale factor of second fixed surface
      ipdstmpl(15)=0        ! Scaled value of second fixed surface
!    ipdstmpl(16)=iyear      !  Year
!    ipdstmpl(17)=imonth     !  Month
!    ipdstmpl(18)=iday+mday  !  Date
      ipdstmpl(19)=4        !  Forecast hour
      ipdstmpl(20)=0        !
      ipdstmpl(21)=0        !
      ipdstmpl(22)=1        !
      ipdstmpl(23)=0        !
      ipdstmpl(24)=0        !
      ipdstmpl(25)=2        !
      ipdstmpl(26)=1        !
      ipdstmpl(27)=23       !
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
!    idrstmpl(2)=0           ! Binary scale factor
      idrstmpl(3)=3         ! Decimal scale factor
!    idrstmpl(4)=15          ! Decimal scale factor
      idrstmpl(5)=0         !
      idrstmpl(6)=1         !
      idrstmpl(7)=0         ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0         ! Primary missing value substitute
      idrstmpl(9)=0         ! Secondary missing value substitute
      idrstmpl(11)=0        !
      idrstmpl(12)=5        !
      idrstmpl(13)=1        !
      idrstmpl(14)=1        !
!      idrstmpl(15)=255      !
      idrstmpl(16)=5        !
      idrstmpl(17)=2
      idrstmpl(18)=2
!
!-- section 6:
      ibmap=255             ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map does not apply)

      do L = 1, nspcmaq

         call gribcreate(cgrib1,max_bytes,listsec0,listsec1,ierr)
         call addgrid(cgrib1,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,idefnum,ierr)

!  calculaet 8hr_ave o3

         if ( varlist(L) .eq. 'O3_8h_max' ) then
            do nt=1,nhours-7
               do i=1,imax
                  do j=1,jmax
                     o3_8h_ave(i,j,nt)=sum(bc_op(i,j,nt:nt+7))/8       ! 8hr running ave
                  enddo
               enddo
            enddo
         endif

         if ( varlist(L) .eq. 'O3_1h_max' ) then
            do i = 1, imax
               do j = 1, jmax
                  o3_1h_max(i,j,mday) = maxval(bc_op(i,j,(mday-1)*24+1:mday*24))  ! daily 8hr_ave max
               enddo
            enddo
         endif

         if ( varlist(L) .eq. 'O3_8h_max' ) then   ! day 1 to total_day-1 has full 24 o3_8h_ave available
                                              ! last day only has 17 o3_8h_ave available (nhour-24:nhour-7)
            do i = 1, imax
               do j = 1, jmax
                  if ( mday .eq. total_day ) then
                     o3_8h_max(i,j,mday) = maxval(o3_8h_ave(i,j,(mday-1)*24+1:(mday-1)*24+17))  ! last day using remaining 17 o3_8h_ave
                  else
                     o3_8h_max(i,j,mday) = maxval(o3_8h_ave(i,j,(mday-1)*24+1:(mday)*24)) ! &2 8hr_max has full 24 o3_8h_ave
                  endif
               enddo
            enddo
         elseif ( varlist(L) .eq. 'pm25_24h_ave' ) then
            do i = 1, imax
               do j = 1, jmax
                  pm25_24h_ave(i,j,mday) = sum(bc_op(i,j,(mday-1)*24+1:mday*24))/24.
                  pm25_1h_max(i,j,mday) = maxval(bc_op(i,j,(mday-1)*24+1:mday*24))  ! daily 8hr_ave max
               enddo
            enddo
         endif

         if ( varlist(L) .eq. 'O3_1h_max' ) then
            do j=1,ny
               do i=1,nx
!                  fld1(i+(j-1)*nx)=o3_1h_max(i,j,mday) *1.  ! ppmv -> ppbv
                  fld1(i+(j-1)*nx)=o3_1h_max(i,j,mday) 
               enddo
            enddo
         endif 

         if ( varlist(L) .eq. 'O3_8h_max' ) then
            do j=1,ny
               do i=1,nx
!                  fld1(i+(j-1)*nx)=o3_8h_max(i,j,mday) *1000.  ! ppmv -> ppbv
                  fld1(i+(j-1)*nx)=o3_8h_max(i,j,mday) 
               enddo
            enddo
         endif

         if ( varlist(L) .eq. 'pm25_24h_ave' ) then
            do j=1,ny
               do i=1,nx
                 fld1(i+(j-1)*nx)=pm25_24h_ave(i,j,mday) 
               enddo
            enddo
         endif

         if ( varlist(L) .eq. 'pm25_1h_max' ) then
            do j=1,ny
               do i=1,nx
                  fld1(i+(j-1)*nx)=pm25_1h_max(i,j,mday)
               enddo
            enddo
         endif

         !! Max 1-hr
         !! ipdstmpl(9)                 is the starting hour time range
         !! Min(ipdstmpl(9)+23, nhours) is the end hour of time range
         !!
         !! Max 8-hr
         !! nowtime8                is the starting hour time range
         !! nowtime8 + ipdstmpl(27) is the end hour of time range
         ipdstmpl(1)=gipds1(indexcmaq(L))
         ipdstmpl(2)=gipds2(indexcmaq(L))
         if (mday .eq. total_day .and. varlist(L).eq.'O3_8h_max' .and. icyc .eq. 6) then
            ipdstmpl(27)=19    !! update from GIT code to match current PROD time range
         else
            ipdstmpl(27)=gipds27(indexcmaq(L))
         endif

         ipdstmpl(24)=0       ! 
         if ( varlist(L).eq.'O3_8h_max') then
        !! Ho-Chun Huang nowtime8=11-icyc+(mday-1)*24 !! update from GIT code to match current PROD time range
        !! Ho-Chun Huang nowtime8=12-icyc+(mday-1)*24 !! change to be consistent to raw post from GIT
            nowtime8=11-icyc+(mday-1)*24 !! update from GIT code to match current PROD time range
            ipdstmpl(19)=markutc+8-1
            if ( mday .eq. 3 .and. icyc .lt. 7 ) then
               ipdstmpl(19)=ihour
            end if
            if ( mday .eq. 2 ) then
               ipdstmpl(19)=ihour
            endif
            !! if ( mday .eq. total_day .and. icyc .eq. 6 ) then
            !!    ipdstmpl(19)=11-5
            !! else
            !!    ipdstmpl(19)=11   ! used to be 12
            !! end if
!! print *,"=====================TESTING TESTING ================================"
!! print *,"mday, ipdstmpl(9) , nowtime8    =", mday, ipdstmpl(9), nowtime8
!! print *,"mday, ipdstmpl(19), ipdstmpl(27)=", mday, ipdstmpl(19), ipdstmpl(27)
!! print *,"=====================TESTING TESTING ================================"
         elseif ( varlist(L).eq.'O3_1h_max') then
            nowtime8=5-icyc+(mday-1)*24
!! Ho-Chun Huang Nov 26 2019, Jainping's PARA13 code use markutc-1
!jp         ipdstmpl(19)=markutc-1
!hc            ipdstmpl(19)=markutc
            ipdstmpl(19)=markutc
         else
            nowtime8=5-icyc+(mday-1)*24
            ipdstmpl(19)=markutc-1
!jp         ipdstmpl(19)=markutc
         endif

         ipdstmpl(9)= nowtime8
          !! Add it back for cmaq.v5.1.9 code
          !! if ( varlist(L).eq.'O3_8h_max' .and. mday .eq. 3 ) then
          !!   ipdstmpl(19)=icyc
          !! endif
!! Ho-Chun Huang Nov 26 2019, Jainping's PARA13 code comment out this section
!!         if ( varlist(L).eq.'O3_8h_max' .and. mday .eq. total_day ) then
!!          ipdstmpl(19)=icyc
!! print *,"=====================TESTING TESTING ================================"
!! print *,"mday, ipdstmpl(9) , nowtime8    =", mday, ipdstmpl(9), nowtime8
!! print *,"mday, ipdstmpl(19), ipdstmpl(27)=", mday, ipdstmpl(19), ipdstmpl(27)
!! print *,"=====================TESTING TESTING ================================"
!!       endif

!jp0
         nowdate9=date_index(iyear, imonth, iday, base_year, calendar)
         call next_time(nowdate9,nowtime,240000)
         call index_to_date(nowdate9,iyear9, imonth9, iday9, base_year, calendar)

         ipdstmpl(16)=iyear9                       ! Year
         ipdstmpl(17)=imonth9                      !  Month
         ipdstmpl(18)=iday9
!jp9


! below is used for get bits for grib2
         if (maxval(fld1)==minval(fld1))then
            idrsnum=0
            print*,' changing to simple packing for constant fields'
         end if
         ibmap1=255
         bmap1=.true.
         if(any(fld1>=SPVAL))then
            ibmap1=0
            where(abs(fld1)>=SPVAL)bmap1=.false.
         endif

         fldscl=5.0

         call g2getbits(ibmap1,fldscl,size(fld1),bmap1,fld1,ibin_scl,idec_scl,inumbits)

         idrstmpl(2)=ibin_scl   !
         idrstmpl(3)=idec_scl   ! SCALING POWER OF 10
         idrstmpl(4)=inumbits   ! numbe of bits used for eack packed value

         call addfield(cgrib1,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen, &
                              coordlist,numcoord,idrsnum,idrstmpl, &
                              idrstmpllen,fld1,nx*ny,ibmap,bmap,ierr)
         call gribend(cgrib1,max_bytes,lengrib,ierr)

         call wryte(ifilw1, lengrib, cgrib1)

      end do  ! L loop %Varlist% 

      nowdate=date_index(iyear, imonth, iday, base_year, calendar)

      print*,"hjp991,iday=",iday

      call next_time(nowdate,nowtime,240000)
      call index_to_date(nowdate,iyear, imonth, iday, base_year, calendar)
    
      print*,"hjp992,iday=",iday

   end do   ! mday loop
     
   print*,"it is done ! jphuang" 
         
end program aqm_post_maxi_bias_cor_grib2 

!-----------------------------------------------------------------------
       subroutine g2getbits(ibm,scl,len,bmap,g,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total
!   number of bits,
!   The argument list is modified to have
!   ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep,
!   .false. skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo
      REAL    :: GROUND,GMIN,GMAX,s,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF

!     write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range

!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
!        I1=1
!        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
!          I1=I1+1
!        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
!     write(0,*)'in
!     g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits
