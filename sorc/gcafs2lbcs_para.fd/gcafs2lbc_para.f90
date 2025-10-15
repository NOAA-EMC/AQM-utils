      program gocart_bnd
!-------------------------------------------------------------------------------------
!   interpolate GOCART Concentration to be lateral boundary condition for regional
!   air quality model,  also output a layer result for checking purpose
!
!   for GCAFS output in NetCDF format
!
!   Author: Youhua Tang
!   Revisions: parallel for RRFS-CMAQ
!-------------------------------------------------------------------------------------
!
!                             nhalo_model=3
!
!                    |----------- nxp-1 -----------| <-- east/west compute points
!                 |---------- north BC data ----------|
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!       ---       ooo           ---j=1---           ooo     ---         ---
!        |        ooo                               ooo      |           |
!        |        ooo                              |ooo      |           |
!                 ooo                        i=1-->|ooo
!   west BC data  ooo|                             |ooo east BC data    nyp-1 <-- north/south compute points
!                 ooo|<--i=isd-nhalo_model          ooo
!        |        ooo|                              ooo      |           |
!        |        ooo                               ooo      |           |
!       ---       ooo    ---j=jsd-nhalo_model---    ooo     ---         ---
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!                 |---------- south BC data ----------|

      use mpi
      use netcdf

      parameter(maxfile=300,nspecies=100,ngocart=11)

      real sfact(ngocart,nspecies),val(nspecies),  &
         checkfact(ngocart,nspecies)

      real,allocatable  :: glon(:,:), glat(:,:),tmpa(:)
      real,allocatable  :: zgocart(:,:,:),pgocart(:,:,:),vgocart(:,:,:), delz(:,:,:),  &
       dpres(:,:,:),xlat(:,:), xlon(:,:), &
       bndx(:,:,:,:,:), bndy(:,:,:,:,:),bndcoordx(:,:,:,:) ,bndcoordy(:,:,:,:),  &
       checkcoord(:,:,:),checksp(:,:,:), topo(:,:),&
       airgocart(:,:,:),tgocart(:,:,:),zhx(:,:,:,:),zhy(:,:,:,:),tmpbndx(:,:,:),tmpbndy(:,:,:)

      character bndname(nspecies)*16,gocartname(ngocart)*8,ctmp*16,  &
       echar(nspecies)*16,mofile(2)*2000,checkname(nspecies)*16,     &
       aline*200,gdatatype*4,modelname*4,gtype*16,arank*2, topofile*2000, &
       lbcfile(2)*2000

      integer netindex(ngocart),checklayer,modate(maxfile),         &
       mosecs(maxfile),julian,ismotime(maxfile),iemotime(maxfile),  &
       idate(7),tlmeta,iret
      logical ingocart,lflag,extrameta,indexfind(nspecies)
      integer monthday(12),dimids(3)
      data monthday/31,30,31,30,31,30,31,31,30,31,30,31/

      data gocartname/'o3mr','so2','dust1','dust2','dust3','dust4','dust5', &
        'bc1','bc2','oc1','oc2'/

      data indexfind/nspecies*.false./

      data iprint/0/

      integer  begyear,begdate,begtime,dtstep,numts,tstepdiff
      namelist /control/bndname,dtstep,tstepdiff,mofile,&  !  input file preffix and suffix
       lbcfile,topofile,inblend  ! inside blending layers

      CALL MPI_Init(ierr)
      CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
      CALL MPI_Comm_size(MPI_COMM_WORLD, npe, ierr)

      call aq_blank(16*nspecies,bndname)
      call aq_blank(16*nspecies,checkname)

      inblend=0.
      sfact(1:ngocart,1:nspecies)=0.
      checkfact(1:ngocart,1:nspecies)=0.
! read converting information

      open(7,file='gcafs2lbc.ini')
      read(7,control)

      call aq_find(nspecies,' ',bndname,lpsec,iflag)   ! BND species
      noutbnd=lpsec-1
      call aq_find(nspecies,' ',checkname,lpsec,iflag)   ! BND species
      ncheck=lpsec-1

      call aq_locate(7,'Species converting Factor',iflag)

      do while(.true.)
       call aq_readhd(7)
       read(7,*,end=98,err=99) ctmp,num
       call aq_find(ngocart,ctmp,gocartname,lpsec1,iflag)
       if(iflag.eq.0) then
        read(7,*)(echar(i),val(i),i=1,num)
	do i=1,num
	 call aq_find(noutbnd,echar(i),bndname,lpsec2,iflag)
	 if(iflag.eq.0) then
	  sfact(lpsec1,lpsec2)=val(i)
	  indexfind(lpsec2)=.true.
	 endif
	 call aq_find(ncheck,echar(i),checkname,lpsec2,iflag)
	 if(iflag.eq.0) checkfact(lpsec1,lpsec2)=val(i)
	end do
       endif
      print*,' Converting factor for ',gocartname(lpsec1),' is ', &
      (sfact(lpsec1,lm),lm=1,noutbnd)
 99   continue
      enddo
 98   close(7)

      call check(nf90_open(trim(topofile),nf90_nowrite, ncid))
      call check(nf90_inq_dimid(ncid,'lon',iddim_lon))
      call check(nf90_inquire_dimension(ncid,iddim_lon,len=imax))
      call check(nf90_inq_dimid(ncid,'lat',iddim_lat))
      call check(nf90_inquire_dimension(ncid,iddim_lat,len=jmax1))

      allocate(xlat(imax,jmax1))
      allocate(xlon(imax,jmax1),topo(imax,jmax1))

      call check(nf90_inq_varid(ncid,'geolon',idvar_geolon))
      call check(nf90_get_var(ncid,idvar_geolon,xlon))
      call check(nf90_inq_varid(ncid,'geolat',idvar_geolat))
      call check(nf90_get_var(ncid,idvar_geolat,xlat))
      call check(nf90_inq_varid(ncid,'orog_raw',idvar_topo))
      call check(nf90_get_var(ncid,idvar_topo,topo))

      do i=1,imax
       do j=1,jmax1
       if(xlon(i,j).lt.0) xlon(i,j)=xlon(i,j)+360
       enddo
      enddo
      call check(nf90_close(ncid))
      print*,'finish reading topofile'

! open  LBC file for rewrite
      nowstep=0+my_rank*dtstep
      jfhour=tstepdiff+nowstep
      write(aline,'(a,i3.3,a)')trim(lbcfile(1)),nowstep,trim(lbcfile(2))

      call check(nf90_open(trim(aline),nf90_write, ncid))
      call check(nf90_inq_dimid(ncid,'lon',iddim_lon))
      call check(nf90_inquire_dimension(ncid,iddim_lon,len=nlon))
      call check(nf90_inq_dimid(ncid,'lat',iddim_lat))
      call check(nf90_inquire_dimension(ncid,iddim_lat,len=nlat))
      call check(nf90_inq_dimid(ncid,'halo',iddim_halo))
      call check(nf90_inquire_dimension(ncid,iddim_halo,len=nhalo))

      nhalo_outside=nhalo-inblend ! outside halo layers
      jmax=jmax1-nhalo_outside*2
      if(nlon.ne.imax.or.nlat.ne.jmax) then
        print*,'dimension mismatch ',nlon,imax,nlat,jmax
        stop
      endif

! read zh
      call check(nf90_inq_dimid(ncid,'lev',iddim_lev))
      call check(nf90_inquire_dimension(ncid,iddim_lev,len=kmax))
      call check(nf90_inq_dimid(ncid,'levp',iddim_levp))
      call check(nf90_inquire_dimension(ncid,iddim_levp,len=kmax1))
      print*,'imax,jmax,kmax=',imax,jmax,kmax
      if(kmax1.ne.(kmax+1)) then
       print*,'kmax,kmax1=',kmax,kmax1
       stop
      endif

      allocate(zhx(imax,nhalo,kmax1,2),zhy(nhalo,jmax,kmax1,2))
      allocate(bndcoordx(imax,nhalo,2,2+kmax),bndcoordy(nhalo,jmax,2,2+kmax))
      allocate(bndx(imax,nhalo,kmax,2,noutbnd),bndy(nhalo,jmax,kmax,2,noutbnd))
      bndx=0.
      bndy=0.
      allocate(tmpbndx(imax,nhalo,kmax),tmpbndy(nhalo,jmax,kmax))

      print*,'read zh_bottom, zh_top'
      call check(nf90_inq_varid(ncid,'zh_bottom',idvar_zh_bottom))
      call check(nf90_get_var(ncid,idvar_zh_bottom,zhx(:,:,:,1)))
      call check(nf90_inq_varid(ncid,'zh_top',idvar_zh_top))
      call check(nf90_get_var(ncid,idvar_zh_top,zhx(:,:,:,2)))
      do k=1,kmax
        zhx(1:imax,1:nhalo,k,1:2)=0.5*(zhx(1:imax,1:nhalo,k,1:2)+zhx(1:imax,1:nhalo,k+1,1:2))  ! convert from interface ASL to layer ASL
      enddo
!      if(maxval(zhx(:,:,:,:)).gt.1e12) then
!          print*,'1 zhx overflowed ', maxval(zhx(:,:,:,:))
!	  stop
!       endif

      print*,'read zh_left, zh_right'
      call check(nf90_inq_varid(ncid,'zh_left',idvar_zh_left))
      call check(nf90_get_var(ncid,idvar_zh_left,zhy(:,:,:,1)))
      call check(nf90_inq_varid(ncid,'zh_right',idvar_zh_right))
      call check(nf90_get_var(ncid,idvar_zh_right,zhy(:,:,:,2)))
      do k=1,kmax
        zhy(:,:,k,:)=0.5*(zhy(:,:,k,:)+zhy(:,:,k+1,:))  ! convert from interface ASL to layer ASL
      enddo
!       if(maxval(zhx(:,:,:,:)).gt.1e12) then
!          print*,'2 zhx overflowed ', maxval(zhx(:,:,:,:))
!	  stop
!       endif


! -open GCAFS file
      write(aline,'(a,i3.3,a)')trim(mofile(1)),jfhour,trim(mofile(2))
      print*,'start reading GCAFS file',aline
      call check(nf90_open(trim(aline),nf90_nowrite, mcid))
      call check(nf90_inq_dimid(mcid,'grid_xt',iddim_xt))
      call check(nf90_inquire_dimension(mcid,iddim_xt,len=igocart))
      call check(nf90_inq_dimid(mcid,'grid_yt',iddim_yt))
      call check(nf90_inquire_dimension(mcid,iddim_yt,len=jgocart))
      call check(nf90_inq_dimid(mcid,'pfull',iddim_pfull))
      call check(nf90_inquire_dimension(mcid,iddim_pfull,len=kgocart))

       print *,trim(aline),'igocart=',igocart,       &
        'jgocart=',jgocart,'kgocart=',kgocart

       allocate(glon(igocart,jgocart))
       allocate(glat(igocart,jgocart))
       allocate(delz(igocart,jgocart,kgocart))
       allocate(dpres(igocart,jgocart,kgocart))
       allocate(pgocart(igocart,jgocart,kgocart+1),STAT=ierr)
       allocate(zgocart(igocart,jgocart,kgocart+1),STAT=ierr)
       allocate(tgocart(igocart,jgocart,kgocart),STAT=ierr)
       allocate(airgocart(igocart,jgocart,kgocart),STAT=ierr)
       allocate(vgocart(igocart,jgocart,kgocart),STAT=ierr)

       allocate(tmpa(kgocart+1),STAT=ierr)

       call check(nf90_inq_varid(mcid,'lon',idvar_glon))
       call check(nf90_get_var(mcid,idvar_glon,glon))
       call check(nf90_inq_varid(mcid,'lat',idvar_glat))
       call check(nf90_get_var(mcid,idvar_glat,glat))

       glatint=glat(1,2)-glat(1,1)
       glonint=glon(2,1)-glon(1,1)

       print*,' Gocart Latitude, Longtitude interval:',glatint,glonint
!       if(maxval(zhx(:,:,:,:)).gt.1e12) then
!          print*,'4 zhx overflowed ', maxval(zhx(:,:,:,:))
!	  stop
!       endif
       if(iprint.eq.1.and.my_rank.eq.0) then
!         open(27,file='dust2.bin',form='unformatted',access='direct',recl=igocart*jgocart*4)
         open(27,file='bc2.bin',form='unformatted',access='direct',recl=igocart*jgocart*4)
	 open(28,file='zhx.bin',form='unformatted',access='direct',recl=imax*kmax1*4)
	 write(28,rec=1)zhx(1:imax,1,1:kmax1,1) ! top
	 write(28,rec=2)zhx(1:imax,1,1:kmax1,2) ! bottom
	 close(28)
       endif
!---calculating lateral boundary horizontal index in GCAFS coordinate

! --- top and bottom
       do i=1,imax
        ix=i
	do j=1,nhalo
 	 do m=1,2
	  if (m.eq.1) then
	   jy=j      ! _bottom
	  else if (m.eq.2) then
	   jy=jmax1-nhalo+j ! top
	  endif

	  do i2=1,igocart
           if(xlon(ix,jy).ge.glon(i2,1).and.xlon(ix,jy).le.glon(i2+1,1)) then
	    bndcoordx(i,j,m,1)=i2+(xlon(ix,jy)-glon(i2,1))/     &   ! i in gcafs coordiate
      	    (glon(i2+1,1)-glon(i2,1))
	    exit
	   endif
	  enddo

          do j2=1,jgocart
	   if( (glatint.gt.0.and.xlat(ix,jy).ge.glat(i2,j2).and.    &
            xlat(ix,jy).le.glat(i2,j2+1)).OR.(glatint.lt.0.and.    &
             xlat(ix,jy).le.glat(i2,j2).and.xlat(ix,jy)  	  &
             .ge.glat(i2,j2+1))) then
	    bndcoordx(i,j,m,2)=j2+(xlat(ix,jy)-glat(i2,j2))/     &   ! j, in gcafs coordiate
     	     (glat(i2,j2+1)-glat(i2,j2))
	    exit
	   endif
	  enddo

         enddo
        enddo
       enddo

! --- left and right

      do j=1,jmax
       jy=j+nhalo_outside

	do i=1,nhalo
	 do m=1,2
	  if (m.eq.1) then
	   ix=i      !   left
	  else if (m.eq.2) then
	   ix=imax-nhalo+i ! right
	  endif

	 do i2=1,igocart
          if(xlon(ix,jy).ge.glon(i2,1).and.xlon(ix,jy).le.glon(i2+1,1)) then
	   bndcoordy(i,j,m,1)=i2+(xlon(ix,jy)-glon(i2,1))/     &   ! i in gocart coordiate
      	    (glon(i2+1,1)-glon(i2,1))
	   exit
	  endif
	 enddo

         do j2=1,jgocart
	  if( (glatint.gt.0.and.xlat(ix,jy).ge.glat(i2,j2).and.    &
           xlat(ix,jy).le.glat(i2,j2+1)).OR.(glatint.lt.0.and.    &
            xlat(ix,jy).le.glat(i2,j2).and.xlat(ix,jy)  	  &
             .ge.glat(i2,j2+1))) then
	   bndcoordy(i,j,m,2)=j2+(xlat(ix,jy)-glat(i2,j2))/     &   ! j, in gocart coordiate
     	    (glat(i2,j2+1)-glat(i2,j2))
	   exit
	  endif
	 enddo

        enddo
       enddo
      enddo

!- read input pressure, height etc
       call check(nf90_inq_varid(mcid,'hgtsfc',idvar_hgtsfc))
       call check(nf90_get_var(mcid,idvar_hgtsfc,zgocart(:,:,kgocart+1))) ! surface elevation
       call check(nf90_inq_varid(mcid,'delz',idvar_delz))
       call check(nf90_get_var(mcid,idvar_delz,delz))  ! delz is negative

       call check(nf90_inq_varid(mcid,'pressfc',idvar_pressfc))
       call check(nf90_get_var(mcid,idvar_pressfc,pgocart(:,:,kgocart+1)))
       call check(nf90_inq_varid(mcid,'dpres',idvar_dpres))
       call check(nf90_get_var(mcid,idvar_dpres,dpres))

       call check(nf90_inq_varid(mcid,'tmp',idvar_tmp))
       call check(nf90_get_var(mcid,idvar_tmp,tgocart))
       call check(nf90_inq_varid(mcid,'spfh',idvar_spfh))
       call check(nf90_get_var(mcid,idvar_spfh,airgocart)) ! specific humidity (kg/kg)

       do k=kgocart,1,-1
	do i=1,igocart
	 do j=1,jgocart
         zgocart(i,j,k)=zgocart(i,j,k+1)-delz(i,j,k)             ! top down, interface height
	 pgocart(i,j,k)=amax1(pgocart(i,j,k+1)-dpres(i,j,k),0.1) ! top down, interface pressure
	 tv=tgocart(i,j,k)*(1+0.608*amax1(airgocart(i,j,k),1.e-15))  ! virtual temperature
	 airgocart(i,j,k)=(pgocart(i,j,k)+pgocart(i,j,k+1))*0.5/tv/287.04 ! air density in kg/m3  R= 287.04 m3 Pa /kg/K
	 enddo
	enddo
       enddo

      if(iprint.eq.1.and.my_rank.eq.0) then
        open(31,file='zgocart.bin',form='unformatted',access='direct', &
	    recl=igocart*jgocart*kgocart*4)
	write(31,rec=1)zgocart(:,:,1:kgocart)
	write(31,rec=2)pgocart(:,:,1:kgocart)
	write(31,rec=3)tgocart
	write(31,rec=4)airgocart
	close(31)
      endif
! ---find vertical index for top and bottom LBC
	do i=1,imax
	 do j=1,nhalo
	  do m=1,2

	  x=bndcoordx(i,j,m,1)
	  y=bndcoordx(i,j,m,2)
	  xratio=x-int(x)
	  yratio=y-int(y)

	  do kp=1,kgocart+1
      	   tmpa(kp)=(1-yratio)*(zgocart(int(x),int(y),kp)*      &    ! horizontally interpolate height
     	    (1-xratio)+zgocart(int(x)+1,int(y),kp)*xratio)+     &
     	    yratio*(zgocart(int(x),int(y)+1,kp)*(1-xratio)+     &
     	    zgocart(int(x)+1,int(y)+1,kp)*xratio)
	   if(kp.ge.2) tmpa(kp-1)=0.5*(tmpa(kp-1)+tmpa(kp)) ! convert to mid-layer
          enddo

          do k=1,kmax
	   if(zhx(i,j,k,m).le.tmpa(kgocart)) then  ! top-down for both
	     bndcoordx(i,j,m,k+2)=real(kgocart)  ! k index start from 3
	   else
 	     do kp=2,kgocart

 	      if(zhx(i,j,k,m).le.tmpa(kp-1).and.zhx(i,j,k,m).ge.tmpa(kp)) then
                bndcoordx(i,j,m,k+2)=kp-1+(tmpa(kp-1)-zhx(i,j,k,m))/  &
     	          (tmpa(kp-1)-tmpa(kp))
                 exit
 	      endif
	     enddo
            endif
            if(zhx(i,j,k,m).ge.tmpa(1)) bndcoordx(i,j,m,k+2)=1.
           enddo

         enddo
       enddo
      enddo

! ---find vertical index for left and right LBC
	do i=1,nhalo
	 do j=1,jmax
	  do m=1,2

	  x=bndcoordy(i,j,m,1)
	  y=bndcoordy(i,j,m,2)
	  xratio=x-int(x)
	  yratio=y-int(y)

	  do kp=1,kgocart+1
      	   tmpa(kp)=(1-yratio)*(zgocart(int(x),int(y),kp)*      &    ! horizontally interpolate height
     	    (1-xratio)+zgocart(int(x)+1,int(y),kp)*xratio)+     &
     	    yratio*(zgocart(int(x),int(y)+1,kp)*(1-xratio)+     &
     	    zgocart(int(x)+1,int(y)+1,kp)*xratio)
	   if(kp.ge.2) tmpa(kp-1)=0.5*(tmpa(kp-1)+tmpa(kp)) ! convert to mid-layer
          enddo

          do k=1,kmax
	   if(zhy(i,j,k,m).le.tmpa(kgocart)) then
	     bndcoordy(i,j,m,k+2)=real(kgocart)  ! k index start from 3
	   else
 	     do kp=2,kgocart

 	      if(zhy(i,j,k,m).ge.tmpa(kp).and.zhy(i,j,k,m).le.tmpa(kp-1)) then
                bndcoordy(i,j,m,k+2)=kp-1+(tmpa(kp-1)-zhy(i,j,k,m))/  &
     	          (tmpa(kp-1)-tmpa(kp))
                 exit
 	      endif
	     enddo
            endif
            if(zhy(i,j,k,m).ge.tmpa(1)) bndcoordy(i,j,m,k+2)=1.
           enddo

         enddo
       enddo
      enddo

      if(iprint.eq.1.and.my_rank.eq.0) then
 	 open(30,file='bndcoordx.bin',form='unformatted',access='direct', &
	    recl=imax*nhalo*4)
	 write(30,rec=1)bndcoordx(:,:,1,1)  ! top_i
	 write(30,rec=2)bndcoordx(:,:,2,1)  ! bottom_i
	 write(30,rec=3)bndcoordx(:,:,1,2)  ! top_j
	 write(30,rec=4)bndcoordx(:,:,2,2)  ! bottom_j
	 do k=1,kmax
	  write(30,rec=4+k) bndcoordx(:,:,1,2+k) ! k in top
	 enddo
	 do k=1,kmax
	   write(30,rec=4+kmax+k) bndcoordx(:,:,2,2+k) ! k in bottom
	 enddo
	 close(30)
      endif
  ! begin species interpolation
  do L1=1,ngocart

    call check(nf90_inq_varid(mcid,gocartname(L1),idvar_spname))
    call check(nf90_get_var(mcid,idvar_spname,vgocart))

    if(gocartname(L1).eq.'bc2'.and.iprint.eq.1.and.my_rank.eq.0) then
      write(27,rec=1)vgocart(:,:,kgocart)
      write(27,rec=2)airgocart(:,:,kgocart)
    endif

   do k=1,kgocart
    do i=1,igocart
     do j=1,jgocart
       if(vgocart(i,j,k).gt.1e18) vgocart(i,j,k)=0. ! for undefine bug
       if(gocartname(L1).eq.'o3mr') vgocart(i,j,k)=vgocart(i,j,k)*1e6/48*28.97 ! kg/kg -> ppmV
               ! SO2 already in ppmv, GCAFS aerosol is already in ug/kg
     enddo
    enddo
   enddo

   do i=1,imax       ! for top/bottom boundary conditions
     do j=1,nhalo
       do m=1,2
        x=bndcoordx(i,j,m,1)
	y=bndcoordx(i,j,m,2)
	xratio=x-int(x)
	yratio=y-int(y)

	tmpa(1:kgocart)=(1-yratio)*(vgocart(int(x),int(y),     & ! horizontally interpolate values
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y),       &
     	 1:kgocart)*xratio)+yratio*(vgocart(int(x),int(y)+1,  &
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y)+1,     &
     	 1:kgocart)*xratio)

	 do k=1,kmax
	  z=bndcoordx(i,j,m,k+2)
	  zratio=z-int(z)
	  tmpvalue=(1-zratio)*tmpa(int(z))+zratio*tmpa(int(z)+1)     ! vertically interpolate values
	  do L2=1,noutbnd
	   bndx(i,j,k,m,L2)=bndx(i,j,k,m,L2)+amax1(tmpvalue,0.)*sfact(L1,L2)
	  enddo
	 enddo

	enddo
       enddo
      enddo

     do i=1,nhalo       ! for left/right boundary conditions
      do j=1,jmax
       do m=1,2
        x=bndcoordy(i,j,m,1)
	y=bndcoordy(i,j,m,2)
	xratio=x-int(x)
	yratio=y-int(y)

	tmpa(1:kgocart)=(1-yratio)*(vgocart(int(x),int(y),     & ! horizontally interpolate values
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y),       &
     	 1:kgocart)*xratio)+yratio*(vgocart(int(x),int(y)+1,  &
     	 1:kgocart)*(1-xratio)+vgocart(int(x)+1,int(y)+1,     &
     	 1:kgocart)*xratio)

	 do k=1,kmax
	  z=bndcoordy(i,j,m,k+2)
	  zratio=z-int(z)
	  tmpvalue=(1-zratio)*tmpa(int(z))+zratio*tmpa(int(z)+1)     ! vertically interpolate values
	  do L2=1,noutbnd
	   bndy(i,j,k,m,L2)=bndy(i,j,k,m,L2)+amax1(tmpvalue,0.)*sfact(L1,L2)
	  enddo
	 enddo

	enddo
       enddo
      enddo

    enddo  ! end gocart species loop


! begin output
     fillval=-9e33 ! or 0./0. for NaN
     print*,'start overwrite'
      do L=1,noutbnd       ! check if gocart supplies all species, otherwise do not overwrite the existing aerosol

        do m=1,2

!! -top/bottom
	  if(sum(bndx(1:imax,1:nhalo,1:kmax,m,L)).le.1e-20 ) then
	    print*, 'm=,', m,' X skip for ', bndname(L), 'my_rank=',my_rank
	    cycle
	  endif
	  if (m.eq.1) then
	   aline=trim(bndname(L))//'_bottom'
	  else
	   aline=trim(bndname(L))//'_top'
	  endif
          if(nf90_inq_varid(ncid,trim(aline),idvar_tmp).ne.nf90_noerr) then ! if not exit, create
	    dimids=(/iddim_lon,iddim_halo,iddim_lev/)
	    call check(nf90_redef(ncid))
	    print*,'add variable ',trim(aline),my_rank
	    call check(nf90_def_var(ncid,trim(aline),nf90_real,dimids,idvar_tmp))
	    call check(nf90_put_att(ncid,idvar_tmp,'_FillValue',fillval))
	    call check(nf90_enddef(ncid))
	    call check(nf90_inq_varid(ncid,trim(aline),idvar_tmp)) ! check exist
	  endif

	  print*,my_rank,'write ',trim(aline),idvar_tmp,minval(bndx(1:imax,1:nhalo,1:kmax,m,L)),maxval(bndx(1:imax,1:nhalo,1:kmax,m,L))

	  if(index(bndname(L),'num').gt.0) then
	    call check(nf90_get_var(ncid,idvar_tmp,tmpbndx))
	    tmpbndx(1:imax,1:nhalo,1:kmax)=tmpbndx(1:imax,1:nhalo,1:kmax)+ &
	      bndx(1:imax,1:nhalo,1:kmax,m,L)
	  else
	    tmpbndx(1:imax,1:nhalo,1:kmax)=bndx(1:imax,1:nhalo,1:kmax,m,L)
	    if(index(bndname(L),'aecj').gt.0) then
	     if(iprint.eq.1.and.my_rank.eq.0.and.m.eq.1) then
	       open(29,file='aecj_top.bin',form='unformatted',access='direct',recl=imax*nhalo*kmax*4)
  	       write(29,rec=1)tmpbndx
	       close(29)
             endif
	     do k=1,4
	      tmpbndx(1:imax,1:nhalo,k)=tmpbndx(1:imax,1:nhalo,5) ! for bug in GEFS EC
	     enddo
	    endif
	  endif
	  call check(nf90_put_var(ncid,idvar_tmp,tmpbndx))

!! left/right
	  if(sum(bndy(1:nhalo,1:jmax,1:kmax,m,L)).le.1e-20 ) then
	    print*, 'm=,',m, ' Y skip for ', bndname(L), 'my_rank=',my_rank
	    cycle
	  endif
	  if (m.eq.1) then
	   aline=trim(bndname(L))//'_left'
	  else
	   aline=trim(bndname(L))//'_right'
	  endif

          if(nf90_inq_varid(ncid,trim(aline),idvar_tmp).ne.nf90_noerr) then ! if not exit, create
 	    call check(nf90_redef(ncid))
	    dimids=(/iddim_halo,iddim_lat,iddim_lev/)
	    call check(nf90_def_var(ncid,trim(aline),nf90_real,dimids,idvar_tmp))
	    call check(nf90_put_att(ncid,idvar_tmp,'_FillValue',fillval))
	    call check(nf90_enddef(ncid))
	    call check(nf90_inq_varid(ncid,trim(aline),idvar_tmp)) ! check exist
	  endif

	  print*,'write ',trim(aline),idvar_tmp,minval(bndy(1:nhalo,1:jmax,1:kmax,m,L)),maxval(bndy(1:nhalo,1:jmax,1:kmax,m,L))

	  if(index(bndname(L),'num').gt.0) then
	    call check(nf90_get_var(ncid,idvar_tmp,tmpbndy))
	    tmpbndy(1:nhalo,1:jmax,1:kmax)=tmpbndy(1:nhalo,1:jmax,1:kmax)+ &
	      bndy(1:nhalo,1:jmax,1:kmax,m,L)
	  else
	    tmpbndy(1:nhalo,1:jmax,1:kmax)=bndy(1:nhalo,1:jmax,1:kmax,m,L)
	    if(index(bndname(L),'aecj').gt.0) then
	     do k=1,4
	      tmpbndy(1:nhalo,1:jmax,k)=tmpbndy(1:nhalo,1:jmax,5) ! for bug in GEFS EC
	     enddo
	    endif
	  endif
	    call check(nf90_put_var(ncid,idvar_tmp,tmpbndy))
	 enddo
	enddo
      call check(nf90_close(ncid))
      call MPI_Finalize(ierr)

contains
    subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
    end subroutine check
      end


      subroutine aq_blank(ntot,y)

      character*1 y(ntot)
      do i=1,ntot
      y(i)=' '
      enddo
      return
      end

      subroutine aq_locate(iunit,char,iflag)
!***********************************************************************
      character*(*) char
      character*80 dum1
      nchar=len(char)
      iflag=0
      do iter=1,10000
      read(iunit,'(a)',end=98) dum1(1:nchar)
!      print*,'dum1= ',dum1(1:nchar)
      if(dum1(1:nchar).eq.char) return
      enddo
98    iflag=1
!      print*,'dum1= ',dum1(1:nchar)
      return
      end

!**********************************************************************
      subroutine aq_find(num,cdum1,sname,lpsec,iflag)
!***********************************************************************
      dimension sname(1)
      character*(*) sname,cdum1
      iflag=0
      do 15 l=1,num
      if(cdum1.ne.sname(l)) go to 15
      lpsec=l
      return
15    continue
      iflag=1
      return
      end

      subroutine aq_readhd(iunit)
!***********************************************************************
      character*1 char(80)
       do iter=1,1000
	read(iunit,100,end=8)  (char(i),i=1,80)
	if(char(1).eq.'$') then
	  write(6,200) iunit,(char(i),i=1,80)
        else if(char(1).eq.'#') then
	else
	  backspace iunit
	  return
        endif
       end do
 8    continue
100   format(80a1)
200   format(2x,'iunit=',i3,2x,80a1)
      end
