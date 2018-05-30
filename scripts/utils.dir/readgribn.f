        program readgribn
c
c * Purpose: Read binary file called 'dump' (from wgrib)
c     and output in conmap format.  Works with NCEP Reanalysis
c     grib files.
c
c * Notes
c
c      (1) The executable is created by:
c            f77 -o readgribn readgribn.f
c          or:
c            g77 -ffortran-bounds-check -o readgribn readgribn.f 
c          (the -ffortran... option is optional).
c
c      (2) The default Gaussian grid file (gaussgrid.dat) and
c          the NCEP Reanalysis table file (reanal-ncep.table.txt) should
c          have the full path specified in the parameter statements for
c          dgaussfile and dnceptfile.  The parameter ntab should also be 
c          set to the max. no. of entries in nceptfile (257).
c
c * Modifications (Kevin Keay):
c
c     (1) 25/10/99 PRES - MSL or sfc - or PRMSL is converted to hPa.
c
c     (2) 26/10/99 grib header specified as a command-line argument.
c
c     (3) 3/11/99  Restored default header form for PMSL (MSLP)  and Z500
c         (these were altered for use with the cyclone tracking software).
c         Output name format changed (Var.Date.Lev.cmp) e.g. PRES.5801.sfc.cmp
c         where:
c           Var    Variable name
c           Date   Start date in grib header
c           Lev    Pressure level (MSL,sfc, 10 etc).
c
c     (4) 6/12/99 Added maximum limits (mlon,mlat) of arrays so that (ilon,ilat)
c         for either type of grid define a subset.
c         The grid is setup at the start of the program.
c
c     (5) 7/12/99 Added command-line options:
c         -g gaussfile:  Uses a Gaussian grid specified in gaussfile. 
c         This grid may be extracted from the corresponding 
c         *.ctl file which accompanies the grib data file.
c         -G: As -g except the file specified by the dgaussfile parameter
c         is used for the grid.
c         If -g or -G is not given a 2.5x2.5 regular lon-lat grid will be used.
c         -n: The description of the quantity (variable) is taken 
c         from a table file.
c         -N: As -n except the file specified by the dnceptfile parameter
c         is used for the table file.
c         If -n or -N is not given the 'hardwired' descriptions in the program
c         are used.
c
c     (6) 20/1/2000 Expected level to be MSL, sfc or a number e.g. 850.
c         Discovered that TCDC may appear as 'low cld lay:' and with
c         low replaced by mid and high.
c
c     (7) 7/1/2001 Replaced routine ngtopt with more flexible code.
c         Added routine parsehdr to extract desired fields easily. 
c         Replaced bit? variables with cquant,cdesc,ctype,cunit,cedesc,
c         f[1-4] (filename) and h[1-6] (header).
c         Note: ctype is not used at the moment.
c         Added NCEP2 version label option (-ncep2) (default: 'NCEP').
c         Added cyclone tracking scheme option (-cyc). This makes some
c         header adjustments for compatibility with cycloc*x.
c         Added option -l: Output filename is lowercase (default: mixed).
c         Added option -e: Extra information in conmap header (uses cedesc).
c
c     (8) 31/1/2001 If -Y2K option is specified then 4 digit years in the
c         range 1900-1920 -> 2000-2020. This is a fix for the wgrib -4yr
c         option which always gives years as 19??.
c
c     (9) 27/4/2001 Fixed an inconsistency in the conmap file output - arrays
c         lat,lon were output without subscripts. This does not cause a
c         error when reading in the conmap file in a subsequent job but causes
c         the file to be larger than usual for the regular grid.
c
c     (10) 3/4/2003 Added -f option: output conmap file is called
c          dump.yy[yy]mmddhh.cmp. This is sometimes useful if renaming of
c          files is required later.
c     (11) 7/12/2003 Added -era40 option: output conmap file has identifier
c          ERA40 in header.
c     (12) 31/3/2004 Added check to see if number of requested files is
c          greater than maxfiles (currently 7320 or 5 years of 6 hr data).
c     (13) 9/7/2006 Added -U option: user grid (subset of regular 2.5x2.5
c          degree grid e.g. -U "-30,5,90,160" for lats -30 to 5 and lons 90 to 160.
c     (14) 20/7/2006 Added -O option: user general regular (non-Gaussian) grid.
c          Max. grid size is 721 lons x 361 lats (0.5x0.5 degree grid).
c          e.g. -O "1.,1.,-30,5,90,160" for lats -30 to 5 and lons 90 to 160
c          on a 1x1 degree regular grid.
c     (15) 30/11/2006 Added options: -m model_name, -s scale_factor, -F -noflip -ione.
c
c * Based on readconmap.f by: Pandora Hope  Date: March 27 1998
c
c * Modified by: Kevin Keay  Date: 17/4/98

c
	parameter(mlon=721,mlat=361) ! KK 6/12/99 Suits a Gaussian grid
				    ! and the usual 2.5x2.5 lon-lat grid
c
	real array(mlon,mlat)       ! KK 6/12/99
	real lat(mlat)
	real lon(mlon+1)            ! Repeat lon 0. as 360.
	real arrayx(mlon+1,mlat)    ! Repeat lon 0. as 360.
	real loninc, latinc
c
	character*80 wgribfile 
	parameter (wgribfile='dump')  ! This is the default name from wgrib
	character*80 ghdrfile  ! KK 26/10/99
c
	character*80 gaussfile ! KK 6/12/99
	character*80 nceptfile 
c
	character*80 dgaussfile 
	character*80 dnceptfile 
	parameter (dgaussfile='/home/kevin/grib/gaussgrid.dat')
	parameter (dnceptfile='/home/kevin/grib/reanal_ncep.table.txt')
c
	parameter (ntab=257)  ! Max. no. of entries in table file
c
	parameter (maxfiles=22000)! Max. no of files to process
				  ! corresponding to entries in hdr
				  ! == ~15 years of 6 hr data
	character head(maxfiles)*120
c
        logical lexist
	integer optind
	character*80 optarg
        character*1 ch
	character file1*200, ofile*200
	character header*80
	character cdesc*60, vname*60
        character*32 cquant,clevel,ctype
        character*32 clevel2
        character*60 cedesc
        character*17 cunit
        character f1*80,f2*40,f3*40,f4*40
        character h1*10,h2*20,h3*10,h4*13,h5*17,h6*10
	character*60 cid
	character*10 cmodel,cmodel1
	real lon1,lon2,lat1,lat2
	real xscale

c----------------------------------------------------------------------

	narg= iargc()
c
        if(narg.eq.0)then  
	  call prusage (dgaussfile,dnceptfile)
	  stop
        else
c
c * Process command-line arguments
c
          idbg= 0
          igauss= 0
          intab= 0
          icyc= 0
          iver= 1   ! NCEP
          ilower= 0
          iexdesc= 0
	  iy2k= 0
	  idfile= 0
	  iugrid= 0
	  iogrid= 0
	  ifcst= 0
	  xscale= 1.
	  iscale= 0
	  iflip= 1   ! Assume array is in N -> S order (usually it is N -> S)
	  ione= 0
c
          narg= iargc()
          i= 1
          do while (i.le.narg)
            call getarg (i,optarg)
            if (optarg.eq.'-1')then ! Concatenated conmap file
              i= i +1
              call getarg (i,optarg)
	      ofile= optarg
	      ione= 1
            elseif (optarg.eq.'-c')then
              i= i +1
              call getarg (i,optarg)
              cid= optarg                     ! Filename is cid.yy[yy]mmddhh.cmp
              idfile= 2
            elseif(optarg.eq.'-d')then        ! Debug
              idbg= 1
            elseif(optarg.eq.'-e')then        ! Extra information in conmap header
              iexdesc= 1
            elseif(optarg.eq.'-f')then        ! Filename is dump.yy[yy]mmddhh.cmp
              idfile= 1
            elseif (optarg.eq.'-g')then
              i= i +1
              call getarg (i,optarg)
              gaussfile= optarg               ! Gaussian grid file
              igauss= 1
            elseif (optarg.eq.'-h')then
              i= i +1
              call getarg (i,optarg)
	      ghdrfile = optarg               ! wgrib header file
            elseif (optarg.eq.'-l')then       ! Lowercase output filename
              ilower= 1
            elseif (optarg.eq.'-m')then       ! User reanalysis or model name e.g. ETA
	      i= i +1
	      call getarg (i,optarg)
	      read(optarg,*)cmodel
	      iver= 4
            elseif (optarg.eq.'-n')then
              i= i +1
              call getarg (i,optarg)
	      nceptfile = optarg              ! NCEP tablefile
              intab= 1
            elseif (optarg.eq.'-s')then       ! Scale data by xscale
	      i= i +1
	      call getarg (i,optarg)
	      read(optarg,*)xscale
	      iscale= 1
            elseif (optarg.eq.'-F') then      ! Forecast time in header
	      ifcst= 1
            elseif (optarg.eq.'-G') then
              gaussfile= dgaussfile           ! Gaussian grid file
              igauss= 1
            elseif (optarg.eq.'-N') then
	      nceptfile = dnceptfile          ! NCEP tablefile
              intab= 1
            elseif (optarg.eq.'-O') then      ! User general regular grid - can't use -G with this
	      iogrid= 1
	      iugrid= -1 ! To aid with logic (avoid confusion with 2.5x2.5 grid)
	      i= i +1
	      call getarg (i,optarg)
	      read(optarg,*)latinc,loninc,lat1,lat2,lon1,lon2
	      write(*,*)'User general lon-lat grid: ',loninc,' x',latinc
	      write(*,*)'lon. range: ',
     * lon1,' -',lon2,' lat. range: ',lat1,' -',lat2
	      if (lon1.gt.lon2)then
	        write(*,*)'ERROR: lon1 should be < lon2'
		stop 'ABORT: Due to error'
	      endif
	      if (lat1.gt.lat2)then
	        write(*,*)'ERROR: lat1 should be < lat2'
		stop 'ABORT: Due to error'
	      endif
            elseif (optarg.eq.'-U') then      ! User grid - can't use -G with this
	      iugrid= 1
	      i= i +1
	      call getarg (i,optarg)
	      read(optarg,*)lat1,lat2,lon1,lon2
	      write(*,*)'User grid: lon. range: ',
     * lon1,' -',lon2,' lat. range: ',lat1,' -',lat2
	      if (lon1.gt.lon2)then
	        write(*,*)'ERROR: lon1 should be < lon2'
		stop 'ABORT: Due to error'
	      endif
	      if (lat1.gt.lat2)then
	        write(*,*)'ERROR: lat1 should be < lat2'
		stop 'ABORT: Due to error'
	      endif
            elseif (optarg.eq.'-ncep2') then
              iver= 2
            elseif (optarg.eq.'-era40') then
              iver= 3
            elseif (optarg.eq.'-cyc') then    ! Cyclone tracking scheme
              icyc= 1
            elseif (optarg.eq.'-Y2K') then    ! Y2K fix 
              iy2k= 1
            elseif (optarg.eq.'-noflip') then    ! Array is S->N already
	      iflip= 0
            endif
            i= i +1
          enddo
c
          if(iexdesc.eq.1.and.icyc.eq.1)then
            write(*,*)'NOTE: -e option ignored with -cyc option'
            iexdesc= 0
          endif
	  if(iugrid.eq.1.and.igauss.eq.1)then
	    write(*,*)'ERROR: Cannot use -G with -U'
	    stop 'ABORT: Due to error'
	  endif
c
c * Read wgrib header file
c
	  open(2,file=ghdrfile)
	  do nf=1,maxfiles
            read(2,'(A)',end=19)head(nf)
	  enddo
19        continue
          close(2)
        endif
c
	if(igauss.eq.1)then ! Gaussian grid
	  inquire(file=gaussfile,exist=lexist)
	  if(lexist)then
	    open(1,file=gaussfile)
          else
	    write(*,*)'ERROR: ',gaussfile,
     * ' not found'
	    write(*,*)'ABORT: Due to error'
	    stop
	  endif
	  read(1,'(4x,I5,7x,F5.0,2x,F5.0)')ilon,flon,loninc
	  read(1,'(4x,I5)')ilat
	  read(1,*)(lat(i),i=1,ilat)
	  close(1)
	  lon(1)= flon
	  do i=2,ilon
            lon(i)= lon(i-1) +loninc
	  enddo
	  lon(ilon+1)= lon(1) +360.
        else
	  if (iogrid.eq.1) then ! General user lon-lat grid
	    ilon= int((lon2 -lon1)/loninc) +1
	    ilat= int((lat2 -lat1)/latinc) +1
	    write(*,*)'User grid: No. lons: ',ilon,' No. lats: ',ilat
	    flon= lon1
	    flat= lat1
	    lon(1)= flon
	    do i=2,ilon
	      lon(i)= lon(i-1) +loninc
	    enddo
ckk	    lon(ilon+1)= lon(1) +360.
            i360= 0
	    xlon360= lon(ilon) +loninc
            if(lon(1).eq.0.and.xlon360.eq.360.)then
              i360= 1
              lon(ilon+1)= xlon360
	    endif
	    lat(1)= flat
	    do i=2,ilat
	      lat(i)= lat(i-1) +latinc
	    enddo
          else
	    if(iugrid.eq.0)then ! Full 2.5x2.5 grid
	      ilon= 144
	      ilat= 73
	      loninc= 2.5
	      latinc= 2.5
	      flon= 0.
	      flat= -90.
	      lon(1)= flon
	      do i=2,ilon
	        lon(i)= lon(i-1) +loninc
	      enddo
	      lon(ilon+1)= lon(1) +360.
	      lat(1)= flat
	      do i=2,ilat
	        lat(i)= lat(i-1) +latinc
	      enddo
	    else ! User 2.5x2.5 grid
	      loninc= 2.5
	      latinc= 2.5
	      ilon= int((lon2 -lon1)/loninc) +1
	      ilat= int((lat2 -lat1)/latinc) +1
	      write(*,*)'User grid: No. lons: ',ilon,' No. lats: ',ilat
	      flon= lon1
	      flat= lat1
	      lon(1)= flon
	      do i=2,ilon
	        lon(i)= lon(i-1) +loninc
	      enddo
ckk	      lon(ilon+1)= lon(1) +360.
	      lat(1)= flat
	      do i=2,ilat
	        lat(i)= lat(i-1) +latinc
	      enddo
	    endif
	  endif
	endif
c
	if(intab.eq.1)then ! use table file
	  inquire(file=nceptfile,exist=lexist)
	  if(lexist)then
	    open(3,file=nceptfile)
          else
	    write(*,*)'ERROR: ',nceptfile,
     * ' not found'
	    write(*,*)'ABORT: Due to error'
	    stop
	  endif
	endif
        if(idbg.eq.1)then
          write(*,*)'igauss,intab: ',igauss,intab
	  if(igauss.eq.1)write(*,'('' gaussfile: '',A)')
     * gaussfile(1:ilen(gaussfile))
	  if(intab.eq.1)write(*,'('' nceptfile: '',A)')
     * nceptfile(1:ilen(nceptfile))
	  write(*,*)'ilon,ilat: ',ilon,ilat
	endif
c
c * Open wgribfile (normally called 'dump') - this may contain more
c   than one dataset to be output as a conmap file
c
	open(unit=1,file=wgribfile,form='unformatted',status='old')
	nff=nf-1
        write(*,*)
	write(6,*)'The number of data files to extract is: ',nff
	if(nff.gt.maxfiles)then
	  write(*,*)'WARNING: Only ',maxfiles,' files will be',
     * ' extracted (current limit)'
          write(*,*)'See parameter (maxfiles= ...)'
	 nff= maxfiles
	endif
c
	do 100 kk=1,nff
	  read(1) ((array(i,j),i=1,ilon),j=1,ilat)  ! KK 6/12/99

c get array to start at -90 !necessary for land.grb, but I think that the data
c is already starting at -90, actually, I don't! :) Pandora
	do j= 1, ilat
	  do i= 1, ilon
            if(iflip.eq.1)then ! If lats are in N -> S order
	      arrayx(i,j)=array(i,ilat+1-j)
	    else
	      arrayx(i,j)=array(i,j)  ! Lats already in S -> N order
	    endif
	  enddo
	enddo
c * Repeat lon 0. as 360.
       if(iugrid.eq.0)then
	  do j= 1,ilat
	    arrayx(ilon+1,j)= arrayx(1,j)
	  enddo
	endif
       if(iogrid.eq.1.and.i360.eq.1)then
          write(*,*)'NOTE: Lon 0 will be duplicated as 360'
	  do j= 1,ilat
	    arrayx(ilon+1,j)= arrayx(1,j)
	  enddo
	endif

c----------------------------------------------------------------------

c
c * Initialise character variables each iteration (for safety)
c
        cquant= ' '
        clevel= ' '
        ctype= ' '
        cdesc= ' '
        cunit= ' '
        cedesc= ' '
        clevel2= ' '
        file1= ' '
        f1= ' '
        f2= ' '
        f3= ' '
        f4= ' '
        header= ' '
        h1= ' '
        h2= ' '
        h3= ' '
        h4= ' '
        h5= ' '
        h6= ' '
c
        call parsehdr (head(kk),iy,im,id,ih,cquant,clevel,ctype,
     * iy2k,ifcst,idbg,ierr)
        if(ierr.ne.0)stop 'ERROR: Error(s) in routine parsehdr'
        if(idbg.eq.1)then
          write(*,*)'iy,im,id,ih: ',iy,im,id,ih
          write(*,*)'cquant: ',cquant
          write(*,*)'clevel: ',clevel
          write(*,*)'ctype: ',ctype
        endif

c----------------------------------------------------------------------

c
c * Mean sea level pressure (MSLP) can be specified as PRMSL
c   or PRES and MSL - convert from Pa -> hPa
c   Note: Also converts to hPa for surface pressure (PRES and sfc)
c
        if(cquant.eq.'PRES'.or.cquant.eq.'PRMSL'
     * .or.cquant.eq.'MSL' ! ERA40
     * .or.(cquant.eq.'MSLET'.and.clevel.eq.'MSL')) then  ! ETA
          scal= 0.01
	  iscale= 1
        else
	  scal= xscale
	endif
	if(iscale.eq.1)then
	  do j= 1,ilat
	    do i= 1,ilon
	      arrayx(i,j)= scal*arrayx(i,j)
	    enddo
	  enddo
          if(iugrid.eq.0)then
	    do j= 1,ilat
	      i= ilon +1
	      arrayx(i,j)= scal*arrayx(i,j)
	    enddo
	  endif
          if(iogrid.eq.1.and.i360.eq.1)then
	    do j= 1,ilat
	      i= ilon +1
	      arrayx(i,j)= scal*arrayx(i,j)
	    enddo
	  endif
	endif
c
c * Set up quantity description for header
c   Other quantities may be added as desired
c
        if(intab.eq.0)then
c
c * These represent MSL or surface pressure - non-standard names
c
	  if (cquant.eq.'sfc') cdesc='Surface Pressure (Pa)'
	  if (cquant.eq.'PRES'.and.clevel.eq.'sfc')
     * cdesc='Surface Pressure (Pa)'
	  if (cquant.eq.'PRES'.and.clevel.eq.'MSL')
     * cdesc='Mean Sea Level Pressure (hPa)'
	  if (cquant.eq.'MSLET'.and.clevel.eq.'MSL')  ! ETA
     * cdesc='Mean Sea Level Pressure (Pa)'
c
c * These are used instead of the table file
c
	  if (cquant.eq.'PRMSL') cdesc='Mean Sea Level Pressure (hPa)'
	  if (cquant.eq.'HGT') cdesc='Geopotential height (gpm)'
	  if (cquant.eq.'RELD') cdesc='Relative divergence (/s)'
	  if (cquant.eq.'RELV') cdesc='Relative vorticity (/s)'
	  if (cquant.eq.'RH') cdesc='Relative humidity (percent)'
	  if (cquant.eq.'SPFH') cdesc='Specific humidity (kg/kg)'
	  if (cquant.eq.'STRM') cdesc='Stream function (m**2/s)'
	  if (cquant.eq.'TMP') cdesc='Temperature (K)'
	  if (cquant.eq.'CBTW') cdesc='Covariance bw T and omega'//
     * ' (K*Pa/s)'
	  if (cquant.eq.'UGRD') cdesc='u wind (m/s)'               
	  if (cquant.eq.'CBUQ') cdesc='Cov bw u and spec hum'//
     * ' (m/s*gm/gm)'
	  if (cquant.eq.'CBTZW') cdesc='Covariance bw u and T (K*m/s)'
	  if (cquant.eq.'CBMZW') cdesc='Covariance bw v and u'//
     * ' (m**2/s**2)'
	  if (cquant.eq.'CBUW') cdesc=
     * 'Covariance bw u and omega (m/s*Pa/s)'
	  if (cquant.eq.'VGRD') cdesc='v wind (m/s)'                             
	  if (cquant.eq.'CBVQ') cdesc='Cov bw v and spec hum'//
     * ' (m/s*gm/gm)'
	  if (cquant.eq.'CBTMW') cdesc='Cov bw v and T (K*m/s)'
	  if (cquant.eq.'CBVW') cdesc='Cov bw v and omega'//
     * ' (m/s*Pa/s)'
	  if (cquant.eq.'VPOT') cdesc='Velocity potential (m**2/s)'        
	  if (cquant.eq.'VTMP') cdesc='Virtual temperature (K)'            
	  if (cquant.eq.'VVEL') cdesc='Pressure vertical velocity (Pa/s)'  
	  if (cquant.eq.'CBQW') cdesc=
     * 'Cov bw spec hum & omega (gm/gm*Pa/s)' 
	  if (cquant.eq.'PWAT') cdesc='Precipitable water (kg/m**2)'
	  if(cquant.eq.'ICEC') cdesc='Ice concentration'//
     * ' [ice=1; no ice=0]'
        else   ! Use table file
	  call nceptab(3,ntab,nceptfile,cquant,cdesc,vname,itab,ierr)
	  if(ierr.ne.0)then
	    write(*,*)'ERROR: Quantity: ',cquant(1:ilen(cquant)),
     * ' not matched'
	    stop 'ABORT: Due to error'
	  endif
	endif
c
c * Extract units (cunit) from description (cdesc) and
c   save rest of description in cedesc. The units are in () or []
c   at the end of cdesc.
c
        do i=ilen(cdesc),1,-1
          if(cdesc(i:i).eq.'('.or.cdesc(i:i).eq.'[')then
            isu= i +1
            ieu= ilen(cdesc) -1
            ied= i -1
            cunit= ' '//cdesc(isu:ieu)
            if(ied.gt.20)then
              cedesc= cdesc(1:19)//' '
            else
              cedesc= cdesc(1:ied)
            endif
            goto 20
          endif
        enddo          
        cedesc= cdesc
        cunit= ' '
20      continue
 
c
c * Setup filename
c   This had four parts - f1,f2,f3,f4
c
        f4= '.cmp'
        if(iy.lt.100)then
          write(f3,'(''.'',4I2.2)')iy,im,id,ih
        else
          write(f3,'(''.'',I4,3I2.2)')iy,im,id,ih
        endif
        if(iver.eq.1)then
          f2= '.ncep'
        elseif(iver.eq.2)then
          f2= '.ncep2'
        elseif(iver.eq.3)then
          f2= '.era40'
        elseif(iver.eq.4)then
c * Change uppercase characters to lowercase
          cmodel1=' '
          do i=1,ilen(cmodel)
            ch= cmodel(i:i)
            idx= ichar(ch)
            if(idx.ge.65.and.idx.le.90)ch=char(idx+32) 
            cmodel1(i:i)= ch
          enddo
          write(f2,'(''.'',A)')cmodel1
        endif    
c
c * clevel2 has 'sigma=' removed and blanks replaced by _
c
        isigma= index (clevel,'sigma=')
        clevel2= ' '
        if(isigma.ne.0)then
          clevel2= clevel(isigma+6:)
          clevel= clevel2
        endif
        do i=1,ilen(clevel)
          clevel2(i:i)= clevel(i:i)
          if(clevel(i:i).eq.' ')clevel2(i:i)= '_' ! Replace blank by _
        enddo
        if(idbg.eq.1)then
          write(*,*)'clevel :',ilen(clevel),':',clevel(1:ilen(clevel))
          write(*,*)'clevel2:',ilen(clevel2),':',
     * clevel2(1:ilen(clevel2))
        endif
c
c * Use different filename for cyclone tracking scheme
c
        if(icyc.eq.1)then
	  if ((cquant.eq.'PRES'.and.clevel.eq.'MSL')
     * .or.cquant.eq.'PRMSL')then
            f1= 'PMSL'
	  elseif (cquant.eq.'MSL'.and.clevel.eq.'sfc')then ! ERA40
            f1= 'PMSL'
	  elseif (cquant.eq.'MSLET'.and.clevel.eq.'MSL')then  ! ETA
            f1= 'PMSL'
          elseif (cquant.eq.'HGT')then
            f1= 'Z'//'.'//clevel2(1:ilen(clevel2))
          elseif (cquant.eq.'RELV')then
            f1= 'RELV'//'.'//clevel2(1:ilen(clevel2))
          elseif (cquant.eq.'ABSV')then
            f1= 'ABSV'//'.'//clevel2(1:ilen(clevel2))
          else
            f1= cquant(1:ilen(cquant))//'.'//clevel2(1:ilen(clevel2))
          endif
        else 
          f1= cquant(1:ilen(cquant))//'.'//clevel2(1:ilen(clevel2))
        endif
c
        if(idbg.eq.1)then
           write(*,*)'f1:',ilen(f1),':',f1(1:ilen(f1))
           write(*,*)'f2:',ilen(f2),':',f2(1:ilen(f2))
           write(*,*)'f3:',ilen(f3),':',f3(1:ilen(f3))
           write(*,*)'f4:',ilen(f4),':',f4(1:ilen(f4))
        endif
c
        if(idfile.eq.0)then
          file1= f1(1:ilen(f1))//f2(1:ilen(f2))//
     * f3(1:ilen(f3))//f4(1:ilen(f4))
	elseif(idfile.eq.1)then
	  file1= 'dump'//f3(1:ilen(f3))//'.cmp'
	elseif(idfile.eq.2)then
	  file1= cid(1:ilen(cid))//f3(1:ilen(f3))//'.cmp'
	endif
c
        if(ilower.eq.1)then   ! Change uppercase characters to lowercase
          do i=1,ilen(file1)
            ch= file1(i:i)
            idx= ichar(ch)
            if(idx.ge.65.and.idx.le.90)ch=char(idx+32) 
            file1(i:i)= ch
          enddo
        endif
c
c * Setup header
c   This has six parts - h1,h2,h3,h4,h5,h6
c
        h1= cquant(1:10)
        if(iexdesc.eq.1)then
          h2= cedesc(1:20)
        else
          h2= ' '
        endif
        if(iver.eq.1)then
          h3= 'NCEP'
        elseif(iver.eq.2)then
          h3= 'NCEP2'
        elseif(iver.eq.3)then
          h3= 'ERA40'
        elseif(iver.eq.4)then
          write(h3,'(A)')cmodel
        endif
c
c * Note: In header write hour as 100*ih e.g. 6 -> 600
c
        if(iy.lt.100)then
          write(h4,'(2x,3I2.2,1x,I4.4)')iy,im,id,100*ih
        else
          write(h4,'(I4,2I2.2,1x,I4.4)')iy,im,id,100*ih
        endif
        h5= cunit
        h6= clevel
c
c * Adjust header for compatibility with cyclone tracking scheme
c   Other quantities may be added if required
c
        if(icyc.eq.1)then
          h2= ' '              ! Ignore -e option if -cyc given
          if(igauss.eq.1)then
            h6= '  GAUSSIAN'
          else
	    if(iogrid.eq.1)then
	      write(h6,'(F3.1,''x'',F3.1,''DEG'')')loninc,latinc
	    else
              h6= '2.5x2.5DEG'
	    endif
          endif
	  if ((cquant.eq.'PRES'.and.clevel.eq.'MSL')
     * .or.cquant.eq.'PRMSL'
     * .or.(cquant.eq.'MSLET'.and.clevel.eq.'MSL')     ! ETA
     * .or.(cquant.eq.'MSL'.and.clevel.eq.'sfc'))then  ! ERA40
            h1= 'PMSL'
            h5= '    MB'
	  elseif (cquant.eq.'HGT')then
            h1= 'Z'
            h5= '    M '
            read(clevel,*)ilevel
            write(h2,'(I4)')ilevel
	  elseif (cquant.eq.'ABSV')then
            h1= 'ABSV'
            h5= '    /S'
            read(clevel,*)ilevel
            write(h2,'(I4)')ilevel
	  elseif (cquant.eq.'RELV')then
            h1= 'RELV'
            h5= '    /S'
            read(clevel,*)ilevel
            write(h2,'(I4)')ilevel
          else                ! Other quantities (may need changing)          
            big= 1.e+30
            xlevel= big
            read(clevel2,*,err=30)xlevel
 30         if(xlevel.eq.big)then
              if(idbg.eq.1)write(*,*)'NOTE: Invalid level: ',clevel2
            endif
            if(xlevel.lt.1.)then         ! Probably a sigma value
              ilevel= int(10000*xlevel)
              write(h2,'(I4)')ilevel
            else
              ilevel= int(xlevel)
              write(h2,'(I4)')ilevel
            endif
            h5= '   '//cunit(1:ilen(cunit))
          endif
        endif 
c
        header= h1//h2//h3//h4//h5//h6

c----------------------------------------------------------------------

        if(ione.eq.1)then
	  file1= ofile
	  if(kk.eq.1)then  ! First record (map)
	    write(*,'('' Output concatenated file (conmap): '',A)')
     * file1(1:ilen(file1))
	    open(unit=10,file=file1,form='unformatted')
	  endif
	else
	  write(*,*)' '
	  write(*,'('' Output file (conmap): '',A)')file1(1:ilen(file1))
	endif
c
	write(*,*)' '
	write(*,'(A)')header
	write(*,*)' '
c 
        if(iugrid.eq.0)then ! Entire grid
	  if(ione.eq.0)then
	    open(unit=10,file=file1,form='unformatted')
	  endif
	  write(10)ilat
	  write(10)(lat(i),i=1,ilat)     ! Fixed KK 27/4/2001
	  write(10)ilon+1  ! Repeat lon 0. as 360.
	  write(10)(lon(i),i=1,ilon+1)   ! Fixed KK 27/4/2001
	  write(10)header
	  write(10)((arrayx(i,j),i=1,ilon+1),j=1,ilat)  ! KK 6/12/99
	  if(ione.eq.0)then
	    close(10)
	  endif
        else ! User grid
	  if(ione.eq.0)then
	    open(unit=10,file=file1,form='unformatted')
	  endif
	  write(10)ilat
	  write(10)(lat(i),i=1,ilat)     ! Fixed KK 27/4/2001
	  if(i360.eq.1)then
	    write(10)ilon+1 ! Repeat lon 0. as 360.  
	    write(10)(lon(i),i=1,ilon+1)   ! Fixed KK 27/4/2001
	    write(10)header
	    write(10)((arrayx(i,j),i=1,ilon+1),j=1,ilat)  ! KK 6/12/99
	  else
	    write(10)ilon  
	    write(10)(lon(i),i=1,ilon)   ! Fixed KK 27/4/2001
	    write(10)header
	    write(10)((arrayx(i,j),i=1,ilon),j=1,ilat)  ! KK 6/12/99
	  endif
	  if(ione.eq.0)then
	    close(10)
	  endif
	endif
c
  100	continue   ! End of kk loop
c
	close(1)
	if(intab.eq.1)close(3)
	if(ione.eq.1)close(10)
	stop
	end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      integer function ilen(string)
c
      character*1 c
      character*(*) string
c
      do i=len(string),1,-1
        c= string(i:i)
        if (c.ne.' ') goto 10
      enddo
c * String is wholly blank
      ilen= -1
      return
c
   10 ilen= i    ! Return no-blank length of string
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine nceptab(iu,ntab,nceptfile,q,cdesc,vname,itab,ierr)
c
c * Purpose: Matches grib quantity q in NCEP Reanalysis table file
c     nceptfile.  The description is returned in cdesc, vname= q and
c     itab is the quantity number from the table.
c
c * Author: Kevin Keay  Date: 6/12/99
c
      character*(*) nceptfile
      character*(*)q, cdesc, vname 
c
      character*100 chvar, chvar2, cval
c
      ierr= 0
c
      do i=1,ntab
        read(iu,'(A)')chvar
	icolon= index(chvar,':')
	cval= chvar(1:icolon-1)
	read(cval,*)itab
	chvar2= chvar(icolon+1:)
	chvar= chvar2
	icolon= index(chvar,':')
	cval= chvar(1:icolon-1)
	vname= cval(1:ilen(cval))
	if(q.eq.vname)then      ! Quantity (variable) matched
	  chvar2= chvar(icolon+1:)
	  cdesc= chvar2(1:ilen(chvar2))
	  return
	endif
      enddo
c 
      ierr= 1    ! No match
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      function ngtopt (list, i, arg)

! version 1.3 90/11/16 12:56:40
c Written by Harvey Davies, CSIRO Division of Atmospheric Research,
c   Aspendale, Victoria
! Get next command-line option. Similar to sh command getopts

! If there are no more options then ngtopt is set to -1. 
! At this stage the value of
! i can be used to access any further arguments. Note that any further calls to
! clopt result in another pass, restarting again at the 1st argument.

! arguments 

! Arguments:
      character*(*) list ! list of valid option letters. Colon (:) indicates
!			   that preceding option takes an argument (input)
      character*1 letter ! letter corresponding to next option (output)
      character*(*) arg ! option-argument corresponding to next option
!			  error message if ngtopt > 1 (output)
      integer i ! subscript of next command-line argument (output)
      integer ngtopt ! exit status (>0 = ichar(letter),
!                                   -1 = end, 0 = errror) (output)

! Other variables:
      character*255 cla ! current command-line argument
      integer       ia ! subscript of current command-line argument
      integer       ind ! character position within list
      integer       k ! character position within current command-line argument
      integer       lcla ! length of cla excluding trailing spaces
      logical       optarg ! true iff option takes an argument
      save

! Functions:
      integer iargc ! function giving arg count

      data ia / 0 /


      letter = '?'
      arg = ' '
      ngtopt = ichar(letter)
      i = ia + 1
      if ( ia .eq. 0 ) then
	k = 0
	lcla = 0
      end if
      k = k + 1
      if ( k .gt. lcla ) then
	ia = ia + 1
	if ( ia .gt. iargc() ) then
	  ngtopt = -1
	else
	  call getarg( ia, cla )          ! Decomment for Unix
	  if ( cla(1:1) .ne. '-' ) then
	    ngtopt = -1
	  else
	    i = ia + 1
	    if ( cla(2:2) .eq. '-' ) ngtopt = -1
	  end if
	end if
	lcla = len( cla )
	do while ( lcla .gt. 1  .and.  cla(lcla:lcla) .eq. ' ' )
	  lcla = lcla - 1
	end do
	k = 2
      end if
      if (ngtopt .ge. 1) then
	ind = index( list, cla(k:k) )
	if ( ind .eq. 0 ) then
	  ngtopt = 0
	  arg = 'illegal option -' // cla(k:k)
	  return
	else if ( ind .lt. len(list) ) then
	  optarg = list(ind+1:ind+1) .eq. ':'
	else
	  optarg = .false.
	end if
	if ( optarg ) then
	  if ( k .lt. lcla ) then
	    arg = cla(k+1:lcla)  
	  else
	    ia = ia + 1
	    if ( ia .gt. iargc() ) then
	      ngtopt = 0
	      arg = 'missing argument -' // list(ind:ind)
	      return
	    end if
	    i = ia + 1
	    call getarg( ia, arg )          ! Decomment for Unix
	  end if
	  k = 0
	  lcla = 0
	end if
	letter = list(ind:ind)
        ngtopt = ichar(letter)
      else
	ia = 0
      end if
c
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine prusage (dgaussfile,dnceptfile)
c
      character*(*) dgaussfile, dnceptfile
c
      character*1 qu
c
      write(*,*)'Usage: readgribn [-g gaussfile][-n nceptfile]'
      write(*,*)'  [-1 catconfile]'
      write(*,*)'  [-deflFGNU][-c cid] [-cyc][-ncep2][-Y2K]'
      write(*,*)'  [-m model_name][-s scale_factor] -h hdr'
      write(*,*)' '
      write(*,*)'Note: For a Gaussian grid either -g or -G must be',
     * ' specified (default: 2.5x2.5 deg. grid)' 
      write(*,*)' '
      write(*,*)'Options: '
      write(*,*)' -1 catconfile: Output concatenated conmap file',
     * '    (default: one file per record (map)'
      write(*,*)' -c cid: Output file is: cid.yy[yy]mmddhh.cmp'
      write(*,*)' -d: Debug'
      write(*,*)' -e: Extra information in conmap header',
     * ' (ignored if -cyc specified)'
      write(*,*)' -f: Output file is: dump.yy[yy]mmddhh.cmp',
     * '   (useful if renaming is required later)'
      write(*,*)' -g gaussfile: Gaussian grid is specified in',
     * ' file gaussfile'
      write(*,*)' -h hdr: wgrib output corresponding to wgrib',
     * ' extracted binary file ''dump'' '
      write(*,*)' -l: Output filename is lowercase (default: mixed)'
      write(*,*)' -m model_name: e.g. ETA; use if not covered by',
     * '-ncep, -ncep2 or -era40 options'
      write(*,*)' -n nceptfile: Use descriptions specified in',
     * ' file nceptfile'
      write(*,*)' -s scale_factor: Only used with geopotential',
     * ' height (Z)'
      write(*,*)' -F: Uses forecast time to construct date-time',
     * ' (default: uses given date-time)' 
      write(*,'(''  -G: Gaussian grid is specified in file: '',A)')
     * dgaussfile(1:ilen(dgaussfile)) 
      write(*,'(''  -N: Use descriptions specified in file: '',A)')
     * dnceptfile(1:ilen(dnceptfile))
      write(*,*)' -O "latinc,loninc,lat1,lat2,lon1,lon2" :',
     * ' user general lon-lat grid'
      write(*,*)' -U "lat1,lat2,lon1,lon2" : user grid (subset of',
     * ' regular 2.5x2.5 deg. grid only)'
      write(*,*)' -cyc: Output conmap header compatible with',
     * ' cyclone tracking scheme'
      write(*,*)' -era40: Output conmap header for ERA40',
     * ' (default: NCEP)'
      write(*,*)' -ncep2: Output conmap header for NCEP2',
     * ' (default: NCEP)'
      write(*,*)' -noflip: Don''t flip latitude direction',
     * ' (default: flip)'
      write(*,*)'  Note: Check with: wgrib -V; if lats are in S -> N',
     * ' order then use -noflip'
      write(*,*)' -Y2K: If 2 digit year is in range 00-20 assumes'
      write(*,*)'       4 digit year of form 20?? (default: year',
     * ' is 19??)'
      write(*,'(''More? [Y]: '',$)')
      read(*,'(A)')qu
      if(qu.eq.'N'.or.qu.eq.'n'.or.qu.eq.'Q'.or.qu.eq.'q')then
        stop
      endif
      write(*,*)'Example for MSLP data:'
         write(*,*)'  wgrib flx.lola.grib.mean.clim.y58-97 ',
     * ' | grep PRES |',
     * '  grep MSL | grep 58010100 |',
     * ' wgrib -i flx.lola.grib.mean.clim.y58-97',
     * ' >! hdr'
      write(*,*)'  readgribn -h hdr'
      write(*,*)'Part example for Gaussian surface data:'
      write(*,*)'  readgribn -G -h hdr.ICEC.sfc'
      write(*,*)'Example using record numbers and NCEP2:'
      write(*,*)'  wgrib -d 123 msl.1979010106.grib >! hdr'
      write(*,*)'  readgribn -h hdr -ncep2'
      write(*,*)'  (output file: PRES.ncep2.79010106.cmp)'
      write(*,*)'  (if -l given: pres.ncep2.79010106.cmp)'
      write(*,*)'  ... and for the cyclone tracking scheme:'
      write(*,*)'  readgribn -h hdr -ncep2 -cyc'
      write(*,*)'  (output file: PMSL.ncep2.79010106.cmp)'
      write(*,*)'  (note PMSL instead of PRES)'
      write(*,*)'Example using wgrib -i and a multiple',
     * ' record grib file:'
      write(*,*)'  wgrib uv.grib | wgrib -i uv.grib >! hdr.uv'
      write(*,*)'  readgribn -h hdr.uv -ncep2'
      write(*,*)'  (this will produce a series of conmap files)'
      
      write(*,*)' '
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine parsehdr(ghdr,iy,im,id,ih,cquant,clevel,ctype,
     * iy2k,ifcst,idiag,ierr)
c
c * Purpose: Parses wgrib header (field separator is a colon)
c     for desired information e.g. date and time.
c     Currently returns:
c       date (iy,im,id)
c       time (ih)
c       quantity (cquant) e.g. PRES
c       level (clevel) e.g. MSL, 500
c       type (ctype) e.g. anl, '0hr fcst'
c
c * Notes:
c
c     (1) 31/1/2001 If iy2k= 1 then 4 digit years in the range
c         1900-1920 -> 2000-2020.
c
c * Author: Kevin Keay  Date: 6/1/2001
c
      character*120 ghdr
      character*32 cquant,clevel,ctype
c
      character*1 cparse
      parameter (cparse= ':')  ! Field separator
      parameter (maxf=20) 
      character*32 chdr(maxf)
      integer ichdr(maxf)
c
      character*120 hdr
c
      ierr= 0
c
      do k=1,maxf
        chdr(k)= ' '
        ichdr(k)= 0
      enddo
c
      lhdr= ilen(ghdr)
      hdr= ghdr(1:lhdr)
      if(idiag.eq.1)write(*,*)hdr
c
      i1= 1
      i2= lhdr
      k= 0
      do while (i1.ne.-1)
        do i=i1,i2
          if(hdr(i:i).eq.cparse)then
            k= k +1
            chdr(k)= hdr(i1:i-1)
            ichdr(k)= i -i1
            i1= i +1
            goto 10
          endif
        enddo
        k= k +1
        chdr(k)= hdr(i1:i2)
        ichdr(k)= i2 -i1 +1
        i1= -1
10      continue
      enddo
      nchdr= k
c
      if(idiag.eq.1)then
        do k=1,nchdr
          write(*,'(I5,2x,I2,2x,A)')k,ichdr(k),chdr(k)(1:ichdr(k))
        enddo
      endif
c
      if(ichdr(3).eq.10)then  ! 2 digit year
        read(chdr(3),'(2x,4I2)')iy,im,id,ih
      elseif(ichdr(3).eq.12)then ! 4 digit year
        read(chdr(3),'(2x,I4,3I2)')iy,im,id,ih
	if(iy2k.eq.1)then
	  if(iy.ge.1900.and.iy.le.1920)then
	    iy= iy +100  ! Y2K fix for wgrib -4yr option
	  endif
	endif
      endif
c * If ifcst= 1 then date-time (d=) is forecast origin
c   Need to add time as given in field 13 e.g. ':3hr fcst:'
      if(ifcst.eq.1)then
        ihx= index(chdr(13),'hr') -1
        read(chdr(13)(1:ihx),*)ihf
	if(idiag.eq.1)write(*,*)'ifcst=1: ihf=',ihf
	if(ihf.lt.24)then
	  ih= ihf
	else ! Need to increment date by 1 day
	  ih= 0
          call datevalu (id,im,iy,jval)
	  jval= jval +1 ! Increment Julian date by 1 day
          call valudate (jval,id,im,iy)
          if(idiag.eq.1)write(*,*)'ifcst=1: ',iy,im,id,ih
	endif
      endif
      if(ichdr(3).lt.10.or.ichdr(3).gt.12)then
        write(*,*)'Header field 3: ',chdr(3)(1:ichdr(3))
        write(*,*)'ERROR: Date-time format not recognised'
        ierr= ierr +1
        iy= 0
        im= 0
        id= 0
        ih= 0
      endif
      if(idiag.eq.1)write(*,*)iy,im,id,ih
c
      cquant= chdr(4)(1:ichdr(4)) 
      clevel= chdr(12)(1:ichdr(12)) 
      ctype= chdr(13)(1:ichdr(13)) 
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine valudate (dval,d,m,y)
c
c     gives day, month, and year for given Julian day
c        written by David Hooke
c
      integer  dval, d, m, y
      integer  dv, t0
c
      t0 = 4
      y = t0*400 + 1
      dv = dval
      y = y + (dv/146097)*400
      dv = mod (dv,146097)
      y = y + (dv/36524)*100
      if (dv .eq. 146096) go to 10
      dv = mod (dv,36524)
      y = y + (dv/1461)*4
      dv = mod (dv,1461)
      y = y + dv/365
      if (dv .eq. 1460) go to 10
      dv = mod (dv,365)
      m    = 1
cc---------
cc      do while (dv .ge. monthlen(m,y))
cc           dv = dv - monthlen (m,y)
cc           m = m + 1
cc      enddo
cc---------
c==========
      iwhile= 0
1000  if(dv.ge.monthlen(m,y))then
        iwhile= 1
        dv= dv - monthlen(m,y)
        m= m+1
      else
        iwhile= 0
      endif
      if(iwhile.eq.1)goto 1000
c==========
      d = dv + 1
      go to 90
  10  continue
      y = y - 1
      m = 12
      d = 31
  90  continue
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine datevalu (d,m,y,dval)
c
c     gives julian day for day, month, and year
c       written by David Hooke
c
      integer  d, m, y, dval, dy, t0
      t0 = 4
      dy = 0
c
c     Compute day of year
c
      do 1000 i = 1,m-1
           dy = dy + monthlen(i,y)     ! days at end of month (m-1)
1000     continue
cc      enddo
      dy = dy + d - 1                  ! days at date (with 1st Jan zero).
c
c     Compute days since start of accumulation period .....................
c       (Day zero is 01/01/1601, negative before then).
c
     0 dval = ((y-1)/100-(t0*4))*36524
     1        + (y-(t0*400+1))/400
     2        + mod ((y-1),100)*365
     3        + mod ((y-1),100)/4
     4        + dy
c
      if (y.le.t0*400) dval = dval - 1
c
      return
      end
 
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      integer function monthlen(imon,iyr)
c
      dimension mday(12)
      data mday/31,28,31,30,31,30,31,31,30,31,30,31/
      monthlen = mday(imon)
      if(imon.eq.2) then
         if(mod(iyr,4).eq.0) then
            monthlen = 29
            if(mod(iyr,100).eq.0) then
               if(mod(iyr,400).ne.0) then 
                  monthlen = 28
               endif
            endif
         endif
      endif
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

