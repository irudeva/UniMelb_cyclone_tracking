      Subroutine llmaprd(iunit,head,nlonlt,nlatlt,narrlt,nlat,lat,
     * nlon,lon,arr,quant,level,lunit,source,dmode,da,hr,unit,grid,
     * offlon,llform,ier)

      integer da,hr
      character quant*8,level*9,lunit*10,source*10,grid*17, 
     * unit*12,dmode*6
      character*80 head

      dimension arr(narrlt)
      real lon(nlonlt),lat(nlatlt)
      logical llform
      character*80 ch80

c     Last revised 18th Oct., 2005.

c-------------------------------------------------------------------------------
c     Read data file
c-------------------------------------------------------------------------------

      ier = 0

      if (llform) then
        head = ' '
   03   read (iunit,'(a80)',err=110,end=50) head
        if (lnblnk(head).eq.0) go to 03
cli        read (iunit,'(/'' Latitudes  ('',i3,''):'')',err=120,end=90)
cli     *   nlat
ckk        read (iunit,'(i3)',err=120,end=90) nlat
        read (iunit,'(A80)')ch80  !ckk Blank line
        read (iunit,'(A80)')ch80  !ckk
	i1= index(ch80,'(') +1
	i2= index(ch80,')') -1
	read(ch80(i1:i2),*,err=120)nlat

          if (nlat.lt.1) then
            write (6,*) ' NLAT = ',nlat,' < 1'
            stop
          endif
          if (nlat.gt.nlatlt) then
            write (6,*) 'NLAT (',nlat,') > NLATLT (',nlatlt,
     *       ') or file type incorrect.'
            stop
          endif
        read (iunit,*,err=130,end=90) (lat(j),j=1,nlat)
cli        read (iunit,'(/'' Longitudes ('',i3,''):'')',err=140,end=90)
cli     *   nlon
ckk        read (iunit,'(i3)',err=140,end=90) nlon
        read (iunit,'(A80)')ch80  !ckk Blank line
        read (iunit,'(A80)')ch80  !ckk
	i1= index(ch80,'(') +1
	i2= index(ch80,')') -1
	read(ch80(i1:i2),*,err=140)nlon

          if (nlon.lt.1) then
            write (6,*) ' NLON = ',nlon,' < 1'
            stop
          endif
          if (nlon.gt.nlonlt) then
            write (6,*) 'NLON (',nlon,') > NLONLT (',nlonlt,')'
            stop
          endif
          narr = nlat*nlon
          if (narr.gt.narrlt) then
            write (6,*) 'NLAT*NLON = ',narr,' > NARRLT = ',narrlt
            stop
          endif
        read (iunit,*,err=150,end=90) (lon(i),i=1,nlon)
        ij1 = 1
        ij2 = nlon
        do 850 j = 1,nlat
          read (iunit,'(/)',end=90)
          read (iunit,*,err=160,end=90) (arr(ij),ij=ij1,ij2)
          ij1 = ij1 + nlon
          ij2 = ij2 + nlon
  850   continue
      else
        read (iunit,err=70,end=50) nlat
        if (nlat.lt.1) then
            write (6,*) ' NLAT = ',nlat,' < 1'
            stop
        endif
        if (nlat.gt.nlatlt) then
          write (6,*) 'NLAT (',nlat,') > NLATLT (',nlatlt,
     *     ') or file type incorrect.'
          stop
        endif

        call qread(iunit,lat,nlat,ier)
        if (ier.eq.2) stop' Error during read of latitudes.'
        if (ier.eq.3) stop' Premature end during read of latitudes.'

        read (iunit,err=70,end=80) nlon
        if (nlon.lt.1) then
            write (6,*) ' NLON = ',nlon,' < 1'
            stop
        endif
        if (nlon.gt.nlonlt) then
          write (6,*) 'NLON = ',nlon,' > NLONLT = ',nlonlt
          stop
        endif
        narr = nlat*nlon
        if (narr.gt.narrlt) then
          write (6,*) 'NLAT*NLON = ',narr,' > NARRLT = ',narrlt
          stop
        endif

        call qread(iunit,lon,nlon,ier)
        if (ier.eq.2) stop' Error during read of longitudes.'
        if (ier.eq.3) stop' Premature end during read of longitudes.'

c * Temporary fix for regional grid with -ve longitudes KK 21/2/2007
c   Note: offlon should be subtracted from the cycdat output longitudes
        if(offlon.gt.0.)then
          do i=1,nlon
	    lon(i)= lon(i) + offlon
          enddo
        endif

        head = ' '
        read (iunit,err=70,end=80) head

        call qread(iunit,arr,narr,ier)
        if (ier.eq.2) stop' Error during read of array data.'
        if (ier.eq.3) stop' Premature end during read of array data.'
      endif

c-------------------------------------------------------------------------------
c     Interpret header line
c-------------------------------------------------------------------------------

      lunit = ' '
      unit = ' '
      if ((head(19:24).eq.'shanal').or.(head(19:24).eq.'SHANAL')) then
        source = 'SHANAL'
        read (head(40:45),'(i6)') da
        read (head(47:50),'(i6)') hr
        dmode = 'YMDHM'
        grid  = ' '
        quant = head(1:6)
        level = head(7:12)
        if (lnblnk(level).ne.0) lunit = 'M'
        if (quant(1:1).eq.'U') unit = 'M/S'
        if (quant(1:1).eq.'V') unit = 'M/S'
        if (quant(1:1).eq.'Z') unit = 'GPM'
        if (quant(1:1).eq.'Q') unit = '(PARTS)'
      else if (head(13:15).eq.'RUN') then
        dmode = ' '
        quant = head(1:6)
        level = head(7:12)
        source = 'RUN'
        grid  = ' '
        irun = 0
        read (head(17:21),'(i5)',err=05) irun
        write (source(6:10),'(i5)') irun
   05   continue
        if (lnblnk(level).ne.0) lunit = 'M'
        if (quant(1:1).eq.'U') unit = 'M/S'
        if (quant(1:1).eq.'V') unit = 'M/S'
        if (quant(1:1).eq.'Z') unit = 'GPM'
        if (quant(1:1).eq.'Q') unit = '(PARTS)'
        grid = ' ' 
        if (head(23:25).eq.'DAY') then
          dmode(1:3) = 'DDD'
          if (head(27:29).eq.'***') then
            dmode = 'YMDHM'
            read (head(49:54),'(i6)') da
            read (head(56:59),'(i4)') hr
          else
            if (head(30:30).eq.':') then
              read (head(27:32),'(i3,x,i2)') da,ihour
              hr = ihour*100
              dmode(4:5) = 'DD'
            else
              read (head(27:32),'(i6)') da
              hr = 0
              dmode(4:5) = 'HM'
            endif
            if (da.lt.1000) then
              read (head(39:40),'(i2)') iyear
              if (iyear.ge.1) then
                da = da + iyear*10000
                dmode(1:3) = 'YDD'
              endif
            endif
          endif
        else
          dmode(1:3) = 'DDD'
          if (head(34:34).eq.':') then
            read (head(31:38),'(i3,x,i4)') da,hr
            dmode(4:5) = 'DD'
          else
            read (head(31:36),'(i6)') da
            hr = 0
            dmode(4:5) = 'HM'
          endif
        endif
      else
c       read (head,'(a8,a9,x,a10,11x,a6,x,a4,x,a12,a17)',err=100)
        read (head,'(a8,x,a9,12x,a10,2x,i6,x,i4,4x,a12,x,a10)',err=100)
     *    quant,level,source,da,hr,unit,grid
        if (lnblnk(level).ne.0 .and. lnblnk(lunit).eq.0) lunit = 'M'
      endif

      if (lnblnk(unit).eq.0) then
        if (quant(1:6).eq.'ZS    ') unit = 'M'
        if (quant(1:6).eq.'PS    ') unit = 'MB'
        if (quant(1:6).eq.'TS    ') unit = 'DEGK'
        if (quant(1:6).eq.'PMSL  ') unit = 'MB'
        if (quant(1:6).eq.'MSLP  ') unit = 'MB'
        if (quant(1:6).eq.'U10   ') unit = 'M/S'
        if (quant(1:6).eq.'V10   ') unit = 'M/S'
        if (quant(1:6).eq.'T2    ') unit = 'DEGK'
        if (quant(1:6).eq.'TD2   ') unit = 'DEGK'

        if (quant(1:6).eq.'Z     ') unit = 'M'
        if (quant(1:6).eq.'T     ') unit = 'DEGK'
        if (quant(1:6).eq.'U     ') unit = 'M/S'
        if (quant(1:6).eq.'V     ') unit = 'M/S'
        if (quant(1:6).eq.'OMEGA ') unit = 'N/M/S'
        if (quant(1:6).eq.'RH    ') unit = '%'
        if (quant(1:6).eq.'Q     ') unit = '(PARTS)'
      endif

      return

c-------------------------------------------------------------------------------
c     Errors
c-------------------------------------------------------------------------------

   50 ier = 1
      return
   70 stop ' Error during read.'
   80 stop ' Premature end during read of lat.-lon. data head'
   90 stop ' Premature end during read of lat.-lon. data'

  100 stop ' Lat-lon data format unrecognised.'

  110 stop ' Error during formatted read of head'
  120 stop ' Error during formatted read of nlat'
  130 stop ' Error during formatted read of lat'
  140 stop ' Error during formatted read of nlon'
  150 stop ' Error during formatted read of lon'
  160 stop ' Error during formatted read of array'

      end

c===============================================================================

      Subroutine qread(iunit,arr,narr,ier)

      real arr(narr)

      ier = 0
      read (iunit,err=10,end=20) arr
      return

 10   ier = 2
      return
 20   ier = 3
      return

      end
