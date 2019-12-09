      Subroutine llmaprd(iunit,head,nlonlt,nlatlt,narrlt,nlat,lat,
     * nlon,lon,arr,quant,level,lunit,source,dmode,da,hr,unit,grid,ier)

      integer da,hr
      character quant*8,level*9,lunit*10,source*10,grid*17, 
     * unit*12,dmode*6
      character*80 head

      dimension arr(narrlt)
      real lon(nlonlt),lat(nlatlt)
      integer nlat

c     Last revised 11th Jan., 1996.

c-------------------------------------------------------------------------------
c     Read data file
c-------------------------------------------------------------------------------

      ier = 0

      read (iunit,err=70,end=50) nlat
      if (nlat.lt.1) stop ' NLAT < 1'
      if (nlat.gt.nlatlt) then
        write (6,*) 'NLAT (',nlat,') > NLATLT (',nlatlt,'), or file',
     *   ' type incorrect.'
        stop
      endif

      call qread(iunit,lat,nlat,ier)
      if (ier.eq.2) stop' Error during read of latitudes.'
      if (ier.eq.3) stop' Premature end during read of latitudes.'

      read (iunit,err=70,end=80) nlon
      if (nlon.lt.1) stop ' NLON < 1'
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

      head = ' '
      read (iunit,err=70,end=80) head

      call qread(iunit,arr,narr,ier)
      if (ier.eq.2) stop' Error during read of array data.'
      if (ier.eq.3) stop' Premature end during read of array data.'

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
      read (head,'(a8,1x,a9,12x,a10,2x,i6,1x,i4,4x,a12,1x,a10)',err=100)
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

  100 stop ' Lat-lon data format unrecognised.'

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
