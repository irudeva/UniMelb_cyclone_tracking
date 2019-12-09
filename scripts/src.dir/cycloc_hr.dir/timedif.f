      Subroutine timedif(da1,hr1,da2,hr2,dmodex,dmin,ddhm,dday,
     * err)

c       This subroutine calculates the difference in time between two
c     given dates and times.

c     Last revised 18 Feb 1991,  R.J. Murray.

c-------------------------------------------------------------------------------
c     Modified 09/12/2019, Ahmad Galea (Science IT University of Melbourne)
c     
c     Date format changed from yymmdd to yyyymmdd.
c-------------------------------------------------------------------------------

c-------------------------------------------------------------------------------
c       (1)  Explanation.
c-------------------------------------------------------------------------------

c     dmode            Mode for interpretation of dates and times, e.g.,
c                        ymdhm  year,month,day,hour,minute (yymmddhhmm)
c                        ydd    year,day(1-365) (yydddd)
c                        ddddd  day(d),decimals of day(D) (ddddddDDDD)
c     da2,hr2          day/date and time of day (hours,mins/decimal day)
c     da1,hr1          day/date and time of day of last data record used

c     dmin             Interval in mins.
c     ddhm             Interval in (whole) days, hours, and minutes (ddhhmm)
c     dday             Interval in days (decimal)

c-------------------------------------------------------------------------------
c       (2)  Declarations.
c-------------------------------------------------------------------------------

      character dmode*6,dmodex*6,ada1*8,ada2*8,ahr1*4,ahr2*4
      integer da1,hr1,da2,hr2
      integer err,dmin,ddhm
      dimension idaym(12)
      logical calyr

      data idaym/0,31,59,90,120,151,181,212,243,273,304,334/
      
      err = 0
      dmode = dmodex
      if (dmode(1:3).eq.'ddd') dmode(1:3) = 'DDD'
      if (dmode(1:3).eq.'ydd') dmode(1:3) = 'YDD'
      if (dmode(1:3).eq.'ymd') dmode(1:3) = 'YMD'
      if (dmode(4:4).eq.'d')   dmode(4:5) = 'DD'
      if (dmode(4:4).eq.'h')   dmode(4:5) = 'HM'
      if (dmode(4:4).eq.'D')   dmode(4:5) = 'DD'
      if (dmode(4:4).eq.'H')   dmode(4:5) = 'HM'
      calyr = .true.
      if (dmode(6:6).eq.'1')   calyr = .false.

      if ((dmode(1:3).ne.'YMD').and.(dmode(1:3).ne.'YDD').and.
     * (dmode(1:3).ne.'DDD')) go to 110
      if ((dmode(4:4).ne.'H').and.(dmode(4:4).ne.' ')) go to 110
      if ((da1.lt.0).or.(da2.lt.0)) go to 130

c     Ensures dates in 4 format YYYYMMDD
      if (dmode(1:1).eq.'D') then
c       if ((da1.gt.999).or.(da2.gt.999)) go to 130
        if ((da1.gt.99999999).or.(da2.gt.999999)) go to 130
      else if (dmode(1:3).eq.'YDD') then
        if ((da1.gt.99999999).or.(da2.gt.999999)) go to 130
      else
        if ((da1.gt.99991231).or.(da2.gt.99991231)) go to 130
      endif

      if (dmode(4:4).eq.'H') then
        write (ahr1,'(i4)') hr1
        read (ahr1(1:2),'(i2)') ihour1
        read (ahr1(3:4),'(i2)') imin1
        write (ahr2,'(i4)') hr2
        read (ahr2(1:2),'(i2)') ihour2
        read (ahr2(3:4),'(i2)') imin2

        if ((ihour1.gt.23).or.(imin1.gt.59)) go to 130
        if ((ihour2.gt.23).or.(imin1.gt.59)) go to 130
        if ((hr1.lt.0).or.(hr2.lt.0)) go to 130
      else
        if ((hr1.ne.0).or.(hr2.ne.0)) then
          err = 1
          write (6,*)' hr2 = hr1 = 0 unless dmode(5:5) = ''H'' '
        endif
      endif

      if (dmode(1:1).eq.'D') then
        iday1 = da1
        iday2 = da2
      else if (dmode(1:3).eq.'YDD') then
        write (ada1,'(i8)') da1
        read (ada1(1:4),'(i4)') iyear1
        read (ada1(5:8),'(i4)') iday1
        write (ada2,'(i8)') da2
        read (ada2(1:4),'(i4)') iyear2
        read (ada2(5:8),'(i4)') iday2
      else
        write (ada1,'(i8)') da1
        read (ada1(1:4),'(i4)') iyear1
        read (ada1(5:6),'(i2)') imonth1
        read (ada1(7:8),'(i2)') iday1
        write (ada2,'(i8)') da2
        read (ada2(1:4),'(i4)') iyear2
        read (ada2(5:6),'(i2)') imonth2
        read (ada2(7:8),'(i2)') iday2

        mody1 = mod(iyear1,4)
        mody2 = mod(iyear2,4)

        if ((imonth1.lt.1).or.(iday1.lt.0)) go to 130
        if ((imonth1.gt.12).or.(iday1.gt.31)) go to 130
        if (((imonth1.eq.4).or.(imonth1.eq.6).or.(imonth1.eq.9).
     *   or.(imonth1.eq.11)).and.(iday1.gt.30)) go to 130
        if (imonth1.eq.2) then
          if (mody1.eq.0) then
            if (iday1.gt.29) go to 130
          else
            if (iday1.gt.28) go to 130
          endif
        endif

        if ((imonth2.lt.1).or.(iday2.lt.1)) go to 130
        if ((imonth2.gt.12).or.(iday2.gt.31)) go to 130
        if (((imonth2.eq.4).or.(imonth2.eq.6).or.(imonth2.eq.9).
     *   or.(imonth2.eq.11)).and.(iday2.gt.30)) go to 130
        if (imonth2.eq.2) then
          if (mody2.eq.0 .and. calyr) then
            if (iday2.gt.29) go to 130
          else
            if (iday2.gt.28) go to 130
          endif
        endif
      endif

      if (dmode(1:1).eq.'Y') then
        iydif = iyear2 - iyear1
        if (iydif.lt.-50) iyear2 = iyear2 + 100
        if (iydif.gt. 50) iyear2 = iyear2 - 100
      endif

      isign = 1
      if (iyear2.gt.iyear1) go to 50
      if (iyear2.lt.iyear1) go to 40
      if (imonth2.gt.imonth1) go to 50
      if (imonth2.lt.imonth1) go to 40
      if (iday2.gt.iday1) go to 50
      if (iday2.lt.iday1) go to 40
      if (ihour2.gt.ihour1) go to 50
      if (ihour2.lt.ihour1) go to 40
      if (imin2.ge.imin1) go to 40
   40 continue

      isign = -1
      iyearx = iyear1
      imonthx = imonth1
      idayx = iday1
      ihourx = ihour1
      iminx = imin1

      iyear1 = iyear2
      imonth1 = imonth2
      iday1 = iday2
      ihour1 = ihour2
      imin1 = imin2

      iyear2 = iyearx
      imonth2 = imonthx
      iday2 = idayx
      ihour2 = ihourx
      imin2 = iminx
   50 continue

      idaysub = 0
      if ((dmode(4:4).eq.'H').or.(dmode(4:4).eq.'h')) then
        idmin = imin2 - imin1
        if (idmin.lt.0) then
          idmin = idmin + 60
          ihour2 = ihour2 - 1
        endif

        idhour = ihour2 - ihour1
        if (idhour.lt.0) then
          idhour = idhour + 24
          idaysub = 1
        endif
      endif

      if ((dmode(1:1).eq.'D').or.(dmode(1:1).eq.'d')) then
        iday2 = iday2 - idaysub
      else if ((dmode(1:3).eq.'YDD').or.(dmode(1:3).eq.'ydd')) then
        iday2 = iday2 - idaysub
        if (iyear2.ne.iyear1) iday2 = iday2 + 365*(iyear2 - iyear1)
      else
        iday1 = iday1 + idaym(imonth1)
        if ((imonth1.ge.3).and.(mody1.eq.0).and.calyr)
     *   iday1 = iday1 + 1
        iday2 = iday2 + idaym(imonth2) - idaysub
        if ((imonth2.ge.3).and.(mody2.eq.0).and.calyr)
     *   iday2 = iday2 + 1

        if (iyear2.ne.iyear1) then
          iyear2m1 = iyear2 - 1
          do 30 iy = iyear1,iyear2m1
            iday2 = iday2 + 365
            mody = mod(iy,4)
            if (mody.eq.0 .and. calyr) iday2 = iday2 + 1
 30       continue
        endif
      endif

      idday = iday2 - iday1

      idmina = 60*idhour + idmin
      dday   = float(idday) + float(idmina)/1440.
      dmin   = 1440*idday + idmina
      ddhm   = 10000*idday + 100*idhour + idmin

      if (isign.eq.-1) then
        dday = -dday
        dmin = -dmin
        ddhm = -ddhm
      endif

      return

 110  continue
      err = 5
      write (6,*) ' Improper argument for dmode.'
      return
 130  continue
      err = 4
      write (6,*) ' Improper dates/times'
      return
 140  continue
      err = 3
      write (6,*) ' Time difference < -990 days.'
      dmin = -1425600
      ddhm = -9900000
      dday = -990.
      return
 150  continue
      err = 2
      write (6,*) ' Time difference > 990 days.'
      dmin = 1425600
      ddhm = 9900000
      dday = 990.
      return

      end
