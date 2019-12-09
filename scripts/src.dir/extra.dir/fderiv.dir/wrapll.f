      Subroutine wrapll(ylat,xlon,nlat,nlon,nlonx,iwrapE,iwrapS,iwrapN)

c         This subroutine examines a given set of latitudes and longitudes and
c     (a) checks that latitudes be in S-N order in the range (-90<=ylat<=90) 
c         and that longitudes be in E-W order;
c     (b) increments longitudes by 360 deg. where necessary; and
c     (c) checks the continuity of latitudes across the poles and of
c         longitudes in the W-E direction.

c-------------------------------------------------------------------------------

c     ylat(j),j=1,nlat = latitudes
c     xlon(j),j=1,nlon = longitudes
c     nlonx   = no. of independent (non-repeating) longitudes

c     iwrapE   = 0   only one longitude in array
c                1   xlon(nlon) not close to xlon(1) (+ 360.)
c                2   xlon(nlon) close to xlon(1) (+ 360.)
c                     ( xlon(1) (+360.) -xlon(nlon) < xlon(2)-xlon-1 )
c                       i.e. periodic domain with no repeated longitudes
c                3   xlon(nlon) = xlon(1) (+ 360.)
c                       i.e. periodic domain with 1 repeated longitude
c                4   xlon(nlon) > xlon(1) (+ 360.)
c                       i.e. periodic domain with 1 or more repeated longitudes

c     iwrapN,S = 0   only one latitude in array
c                1   ylat(nlat) not close to 90N,S
c                2   ylat(nlat) close to 90N,S
c                     ( for N: 90.-ylat(nlat) < ylat(nlat)-ylat(nlat-1)
c                           S: ylat(1)-(-90.) < ylat(2)   -ylat(1)      
c                       i.e. periodic at pole without polar longitude )
c                3   ylat(nlat) = 90N/S
c                       i.e. periodic at pole with polar longitude

c-------------------------------------------------------------------------------

      real ylat(nlat),xlon(nlon)
      logical err

c     Check that latitudes be in S-N order
c     ------------------------------------

      ylast = ylat(1)
      do 100 j=2,nlat
        if (ylat(j).le.ylast) then
          write (6,'(a,100f6.1)') ' Latitudes: ',ylat
          stop ' Latitudes not in S-N order.'
        endif
        ylast = ylat(j)
  100 continue
      if (ylat(1).lt.-90.) write (6,*)
     *  ' First latitude < -90.'
      if (ylat(nlat).gt.90.) write (6,*)
     *  ' Last latitude > 90.'

c     Check that longitudes be in W-E order
c     -------------------------------------

      err = .false.
      xadd = 0.
      xlast = xlon(1)
      do 200 i=2,nlon
        xlon(i) = xlon(i) + xadd
        if (xlon(i).lt.xlast) then
           xlon(i) = xlon(i) + 360.0
           xadd   = xadd    + 360.0
        endif
        if ((xlon(i).eq.xlast).or.(xlon(i)-xlon(1).gt.540.)
     *   .or.(xlon(i)-xlast.gt.180.)) err = .true.
        xlast = xlon(i)
  200 continue
      if (err) then
        write (6,'(a,100f6.1)') ' Longitudes: ',xlon
        stop ' Longitudes not in W-E order.'
      endif

c     Check continuity of latitudes across S Pole
c     -------------------------------------------

      if (nlat.eq.1) then
        iwrapS = 0
      else if (abs(ylat(1)+90.).lt.0.001) then
        iwrapS = 3
      else
        aylatlolt = 2*ylat(1)-ylat(2)
        iwrapS = 1
        if (aylatlolt.le.-89.975) iwrapS = 2
      endif

c     Check continuity of latitudes across N Pole
c     -------------------------------------------

      if (nlat.eq.1) then
        iwrapN = 0
      else if (ylat(nlat).ge.89.975) then
        iwrapN = 3
      else
        aylathilt = 2*ylat(nlat)-ylat(nlat-1)
        iwrapN = 1
        if (aylathilt.ge.90.) iwrapN = 2
      endif

c     Check continuity of latitudes across S Pole
c     -------------------------------------------

      axlonra = xlon(nlon) - xlon(1)
      if (nlon.eq.1) then
        iwrapE = 0
      else if (axlonra.gt.360.025) then
        iwrapE = 4
        exlon = xlon(1) + 359.975
        do 300 i = nlon,1,-1
          if (xlon(i).lt.exlon) then
            nlonx = i
            return
          endif
  300   continue 
      else if (axlonra.ge.359.975) then
        iwrapE = 3
        nlonx = nlon - 1
      else
        dmax1 = max((xlon(2)-xlon(1)),
     *               (xlon(nlon)-xlon(nlon-1))) !KK 11/12/2003
        dmax2 = xlon(1)+360.-xlon(nlon)
        iwrapE = 1
        if (dmax2.le.dmax1) iwrapE = 2
        nlonx = nlon
      endif

      return
      end
