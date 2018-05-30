      Subroutine globtest(nlat,nlon,nlonx,iworldx,iworldS,
     * iworldN,lat,lon,njlt,nilt)

c         This routine tests continuity across E and W boundaries
c     and across the Poles.

c     iworldx = 1  No periodicity in longitude
c               2  Periodic in longitude with no repeated logitudes
c               3  Periodic in longitude with initial = final long

c     iworldS = 1  No periodicity in latitude across S Pole
c               2  Periodic in latitude with no value given for SP
c               3  Periodic in latitude with value given for SP

c     Written by R.J. Murray.

c-----------------------------------------------------------------------

      real lat(njlt),lon(nilt)

          if (abs(lat(1)+90.).lt.0.1) then
              iworldS = 3
          else if (nlat.eq.1) then
              iworldS = 0
          else
              alatlolt = 2*lat(1)-lat(2)
              iworldS = 1
              if (alatlolt.le.-90.) iworldS = 2
          endif
          if (abs(lat(nlat)-90.).lt.0.1) then
              iworldN = 3
          else if (nlat.eq.1) then
              iworldN = 0
          else
              alathilt = 2*lat(nlat)-lat(nlat-1)
              iworldN = 1
              if (alathilt.ge.90.) iworldN = 2
          endif

          alonra = lon(nlon) - lon(1)
          if (alonra.gt.360.001) stop ' Longitude range > 360 deg.'
          if (alonra.ge.359.975) then
              iworldx = 3
              nlonx = nlon - 1
          else if (nlon.eq.1) then
              iworldx = 0
              nlonx = 1
          else
              circumexp = alonra*float(nlon)/float(nlon-1)
              iworldx = 1
              if (circumexp.ge.359.975) iworldx = 2
              nlonx = nlon
          endif

          return
          end
