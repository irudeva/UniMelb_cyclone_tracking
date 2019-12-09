      Subroutine globtest(nlat,nlon,nlonx,iworldx,iworldS,
     * iworldN,lat,lon,njlt,nilt)

      real lat(njlt),lon(nilt)

          if (lat(1).eq.-90.) then
              iworldS = 3
          else if (nlat.eq.1) then
              iworldS = 0
          else
              alatlolt = 2*lat(1)-lat(2)
              iworldS = 1
              if (alatlolt.le.-90.) iworldS = 2
          endif
          if (lat(nlat).eq.90.) then
              iworldN = 3
          else if (nlat.eq.1) then
              iworldN = 0
          else
              alathilt = 2*lat(nlat)-lat(nlat-1)
              iworldN = 1
              if (alathilt.ge.90.) iworldN = 2
          endif

          alonra = lon(nlon) - lon(1)
          if (alonra.gt.360.025) then
              iworldx = 4
              elon = lon(1) + 359.975
              do 100 i = nlon,1,-1
                  if (lon(i).lt.elon) then
                      nlonx = i
                      return
                  endif
  100         continue 
          else if (alonra.ge.359.975) then
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
