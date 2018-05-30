      Subroutine psconv

      real lon,lat
      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      Entry pstoll(xcen,ycen,rproj,fhem,x,y,lon,lat)

      xs = x - xcen
      ys = y - ycen
      rs = sqrt(xs*xs + ys*ys)
      lon = rad*atan2(xs,-ys*fhem)
      lat = (c90 - c2rad*atan(rs/rproj))*fhem
      if (lon.lt.c0)   lon = lon + c360
      if (lon.gt.c360) lon = lon - c360

      return

      Entry lltops(xcen,ycen,rproj,fhem,lon,lat,x,y)

      clat = c90-lat*fhem
      rs = rproj*tan(clat*r2rad)
      alon = rrad*lon
      x = xcen + rs*sin(alon)
      y = ycen - rs*cos(alon)*fhem

      return
      end
