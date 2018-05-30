      Subroutine intgpr(nh,nk,f,fxx,fyy,fxxyy,projn,rproj,intopt,
     * ni,nj,xcen,ycen,lon,lat,fij,spval,ierr,idiag,ier)

c       This subroutine transforms a latitude-longitude array into a
c     an array superimposed upon a polar stereographic projection centred
c     upon the N or S Pole using bicubic spline interpolation.  

c     Written by R.J. Murray
c     Last revised 1st Aug., 1995.

c-------------------------------------------------------------------------------
c     Explanation
c-------------------------------------------------------------------------------

c     lon(h),lat(k)    Longitudes and latitudes of the (nh*nk) input array
c                        (These may be as expanded by the calling programme.)
c     f(h,k)           Array value at (h,k) grid point of the array

c     projn            Projection required: S or s = PS SH, N or n = PS NH
c     rproj            No. of grid point spaces between pole and equator in
c                        PS projection
c     (x,y)            Coordinates (i,j) on the (ni*nj) PS array
c     (xcen,ycen)      Position on array of pole of projection.
c     (xs,ys)          Coordinates w.r.t. the pole (xs = x -xcen, &c.)
c     colat,theta      Colatitude and orientation (w.r.t. GM)
c     latij,lonij      Latitude and longitude of grid point (i,j)
c     hlo,hhi,klo,khi  Coordinates of the latitude-longitude grid box
c                        surrounding (i,j)
c     A1,A2,A3,A4      Interpolative fractions in the longitudinal direction
c     B1,B2,B3,B4      Interpolative fractions in the latitudinal direction
c     fij(i,j)         Interpolated value at the (i,j) point on PS array

c-------------------------------------------------------------------------------
c     Declarations
c-------------------------------------------------------------------------------

      parameter (rad=57.295779)
      parameter (r6=1./6.)

      character projn*1
      integer hlo,hhi,hdif,hav
      real lon(nh),lat(nk)
      real latij,latlo,lathi,lonij,lonlo,lonhi
      dimension f(nh,nk),fxx(nh,nk),fyy(nh,nk),fxxyy(nh,nk)
      dimension fij(ni,nj)

c-------------------------------------------------------------------------------
c     Interpolation for each (PS) grid point
c-------------------------------------------------------------------------------

      do 90 j = 1,nj
        y = j
        ys = y - ycen
        do 80 i = 1,ni
          x = i
          xs = x - xcen

c         Grid position in latitude and longitude
c         ---------------------------------------

          rssq = xs*xs + ys*ys
          rs = sqrt(rssq)
          colat = 2.*rad*atan2(rs,rproj)
          theta = rad*atan2(xs,ys)
          if ((projn.eq.'N').or.(projn.eq.'n')) then
            latij = 90. - colat
            lonij = 180. - theta 
          else
            latij = -90. + colat
            lonij = theta
          endif
          if (lonij.lt.0.)   lonij = lonij + 360.
          if (lonij.gt.360.) lonij = lonij - 360.

c         Longitude array position (w.r.t. coordinate h)
c         ----------------------------------------------

          if ((lonij.gt.lon(nh)).or.(lonij.lt.lon(1))) go to 40

          hlo = 1
          hhi = nh
  10      continue
          hdif = hhi - hlo
          if (hdif.gt.1) then
            hav = (hhi + hlo)/2
            if (lon(hav).gt.lonij) then
              hhi = hav
            else
              hlo = hav
            endif
          go to 10
          endif
          lonlo = lon(hlo)
          lonhi = lon(hhi)
          A2 = (lonij - lonlo)/(lonhi - lonlo)
          A1 = 1. - A2

c         Latitude array position (w.r.t. coordinate k)
c         ---------------------------------------------

          if ((latij.gt.lat(nk)).or.(latij.lt.lat(1))) go to 40

          klo = 1
          khi = nk
  20      continue
          kdif = khi - klo
          if (kdif.gt.1) then
            kav = (khi + klo)/2
            if (lat(kav).gt.latij) then
              khi = kav
            else
              klo = kav
            endif
          go to 20
          endif
          latlo = lat(klo)
          lathi = lat(khi)
          B2 = (latij - latlo)/(lathi - latlo)
          B1 = 1. - B2

          if (A1.ne.0) then
            if ((B1.ne.0.).and.(f(hlo,klo).eq.spval)) go to 40
            if ((B2.ne.0.).and.(f(hlo,khi).eq.spval)) go to 40
          endif
          if (A2.ne.0) then
            if ((B1.ne.0.).and.(f(hhi,klo).eq.spval)) go to 40
            if ((B2.ne.0.).and.(f(hhi,khi).eq.spval)) go to 40
          endif

          if ((intopt.eq.1).or.((inopt.eq.2).and.(rs.gt.rproj))) then

c           Bilinear interpolation
c           ----------------------

            fij(i,j) =  (f(hlo,klo)*A1 + f(hhi,klo)*A2)*B1
     *                + (f(hlo,khi)*A1 + f(hhi,khi)*A2)*B2

          else

c           Bicubic spline interpolation
c           ----------------------------

            if (A2.eq.0.) then
              if (B2.eq.0.) then
                fij(i,j) =  f(hlo,klo)
              else
                dlat = lat(khi) - lat(klo)
                dlatsqr6 = dlat*dlat*r6
                B3 = B1*(B1*B1 - 1.)*dlatsqr6
                B4 = B2*(B2*B2 - 1.)*dlatsqr6
                fij(i,j) =  B1*f(hlo,klo)        + B2*f(hlo,khi)
     *                    + B3*fyy(hlo,klo)      + B4*fyy(hlo,khi)
              endif
            else 
              dlon = lon(hhi) - lon(hlo)
              dlonsqr6 = dlon*dlon*r6
              A3 = A1*(A1*A1 - 1.)*dlonsqr6
              A4 = A2*(A2*A2 - 1.)*dlonsqr6
              if (B2.eq.0.) then
                fij(i,j) =  A1*f(hlo,klo)        + A2*f(hhi,klo) 
     *                    + A3*fxx(hlo,klo)      + A4*fxx(hhi,klo)
              else
                dlat = lat(khi) - lat(klo)
                dlatsqr6 = dlat*dlat*r6
                B3 = B1*(B1*B1 - 1.)*dlatsqr6
                B4 = B2*(B2*B2 - 1.)*dlatsqr6
                fij(i,j) =  B1*(A1*f(hlo,klo)    + A2*f(hhi,klo)   
     *                         +A3*fxx(hlo,klo)  + A4*fxx(hhi,klo))
     *                    + B2*(A1*f(hlo,khi)    + A2*f(hhi,khi)   
     *                         +A3*fxx(hlo,khi)  + A4*fxx(hhi,khi))
     *                    + B3*(A1*fyy(hlo,klo)  + A2*fyy(hhi,klo) 
     *                         +A3*fxxyy(hlo,klo)+ A4*fxxyy(hhi,klo))
     *                    + B4*(A1*fyy(hlo,khi)  + A2*fyy(hhi,khi) 
     *                         +A3*fxxyy(hlo,khi)+ A4*fxxyy(hhi,khi)) 
              endif
            endif
          endif

          go to 50
  40      continue
          fij(i,j) = spval
  50      continue
          if (idiag.ge.4) write (6,*) i,j,lonij,hlo,hhi,lon(hlo),
     *     lon(hhi),latij,klo,khi,lat(klo),lat(khi)
  80    continue
  90  continue

      return
      end

