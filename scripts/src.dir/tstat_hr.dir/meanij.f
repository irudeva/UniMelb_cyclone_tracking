      Subroutine meanij(iop,ioz,imin,imax,jmin,jmax,xcen,ycen,
     * rproj,projn,pij,cij,ug,vg,idiag)
      
      parameter (nhlt=65,nklt=54,margin=8)
      parameter (rad=57.295779)
      parameter (pi=3.14159265,twoomega=4.*pi/86400.)
      parameter (g=9.80)

      parameter (nhklt=nhlt*nklt)
      parameter (nh2lt=nhlt+2*margin,nk2lt=nklt+2*margin)
      parameter (nhk2lt=nh2lt*nk2lt)

      dimension phk(nhklt)
      dimension p(nhk2lt),pxx(nhk2lt),pyy(nhk2lt),pxxyy(nhk2lt)
      dimension zhk(nhklt)
      dimension z(nhk2lt),zxx(nhk2lt),zyy(nhk2lt),zxxyy(nhk2lt)
      equivalence (phk(1),zhk(1))
      equivalence (p(1),z(1))
      equivalence (pxx(1),zxx(1))
      equivalence (pyy(1),zyy(1))
      equivalence (pxxyy(1),zxxyy(1))
      dimension pij(imin:imax,jmin:jmax),cij(imin:imax,jmin:jmax)
      dimension ug(imin:imax,jmin:jmax),vg(imin:imax,jmin:jmax)

      real lon(nhlt),lat(nklt),lon2(nh2lt),lat2(nh2lt)
      real lonij,latij,lonlo,latlo,lonhi,lathi
      integer hloklo,hlokhi,hhiklo,hhikhi
      integer hlo,hhi,hav,hdif
      character*80 llfile,llhead,head
      character projn*1

c-------------------------------------------------------------------------------
c     Pressure and Laplacian
c-------------------------------------------------------------------------------

c     Read pmsl array
c     ---------------

   10 read (iop,end=20) nlat
      read (iop) (lat(ilat),ilat=1,nlat)
      read (iop) nlon
      read (iop) (lon(ilon),ilon=1,nlon)
      read (iop) head
      write (6,*) head
      read (iop) (phk(ij),ij=1,nlon*nlat)
      go to 30
   20 write (6,*) ' Unexpected end of pmsl file'
      stop
   30 continue

c     Expand pmsl array
c     -----------------

      llfile = 'llout.p'
      call llexpand(nhklt,nhk2lt,nlon,nlat,lon,lat,nh2,nk2,lon2,
     * lat2,0,8,8,8,8,phk,p,llhead,llfile,87,idiag,ie3)
      if (idiag.ge.2) write (6,*) ' Arrays expanded.'
      
c     Obtain pmsl derivatives
c     -----------------------

      call bisplcf(nh2,nk2,lon2,lat2,
     * p,pxx,pyy,pxxyy,1,nh2,1,nk2,idiag,ie4)
      if (idiag.ge.2) write (6,*) ' Splines done'

c     Find geopotential gradient for (i,j) points
c     -------------------------------------------

      do 100 j = jmin,jmax
        ys = float(j) - ycen
      do 100 i = imin,imax

c       Convert (i,j) to (lon,lat)
c       --------------------------

        xs = float(i) - xcen
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

        if ((lonij.gt.lon2(nh2)).or.(lonij.lt.lon2(1)).or.
     *    (latij.gt.lat2(nk2)).or.(latij.lt.lat2(1))) then
          write (6,*) i,j,xs,ys,latij,lonij
          stop ' Outside lat,lon range'
        endif

        hlo = 1
        hhi = nh2
  50    continue
        hdif = hhi - hlo
        if (hdif.gt.1) then
          hav = (hhi + hlo)/2
          if (lon2(hav).gt.lonij) then
            hhi = hav
          else
            hlo = hav
          endif
          go to 50
        endif
        lonlo = lon2(hlo)
        lonhi = lon2(hhi)

        klo = 1
        khi = nk2
  60    continue
        kdif = khi - klo
        if (kdif.gt.1) then
          kav = (khi + klo)/2
          if (lat2(kav).gt.latij) then
            khi = kav
          else
            klo = kav
          endif
        go to 60
        endif

        latlo = lat2(klo)
        lathi = lat2(khi)

        dlon = lonhi - lonlo
        dlat = lathi - latlo
        dlonsq = dlon*dlon
        dlatsq = dlat*dlat

        hloklo = hlo + (klo-1)*nh2
        hhiklo = hhi + (klo-1)*nh2
        hlokhi = hlo + (khi-1)*nh2
        hhikhi = hhi + (khi-1)*nh2

c       Pressure value
c       --------------

        A2 = (lonij - lonlo)/dlon
        A1 = 1. - A2
        A3 = A1*(A1*A1 - 1.)*dlonsq/6.
        A4 = A2*(A2*A2 - 1.)*dlonsq/6.
        B2 = (latij - latlo)/dlat
        B1 = 1. - B2
        B3 = B1*(B1*B1 - 1.)*dlatsq/6.
        B4 = B2*(B2*B2 - 1.)*dlatsq/6.

        pij(i,j) = B1*(A1*p(hloklo) + A2*p(hhiklo)+
     *                 A3*pxx(hloklo)  + A4*pxx(hhiklo))   +
     *             B2*(A1*p(hlokhi)    + A2*p(hhikhi)   +
     *                 A3*pxx(hlokhi)  + A4*pxx(hhikhi))   +
     *             B3*(A1*pyy(hloklo)  + A2*pyy(hhiklo) +
     *                 A3*pxxyy(hloklo)+ A4*pxxyy(hhiklo)) +
     *             B4*(A1*pyy(hlokhi)  + A2*pyy(hhikhi) +
     *                 A3*pxxyy(hlokhi)+ A4*pxxyy(hhikhi)) 

        cij(i,j) = B1*(A1*pxx(hloklo)  + A2*pxx(hhiklo))   +
     *             B2*(A1*pxx(hlokhi)  + A2*pxx(hhikhi))   +
     *             B3*(A1*pxxyy(hloklo)+ A2*pxxyy(hhiklo)) +
     *             B4*(A1*pxxyy(hlokhi)+ A2*pxxyy(hhikhi)) +
     *             A1*(B1*pyy(hloklo)  + B2*pyy(hlokhi))   +
     *             A2*(B1*pyy(hhiklo)  + B2*pyy(hhikhi))   +
     *             A3*(B1*pxxyy(hloklo)+ B2*pxxyy(hlokhi)) +
     *             A4*(B1*pxxyy(hhiklo)+ B2*pxxyy(hhikhi)) 

c       f  = twoomega*sin(latij/rad)
c       rho= 0.0012
c       vort = 1./(f*rho)*cc/(111111.**2)

  100 continue

c-------------------------------------------------------------------------------
c     500 mb. geostrophic winds
c-------------------------------------------------------------------------------

c     Read z500 array
c     ---------------

  110 read (ioz,end=120) nlat
      read (ioz) (lat(ilat),ilat=1,nlat)
      read (ioz) nlon
      read (ioz) (lon(ilon),ilon=1,nlon)
      read (ioz) head
      write (6,*) head
      read (ioz) (zhk(ij),ij=1,nlon*nlat)
      go to 130
  120 write (6,*) ' Unexpected end of z500 file'
      stop
  130 continue

c     Expand z500 array
c     -----------------

      llfile = 'llout.z'
      call llexpand(nhklt,nhk2lt,nlon,nlat,lon,lat,nh2,nk2,lon2,
     * lat2,0,8,8,8,8,zhk,z,llhead,llfile,88,idiag,ie3)
      if (idiag.ge.2) write (6,*) ' Arrays expanded.'
      
c     Obtain z500 derivatives
c     -----------------------

      call bisplcf(nh2,nk2,lon2,lat2,
     * z,zxx,zyy,zxxyy,1,nh2,1,nk2,idiag,ie4)
      if (idiag.ge.2) write (6,*) ' Splines done'

c     Find geopotential gradient for (i,j) points
c     -------------------------------------------

      do 200 j = jmin,jmax
        ys = float(j) - ycen
      do 200 i = imin,imax

c       Convert (i,j) to (lon,lat)
c       --------------------------

        xs = float(i) - xcen
        rssq = xs*xs + ys*ys
        rs = sqrt(rssq)
        colat = 2.*rad*atan2(rs,rproj)
        ug(i,j) = spval
        vg(i,j) = spval
        if (colat.gt.85.) go to 190
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

        if ((lonij.gt.lon2(nh2)).or.(lonij.lt.lon2(1)).or.
     *    (latij.gt.lat2(nk2)).or.(latij.lt.lat2(1))) then
          write (6,*) i,j,xs,ys,latij,lonij
          stop ' Outside lat,lon range'
        endif

        hlo = 1
        hhi = nh2
 150    continue
        hdif = hhi - hlo
        if (hdif.gt.1) then
          hav = (hhi + hlo)/2
          if (lon2(hav).gt.lonij) then
            hhi = hav
          else
            hlo = hav
          endif
          go to 150
        endif
        lonlo = lon2(hlo)
        lonhi = lon2(hhi)

        klo = 1
        khi = nk2
 160    continue
        kdif = khi - klo
        if (kdif.gt.1) then
          kav = (khi + klo)/2
          if (lat2(kav).gt.latij) then
            khi = kav
          else
            klo = kav
          endif
        go to 160
        endif

        latlo = lat2(klo)
        lathi = lat2(khi)

        dlon = lonhi - lonlo
        dlat = lathi - latlo
        dlonsq = dlon*dlon
        dlatsq = dlat*dlat

        hloklo = hlo + (klo-1)*nh2
        hhiklo = hhi + (klo-1)*nh2
        hlokhi = hlo + (khi-1)*nh2
        hhikhi = hhi + (khi-1)*nh2

        A2 = (lonij - lonlo)/dlon
        A1 = 1. - A2
        A3 = A1*(A1*A1 - 1.)*dlonsq/6.
        A4 = A2*(A2*A2 - 1.)*dlonsq/6.
        B2 = 1.
        B1 = -1.
        y2 = (latij - latlo)/dlat
        y1 = 1. - y2
        B3 = -(3.*y1*y1 - 1.)*dlatsq/6.
        B4 =  (3.*y2*y2 - 1.)*dlatsq/6.

        zy = B1*(A1*z(hloklo)    + A2*z(hhiklo)   +
     *           A3*zxx(hloklo)  + A4*zxx(hhiklo))   +
     *       B2*(A1*z(hlokhi)    + A2*z(hhikhi)   +
     *           A3*zxx(hlokhi)  + A4*zxx(hhikhi))   +
     *       B3*(A1*zyy(hloklo)  + A2*zyy(hhiklo) +
     *           A3*zxxyy(hloklo)+ A4*zxxyy(hhiklo)) +
     *       B4*(A1*zyy(hlokhi)  + A2*zyy(hhikhi) +
     *           A3*zxxyy(hlokhi)+ A4*zxxyy(hhikhi)) 

        A2 = 1.
        A1 = -1.
        x2 = (lonij - lonlo)/dlon
        x1 = 1. - x2
        A3 = -(3.*x1*x1 - 1.)*dlonsq/6.
        A4 =  (3.*x2*x2 - 1.)*dlonsq/6.
        B2 = (latij - latlo)/dlat
        B1 = 1. - B2
        B3 = B1*(B1*B1 - 1.)*dlatsq/6.
        B4 = B2*(B2*B2 - 1.)*dlatsq/6.

        zx = B1*(A1*z(hloklo)    + A2*z(hhiklo)   +
     *           A3*zxx(hloklo)  + A4*zxx(hhiklo))   +
     *       B2*(A1*z(hlokhi)    + A2*z(hhikhi)   +
     *           A3*zxx(hlokhi)  + A4*zxx(hhikhi))   +
     *       B3*(A1*zyy(hloklo)  + A2*zyy(hhiklo) +
     *           A3*zxxyy(hloklo)+ A4*zxxyy(hhiklo)) +
     *       B4*(A1*zyy(hlokhi)  + A2*zyy(hhikhi) +
     *           A3*zxxyy(hlokhi)+ A4*zxxyy(hhikhi)) 

        zx = zx/(dlon*111111.)
        zy = zy/(dlat*111111.)
        f  = twoomega*sin(latij/rad)

        ugx = -g/f * zy
        vgx =  g/f * zx

        if ((projn.eq.'N').or.(projn.eq.'n')) then
          sinth =  xs/rs
          costh = -ys/rs
        else
          sinth = -xs/rs
          costh =  ys/rs
        endif
        ug(i,j) =  ugx*costh - vgx*sinth
        vg(i,j) =  ugx*sinth + vgx*costh
  190   continue
  200 continue

      return
      end
