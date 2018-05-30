      Subroutine steer2(ioup,iovp,fileu,filev,spval,idiag)

c       This routine estimates the cyclone advection velocity from 
c     an upper level wind or geopotential height field.  In the
c     former case the velocities are simply obtained by linear
c     interpolation from the wind data.  In the latter, the velocity 
c     is calculated geostrophically, viz.,

c         (u,v) = -g/fcor (dz/dy, -dzdx),

c     and a bicubic spline interpolation is used to find the
c     geopotential gradients in the E and N directions.  
c     The 1/fcor factor is scaled by an atan**2 factor to limit the
c     velocities to a linear dependence on latitude near the equator.

c       In both types of calculation, the velocities may be scaled by 
c     a chosen factor "upfact" to allow for proportionate differences 
c     between the strength of the westerlies at MSL and at steering 
c     height.

c     Written 20th May 1993, R.J. Murray.

c-------------------------------------------------------------------------------
c     Explanation
c-------------------------------------------------------------------------------


c     nlon,nlat        Dimensions of lat-lon array
c     lon,lat          Longitudes and latitudes
c     f(i,j)           Function value at grid point (i,j), or its negative
c                        when highs are sought
c     fxx,fyy,fxxyy    A cubic spline coefficient arrays (ni*nj) (partial 
c                        derivatives of function w.r.t. xx,yy,xxyy
c         f=u,fxx=v    for actual wind interpolation
c         f = z        for geostrophic wind calculation
c     xc,yc            Cyclone positions
c     upc,vpc          Calculated velocities

c     nillt,njllt  = dimensions of array for storing z data
c     nlon,nlat    = dimensions of z data
c     margin       = margin for wrapping data for spline fitting
c     iunit        = logical unit for z input
c     nklt         = dimension of cyclone data array
c     nk           = number of cyclones in analysis period
c     xc,yc        = x and y coordinates of cyclones
c     xcen,ycen    = x and y coordinates of projection pole
c     rproj        = projection radius of Equator
c     hemis        = N or S (pole projection)
c     dac,hrc      = date and time of cyclone data
c     daz,hrz      = date and time of z data
c     idiag        = diagnostic write level
c     latc,lonc    = lat. and lon. of cyclone
c     ilo,ihi      = / i and j coordinates of lat.-lon. grid
c     jlo,jhi      = \ surrounding latc,lonc
c     lonlo,lonhi  = / longitudes and latitudes surrounding
c     latlo,lathi  = \ latc,lonc
c     A1,..,B4     = bicubic spline interpolating coefficients
c     zx,zy        = gradients of geopotential
c     rfcor        = reciprocal ofCoriolis factor at latc,lonc
c     grav         = acceleration of gravity

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------
      
      character*80 fileu,filev,headu,headv
      character quantu*8,levelu*9,lunitu*10,sourceu*10,unitu*12,
     * dmodeu*6,grid*17
      character quantv*8,levelv*9,lunitv*10,sourcev*10,unitv*12,
     * dmodev*6
      integer dau,hru,dav,hrv

      parameter (r6=1./6.,r180=1./180.)
      parameter (grav=9.80616)

      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      logical diag

#include "cyc1.h"
#include "cyc2.h"
#include "cyc4.h"
#include "cyc5.h"

c-------------------------------------------------------------------------------
c     Determine method of calculation
c-------------------------------------------------------------------------------

      diag = idiag.ge.2
      if (qsteer(1:1).eq.'U') then
        go to 100
      else if (qsteer(1:1).eq.'Z') then
        go to 200
      else
        write (6,*) ' qsteer = ',qsteer,' not known.'
        stop
      endif

c-------------------------------------------------------------------------------
c     Interpolate velocity at cyclone centre from given winds
c-------------------------------------------------------------------------------

c     Read wind velocity component arrays (f=u,fxx=v)
c     -----------------------------------

      quantu = ' '
      quantv = ' '
  100 continue
      call llmaprd(ioup,headu,nlonlt,nlatlt,nlllt,nlat,lat,nlon,lon,
     * f  ,quantu,levelu,lunitu,sourceu,dmodeu,dau,hru,unitu,grid,ie1)
      if (idiag.ge.1) write (6,'(a)') headu
      if (dau.gt.da .or. (dau.eq.da .and. hru.gt.hr)) then
        write (6,*) 'U steering flow file not present for ',da,hr
        stop
      endif
      if (quantu(1:1).ne.'U') stop ' Quantity for U incorrect.'
      read (levelu,'(i9)',err=105) ilevel
      write (qsteer(2:5),'(i4)') ilevel
      if (qsteer(2:2).eq.' ') qsteer(2:2) = '0'
      if (qsteer(3:3).eq.' ') qsteer(3:3) = '0'
      go to 106
  105 qsteer(2:5) = level(1:4)
  106 continue
      if (dau.lt.da .or. (dau.eq.da .and. hru.lt.hr)) go to 100
  110 continue
      call llmaprd(iovp,headv,nlonlt,nlatlt,nlllt,nlat,lat,nlon,lon,
     * fxx,quantv,levelv,lunitv,sourcev,dmodev,dav,hrv,unitv,grid,ie1)
      if (idiag.ge.1) write (6,'(a)') headv
      if (dav.gt.da .or. (dav.eq.da .and. hrv.gt.hr)) then
        write (6,*) 'V steering flow file not present for ',da,hr
        stop
      endif
      if (dav.lt.da .or. (dav.eq.da .and. hrv.lt.hr)) go to 110
      if (quantv(1:1).ne.'V') stop ' Quantity for V incorrect.'
      if (levelv.ne.levelu) stop ' Quantities for U and V differ.'
      if (diag) write (6,'(a/a/a)') ' Steering wind arrays read.',
     * headu,headv

c     Check file attributes
c     ---------------------

      if (quantu(1:1).ne.'U'.or.quantv(1:1).ne.'V')
     * write (6,*) ' Warning: ',quantu(1:1),' or ',quantv(1:1),
     * ' may be the wrong steering flow quantity.'
      if (unitu(1:5).ne.'M/S  '.or.unitv(1:5).ne.'M/S  ') then
        write (6,*) ' Do not know how to interpolate velocity from',
     *   ' velocities in ',unitu,' or ',unitv,'.'
        stop
      endif

c     Interpolate winds to cyclone positions
c     --------------------------------------

      call steeru(f,fxx,nlon,nlat,lon,lat,nk,rdustr,spval,diag)
      if (diag) write (6,*) ' Steering flow linearly interpolated'

      return

c-------------------------------------------------------------------------------
c     Compute velocity at cyclone centre from geopotential heights
c-------------------------------------------------------------------------------

c     Read geopotential height component array
c     ----------------------------------------

  200 continue
      call llmaprd(ioup,headu,nlonlt,nlatlt,nlllt,nlat,lat,nlon,lon,
     * fhk,quantu,levelu,lunitu,sourceu,dmodeu,dau,hru,unitu,grid,ie1)
      if (dau.gt.da .or. (dau.eq.da .and. hru.gt.hr)) then
        write (6,*) 'Z steering flow file not present for ',da,hr
        stop
      endif
      if (dau.lt.da .or. (dau.eq.da .and. hru.lt.hr)) go to 200
      if (diag) write (6,'(a80)') ' Steering z arrays read.',headu

c     Check file attributes
c     ---------------------

      if (quantu(1:1).ne.'U') write (6,*) ' Warning: ',quantu(1:1),
     * ' may be the wrong steering flow quantity.'
      if (unitu(1:5).ne.'M/S  ') then
        write (6,*) ' Do not know how to interpolate from',
     *   ' geopotential heights in ',unitu,'.'
        stop
      endif

c     Expand geopotential array
c     -------------------------

      call llexpand(nlllt,nll2lt,nlon,nlat,lon,lat,nlon2,nlat2,
     * lon2,lat2,0,8,8,8,8,fhk,f,headu,fileu,88,idiag,ie3)
      if (diag) write (6,*) ' Steering z arrays expanded.'
      
c     Obtain bicubic spline coefficients f field
c     ------------------------------------------

      call bisplsv(nlon2,nlat2,lon2,lat2,
     * f,fxx,fyy,fxxyy,1,nlon2,1,nlat2,spval,idiag,ie4)
      if (diag) write (6,*) ' Steering z splines done'

c     Interpolate geostrophic winds to cyclone positions
c     --------------------------------------------------

      call steerg(f,fxx,fyy,fxxyy,nlon2,nlat2,lon2,lat2,nk,spval,
     * diag)
      if (diag) write (6,*) ' Geostropic steering flow interpolated'

      return
      end

c=======================================================================

      Subroutine steeru(u,v,nlon,nlat,lon,lat,nk,rdustr,spval,diag)
      dimension u(nlon,nlat),v(nlon,nlat)
      real lon(nlon),lat(nlat)
      real lonc,lonu,latu
      logical diag
#include "cyc4.h"

      parameter (pi=3.1415926535898,c2pi=2.*pi,p5pi=0.5*pi)
      parameter (rad=180./pi,rrad=1./rad)
      parameter (rsqrt2=0.707106781,rsqrt2m=-rsqrt2)

      dimension sinthe(9),costhe(9),the(9)
      data the/0.,45.,90.,135.,180.,225.,270.,315.,360./
      data sinthe/0.,rsqrt2,1.,rsqrt2,0.,rsqrt2m,-1.,rsqrt2m,0./
      data costhe/1.,rsqrt2,0.,rsqrt2m,-1.,rsqrt2m,0.,rsqrt2,1./

      if (rdustr.eq.0) then
        nupt = 1
      else
        nupt = 9
        chi    = rrad*rdustr*0.67678
        sinchi = sin(chi)
        coschi = cos(chi)
        tanchi = sinchi/coschi
      endif

      if (diag) write (6,'(/a3,2(a6,a3,3a6),3(4a6,x)/)') 
     * 'kc','lonu','ilo','lonlo','lonhi','A1',
     *      'latu','jlo','latlo','lathi','B1',
     * 'ulolo','uhilo','ulohi','uhihi','vlolo',
     * 'vhilo','vlohi','vhihi','upc','vpc'

      do 100 kc=1,nk
        if (nupt.ne.1) then
          phi = rrad*yc(kc)
          sinphi = sin(phi)
          cosphi = cos(phi)
          tanphi = sinphi/cosphi
        endif
        ncount = 0
        lonc = xc(kc)
        sumup2 = 0.
        sumvp2 = 0.
        upc(kc) = 0.
        vpc(kc) = 0.
        do 80 iupt = 1,nupt
          lonu = lonc
          if (iupt.ne.1) then
            latu = 90.-rad*acos( costhe(iupt)*sinchi*cosphi 
     *                                  + coschi*sinphi )
            lonu = rad*atan2(sinthe(iupt),
     *              (cosphi/tanchi-sinphi*costhe(iupt))) + lonc
            gam  =     atan2(sinthe(iupt),
     *              (sinchi*tanphi-coschi*costhe(iupt)))
     *              +rrad*the(iupt)-pi
            cosgam = cos(gam)
            singam = sin(gam)
            if (lonu.ge.360.) lonu = lonu - 360.
            if (lonu.lt.  0.) lonu = lonu + 360.
            if (latu.gt. 90.) latu =  180. - latu
            if (latu.lt.-90.) latu = -180. - latu
          else
            lonu = lonc
            latu = yc(kc)
          endif

          if  (lonu.gt.lon(nlon).or.lonu.lt.lon(1)
     *     .or.latu.gt.lat(nlat).or.latu.lt.lat(1)) then
            write (6,*) lonu,latu,' outside lat,lon range.'
            if (iupt.eq.1) go to 90
            go to 70
          endif

          call srchls(lonu,lon,nlon,ilo,ihi)
          call srchls(latu,lat,nlat,jlo,jhi)

          if  (u(ilo,jlo).eq.spval.or.u(ihi,jlo).eq.spval
     *     .or.u(ilo,jhi).eq.spval.or.u(ihi,jhi).eq.spval) then
c           write (6,*) ' Special values in interpolation.'
            if (iupt.eq.1) go to 90
            go to 70
          endif
          ncount = ncount + 1

          A2 = (lonu-lon(ilo))/(lon(ihi)-lon(ilo))
          A1 = 1. - A2
          B2 = (latu-lat(jlo))/(lat(jhi)-lat(jlo))
          B1 = 1. - B2

          up1 =  (u(ilo,jlo)*A1 + u(ihi,jlo)*A2)*B1
     *             + (u(ilo,jhi)*A1 + u(ihi,jhi)*A2)*B2
          vp1 =  (v(ilo,jlo)*A1 + v(ihi,jlo)*A2)*B1
     *             + (v(ilo,jhi)*A1 + v(ihi,jhi)*A2)*B2

          if (nupt.eq.1) then
            upc(kc) = up1
            vpc(kc) = vp1
          else
            if (iupt.eq.1) then
              up2 = up1
              vp2 = vp1
            else
              up2 =  cosgam*up1 + singam*vp1
              vp2 = -singam*up1 + cosgam*vp1
            endif
            sumup2 = sumup2 + up2
            sumvp2 = sumvp2 + vp2
          endif

          if (diag) then
            if (nupt.eq.1) then
              write (6,'(i3,2(f6.1,i3,2f6.1,f6.2),
     *         3(4f6.2,x))') 
     *         kc,lonu,ilo,lon(ilo),lon(ihi),A1,
     *            latu,jlo,lat(jlo),lat(jhi),B1,
     *         u(ilo,jlo),u(ihi,jlo),u(ilo,jhi),u(ihi,jhi),
     *         v(ilo,jlo),v(ihi,jlo),v(ilo,jhi),v(ihi,jhi),
     *         up1,vp1
            else if (iupt.eq.1) then
              write (6,'(i3,2(f6.1,i3,2f6.1,f6.2),
     *         3(4f6.2,x))') 
     *         kc,lonu,ilo,lon(ilo),lon(ihi),A1,
     *            latu,jlo,lat(jlo),lat(jhi),B1,
     *         u(ilo,jlo),u(ihi,jlo),u(ilo,jhi),u(ihi,jhi),
     *         v(ilo,jlo),v(ihi,jlo),v(ilo,jhi),v(ihi,jhi),
     *         up1,vp1,up2,vp2
            else
              write (6,'(3x,2(f6.1,i3,2f6.1,f6.2),
     *         4(4f6.2,x))') lonu,ilo,lon(ilo),lon(ihi),A1,
     *                       latu,jlo,lat(jlo),lat(jhi),B1,
     *         u(ilo,jlo),u(ihi,jlo),u(ilo,jhi),u(ihi,jhi),
     *         v(ilo,jlo),v(ihi,jlo),v(ilo,jhi),v(ihi,jhi),
     *         up1,vp1,up2,vp2
            endif
          endif
   70     continue
   80   continue
        if (ncount.eq.0) go to 90
        if (nupt.gt.1) then
          upc(kc) = sumup2/float(ncount)
          vpc(kc) = sumvp2/float(ncount)
          if (diag) write (6,'(119x,2f6.2)') upc(kc),vpc(kc)
        endif
   90   continue
  100 continue

      return
      end

c=======================================================================

      Subroutine steerg(z,zxx,zyy,zxxyy,nlon,nlat,lon,lat,nk,spval,
     * diag)

      dimension z  (nlon,nlat),zxx  (nlon,nlat),
     *          zyy(nlon,nlat),zxxyy(nlon,nlat)
      real lon(nlon),lat(nlat)
      real lonc,latc,latr
      logical diag

#include "cyc4.h"

      parameter (r6=1./6.,r180=1./180.)
      parameter (grav=9.80616)
      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)
      parameter (secpday=86400.*365.25/366.25,
     * rtwoomega=1./(4.*pi/secpday))

      if (diag) write (6,'(/a3,10a7/)') 'kc','xc','yc','lonlo','lonhi',
     * 'latlo','lathi','zx*deglat','zy*deglat','rfcor','upc','vpc'

      do 100 kc=1,nk
        lonc = xc(kc)
        latc = yc(kc)

        if  (lonc.gt.lon(nlon).or.lonc.lt.lon(1)
     *   .or.latc.gt.lat(nlat).or.latc.lt.lat(1)) then
          upc(kc) = 0.
          vpc(kc) = 0.
          write (6,*) lonc,latc,' outside lat,lon range.'
          go to 90
        endif

        call srchls(lonc,lon,nlon,ilo,ihi)
        call srchls(latc,lat,nlat,jlo,jhi)

        if  (z(ilo,jhi).eq.spval.or.z(ihi,jhi).eq.spval
     *   .or.z(ilo,jhi).eq.spval.or.z(ihi,jhi).eq.spval) then
          upc(kc) = 0.
          vpc(kc) = 0.
c         write (6,*) ' Special values in interpolation.'
          go to 90
        endif

        latr = latc*rrad

        lonlo = lon(ilo)
        lonhi = lon(ihi)
        dlon = lonhi - lonlo
        dlonsq = dlon*dlon
        latlo = lat(jlo)
        lathi = lat(jhi)
        dlat = lathi - latlo
        dlatsq = dlat*dlat

c       Interpolate dz/dlat
c       -------------------

        A2 = (lonc - lonlo)/dlon
        A1 = 1. - A2
        A3 = A1*(A1*A1 - 1.)*dlonsq*r6
        A4 = A2*(A2*A2 - 1.)*dlonsq*r6
        B2 = 1./dlat
        B1 = -B2
        y2 = (latc - latlo)/dlat
        y1 = 1. - y2
        B3 = -(3.*y1*y1 - 1.)*dlat*r6
        B4 =  (3.*y2*y2 - 1.)*dlat*r6

        zy =  (B1*(A1*z(ilo,jlo)    + A2*z(ihi,jlo)   +
     *             A3*zxx(ilo,jlo)  + A4*zxx(ihi,jlo))   +
     *         B2*(A1*z(ilo,jhi)    + A2*z(ihi,jhi)   +
     *             A3*zxx(ilo,jhi)  + A4*zxx(ihi,jhi))   +
     *         B3*(A1*zyy(ilo,jlo)  + A2*zyy(ihi,jlo) +
     *             A3*zxxyy(ilo,jlo)+ A4*zxxyy(ihi,jlo)) +
     *         B4*(A1*zyy(ilo,jhi)  + A2*zyy(ihi,jhi) +
     *             A3*zxxyy(ilo,jhi)+ A4*zxxyy(ihi,jhi)))
     *       /deglat

c       Interpolate dz/dlon
c       -------------------

        A2 = 1./dlon
        A1 = -A2
        x2 = (lonc - lonlo)/dlon
        x1 = 1. - x2
        A3 = -(3.*x1*x1 - 1.)*dlon*r6
        A4 =  (3.*x2*x2 - 1.)*dlon*r6
        B2 = (latc - latlo)/dlat
        B1 = 1. - B2
        B3 = B1*(B1*B1 - 1.)*dlatsq*r6
        B4 = B2*(B2*B2 - 1.)*dlatsq*r6

        zx =  (B1*(A1*z(ilo,jlo)    + A2*z(ihi,jlo)   +
     *             A3*zxx(ilo,jlo)  + A4*zxx(ihi,jlo))   +
     *         B2*(A1*z(ilo,jhi)    + A2*z(ihi,jhi)   +
     *             A3*zxx(ilo,jhi)  + A4*zxx(ihi,jhi))   +
     *         B3*(A1*zyy(ilo,jlo)  + A2*zyy(ihi,jlo) +
     *             A3*zxxyy(ilo,jlo)+ A4*zxxyy(ihi,jlo)) +
     *         B4*(A1*zyy(ilo,jhi)  + A2*zyy(ihi,jhi) +
     *             A3*zxxyy(ilo,jhi)+ A4*zxxyy(ihi,jhi)))
     *       /(deglat*cos(latr))

c       Calculate geostrophic steering flow (E and N components)
c       --------------------------------------------------------

        if (abs(latr).lt.1.) then
          rsinphi =latr*720./pi**3/alatgv**2/(2.*atan(90./alatgv)/pi)**2
        else
          rsinphi = (atan(latr/alatgv)/atan(90./alatgv))**2/sin(latc)
        endif
c       rsinphi = 1./sin(latr)

        rfcor = rsinphi*rtwoomega
        grf = grav * rfcor

        upc(kc) = -grf * zy * upfact
        vpc(kc) =  grf * zx * upfact

        if (diag) write (6,'(i3,10f7.2)') kc,xc(kc),yc(kc),lonlo,lonhi,
     *   latlo,lathi,zx*deglat,zy*deglat,rfcor,upc(kc),vpc(kc)

   90   continue
  100 continue

      return
      end

