      Subroutine supvar(iosv,spval,idiag)

c       This routine interpolates arrays of supplementary variables
c     to cyclone positions.

c     Written 24th May 1996, R.J. Murray.

c-------------------------------------------------------------------------------
c     Explanation
c-------------------------------------------------------------------------------

c     iosv(isup)   = logical unit for variable "isup" input
c     nlon,nlat    = dimensions of lat-lon array
c     lon,lat      = longitudes and latitudes
c     nk           = number of cyclones in analysis period
c     xc,yc        = x and y coordinates of cyclones
c     das,hrs      = date and time of array data
c     idiag        = diagnostic write level
c     latc,lonc    = lat. and lon. of cyclone
c     latu,lonu    = lat. and lon. of points at which variables are to be
c                      interpolated
c     ilo,ihi      = / i and j coordinates of lat.-lon. grid
c     jlo,jhi      = \ surrounding latc,lonc
c     lonlo,lonhi  = / longitudes and latitudes surrounding
c     latlo,lathi  = \ latc,lonc
c     A1,..,B4     = bicubic spline interpolating coefficients
c     s(i,j,isup)  = Supplementary value of function "isup" at point (i,j)
c     sc(kc,isup)  = Calculated value of function "isup" at (xc,yc)

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------
      
      character*80 heads
      character quants*8,levels*9,lunits*10,sources*10,units*12,
     * dmodes*6,grid*17
      integer das,hrs
      logical diag

#include "cyc1.h"
#include "cyc2.h"
#include "cyc4.h"
#include "cyc5.h"

      dimension iosv(nsupmx)

      parameter (pi=3.1415926535898,c2pi=2.*pi,p5pi=0.5*pi)
      parameter (rad=180./pi,rrad=1./rad)
      parameter (rsqrt2=0.707106781,rsqrt2m=-rsqrt2)

      common /blchi/sinchi,coschi,tanchi
      common /blphi/sinphi(nklt),cosphi(nklt),tanphi(nklt)

c-----------------------------------------------------------------------
c     Precalculate certain angular variables
c-----------------------------------------------------------------------

      diag = idiag.ge.2

      if (rdsupv.ne.0) then
        do 80 isup = 1,itabc5
          if (iavsup(isup).eq.1) go to 90
   80   continue
        go to 110
   90   continue
        chi    = rrad*rdsupv*0.67678
        sinchi = sin(chi)
        coschi = cos(chi)
        tanchi = sinchi/coschi

        do 100 kc=1,nk
          phi = rrad*yc(kc)
          sinphi(kc) = sin(phi)
          cosphi(kc) = cos(phi)
          tanphi(kc) = sinphi(kc)/cosphi(kc)
  100   continue
  110   continue
      endif

c-----------------------------------------------------------------------
c     Read and interpolate each variable
c-----------------------------------------------------------------------

      do 220 isup = 1,itabc5

c       Read supplementary variable arrays
c       ----------------------------------

        quants = ' '
  200   continue
        call llmaprd(iosv(isup),heads,nlonlt,nlatlt,nlllt,
     *   nlat,lat,nlon,lon,f,quants,levels,lunits,sources,
     *   dmodes,das,hrs,units,grid,ie1)
c     write (6,*) heads
        if (idiag.ge.1) write (6,'(a)') heads
        if (das.lt.da .or. (das.eq.da .and. hrs.lt.hr)) go to 200
        if (das.gt.da .or. (das.eq.da .and. hrs.gt.hr)) then
          write (6,*) 'File not present for supplementary variable ',
     *     isup,': ',da,hr
          stop
        endif

c       Interpolate variables to cyclone positions
c       ------------------------------------------

        nupt = 1
        if ((rdsupv.ne.0.) .and. iavsup(isup).eq.1) nupt = 9
        call supvar2(isup,f,nlon,nlat,lon,lat,nk,nupt,
     *   rdsupv,spval,diag)
  220 continue

      return
      end

c=======================================================================

      Subroutine supvar2(isup,s,nlon,nlat,lon,lat,nk,nupt,
     * rdsupv,spval,diag)

#include "cyc4.h"
      logical diag
      real lon(nlon),lat(nlat),s(nlon,nlat)
      real lonc,lonu,latu

      parameter (pi=3.1415926535898,c2pi=2.*pi,p5pi=0.5*pi)
      parameter (rad=180./pi,rrad=1./rad)
      parameter (rsqrt2=0.707106781,rsqrt2m=-rsqrt2)

      common /blchi/sinchi,coschi,tanchi
      common /blphi/sinphi(nklt),cosphi(nklt),tanphi(nklt)

      dimension sinthe(9),costhe(9),the(9)
      data the/0.,45.,90.,135.,180.,225.,270.,315.,360./
      data sinthe/0.,rsqrt2,1.,rsqrt2,0.,rsqrt2m,-1.,rsqrt2m,0./
      data costhe/1.,rsqrt2,0.,rsqrt2m,-1.,rsqrt2m,0.,rsqrt2,1./

      if (diag) write (6,'(/a3,2(a6,a3,3a6),3(4a6,x)/)') 
     * 'kc','lonu','ilo','lonlo','lonhi','A1',
     *      'latu','jlo','latlo','lathi','B1',
     * 'slolo','shilo','slohi','shihi','sc'

      do 100 kc=1,nk
        lonc = xc(kc)
        ncount = 0
        sums = 0.
   40   continue
        do 80 iupt = 1,nupt
          lonu = lonc
          if (iupt.ne.1) then
            latu = 90.-rad*acos(costhe(iupt)*sinchi*cosphi(kc) 
     *                                     + coschi*sinphi(kc))
            lonu = rad*atan2(sinthe(iupt),
     *              (cosphi(kc)/tanchi-sinphi(kc)*costhe(iupt)))
     *              + lonc
            gam  =     atan2(sinthe(iupt),
     *              (sinchi*tanphi(kc)-coschi*costhe(iupt)))
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
     *     .or.latu.gt.lat(nlat).or.latu.lt.lat(1)) 
     *     then
            write (6,*) lonu,latu,' outside lat,lon range.'
            if (iupt.eq.1) go to 90
            go to 70
          endif

          call srchls(lonu,lon(1),nlon,ilo,ihi)
          call srchls(latu,lat(1),nlat,jlo,jhi)

          A2 = (lonu-lon(ilo))/(lon(ihi)-lon(ilo))
          A1 = 1. - A2
          B2 = (latu-lat(jlo))/(lat(jhi)-lat(jlo))
          B1 = 1. - B2

          if ((s(ilo,jlo).eq.spval).or.
     *        (s(ihi,jlo).eq.spval.and.B2.gt.0.).or.
     *        (s(ilo,jhi).eq.spval.and.A2.gt.0.).or.
     *        (s(ihi,jhi).eq.spval.and.
     *          ((A2.gt.0.).or.(B2.gt.0.)))) then
c           write (6,*) ' Special values in interpolation.'
            if (iupt.eq.1) go to 90
            go to 70
          endif
          ncount = ncount + 1

          s1 =  (s(ilo,jlo)*A1 + s(ihi,jlo)*A2)*B1
     *        + (s(ilo,jhi)*A1 + s(ihi,jhi)*A2)*B2

          if (nupt.eq.1) then
            sc(kc,isup) = s1
          else
            sums = sums + s1
          endif
  
          if (diag) then
            if (iupt.eq.1) then
              write (6,'(i3,2(f6.1,i3,2f6.1,f6.2),3(4f7.1,x))') 
     *         kc,lonu,ilo,lon(ilo),lon(ihi),A1,
     *            latu,jlo,lat(jlo),lat(jhi),B1,
     *         s(ilo,jlo),s(ihi,jlo),s(ilo,jhi),s(ihi,jhi),s1
            else
              write (6,'(3x,2(f6.1,i3,2f6.1,f6.2),4(4f7.1,x))') 
     *         lonu,ilo,lon(ilo),lon(ihi),A1,
     *         latu,jlo,lat(jlo),lat(jhi),B1,
     *         s(ilo,jlo),s(ihi,jlo),s(ilo,jhi),s(ihi,jhi),s1
            endif
          endif
   70     continue
   80   continue
   90   continue

        if (ncount.eq.0) then 
          sc(kc,isup) = spvc
        else if (nupt.gt.1) then
          sc(kc,isup) = sums/float(ncount)
          if (diag) write (6,'(86x,2f6.2)') sc(kc,isup)
        endif
  100 continue

      return
      end
