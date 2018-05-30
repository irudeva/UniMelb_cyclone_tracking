      Subroutine steer1(scalt,f,fxx,fyy,fxxyy,z,xc,yc,fc,
     * upe,vpn,diag)

c       This part of the routine estimates the cyclone advection velocity 
c     from the character of the surrounding pressure field.  Although the
c     wind velocity at the centre of a low is zero, the average flow in 
c     the area around it is not, and the areal average of the geostrophic 
c     within a certain radius from the centre may be taken as representative
c     of the steering velocity in the lower half of the troposphere.

c         The required velocity is obtained from a pressure gradient 
c     obtained by averaging the direction weighted pressure differences
c     from the central pressure at a number of points of bearing dir_i
c     around a circle of radius rdustr, viz.,

c         (u,v) = -1/(rho fcor) (dp/dy, -dpdx),
c     where,
c         dpdx = (sum (f_i - fc)*sin dir_i)/(sum (sin dir_i)^2 * rdustr)
c         dpdy = (sum (f_i - fc)*cos dir_i)/(sum (cos dir_i)^2 * rdustr)

c     The density, rhoa=1.2 Kg./cu.m. may need to be altered at upper
c     levels or at extreme latitudes.  If geopotential heights be used, 

c         (u,v) = -g/fcor (dp/dy, -dpdx),
c   
c     and dzdx,dzdy are calculated as above

c         The 1/fcor factor is scaled by an atan**2 factor to limit the
c     velocities to a linear dependence on latitude near the equator.
c     The velocities may be scaled by a chosen factor upfact to allow
c     for proportionate differences between the strength of the 
c     westerlies at MSL and at steering height.

c         The output velocities are in m/s.

c     Written 1993, R.J. Murray.
c     Last modified, 15th May, 1996.

c-------------------------------------------------------------------------------
c     Explanation
c-------------------------------------------------------------------------------

c     ni,nj            Size of PS array in 90degE (i) and GM (j) directions
c     f(i,j)           Function value at grid point (i,j), or its negative
c                        when highs are sought
c     fx,fxx,fxy,&c.   A cubic spline coefficient arrays (ni*nj) (partial 
c                        derivatives of function w.r.t. x,x**2,xy,&c.
c     scalt            Map scale factor (deg.lat./grid space)

c-------------------------------------------------------------------------------
c     Declarations
c-------------------------------------------------------------------------------

      parameter (nii=3,nradlt=120,ndirlt=48)
c     parameter (spval=99999.9)
      parameter (r6=1./6.,r180=1./180.)
      parameter (grav=9.80616)

      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)
      parameter (deglat=1.1111e5)

      dimension f(ni,nj),fxx(ni,nj),fyy(ni,nj),fxxyy(ni,nj)
      dimension z(ni,nj)
      logical diag,testzs

#include "cyc1.h"
#include "cyc3.h"

c-------------------------------------------------------------------------------
c     Calculation
c-------------------------------------------------------------------------------

c     Compute latitude and longitude quantities
c     -----------------------------------------

      xs = xc - xcen
      ys = yc - ycen
      rs = sqrt(xs*xs + ys*ys)
      alonc = atan2(xs,-ys*fhem)
      clatc = c2*atan(rs/rproj)
      alatc = fhem*(pi*0.5-clatc)

c     Set up sampling radius and angular increments
c     ---------------------------------------------

c     rdustr = 5.
c     npgdir = 12
c     alatgv = 8.
c     rhoa = 1.2 ! Kg./cu.m.
      if (rdustr.le.0.) stop ' steer1: "rdustr must be > 0."'

      radp = rdustr/scalt
      ddir = 360./float(npgdir)*rrad

      if (diag) then
        write (6,'(/5a8)') 'xc','yc','alonc','alatc','scalt'
        write (6,'(5f8.3)') xc,yc, alonc*rad,alatc*rad,scalt
        write (6,'(4a8)') 'rdustr','radp','npgdir','ddir'
        write (6,'(2f8.3,i8,f8.3)') rdustr,radp,npgdir,ddir*rad
      endif

c     Compute topographic diminution for cyclone centre
c     -------------------------------------------------

      testzs = zscr2.gt.0.
      if (testzs) then
        ilo = int(xc)
        ihi = ilo + 1
        A2 = (xc - float(ilo))
        A1 = 1. - A2

        jlo = int(yc)
        jhi = jlo + 1
        B2 = (yc - float(jlo))
        B1 = 1. - B2

        ztop =  (z(ilo,jlo)*A1 + z(ihi,jlo)*A2)*B1
     *        + (z(ilo,jhi)*A1 + z(ihi,jhi)*A2)*B2

        if (diag) then
          write (6,'(3a8)') 'zscr1','zscr2','ztop'
          write (6,'(2f8.3,f8.1)') zscr1,zscr2,ztop
        endif
        if (ztop.gt.zscr2) then
          if (diag) write (6,'(a)') ' No cyclone velocity predicted.'
          upe = 0.
          vpn = 0.
          return
        else if (ztop.gt.zscr1) then
          zfact0 = (zscr2-ztop)/(zscr2-zscr1)
        else
          zfact0 = 1.
        endif
        if (diag) write (6,'(a8,f8.3)') 'zfact0 = ',zfact0
      endif

c     Calculate differences from central pressure around circle
c     ---------------------------------------------------------

      sumfcos = 0.
      sumfsin = 0.
      sumcos2 = 0.
      sumsin2 = 0.

      if (diag) write (6,'(/a4,a8,x,2a8,a10,2a7,7a10)') 'idir',
     * 'dir','x','y','fdir','fdif','ztop','zfact','cosdir','sindir',
     * 'sumfcos','sumfsin','sumcos2','sumsin2'

      do 700 idir = 1,npgdir

c       Compute positions on averaging circle
c       -------------------------------------

        dir    = float(idir)*ddir
        cosdir = cos(dir)
        sindir = sin(dir)
        x = radp*sindir + xc
        y = radp*cosdir + yc
      
        if ((x.gt.float(ni)).or.(x.lt.float(1))) go to 690
        if ((y.gt.float(nj)).or.(y.lt.float(1))) go to 690

c       Compute interpolating factors
c       -----------------------------

        ilo = int(x)
        ihi = ilo + 1
        A2 = (x - float(ilo))
        A1 = 1. - A2
        A3 = A1*(A1*A1 - 1.)/6.
        A4 = A2*(A2*A2 - 1.)/6.

        jlo = int(y)
        jhi = jlo + 1
        B2 = (y - float(jlo))
        B1 = 1. - B2
        B3 = B1*(B1*B1 - 1.)/6.
        B4 = B2*(B2*B2 - 1.)/6.

c       Exclude positions at which topography > zsmax metres
c       ----------------------------------------------------

        if (testzs) then
          ztop =  (z(ilo,jlo)*A1 + z(ihi,jlo)*A2)*B1
     *          + (z(ilo,jhi)*A1 + z(ihi,jhi)*A2)*B2
          if (ztop.gt.zscr2) then
            fdir = 0.
            fdif = 0.
            fdifz = 0.
            zfact = 0.
            go to 680
          else if (ztop.gt.zscr1) then
            zfact = (zscr2-ztop)/(zscr2-zscr1)
          else
            zfact = 1.
          endif
crmw      amin0 -> min
          zfact = min(zfact,zfact0)
c         zfact = zfact0
        else
          zfact = 1.
        endif

c       Determine function value at this position
c       -----------------------------------------

        fdir= B1*(A1*f    (ilo,jlo)+ A2*f    (ihi,jlo)+
     *            A3*fxx  (ilo,jlo)+ A4*fxx  (ihi,jlo)) +
     *        B2*(A1*f    (ilo,jhi)+ A2*f    (ihi,jhi)+
     *            A3*fxx  (ilo,jhi)+ A4*fxx  (ihi,jhi)) +
     *        B3*(A1*fyy  (ilo,jlo)+ A2*fyy  (ihi,jlo)+
     *            A3*fxxyy(ilo,jlo)+ A4*fxxyy(ihi,jlo)) +
     *        B4*(A1*fyy  (ilo,jhi)+ A2*fyy  (ihi,jhi)+
     *            A3*fxxyy(ilo,jhi)+ A4*fxxyy(ihi,jhi))
        fdif  = fdir - fc
        fdifz = fdif*zfact

        sumfcos = sumfcos + fdifz *cosdir
        sumfsin = sumfsin + fdifz *sindir

  680   continue

        sumcos2 = sumcos2 + cosdir*cosdir
        sumsin2 = sumsin2 + sindir*sindir

        if (diag) write (6,'(i4,f8.3,x,2f8.3,f10.3,f7.3,f7.1,7f10.3)')
     *   idir,dir*rad,x,y,fdir,fdif,ztop,zfact,cosdir,sindir,sumfcos,
     *   sumfsin,sumcos2,sumsin2
  690 continue
  700 continue

c     Calculate function f (pressure/geopotential) gradient
c     -----------------------------------------------------

      summin = float(npgdir)/6.
      if (sumcos2.lt.summin .or. sumsin2.lt.summin) then
        fgrdx = 0.
        fgrdy = 0.
        fgrde = 0.
        fgrdn = 0.
        upe   = 0.
        vpn   = 0.
        rrhofam = 0.
        go to 800
      endif

      fgrdx = sumfsin/sumsin2
      fgrdy = sumfcos/sumcos2
      sinthe = -fhem*xs/rs
      costhe = -fhem*ys/rs
      fgrde = fgrdx*costhe - fgrdy*sinthe
      fgrdn = fgrdx*sinthe + fgrdy*costhe

c     Scale to geostrophic velocities
c     -------------------------------

      alat1 = alatc*rad
      if (abs(alat1).lt.1) then
c       rsinphi = alat1*720./pi**3/alatgv**2/(2.*atan(90./alatgv)/pi)**2
        rsinphi = alat1*rad/(alatgv*atan(90./alatgv))**2
      else
        rsinphi = (atan(alat1/alatgv)/atan(90./alatgv))**2/sin(alatc)
      endif
      secpday = 86400.*365.25/366.25
      r2omega = 1./(4.*pi/secpday)

      if (unit(1:5).eq.'M    ') then
        geofac = grav
      else if (unit(1:5).eq.'MB   ') then
        geofac = 1./rhoa * 100.      ! 100. to convert to N/m**2
      else
        write (6,*) ' Do not know how to find geostrophic velocity',
     *   ' from pressures/heights in ',unit,'.'
        stop
      endif

      rdelta = 1./(rdustr*deglat)

      rrhofam = -rsinphi*r2omega*geofac*rdelta

      upe =  rrhofam*fgrdn*upfact
      vpn = -rrhofam*fgrde*upfact

  800 continue

      if (diag) then
        write (6,'(/6a13)') 'alonc','alatc','rsinphi','geofac',
     *   'r2omega','rdelta'
        write (6,'(6f13.5)') alonc*rad,alat1,rsinphi,geofac,r2omega,
     *   rdelta
        write (6,'(6a13)') 'rrhofam','sumfcos','sumfsin','sumcos2',
     *   'sumsin2','upfact'
        write (6,'(6f13.5)') rrhofam,sumfcos,sumfsin,sumcos2,
     *   sumsin2,upfact
        write (6,'(6a13)') 'fgrdx','fgrdy','fgrde','fgrdn',
     *   'upe','vpn'
        write (6,'(6f13.5)') fgrdx,fgrdy,fgrde,fgrdn,upe,vpn
      endif

      return
      end
