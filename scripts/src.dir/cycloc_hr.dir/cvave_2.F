      Subroutine cvave(cvarad,scalt,f,fxx,fyy,fxxyy,z,
c#ifdef delsqzs
     * zxx,zyy,zxxyy,
c#endif
     * xc,yc,fc,cx,ca,itabc3,rdincr,nrddir,sphtrg,ftopeq,radius,depth,
     * diag1,diag2)

c         A subroutine which reckons a characteristic value of the Laplacian
c     del**2 f at a high or low centre of a function (f) fitted to a 
c     PS array by bicubic spline interpolation.  This is obtained from
c     a weighting of the Laplacian at the high/low centre and its average 
c     value over a small radius around it.  The value of the parameter ("ca")
c     can be taken as a measure for the intensity of a high or low.  The
c     calculation differs from that used by Murray and Simmonds (1991a and b)
c     in that the average is now calculated not at grid points and with a
c     tapered weighting function but by interpolation of the Laplacian
c     between grid points and with a uniform weighting.

c         The programme also calculates the `depth' and `radius' of a high
c     or low from averages of delsq f along radial lines emanating from the 
c     cyclone centre and proceding to the points at which delsq f ceases to
c     decrease.  The use of these quantities is to be considered 
c     experimental at this stage.

c     Written 1993, R.J. Murray.
c     Last modified, 1st Jan., 1996.
 
c-------------------------------------------------------------------------------
c       (1)  Explanation.
c-------------------------------------------------------------------------------

c     ni,nj            Size of PS array in 90degE (i) and GM (j) directions
c     f(i,j)           Function value at grid point (i,j), or its negative
c                        when highs are sought
c     fx,fxx,fxy,&c.   A cubic spline coefficient arrays (ni*nj) (partial 
c                        derivatives of function w.r.t. x,x**2,xy,&c.
c     scalt            Map scale factor (deg.lat./grid space)
c     cv               del**2 f = (fxx + fyy)/2
c     rc               Limiting circle for finding cv average
c     ca               Average curvature over a circle of radius cvarad
c     cvarad           Radius deg.lat.) around centre over which average 
c                        ca is found (if cvarad > 0).
c     itabc3           1=calc min.p,2=also cv,3 or more = also dpc,rdc
c     depth            Depth of cyclone
c     radius           Radius of cyclone

c-------------------------------------------------------------------------------
c     Declarations
c-------------------------------------------------------------------------------

      parameter (nradlt=120,ndirlt=48)
      parameter (r6=1./6.,r180=1./180.)

      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898,p5pi=0.5*pi)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      dimension f(ni,nj),fxx(ni,nj),fyy(ni,nj),fxxyy(ni,nj)
      dimension z(ni,nj)
c#ifdef delsqzs
     *,         zxx(ni,nj),zyy(ni,nj),zxxyy(ni,nj)
c#endif
      dimension r(nradlt)
      dimension imissd(ndirlt),p(ndirlt),ra(ndirlt)
      logical diag1,diag2,sphtrg

#include "cyc3.h"

      scos(xx,yy,zz) = acos( cos(xx)*sin(yy)*sin(zz) + cos(yy)*cos(zz) )
      stan(xx,yy,zz) = atan2(sin(xx),(sin(zz)/tan(yy)-cos(zz)*cos(xx)))

c-------------------------------------------------------------------------------
c     Calculate trig. funs. of cyclone centre if using spherical trig.
c-------------------------------------------------------------------------------

crmw       diag1 = .true.
 
      if (sphtrg) then
        xs = xc - xcen
        ys = yc - ycen
        rs = sqrt(xs*xs + ys*ys)
        alonc = atan2(xs,-ys*fhem)
        clatc = c2*atan(rs/rproj)
        alatc = fhem*(pi*0.5-clatc)
        sinclatc = sin(clatc)
        cosclatc = cos(clatc)
        alpha = cvarad*rrad
        agamc = alonc
        if (fhem.eq.-1.) agamc = pi - agamc
      endif

      if (cvarad.le.0.) go to 110

c-------------------------------------------------------------------------------
c     Average of cv in a circle of radius rc
c-------------------------------------------------------------------------------

c       The value of cv (the laplacian) is taken as a measure of the
c     intensity of a cyclone.  For systems assumed to be of roughly constant
c     radius and of paraboloidal shape, this serves as a rough measure of 
c     the depth of the system relative to the surrounding field.  Since cv
c     varies fairly rapidly in space and cyclonic depressions are not
c     paraboloidal, the value of cv is averaged over the are of a circle of 
c     radius cvarad (provided cvarad > 0).  This may be done by sampling cv
c     over a range of points within the circle.  A more efficient calculation,
c     used in this implementation, makes use of Stoke's theorem (or within the
c     geostrophic assumption, the circulation theorem), which in terms of
c     PMSL may be written

c       int del**2 p dA =  int k cross grad p dot dl,  hence
c       ave del**2 p = int del**2 p / (pi r**2)
c                    = 2 pi r/n sum_n k cross grad p / (pi r**2)
c                    = 2/r sum_n k cross grad p/n

      ncva = 12.
      rcva = cvarad/scalt
      ddir = 360./float(ncva)
      sumfg = 0.
      sumzg = 0.
      sumn = 0.

      do 100 idir = 1,ncva
        dir = float(idir)*ddir/rad
        cosdir = cos(dir)
        sindir = sin(dir)
        if (sphtrg) then
          theta = agamc + p5pi - dir
          agam = agamc + stan(theta,alpha,clatc)
          clat =         scos(theta,alpha,clatc)
          rs = rproj*tan(clat*0.5)
          x = xcen + rs*sin(agam)
          y = ycen - rs*cos(agam)
          xxx = x
          yyy = y
        else
          x = rcva*cosdir + xc
          y = rcva*sindir + yc
        endif
c       write (6,'(a4,10f8.3)') 'xxx ',x,y,dir,agamc*rad,
c    *   agam*rad,clat*rad
      
        if ((x.gt.float(ni)).or.(x.lt.float(1))) go to 90
        if ((y.gt.float(nj)).or.(y.lt.float(1))) go to 90

        ilo = int(x)
        ihi = ilo + 1
        jlo = int(y)
        jhi = jlo + 1

        if (f(ilo,jlo).eq.spval .or. f(ihi,jlo).eq.spval .or.
     *      f(ilo,jhi).eq.spval .or. f(ihi,jhi).eq.spval) then
          gf = spval
          go to 90
        endif

        A2 = (x - float(ilo))
        A1 = 1. - A2
        A3 = A1*(A1*A1 - 1.)/6.
        A4 = A2*(A2*A2 - 1.)/6.
        A5 = -(3.*A1*A1 - 1.)/6.
        A6 =  (3.*A2*A2 - 1.)/6.

        B2 = (y - float(jlo))
        B1 = 1. - B2
        B3 = B1*(B1*B1 - 1.)/6.
        B4 = B2*(B2*B2 - 1.)/6.
        B5 = -(3.*B1*B1 - 1.)/6.
        B6 =  (3.*B2*B2 - 1.)/6.

        fx = B1*(   f    (ihi,jlo) -    f    (ilo,jlo) +
     *           A6*fxx  (ihi,jlo) + A5*fxx  (ilo,jlo)) +
     *       B2*(   f    (ihi,jhi) -    f    (ilo,jhi) +
     *           A6*fxx  (ihi,jhi) + A5*fxx  (ilo,jhi)) +
     *       B3*(   fyy  (ihi,jlo) -    fyy  (ilo,jlo) +
     *           A6*fxxyy(ihi,jlo) + A5*fxxyy(ilo,jlo)) +
     *       B4*(   fyy  (ihi,jhi) -    fyy  (ilo,jhi) +
     *           A6*fxxyy(ihi,jhi) + A5*fxxyy(ilo,jhi))

        fy = A1*(   f    (ilo,jhi) -    f    (ilo,jlo) +
     *           B6*fyy  (ilo,jhi) + B5*fyy  (ilo,jlo)) +
     *       A2*(   f    (ihi,jhi) -    f    (ihi,jlo) +
     *           B6*fyy  (ihi,jhi) + B5*fyy  (ihi,jlo)) +
     *       A3*(   fxx  (ilo,jhi) -    fxx  (ilo,jlo) +
     *           B6*fxxyy(ilo,jhi) + B5*fxxyy(ilo,jlo)) +
     *       A4*(   fxx  (ihi,jhi) -    fxx  (ihi,jlo) +
     *           B6*fxxyy(ihi,jhi) + B5*fxxyy(ihi,jlo))

        fg = cosdir*fx + sindir*fy
        sumfg = sumfg + fg
        sumn  = sumn  + 1.


c#ifdef delsqzs
        if (ftopeq.ne.0.) then
          zx = B1*(   z    (ihi,jlo) -    z    (ilo,jlo) +
     *             A6*zxx  (ihi,jlo) + A5*zxx  (ilo,jlo)) +
     *         B2*(   z    (ihi,jhi) -    z    (ilo,jhi) +
     *             A6*zxx  (ihi,jhi) + A5*zxx  (ilo,jhi)) +
     *         B3*(   zyy  (ihi,jlo) -    zyy  (ilo,jlo) +
     *             A6*zxxyy(ihi,jlo) + A5*zxxyy(ilo,jlo)) +
     *         B4*(   zyy  (ihi,jhi) -    zyy  (ilo,jhi) +
     *             A6*zxxyy(ihi,jhi) + A5*zxxyy(ilo,jhi))

          zy = A1*(   z    (ilo,jhi) -    z    (ilo,jlo) +
     *             B6*zyy  (ilo,jhi) + B5*zyy  (ilo,jlo)) +
     *         A2*(   z    (ihi,jhi) -    z    (ihi,jlo) +
     *             B6*zyy  (ihi,jhi) + B5*zyy  (ihi,jlo)) +
     *         A3*(   zxx  (ilo,jhi) -    zxx  (ilo,jlo) +
     *             B6*zxxyy(ilo,jhi) + B5*zxxyy(ilo,jlo)) +
     *         A4*(   zxx  (ihi,jhi) -    zxx  (ihi,jlo) +
     *             B6*zxxyy(ihi,jhi) + B5*zxxyy(ihi,jlo))

c         zx = B1*(z(ihi,jlo)-z(ilo,jlo)) + B2*(z(ihi,jhi)-z(ilo,jhi))
c         zy = A1*(z(ilo,jhi)-z(ilo,jlo)) + A2*(z(ihi,jhi)-z(ihi,jlo))
          zg = cosdir*zx + sindir*zy
          sumzg = sumzg + zg
        endif
c#endif
   90 continue
  100 continue
  110 continue

      if (sumn.eq.0.) then
        ca  = cx
        if (diag1) write (6,*) ' sumn=0; ca=cx=',ca
      else
        fact = 1./sumn * 2./rcva /(scalt*scalt)
        ca  = sumfg*fact
        if (diag1) then
          write (6,'(10a8)') 'scalt','sumfg','sumn','rcva','ca'
          write (6,'(10f8.3)') scalt,sumfg,sumn,rcva,ca
        endif
c#ifdef delsqzs
        if (ftopeq.ne.0.) then
          caz = sumzg*fact*ftopeq
          if (diag1.and.diag2) then
            write (6,'(/10a8)') 'sumzg','delsqzs','caz','ca-abs(caz)'
            write (6,'(10f8.3/)') sumzg,sumzg*fact,caz,ca-abs(caz)
          endif
          call pstoll(xcen,ycen,rproj,fhem,xc,yc,aloni,alatj)
          if (diag2) write (6,120) 'ca: ',xc,yc,aloni,alatj,
     *     ca,caz,ca-abs(caz)
  120     format (a4,4f7.1,3f9.4)
          ca = ca-abs(caz)
        endif
c#endif
      endif

      if (itabc3.lt.3) return

c-------------------------------------------------------------------------------
c     Parameters from radial averages
c-------------------------------------------------------------------------------

c       Measures of the depth and radius of a low are obtained by sampling
c     cv along nrddir radial directions from the low centre.  An envelope 
c     of points is found within which cv is uniformly and decreasing along
c     radial lines emanating from the cyclone centre.
c
c           radius = sqrt(area of the cyclone/pi)
c           depth  = the difference between the central pressure and
c                      the harmonic average with respect to radius of
c                      the pressures around the periphery of the low.

c     Set up radial and angular increments
c     ------------------------------------

      dr = rdincr/scalt
      do 200 k = 1,nradlt
        r(k) = float(k)*dr
  200 continue

      ddir = 360./float(nrddir)

c     Search for delsq f = 0 along radial lines
c     -----------------------------------------

      if (diag1) write (6,'(/2a3,14a8/)') 'id','k','cvls2','cvls1','cv'
     * ,'alpha','r(k-1)','ra(idir)','p'
      sumrsq = 0.
      sumrr = 0.
      sumfor = 0.

      do 300 idir = 1,nrddir
        dcvlst =  1.e10
        cvls1  = -1.e10
        cvls2  = -1.e10
        dir    = float(idir)*ddir/rad
        cosdir = cos(dir)
        sindir = sin(dir)
        imissd(idir) = 0
        p(idir) = 0.
        ra(idir) = 0.

        do 230 k = 1,nradlt

c         Determine position of point on projection

          x = r(k)*cosdir + xc
          y = r(k)*sindir + yc
        
          if ((x.gt.float(ni)).or.(x.lt.float(1))) go to 600
          if ((y.gt.float(nj)).or.(y.lt.float(1))) go to 600

c         Calculate delsq f at this point

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

          if (f(ilo,jlo).eq.spval .or. f(ihi,jlo).eq.spval .or.
     *        f(ilo,jhi).eq.spval .or. f(ihi,jhi).eq.spval) go to 600

          cv = B1*(A1*fxx(ilo,jlo)  + A2*fxx(ihi,jlo))   +
     *         B2*(A1*fxx(ilo,jhi)  + A2*fxx(ihi,jhi)) +
     *         B3*(A1*fxxyy(ilo,jlo)+ A2*fxxyy(ihi,jlo)) +
     *         B4*(A1*fxxyy(ilo,jhi)+ A2*fxxyy(ihi,jhi))+
     *         A1*(B1*fyy(ilo,jlo)  + B2*fyy(ilo,jhi))   +
     *         A2*(B1*fyy(ihi,jlo)  + B2*fyy(ihi,jhi)) +
     *         A3*(B1*fxxyy(ilo,jlo)+ B2*fxxyy(ilo,jhi)) +
     *         A4*(B1*fxxyy(ihi,jlo)+ B2*fxxyy(ihi,jhi)) 

c         write (6,'(i3,f7.2,i3,2f12.6/3x,f7.2,i3,2f12.6)')
c    *     k,x,ilo,A2,p,  y,jlo,B2,cv

c         Check whether delsq f has started to increase again;
c         if so, discontinue radial search.

          dcv = cv - cvls1
          if ((dcvlst.le.0.).and.(dcv.gt.0.)) then
            imissd(idir) = 1
            if (diag1) then
              alpha = (cvls2-cv)/(cvls2-2.*cvls1+cv) * 0.5
              write (6,'(2i3,3f8.3,11f8.2)') idir,k,
     *         cvls2,cvls1,cv,alpha
            endif
            go to 290
          endif

c         Check whether delsq f < 0; if so, exit loop to 240

          if (cv.lt.0.) go to 240

          cvls2  = cvls1
          cvls1  = cv
          dcvlst = dcv
  230   continue
        go to 600
  240   continue

c       Interpolate to find radius and position and position on the
c       projection at which cv = 0.  If (sphtrg) determine this 
c       radius as a distance on the sphere.  The radius ra(idir)
c       (by whichever method calculated) is in deg.lat.
        
        alpha = cvls1/(cvls1-cv)


crmwdbg    rmwdebug ad-hoc fix
crmw
crmw    g77 problem when k=1, r(k-1) can be negative and the log
crmw    function below gives a nan
crmw
cc        if (r(k-1).gt.0) then
cc           rpt = r(k-1) + alpha*dr
cc        else
cc           rpt = r(k) + alpha*dr
cc        endif
crmwdbg

c * g77 under Cygwin: Can still get NaN since k=1 corresponds to
c   r(0) which is not a valid array element. From the definition
c   of r(k) above:
c     r(k) = float(k)*dr
c   so  r(0)= 0. KK 13/12/2003

        if(k.eq.1)then
          rpt = alpha*dr
        else
          rpt = r(k-1) + alpha*dr
        endif

        x = rpt*cosdir + xc
        y = rpt*sindir + yc
        
        if (sphtrg) then
          xs = x - xcen
          ys = y - ycen
          rs = sqrt(xs*xs + ys*ys)
          alon = atan2(xs,-ys*fhem)
          clat = c2*atan(rs/rproj)

          ra(idir) = rad*acos(cos(alon-alonc)*sin(clat)*sinclatc
     *                                       +cos(clat)*cosclatc)
        else

          ra(idir) = rpt*scalt

        endif

c       Determine function value at this position

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

        p(idir)= B1*(A1*f    (ilo,jlo)+ A2*f    (ihi,jlo)+
     *               A3*fxx  (ilo,jlo)+ A4*fxx  (ihi,jlo)) +
     *           B2*(A1*f    (ilo,jhi)+ A2*f    (ihi,jhi)+
     *               A3*fxx  (ilo,jhi)+ A4*fxx  (ihi,jhi)) +
     *           B3*(A1*fyy  (ilo,jlo)+ A2*fyy  (ihi,jlo)+
     *               A3*fxxyy(ilo,jlo)+ A4*fxxyy(ihi,jlo)) +
     *           B4*(A1*fyy  (ilo,jhi)+ A2*fyy  (ihi,jhi)+
     *               A3*fxxyy(ilo,jhi)+ A4*fxxyy(ihi,jhi))

        if (diag1) write (6,'(2i3,8x,2f8.3,11f8.2)') idir,k,
     *   cvls1,cv,alpha,r(k-1),ra(idir),p(idir)
  290 continue
  300 continue

c     Calculate area enclosed by endpoints and hence the `radius'
c     -----------------------------------------------------------

      if (diag1) write (6,'(/2a3,19a8/)') 'i1','i2','ra1','ra2',
     * 'ratm1','fact','fmult','sumrsq'
      do 310 idir1 = nrddir,1,-1
        if (imissd(idir1).eq.0) go to 320
  310 continue
  320 continue
      idir1x = idir1 - nrddir

      do 400 idir2 = 1,nrddir
        if (imissd(idir2).eq.1) go to 390
        fmult = idir2 - idir1x
        ra1 = ra(idir1)
        ra2 = ra(idir2)
        ratio = ra2/ra1
        ratm1 = ratio - 1.

        if (abs(ratm1).gt.0.1) then
          fact = (2.+ratm1)*ratm1/log(ratio) * 0.5
        else
          fact = 1. + ratm1*(1. + ratm1*(r6 - r180*ratm1*ratm1))
        endif
        sumrsq = sumrsq + ra1*ra1*fact*fmult
        if (diag1) write (6,'(2i3,19f8.2)') idir1,idir2,ra1,ra2,ratm1,
     *   fact,fmult,sumrsq
        idir1 = idir2
        idir1x = idir2
  390   continue
  400 continue

      if (sumrsq.eq.0.) go to 600

c       Scale sumrsq for sector fraction and calculate a radius, 
c     based on the assumption that 
c              A = pi*r**2 = pi*1/nrddir*(sum ra**2)
c                     r**2 =    1/nrddir*(sum ra**2)

      radius = sqrt(sumrsq/float(nrddir))

c     Calculate the environmental pressure
c     ------------------------------------

      if (diag1) write (6,'(/3a3,19a8/)') 'i','i1','i2','alpha',
     * 'p(i1)','p(i2)','ra(i1)','ra(i2)','p(i)','ra(i)',
     * 'rr','for','sumrr','sumfor'
      do 500 idir = 1,nrddir
        if (imissd(idir).eq.1) then
          do 420 idir1x = idir,idir-nrddir,-1
            idir1 = mod(idir1x+nrddir-1,nrddir)+1
            if (imissd(idir1).eq.0) go to 430
  420     continue
  430     continue
          do 440 idir2x = idir,idir+nrddir
            idir2 = mod(idir2x-1,nrddir)+1
            if (imissd(idir2).eq.0) go to 450
  440     continue
  450     continue
          alpha = float(idir - idir1x)/float(idir2x - idir1x)
          p(idir)  = p(idir1)*(1.-alpha) + p(idir2)*alpha
          ra(idir) = ra(idir1)**(1.-alpha)*ra(idir2)**alpha
        endif
        rr = 1./ra(idir)
        for = p(idir)*rr
        sumrr = sumrr + rr
        sumfor  = sumfor  + for
        if (diag1) then
          if (imissd(idir).eq.1) then
            write (6,'(3i3,19f8.2)') idir,idir1,idir2,alpha,
     *       p(idir1),p(idir2),ra(idir1),ra(idir2),p(idir),ra(idir),
     *       rr,for,sumrr,sumfor
          else
            write (6,'(i3,46x,19f8.2)') idir,p(idir),ra(idir),
     *       rr,for,sumrr,sumfor
          endif
        endif
  500 continue

      fbar = sumfor/sumrr
      depth = fbar-fc

      go to 610
  600 continue
      radius = 0.
      fbar   = spvc
      depth  = 0.
  610 continue

      if (diag1) write (6,'(/10a8)') 'xc','yc','ca',
     * 'sumrsq','radius','fbar','fc','depth'
      if (diag1) write (6,'(10f8.2/)') xc,yc,ca,sumrsq,radius,
     * fbar,fc,depth

      return
      end
