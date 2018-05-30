c#ifdef delsqzs
      Subroutine cycp(f,fxx,fyy,fxxyy,q,z,zxx,zyy,zxxyy,idiagy,ier)
c#else
c     Subroutine cycp(f,fxx,fyy,fxxyy,q,z,idiagy,ier)
c#endif

c       A subroutine (of cyclocp) which scans a PS array for grid points
c     likely to be close to maxima or minima of a function fitted to them
c     and from each such point searches for the centre of a closed high/low
c     (using the subroutine fmin) and, if one be not found, an open high/low.  

c     Written 21 Feb 1991, R.J. Murray.
c     Last revised 11th Jan., 1996, R.J. Murray.

c-------------------------------------------------------------------------------
c     Explanation.
c-------------------------------------------------------------------------------

c     fhem             Projection: fhem=1. : PS NH, fhem=-1.: PS SH
c     rproj            No. of grid point spaces between pole and equator
c     ni,nj            Size of PS array in 90degE (i) and GM (j) directions
c     nilt,njlt,nijlt  Maximum dimensions of ni,nj, and ni*nj
c     (xcen,ycen)      Position on array of pole of projection.  (This
c                        will normally be at the centre of the array, but
c                        may be at any grid point or intermediate location.)
c     f(i,j)           Function value at grid point (i,j), or its negative
c                        when highs are sought
c     fx,fxx,fxy,&c.   Bicubic spline coefficient arrays (ni*nj) (partial 
c                        derivatives of function w.r.t. x,x**2,xy,&c.
c     q(i,j)           Laplacian of f,i.e. fxx(i,j)+fyy(i,j); for highs, its -ve

c     hilo             Highs (H) or lows (L) required
c     iopmxc           Closed features only (0) or both open & closed (1)?

c     latmnc,latmxc    Minimum & maximum latitudes of search
c     lonmnc,lonmxc    Minimum & maximum longs. of search (no limit if both 0)
c     alatlt           Maximum absolute latitude beyond equator on PS projn.
c     clatmx,clatmn    Maximum and minimum radius of search in colatitude (deg.)
c     rsmx,rsmn        Maximum and minimum radius (rs) in grid units
c     imnc,imxc        Limits of search in i direction (0,0 = no x limits)
c     jmnc,jmxc        Limits of search in j direction (0,0 = no y limits)
c     iL,iR,jD,jU      Left,right,lower, & upper limits of search (taking
c                        also clatmx into account
c     nshell
c     nshell           No. (4,8,12,20,24) of surrounding points for scanning
c     sdrmx,sdrmxG     Maximum total distance (sigma dr) of new position 
c                        from starting position in deg.lat. and grid spaces
c     drmx1,drmx2      Maximum distance of movement in one iteration, for
c                        closed and open depressions, respectively
c     itmx1,itmx2      The maximum number of new grid squares to be traversed in
c                        searching for a closed or open depression, respectively
c     diflt1           Closest allowable spacing of systems
c     diflt2           Minimum separation of starting grid point and closed
c                        system position for allowing a parallel search for an
c                        open depression (parameter statement)
c     scalt            Map scale factor (deg.lat./grid space)
c     iq(k),jq(k)      The grid point positions of the curvature centres (points
c                        at which q is maximised for lows & minimised for highs)
c     nklt             The number of `curvature' centres allowed

c     (x,y)            Current estimate of the minimum position, in rectangular
c                        coordinates in which the integral positions (x=i,y=j)
c                        are the grid points at which function (f) is specified
c                        function, f, is defined at the integral values (i,j)
c     frr              The average 2nd derivative, (fxx + fyy)/2 at (x,y)
c     fuu,fvv          The min. and max. 2nd derivatives at (x,y)
c     laxis            Orientation of the major (u) axes w.r.t. (x,y)
c     cx               delsq f = (fxx + fyy)/2)
c                        (latitude scaled, in f units/deg.lat.**2)
c     cq               delsq p grid point minimum (iq,jq)
c     cd               delsq p value at high/low centre (xc,yc) = frr/scalt**2
c     ca               Average delsq p in a circle around (xc,yc) (or cd
c                         if cvarad = 0.)
c     cvarad           Radius around centre over which average delsq p is found
c     rdincr           Radial increment in deg.lat. for radius/depth
c     nrddir           No. of radial directions for radius/depth
c     cmnh             Minimum value of ch (function units/deg.lat.**2)
c     cmnc0            Minimum value of cx (function units/deg.lat.**2)
c     cmnc1            Minimum value of ca, closed depressions
c     cmnc2            Minimum value of ca, open depressions
c     fmxc             Maximum function value at centre
c     frmxc            Maximum absolute gradient of function at centre of open
c                        depression (f units/deg.lat.)
c     frcmxc           Maximum ratio of gradient/laplacian (deg.lat.)
c                        (The last 5 limits refer to lows; minima and maxima 
c                        are reversed when applied to highs.) 
c     ier2             Number identifying search error type:
c                        91  Open system close to closed system
c                        92  Depression close to another of same type
c                        93  ca < cmnc1,cmnc2, depth < dpmn
c                              (the condition is reversed for highs)
c                        94  fr/ca > frcvmxc or fr > frmxc (open systems)
c                        95  swv > swvmn (open systems)
c                        96  f > fmxc
c                        97  cx < cmnc0
c                        98  zs > zsmax 
c                        99  Latitude limits exceeded
c     idiagy           Diagnostic output option:
c                        1,2,3 = Print iop,x,y,f,cx,ca,lon,lat for each high/low
c                        2,3   = Also print table of ch &c., all seaches
c                        4     = Output iteration details for closed systems
c                        5     = Output iteration details for open systems
c                        6     = Diagnostics of delsqp averaging (closed systems
c                        7     = Diagnostics of delsqp averaging (open systems)

c     Output quantities
c     -----------------

c     nk               The number of highs/lows found
c     iopc(k)          High/low type (closed=0, open=1) of centre kc
c                        This array is also used to give the search status of
c                        each putative high/low (using numbers > 1)
c     (xc(k),yc(k))    Grid point location of centre, or lon & lat
c     fc(k)            Function value at high/low centre
c     cc(k)            Laplacian of f (1/2 del**2 f) averaged about high/low
c                        centre by subroutine cvave
c     cxc(k)           Laplacian of f at cyclone centre (xc(k),yc(k))
c     rdc(k)           `Radius' of depression
c     dpc(k)           `Depth' of depression
c     upc,vpc(k)       E and N steering velocity

c-------------------------------------------------------------------------------
c     Declarations and functions.
c-------------------------------------------------------------------------------

c     Type declarations
c     -----------------

      real latc,lonc
      real latv,lonv
      character ak*3,aijcvh*35,afound*60
      logical diag1,diag2,diag3,diag4,diag5,diag6,diag7,diag8,diag9
      logical highs,testzs,upredf,step,found,minerr,artifc
crmw
      logical mderiv
      real crmwarray
crmw
c     Parameter constants
c     -------------------

      parameter (rad=57.2957,rrad=1./rad,r2rad=1./(2.*rad))
      parameter (r3=1./3.,r6=1./6.)

c     Common blocks and arrays
c     ------------------------

c     (Block cyc1.h not included: 2-D arrays passed in argument list
c     to allow dimensioning to be changed.)

#include "cyc1.h"     
#include "cyc2.h"     
#include "cyc3.h"     
#include "cyc4.h"     
      COMMON /BLTMP/XV(nklt),YV(nklt) !ckk NKLT

      dimension f(ni,nj),fxx(ni,nj),fyy(ni,nj),fxxyy(ni,nj)
      dimension z(ni,nj),q(ni,nj)
c#ifdef delsqzs
      dimension zxx(ni,nj),zyy(ni,nj),zxxyy(ni,nj)
c#endif
      dimension iq(nklt),jq(nklt)     ! nklt defined in 'cyc3.h'
      dimension sink2(nklt),cosk2(nklt)

      parameter (nshlt=24)
      dimension irel(nshlt),jrel(nshlt)
  
c     Namelist
c     --------

      namelist /dercon/klast,fhem,scaleconst,rprojsq,clatmn,clatmx,
     * rsmx,rsmx2,rsmn,rsmn2,rsmn2,rsmxsq,rsmx2sq,rsmnsq,rsmn2sq,
     * imnl,imxl,jmnl,jmxl,imnr,imxr,jmnr,jmxr,iL,iR,jD,jU,
     * iL3,iR3,jD3,jU3,iL2,iR2,jD2,jU2

c     Statement function
c     ------------------

      rsq(xa,xb,ya,yb) = (xa-xb)*(xa-xb) + (ya-yb)*(ya-yb)

c-------------------------------------------------------------------------------
c     Setting up of arrays, &c.
c-------------------------------------------------------------------------------

      ier = 0

c     Logical constants.
c     ------------------

      highs = hilo(1:1).eq.'H'
      diag1 = (idiagy.eq.1).or.(idiagy.eq.2).or.(idiagy.eq.3)
      diag2 = (idiagy.eq.2).or.(idiagy.eq.3)
      diag3 = idiagy.eq.3
      diag4 = idiagy.eq.4
      diag5 = idiagy.eq.5
      diag6 = idiagy.eq.6
      diag7 = idiagy.eq.7
      diag8 = idiagy.eq.8
      diag9 = idiagy.eq.9
      testzs = zsmax.lt.1.e+10
      upredf = qsteer(1:1).eq.'F'

c     Constants.
c     ----------

      scaleconst = 2.*rproj*rad
      rprojsq    = rproj*rproj

c     Colatitude limits.
c     ------------------

      if (latmnc.gt.latmxc) stop ' Minimum latitude > maximum latitude.'
      clatlt = 90. + alatlt
      if ((latmnc.eq.0.).and.(latmxc.eq.0.)) then
        clatmn = 0.
        clatmx = clatlt
      else if (fhem.eq.-1.) then
        clatmn = 90. + latmnc
        clatmx = min(clatlt,90.+latmxc)
      else if (fhem.eq.1.) then
        clatmn = 90. - latmxc
        clatmx = min(clatlt,90.-latmnc)
      else
        stop ' fhem improper value.'
      endif

c     Limiting radii of search in grid units.
c     ---------------------------------------

      rsmx    = rproj*tan(clatmx*r2rad)
      rsmx2   = rsmx + 2.
      if (clatmn.gt.0.) then
        rsmn    = rproj*tan(clatmn/rad/2.)
        rsmn2   = rsmn - 2.
        if (rsmn2.lt.0) rsmn = 0.
      else
        rsmn    = 0.
        rsmn2   = 0.
      endif
      rsmxsq  = rsmx*rsmx
      rsmx2sq = rsmx2*rsmx2
      rsmnsq  = rsmn*rsmn
      rsmn2sq = rsmn2*rsmn2

      diflt1sq = diflt1*diflt1
      diflt2sq = diflt2*diflt2

c     Limiting i and j values.
c     ------------------------

      imnl = nint(xcen - rsmx)
      imxl = nint(xcen + rsmx)
      jmnl = nint(ycen - rsmx)
      jmxl = nint(ycen + rsmx)

      imnr = imnc
      if (imnc.lt.1) imnr = 1
      imxr = imxc
      if ((imxc.gt.ni).or.(imxc.lt.1)) imxr = ni
      if (imxr.lt.imnr) stop ' Max i of search less than min i .'
      jmnr = jmnc
      if (jmnc.lt.1) jmnr = 1
      jmxr = jmxc
      if ((jmxc.gt.nj).or.(jmxc.lt.1)) jmxr = nj
      if (jmxr.lt.jmnr) stop ' Max j of search less than min j .'

      iL = max0(imnr,imnl)
      iR = min0(imxr,imxl)
      jD = max0(jmnr,jmnl)
      jU = min0(jmxr,jmxl)

      iL3 = iL - 3
      iR3 = iR + 3
      jU3 = jU + 3
      jD3 = jD - 3
      if (iL3.lt.1)  iL3 = 1
      if (iR3.gt.ni) iR3 = ni
      if (jU3.gt.nj) jU3 = nj
      if (jD3.lt.1)  jD3 = 1
      jD2 = jD3 + 1
      jU2 = jU3 - 1
      iL2 = iL3 + 1
      iR2 = iR3 - 1

c     Inversion of function for finding highs.
c     ----------------------------------------

c       If highs be required, the function is inverted so that lows may 
c     be sought.

      if (highs) then
        do 120 j = 1,nj
          do 110 i = 1,ni
            if (f(i,j).ne.spval) then
              f(i,j)     = -f(i,j)
              fxx(i,j)   = -fxx(i,j)
              fyy(i,j)   = -fyy(i,j)
              fxxyy(i,j) = -fxxyy(i,j)
            endif
  110     continue
  120   continue
      endif

c     Evaluation of Gradient/Laplacian for all grid points
c     ----------------------------------------------------

      if (mscrn.ge.1) mderiv = .true.
      do 140 j = 1,nj
        do 130 i = 1,ni
          if (mscrn.eq.1) then
            if (f(i,j).eq.spval .or. f(i+1,j).eq.spval
     *       .or. f(i,j+1).eq.spval) then
              q(i,j) = 1.e+10
            else
              fx = f(i+1,j) - f(i,j) - r6*fxx(i+1,j) - r3*fxx(i,j)
              fy = f(i,j+1) - f(i,j) - r6*fyy(i,j+1) - r3*fyy(i,j)
              q(i,j) = sqrt(fx*fx + fy*fy)
            endif
          else
            if (f(i,j).eq.spval) then
              q(i,j) = 0.
            else
              q(i,j) = fxx(i,j) + fyy(i,j)
            endif
          endif
  130   continue
  140 continue

c     Summary of derived constants.
c     -----------------------------

      if (diag3) write (6,dercon)

c     Scanning points
c     ---------------

c       These are the positions relative to the target point at which the
c     function value is to be compared with that of the target point.  For
c     symmetry, sensible values of nshell are 4 (Laplacian star), 8, 12,
c     20, and 24.

      if (nshell.ne.4 .and. nshell.ne.8 .and. nshell.ne.12 
     *                .and. nshell.ne.20.and. nshell.ne.24)
     *  stop ' Choose nshell = 4,8,12,20, or 24.'

      data irel/1,0,-1,0,1,-1,-1,1,2,0,-2,0,2,1,-1,-2,-2,-1,1,2,2,
     * -2,-2,2/
      data jrel/0,1,0,-1,1,1,-1,-1,0,2,0,-2,1,2,2,1,-1,-2,-2,-1,2,
     * 2,-2,-2/

      if (diag3) then
        write (6,'(/a,i2,a/)') ' Relative positions used in scanning (',
     *   nshell,'):'
        write (6,'(4(2x,''('',i2,'','',i2,'')''))') 
     *   (irel(ishell),jrel(ishell),ishell=1,nshell)
      endif

c     Set certain quantities not always calculated to zero
c     ----------------------------------------------------

      cx     = 0.
      ca     = 0.
      radius = 0.
      depth  = 0.

c-------------------------------------------------------------------------------
c     Location of `curvature' centres and search for closed highs/lows.
c-------------------------------------------------------------------------------

      if (diag2) write (6,'(/'' Search for depressions.''//
     * a3,a11,3a6,3a6,1x,a4,1x,a3,1x,a3,a15,2a6,a9,2x,6a6)')
     * 'k','   (i,j)   ','lon','lat','cq','gq','cd','ca','laxp',
     * 'err','err','     (x,y)     ','lon','lat','fd','dep',
     * 'swv','xh','yh','ch'

      k = klast
c     k = 0
      do 500 j = jU2,jD2,-1
        yq = j
        ys = yq - ycen
        yssq = ys*ys
        if (yssq.gt.rsmx2sq) go to 490
        xsltj = sqrt(rsmx2sq - yssq)
        iLj = nint(xcen - xsltj)
        iRj  = nint(xcen + xsltj)
        if (iLj.lt.iL2) iLj = iL2
        if (iRj.gt.iR2) iRj = iR2

        do 480 i = iLj,iRj
          fq = f(i,j)
          if (fq.eq.spval) go to 470
          qq = q(i,j)

          if (mscrn.eq.1) then
            gq = qq
            if (f(i,j).eq.spval) then
              hq = 0.
            else
              hq = fxx(i,j) + fyy(i,j)
            endif
          else
            hq = qq
            if (diag2) then
              if (f(i,j).eq.spval .or. f(i+1,j).eq.spval
     *         .or. f(i,j+1).eq.spval) then
                gq = 1.e+10
                fx2 = spval
                fy2 = spval
                fxy = spval
                fggq = spval
                fccq = spval
              else
                fx = f(i+1,j) - f(i,j) - r6*fxx(i+1,j) - r3*fxx(i,j)
                fy = f(i,j+1) - f(i,j) - r6*fyy(i,j+1) - r3*fyy(i,j)
                gq = sqrt(fx*fx + fy*fy)
                if (gq.lt.1.e-7) then
                  fggq = fxx(i,j)
                  fccq = fyy(i,j)
                else
                  cgax = fx/gq
                  sgax = fy/gq

                  fx2 = fxx(i,j)
                  fy2 = fyy(i,j)
                  fxy =   (    f    (i+1,j+1)  -   f    (i,j+1)
     *                     -r6*fxx  (i+1,j+1)  -r3*fxx  (i,j+1))
     *                -   (    f    (i+1,j  )  -   f    (i,j  )
     *                     -r6*fxx  (i+1,j  )  -r3*fxx  (i,j  ))
     *                -r6*(    fyy  (i+1,j+1)  -   fyy  (i,j+1)
     *                     -r6*fxxyy(i+1,j+1)  -r3*fxxyy(i,j+1))
     *                -r3*(    fyy  (i+1,j  )  -   fyy  (i,j  )
     *                     -r6*fxxyy(i+1,j  )  -r3*fxxyy(i,j  ))

                  fggq= cgax*cgax*fx2 + 2.*sgax*cgax*fxy + sgax*sgax*fy2
                  fccq= sgax*sgax*fx2 - 2.*sgax*cgax*fxy + cgax*cgax*fy2
                endif
              endif
            endif
          endif

          if (hq.le.0.) go to 470

          xq = i
          xs = xq - xcen
          rssq = xs*xs + ys*ys
          if (rssq.gt.rsmx2sq) go to 470
          if (rssq.lt.rsmn2sq) go to 470

c         Determine if grid point q be a local maximum/minimum
c         ----------------------------------------------------

c           Scanning for likely gridpoints is carried out by seeking a Laplacian
c         greater than that at any of the "nshell" surrounding grid points.  The
c         positions of these grid points (irel,jrel) relative to the point of 
c         interest are defined in a data statement earlier.  The positions are
c         arranged in groups of 4 or 8 at increasing radii.

          do 220 ishell = 1,nshell
            jg = j + jrel(ishell)
            ig = i + irel(ishell)
            if (nshell.gt.8) then
              if ((ig.lt.1).or.(ig.gt.ni).or.(jg.lt.1).or.
     *         (jg.gt.nj)) go to 210
            endif
            if (mscrn.eq.1) then
              if (q(ig,jg).lt.qq .or. q(ig,jg).eq.spval) go to 470
            else if (mscrn.eq.2) then
              if (q(ig,jg).gt.qq .or. q(ig,jg).eq.spval) go to 470
            else
              if (f(ig,jg).lt.fq .or. f(ig,jg).eq.spval) go to 470
            endif
  210       continue
  220     continue

c         Determine if cq > cmnh
c         ----------------------

c           The map factor, scaltq (no. of degrees per grid space), is 
c         calculated for the grid point.  This enables the Laplacian (hq) 
c         to be scaled for latitude in function units/deg.lat**2; 
c         for a low to be sought ch must be > cmnh.

          scaltq   = scaleconst/(rprojsq + rssq)
          scaltqsq = scaltq*scaltq
          cq       = hq/scaltqsq
          if (diag2) frq = gq/scaltq
          if (cq.lt.cmnh) go to 470
          if (diag2) then
            fggq = fggq/scaltqsq
            fccq = fccq/scaltqsq
          endif

c#ifdef delsqzs
c         Add additional penalty according to delsq zs
c         --------------------------------------------

          if (ftopeq.ne.0.) then
            d2z = (zxx(i,j) + zyy(i,j))/scaltqsq
            cqz = ftopeq*d2z
            call pstoll(xcen,ycen,rproj,fhem,float(i),float(j),
     *       aloni,alatj)
            if (diag9) write (6,230)
     *       'cq: ',i,j,aloni,alatj,cq,cqz,cq-abs(cqz) 
  230       format (a4,2(i5,2x),2f7.1,3f9.4)
  240       format (a4,4f7.1,3f9.4)
            cq = cq - abs(cqz)
            if (cq.lt.cmnh) go to 470
          endif
c#endif

c         Exclude areas of topographic height > zsmax metres
c         --------------------------------------------------

CC        if (testzs) then
CC          zsq = amax0(z(i,j),z(i+1,j),z(i,j+1),z(i-1,j),z(i,j-1))
CC          zsq = amin0(z(i,j),z(i+1,j),z(i,j+1),z(i-1,j),z(i,j-1))
CC          zsq = z(i,j)
CC          if (zsq.gt.zsmax) then
CC            if (diag2) then
CC              call pstoll(xcen,ycen,rproj,fhem,float(i),float(j),
CC   *           aloni,alatj)
CC              write (6,'(3x,'' ('',i3,'','',i3,'') '',
CC   *           2f6.1,f6.3,''  topog. = '',2f7.1, ''m.'')') 
CC   *           i,j,aloni,alatj,ch,z(i,j),zsq
CC250           format (3x,a35,17x,i3,1x,i3)
CC            endif
CC            go to 470
CC          endif
CC        endif

          sdrmxG = sdrmx/scaltq 

c         Search for a minimum (iop=0), or else an inflection point (iop=1)
c         -----------------------------------------------------------------

          iopsrch = max0(0,min0(1,iopmxc))
          if (icendp.ge.5) iopsrch = 0

          do 460 iop = 0,iopsrch
            ier2 = iop

            if (icendp.ge.5) then
              xd = xq
              yd = yq
              fd = 0.
              swv = 0.
              laxp = 0.
              ier1 = 0
              go to 300
            endif

c           Find depression
c           ---------------

c             Subroutine fmin seeks a closed depression, at which there must 
c           be a true minimum (fuu>0, fu=fv=0 at (x,y)); failure to do so 
c           results in an error condition (ier1>0).  If a closed depression
c           shall not have been found and iopmxc > 0, this loop is repeated, 
c           and subroutine frmin seeks an open depression, where there must 
c           be a clear trough axis (fuu < fvv), and fuu=0, fv=0 at (x,y).

            if (iop.eq.0) then
              call fmin(xq,yq,iL3,iR3,jD3,jU3,0.,drmx1,sdrmxG,
     *         itmx1,xd,yd,fd,frr,fuu,laxp,ier1,diag4,iconcv)
            else
              call frmin(xq,yq,iL3,iR3,jD3,jU3,0.05,drmx2,sdrmxG,
     *         itmx2,xd,yd,fd,gd,frr,fuuu,laxp,ier1,diag5,iopmxc)
            endif

            artifc = iop.eq.1.and.iopmxc.ge.2.and.ier1.eq.5

c           Check whether a cyclone has been discovered AT this position
c           ------------------------------------------------------------

            found = .false.
            if (ier1.eq.0.or.ier1.eq.8.or.ier1.eq.9.or.artifc) then
              minerr = .false.
              do 270 kdash = klast+1,k
                ydif = abs(yd - yc(kdash))
                xdif = abs(xd - xc(kdash))
                if (ydif.gt.0.01) go to 260
                if (xdif.gt.0.01) go to 260
                found = .true.
                go to 280
  260           continue
  270         continue
  280         continue
            else
              minerr = .true.
            endif

c           Write diagnostics of aborted search, if required
c           ------------------------------------------------

            if (minerr .or. found) then
              if (diag2) then
                aijcvh = ' '
                afound = ' '
                if (iop.eq.0) then
                  call pstoll(xcen,ycen,rproj,fhem,float(i),float(j),
     *             aloni,alatj)
                  write (aijcvh,'('' ('',i3,'','',i3,'') '',
     *             2f6.1,2f6.2)') i,j,aloni,alatj,cq,frq
                else                                        !XX
                  write (aijcvh,'(23x,2f6.2)') fccq,fggq    !XX
                endif
                if (found) write (afound,'(4x,'' ('',f5.1,'','',f5.1,
     *           '')   System already found.'')') xd,yd
                write (6,290) aijcvh,ier1,afound(1:lnblnk(afound))
  290           format (3x,a35,18x,i3,a)
              endif
              go to 450
            endif

c           Calculate quantities associated with the depression centre
c           ----------------------------------------------------------

            rssq     = rsq(xd,xcen,yd,ycen)
            scaltd   = scaleconst/(rprojsq + rssq)
            scaltdsq = scaltd*scaltd
            scaltdcu = scaltd*scaltdsq
            cd       = 2.*frr/scaltdsq
            swv      = 0.
            if (iop.eq.1) swv = fuuu/scaltdcu

c#ifdef delsqzs
c           Allow for curvature of the terrain
c           ----------------------------------

            if (ftopeq.ne.0.) then
              ilo = int(xd)
              ihi = ilo + 1
              jlo = int(yd)
              jhi = jlo + 1

              A2 = (xd - float(ilo))
              A1 = 1. - A2
              A3 = A1*(A1*A1 - 1.)/6.
              A4 = A2*(A2*A2 - 1.)/6.

              B2 = (yd - float(jlo))
              B1 = 1. - B2
              B3 = B1*(B1*B1 - 1.)/6.
              B4 = B2*(B2*B2 - 1.)/6.

c             wilojlo = z(ilo+1,jlo)+z(ilo-1,jlo)+
c    *                  z(ilo,jlo+1)+z(ilo,jlo-1)-4.*z(ilo,jlo)
c             wihijlo = z(ihi+1,jlo)+z(ihi-1,jlo)+
c    *                  z(ihi,jlo+1)+z(ihi,jlo-1)-4.*z(ihi,jlo)
c             wilojhi = z(ilo+1,jhi)+z(ilo-1,jhi)+
c    *                  z(ilo,jhi+1)+z(ilo,jhi-1)-4.*z(ilo,jhi)
c             wihijhi = z(ihi+1,jhi)+z(ihi-1,jhi)+
c    *                  z(ihi,jhi+1)+z(ihi,jhi-1)-4.*z(ihi,jhi)
c             d2z = (B1*(A1*wilojlo + A2*wihijlo) +
c    *               B2*(A1*wilojhi + A2*wihijhi))/scaltdsq

              d2z = (B1*(A1*zxx(ilo,jlo)  + A2*zxx(ihi,jlo))   +
     *               B2*(A1*zxx(ilo,jhi)  + A2*zxx(ihi,jhi))   +
     *               B3*(A1*zxxyy(ilo,jlo)+ A2*zxxyy(ihi,jlo)) +
     *               B4*(A1*zxxyy(ilo,jhi)+ A2*zxxyy(ihi,jhi)) +
     *               A1*(B1*zyy(ilo,jlo)  + B2*zyy(ilo,jhi))   +
     *               A2*(B1*zyy(ihi,jlo)  + B2*zyy(ihi,jhi))   +
     *               A3*(B1*zxxyy(ilo,jlo)+ B2*zxxyy(ilo,jhi)) +
     *               A4*(B1*zxxyy(ihi,jlo)+ B2*zxxyy(ihi,jhi)))
     *               /scaltdsq

              cdz = ftopeq*d2z
              call pstoll(xcen,ycen,rproj,fhem,xd,yd,aloni,alatj)
              if (diag9) write (6,240) 'cd: ',xd,yd,aloni,alatj,
     *         cd,cdz,cd-abs(cdz) 
              cd = cd - abs(cdz)
            endif
c#endif

c           Calculate the position and strength of the associated
c           delsqp maximum
c           -----------------------------------------------------

            xh = 0.
            yh = 0.
            ch = 0.

  300       continue

            nqtrng = 7
c           nqtrng = 8
            if (nqtrng.gt.0) then
              xh = nint(xd*8.)*0.125
              yh = nint(yd*8.)*0.125
c             xh = nint(xd*4.)*0.25
c             yh = nint(yd*4.)*0.25

              ilo = int(xh)
              ihi = ilo + 1
              A2 = (xh - float(ilo))
              A1 = 1. - A2
              A3 = A1*(A1*A1 - 1.)/6.
              A4 = A2*(A2*A2 - 1.)/6.

              jlo = int(yh)
              jhi = jlo + 1
              B2 = (yh - float(jlo))
              B1 = 1. - B2
              B3 = B1*(B1*B1 - 1.)/6.
              B4 = B2*(B2*B2 - 1.)/6.

              fx2 = B1*(A1*fxx(ilo,jlo)  + A2*fxx(ihi,jlo))   +
     *              B2*(A1*fxx(ilo,jhi)  + A2*fxx(ihi,jhi))   +
     *              B3*(A1*fxxyy(ilo,jlo)+ A2*fxxyy(ihi,jlo)) +
     *              B4*(A1*fxxyy(ilo,jhi)+ A2*fxxyy(ihi,jhi))
              fy2 = A1*(B1*fyy(ilo,jlo)  + B2*fyy(ilo,jhi))   +
     *              A2*(B1*fyy(ihi,jlo)  + B2*fyy(ihi,jhi))   +
     *              A3*(B1*fxxyy(ilo,jlo)+ B2*fxxyy(ilo,jhi)) +
     *              A4*(B1*fxxyy(ihi,jlo)+ B2*fxxyy(ihi,jhi)) 
              hh = fx2 + fy2

              xs = xh - xcen
              ys = yh - ycen
              scalth  = scaleconst/(rprojsq + xs*xs + ys*ys)
              scalthsq = scalth*scalth
              ch = hh/scalthsq

              if (diag8) then  !!!
                write (6,'(/a,a,2f7.2//2a6,4a9/)') ' Refinement of',
     *           '  vorticity  centre:',x,y,'iqtrng','itry','delta',
     *           'xh','yh','ch'
                write (6,'(2i6,6f9.3)') 0,0,0.,xd,yd,cd
                write (6,'(2i6,6f9.3)') 0,0,0.,xh,yh,ch
              endif

              do 340 iqtrng = 1,nqtrng
                delta = 0.5**(iqtrng+1)
                do 320 itry = 1,100
                  xlst = xh
                  ylst = yh
                  step = .false.
                  do 310 ishell = 1,8
                    xx = xlst + float(irel(ishell))*delta
                    yy = ylst + float(jrel(ishell))*delta

                    ilo = int(xx)
                    ihi = ilo + 1
                    A2 = (xx - float(ilo))
                    A1 = 1. - A2
                    A3 = A1*(A1*A1 - 1.)/6.
                    A4 = A2*(A2*A2 - 1.)/6.

                    jlo = int(yy)
                    jhi = jlo + 1
                    B2 = (yy - float(jlo))
                    B1 = 1. - B2
                    B3 = B1*(B1*B1 - 1.)/6.
                    B4 = B2*(B2*B2 - 1.)/6.
                    fx2 = B1*(A1*fxx(ilo,jlo)  + A2*fxx(ihi,jlo))   +
     *                    B2*(A1*fxx(ilo,jhi)  + A2*fxx(ihi,jhi))   +
     *                    B3*(A1*fxxyy(ilo,jlo)+ A2*fxxyy(ihi,jlo)) +
     *                    B4*(A1*fxxyy(ilo,jhi)+ A2*fxxyy(ihi,jhi))
                    fy2 = A1*(B1*fyy(ilo,jlo)  + B2*fyy(ilo,jhi))   +
     *                    A2*(B1*fyy(ihi,jlo)  + B2*fyy(ihi,jhi))   +
     *                    A3*(B1*fxxyy(ilo,jlo)+ B2*fxxyy(ilo,jhi)) +
     *                    A4*(B1*fxxyy(ihi,jlo)+ B2*fxxyy(ihi,jhi)) 
                    hh = fx2 + fy2

                    xs = xx - xcen
                    ys = yy - ycen
                    scalth1  = scaleconst/(rprojsq + xs*xs + ys*ys)
                    scalthsq1 = scalth1*scalth1
                    ch1     = hh/scalthsq
                    if (ch1.gt.ch) then
                      ch = ch1
                      xh = xx
                      yh = yy
                      scalth = scalth1
                      scalthsq = scalthsq1
                      step = .true.
                    endif
  310             continue
                  if (diag8) write (6,'(2i6,4f9.3)') 
     *             iqtrng,itry,delta,xh,yh,ch
                  if (.not.step) go to 330
  320           continue
                rqtr = sqrt((xh-xd)**2+(yh-yd)**2)*scalth
		if(diag3) then !ckk
                  write (6,'(a10,i4,7f8.2)') '100 tries: ',
     *            iqtrng,xd,yd,xh,yh,rqtr
                endif
  330           continue
                if(diag3) then  !ckk
                  if (itry.gt.50) write (6,*) i,j,iqtrng,itry
     *      ,xd,yd,xh,yh,sqrt((xh-xd)**2+(yh-yd)**2)*scalth
                endif
  340         continue
            endif

            if (icendp.ge.2) then
              ilo = int(xh)
              ihi = ilo + 1
              jlo = int(yh)
              jhi = jlo + 1

              A2 = (xh - float(ilo))
              A1 = 1. - A2
              A3 = A1*(A1*A1 - 1.)/6.
              A4 = A2*(A2*A2 - 1.)/6.

              B2 = (yh - float(jlo))
              B1 = 1. - B2
              B3 = B1*(B1*B1 - 1.)/6.
              B4 = B2*(B2*B2 - 1.)/6.

              fh = B1*(A1*f    (ilo,jlo)+ A2*f    (ihi,jlo)+
     *                 A3*fxx  (ilo,jlo)+ A4*fxx  (ihi,jlo)) +
     *             B2*(A1*f    (ilo,jhi)+ A2*f    (ihi,jhi)+
     *                 A3*fxx  (ilo,jhi)+ A4*fxx  (ihi,jhi)) +
     *             B3*(A1*fyy  (ilo,jlo)+ A2*fyy  (ihi,jlo)+
     *                 A3*fxxyy(ilo,jlo)+ A4*fxxyy(ihi,jlo)) +
     *             B4*(A1*fyy  (ilo,jhi)+ A2*fyy  (ihi,jhi)+
     *                 A3*fxxyy(ilo,jhi)+ A4*fxxyy(ihi,jhi))

c#ifdef delsqzs
              if (ftopeq.ne.0.) then
                d2z = (B1*(A1*zxx(ilo,jlo)  + A2*zxx(ihi,jlo))   +
     *                 B2*(A1*zxx(ilo,jhi)  + A2*zxx(ihi,jhi))   +
     *                 B3*(A1*zxxyy(ilo,jlo)+ A2*zxxyy(ihi,jlo)) +
     *                 B4*(A1*zxxyy(ilo,jhi)+ A2*zxxyy(ihi,jhi)) +
     *                 A1*(B1*zyy(ilo,jlo)  + B2*zyy(ilo,jhi))   +
     *                 A2*(B1*zyy(ihi,jlo)  + B2*zyy(ihi,jhi))   +
     *                 A3*(B1*zxxyy(ilo,jlo)+ B2*zxxyy(ilo,jhi)) +
     *                 A4*(B1*zxxyy(ihi,jlo)+ B2*zxxyy(ihi,jhi)))
     *                 /scalthsq
c               d2z = (B1*(A1*w(ilo,jlo) + A2*w(ihi,jlo)) +
c    *                 B2*(A1*w(ilo,jhi) + A2*w(ihi,jhi)))/scalthsq
                chz = ftopeq*d2z

                call pstoll(xcen,ycen,rproj,fhem,xh,yh,aloni,alatj)
                if (diag9) write (6,240) 'ch: ',xh,yh,aloni,alatj,
     *           ch,chz,ch-abs(chz) 
                ch = ch - abs(chz)
              endif
c#endif

              distsq = rsq(xd,xh,yd,yh)
            endif

            if (icendp.lt.2 .or. 
     *       (distsq.gt.sdrmxG*sdrmxG .or. fh.lt.fd)) then
              xh = xd
              yh = yd
              fh = fd
              ch = cd
              scalth = scaltd
              scalthsq = scalth*scalth

              ilo = int(xd)
              ihi = ilo + 1
              jlo = int(yd)
              jhi = jlo + 1
            endif

            if (artifc .or. diag2) then
              A2 = (xh - float(ilo))
              A1 = 1. - A2
              A3 = A1*(A1*A1 - 1.)/6.
              A4 = A2*(A2*A2 - 1.)/6.
              A5 = -(3.*A1*A1 - 1.)/6.
              A6 =  (3.*A2*A2 - 1.)/6.

              B2 = (yh - float(jlo))
              B1 = 1. - B2
              B3 = B1*(B1*B1 - 1.)/6.
              B4 = B2*(B2*B2 - 1.)/6.
              B5 = -(3.*B1*B1 - 1.)/6.
              B6 =  (3.*B2*B2 - 1.)/6.

              fx =  B1*(   f    (ihi,jlo) -    f    (ilo,jlo) +
     *                  A6*fxx  (ihi,jlo) + A5*fxx  (ilo,jlo)) +
     *              B2*(   f    (ihi,jhi) -    f    (ilo,jhi) +
     *                  A6*fxx  (ihi,jhi) + A5*fxx  (ilo,jhi)) +
     *              B3*(   fyy  (ihi,jlo) -    fyy  (ilo,jlo) +
     *                  A6*fxxyy(ihi,jlo) + A5*fxxyy(ilo,jlo)) +
     *              B4*(   fyy  (ihi,jhi) -    fyy  (ilo,jhi) +
     *                  A6*fxxyy(ihi,jhi) + A5*fxxyy(ilo,jhi)) 
              fy =  A1*(   f    (ilo,jhi) -    f    (ilo,jlo) +
     *                  B6*fyy  (ilo,jhi) + B5*fyy  (ilo,jlo)) +
     *              A2*(   f    (ihi,jhi) -    f    (ihi,jlo) +
     *                  B6*fyy  (ihi,jhi) + B5*fyy  (ihi,jlo)) +
     *              A3*(   fxx  (ilo,jhi) -    fxx  (ilo,jlo) +
     *                  B6*fxxyy(ilo,jhi) + B5*fxxyy(ilo,jlo)) +
     *              A4*(   fxx  (ihi,jhi) -    fxx  (ihi,jlo) +
     *                  B6*fxxyy(ihi,jhi) + B5*fxxyy(ihi,jlo)) 
              fx2 = B1*(A1*fxx  (ilo,jlo) + A2*fxx(ihi,jlo))   +
     *              B2*(A1*fxx  (ilo,jhi) + A2*fxx(ihi,jhi))   +
     *              B3*(A1*fxxyy(ilo,jlo) + A2*fxxyy(ihi,jlo)) +
     *              B4*(A1*fxxyy(ilo,jhi) + A2*fxxyy(ihi,jhi))
              fy2 = A1*(B1*fyy  (ilo,jlo) + B2*fyy(ilo,jhi))   +
     *              A2*(B1*fyy  (ihi,jlo) + B2*fyy(ihi,jhi))   +
     *              A3*(B1*fxxyy(ilo,jlo) + B2*fxxyy(ilo,jhi)) +
     *              A4*(B1*fxxyy(ihi,jlo) + B2*fxxyy(ihi,jhi)) 
              fxy =    (   f    (ihi,jhi) -    f    (ilo,jhi) +
     *                  A6*fxx  (ihi,jhi) + A5*fxx  (ilo,jhi)) -
     *                 (   f    (ihi,jlo) -    f    (ilo,jlo) +
     *                  A6*fxx  (ihi,jlo) + A5*fxx  (ilo,jlo)) +
     *              B6*(   fyy  (ihi,jhi) -    fyy  (ilo,jhi) +
     *                  A6*fxxyy(ihi,jhi) + A5*fxxyy(ilo,jhi)) +
     *              B5*(   fyy  (ihi,jlo) -    fyy  (ilo,jlo) +
     *                  A6*fxxyy(ihi,jlo) + A5*fxxyy(ilo,jlo))

              gh = sqrt(fx*fx + fy*fy)
              if (gh.lt.1.e-7) then
                cax = 90.
                fggh = fxx(i,j)/scalthsq
                fcch = fyy(i,j)/scalthsq
              else
                cax = rad*atan2(fx,-fy)
                cgax = fx/gh
                sgax = fy/gh
                fggh =(cgax*cgax*fx2 + 2.*sgax*cgax*fxy + sgax*sgax*fy2)
     *              /scalthsq
                fcch =(sgax*sgax*fx2 - 2.*sgax*cgax*fxy + cgax*cgax*fy2)
     *              /scalthsq
              endif

              fcva = (fx2 + fy2)*0.5
              fecc = (fx2 - fy2)*0.5
              fcvd = sqrt(fecc*fecc + fxy*fxy)
              fuuh = (fcva - fcvd)/scalthsq
              if (fcvd.lt.1.e-7) then
                alax = 0.
              else
                alax = 90. + rad*atan2(fxy,fecc)/2.
                if (alax.eq.180.) alax = 0
              endif

c             if ((fuuh.lt.0.).and.(iconcv.ge.1)) go to 350
              if (artifc.and.(fcch.lt.fccmn.or.fuuh.lt.0.)) go to 350
c    *         .and.fcch.lt.fccmn) go to 350
              go to 360

  350         continue
              if (diag2) then
                write (6,'(/30a7)') 'xh','yh','fx','fy','gh','fx2',
     *           'fy2','ch','fxy','cax','fcch','fggh','fcva','fcvd',
     *           'fecc','alax','fuuh'
                write (6,'(20f7.3/)') xh,yh,fx/scalth,fy/scalth,
     *           gh/scalth,fx2/scalthsq,fy2/scalthsq,ch,fxy/scalthsq,
     *           cax,fcch,fggh,fcva/scalthsq,fcvd/scalthsq,
     *           fecc/scalthsq,alax,fuuh
                write (6,*) ' '
              endif
              ier2 = 95
  360         continue
            endif

c           Assign position and strength
c           ----------------------------

            if (icendp.ge.4) then
              x = xh
              y = yh
              fx = fh
            else
              x = xd
              y = yd
              fx = fd
            endif

            if ((icendp.ge.2 .and. ch.gt.cd) .or. 
     *           icendp.ge.4) then
              xave = xh
              yave = yh
              fave = fh
              scalta = scalth
              cx = ch
            else
              xave = xd
              yave = yd
              fave = fd
              cx = cd
              scalta = scaltd
            endif

c           Check whether cyclone conforms to specified properties
c           ------------------------------------------------------

c             The position of the minimum must be within the limiting 
c           colatitude and (i,j) limits and various properties 
c           (ca,fr,fr/ca) must lie within specified limits.

            rssq     = rsq(x,xcen,y,ycen)
            scaltc   = scaleconst/(rprojsq + rssq)
            scaltcsq = scaltc*scaltc

            ca     = 0.
            depth  = 0.
            radius = 0.

            if (iop.eq.1) then
              frd    = gd/scaltd
              frc    = frd/cd
              if ((frcmxc.gt.0.).and.(frc.gt.frcmxc))    ier2 = 94
              if ((frmxc.gt.0.).and.(frd.gt.frmxc))      ier2 = 94
              if (abs(swv).lt.swvmn .and. (.not.artifc)) ier2 = 95
            endif
            if ((fmxc.ne.-999.).and.(fx.gt.fmxc))        ier2 = 96
c           if (cd.lt.cmnc0 .and. (.not.artifc))         ier2 = 97
c           if (cx.lt.cmnc0)                             ier2 = 97
            if (rssq.gt.rsmxsq) then                     ier2 = 99
            if (rssq.lt.rsmnsq) then                     ier2 = 99
            if (ier2.ge.90) go to 400

            if (testzs .or. itabc3.ge.4) then
              ilo = min0(ni-1,int(x))
c             ilo = min0(ni-1,int(xd))
              ihi = ilo + 1
              jlo = min0(nj-1,int(y))
c             jlo = min0(nj-1,int(yd))
              jhi = jlo + 1
              if (z(ilo,jlo).gt.zsmax .or. z(ihi,jlo).gt.zsmax .or. 
     *            z(ilo,jhi).gt.zsmax .or. z(ihi,jhi).gt.zsmax .or. 
     *            itabc3.ge.4) then
                A2 = x-float(ilo)
                A1 = 1. - A2
                B2 = y-float(jlo)
                B1 = 1. - B2
                zsx = B1*(A1*z(ilo,jlo) + A2*z(ihi,jlo))+
     *                B2*(A1*z(ilo,jhi) + A2*z(ihi,jhi))
                if (zsx.gt.zsmax)                         ier2 = 98
              endif

c             ilo = min0(ni-1,int(xh))
c             ihi = ilo + 1
c             jlo = min0(nj-1,int(yh))
c             jhi = jlo + 1
c             if (z(ilo,jlo).gt.zsmax .or. z(ihi,jlo).gt.zsmax .or. 
c    *            z(ilo,jhi).gt.zsmax .or. z(ihi,jhi).gt.zsmax) then
c               A2 = x-float(ilo)
c               A1 = 1. - A2
c               B2 = y-float(jlo)
c               B1 = 1. - B2
c               zsh = B1*(A1*z(ilo,jlo) + A2*z(ihi,jlo))+
c    *                B2*(A1*z(ilo,jhi) + A2*z(ihi,jhi))
c               if (zsh.gt.zsmax)                         ier2 = 98
c             endif
            endif

            if (ier2.ge.90) go to 400

c           Compute estimates of cyclone intensity, depth, and radius
c           ---------------------------------------------------------

c             A computation is made of the average value of the curvature,
c           ca, over a radius cvarad (deg.lat.), which gives a better 
c           measure of the intensity of the system than the curvature at the 
c           central position.

c             If the average ca >= cmnc{1,2} at the minimum, the position, 
c           central pressure, and ca of the centre will be recorded.
c           The subroutine cvave also computes measures of the radius and
c           depth of such centres, if required (itabc3=3)

c             The position and pressure of the centre for the purpose of
c           these calculations may be taken as the pressure minimum (or
c           inflection point for an open depression) if icendp = 1, or
c           the delsq p centre if icendp >= 2.

            if (cvarad.gt.0. .or. itabc3.ge.3)
     *       call cvave(cvarad,scalta,f,fxx,fyy,fxxyy,z,
c#ifdef delsqzs
     *       zxx,zyy,zxxyy,
c#endif
     *       xave,yave,fave,cx,ca,itabc3,rdincr,nrddir,sphtrg,ftopeq,
     *       radius,depth,diag6,diag9)

c           Test for minimum strength
c           -------------------------

            if (cvarad.le.0.) ca = cx
c           if (ca.ge.cx)     ca = cx
            if (ca.lt.cmnc1. and .iop.eq.0)      ier2 = 93
            if (ca.lt.cmnc2. and .iop.eq.1)      ier2 = 93
            if (itabc3.ge.3. and. depth.lt.dpmn) ier2 = 93

c           Test for weak cyclone status (iop>=10)
c           -------------------------------------

            if (ier2.lt.90) then
              iertmp = iop
              if (artifc) iertmp = 2

              ier2 = iertmp
c             if (cx.lt.cmncw)      ier2 = iertmp + 10
              if (ca.lt.cmncw)      ier2 = iertmp + 10
              if (depth.lt.dpmnw)   ier2 = iertmp + 10
              if ((fmxcw.ne.-999.).and.
     *           (fx.gt.fmxcw))     ier2 = iertmp + 10
              if (iop.eq.1.and.abs(swv).lt.swvmnw) ier2 = iertmp + 10
              if (mod(ier2,10).gt.iopmxc)   ier2 = 93
              if (ier2.gt.istmxc)           ier2 = 93
            endif

c           Check whether a cyclone has been discovered NEAR this position
c           --------------------------------------------------------------

c             A check is made to see whether the centre or vorticity centre
c           lies within a radius ("diflt1") of an already discovered centre
c           or vorticity centre, respectively; if so, one of the systems 
c           is eliminated.

            if (ier2.lt.90 .and. k.gt.klast) then
              do 380 kdash = klast+1,k
                if (mod(iopc(kdash),10).gt.iopmxc) go to 370
                if (iopc(kdash).gt.istmxc) go to 370

                ydif = abs(yh - yv(kdash))*scalth
                xdif = abs(xh - xv(kdash))*scalth
                if (ydif.le.0.1 .and. xdif.le.0.1) then
c                 rdifsq = rsq(xh,xv(kdash),yh,yv(kdash))*scalthsq
                  rdifsq = xdif*xdif + ydif*ydif
                  if (rdifsq.le.0.01) then
                    if (ier2.gt.iopc(kdash)) then 
                      ier2 = 91
                      go to 390
                    else if (ier2.lt.iopc(kdash)) then
                      iopc(kdash) = 91
                      if (diag2) write (6,*) ' System ',
     *                 kdash,' eliminated (ier2 = 91)'
                      go to 370
                    else
                      drsqkdash = rsq(xh,xc(kdash),yh,yc(kdash))
                      drsqk     = rsq(xh,x,yh,y)
                      if (drsqk.lt.drsqkdash) then
                        iopc(kdash) = 92
                        if (diag2) write (6,*) ' System ',
     *                   kdash,' eliminated (ier2 = 92)'
                        go to 370
                      else
                        ier2 = 92
                        go to 390
                      endif
                    endif
                  endif
                endif

                if (icendp.ge.4) go to 370

                ydif = abs(y - yc(kdash))*scaltc
                xdif = abs(x - xc(kdash))*scaltc
                if (ydif.le.diflt1 .and. xdif.le.diflt1) then
c                 rdifsq = rsq(x,xc(kdash),y,yc(kdash))*scaltcsq
                  rdifsq = xdif*xdif + ydif*ydif
                  if (rdifsq.le.diflt1sq) then
                    if (ier2.gt.iopc(kdash)) then 
                      ier2 = 91
                      go to 390
                    else if (ier2.lt.iopc(kdash)) then
                      iopc(kdash) = 91
                      if (diag2) write (6,*) ' System ',
     *                 kdash,' eliminated (ier2 = 91)'
                      go to 370
                    else
                      cckdash = cc(kdash)
                      if (highs) cckdash = -cckdash
                      if (ca.gt.cckdash) then
                        iopc(kdash) = 92
                        if (diag2) write (6,*) ' System ',
     *                   kdash,' eliminated (ier2 = 92)'
                        go to 370
                      else
                        ier2 = 92
                        go to 390
                      endif
                    endif
                  endif
                endif
  370           continue
  380         continue
  390         continue
            endif

c           Restore sign for properties of highs
c           ------------------------------------

c             For finding highs the sign of the function values over the 
c           array was reversed in order to allow the minimum finding routine 
c           to be used.  The central function values and certain derivatives 
c           are restored here.  (Depth is not inverted.)

            fxp = fx
            if (highs) then
              fd  = -fd
              fx  = -fx
              ca  = -ca
              cvx = -cvx
              ch  = -ch
            endif

  400       continue

c           Increment counter
c           -----------------

            if (ier2.lt.90) then
              k = k + 1
              write (ak,'(i3)') k
            else
              ak = ' '
            endif

c           Write diagnostics of search
c           ---------------------------

c             The positions and properties of systems (where applicable) and the
c           error status of the search are summarised, if required.

            if (diag2) then
              aijcvh = ' '
              call pstoll(xcen,ycen,rproj,fhem,x,y,alonx,alaty)
              if (iop.eq.0) then
                call pstoll(xcen,ycen,rproj,fhem,float(i),float(j),
     *           aloni,alatj)
                write (aijcvh,'('' ('',i3,'','',i3,'') '',
     *           2f6.1,2f6.2)') i,j,aloni,alatj,cq,frq
              else                                       !XX
                write (aijcvh,'(23x,2f6.2)') fcch,fggh   !XX
              endif
              write (6,410) ak,aijcvh,cd,ca,laxp,ier1,ier2,
     *         xd,yd,alonx,alaty,fd,depth,swv,xh,yh,ch
  410         format (a3,a35,2f6.2,1x,i4,1x,
     *         i3,1x,i3,' (',f5.1,',',f5.1,') ',2f6.1,f9.3,1x,
     *         f7.2,f6.3,2f6.1,2f6.2)
            endif

            if (ier2.ge.90) go to 450

c           Enter cyclone properties in cyclone data arrays
c           -----------------------------------------------

c             The cyclone count is incremented by 1 and the information 
c           relating to the present system is placed in arrays iopc,xc,yc,
c           fc,cc, and also cxc,rdc,dpc,upc,vpc (if raddep).

            if (k.eq.nklt) then
              write (6,420) da,hr,nklt
  420         format (/' Warning (',i6,1x,i4,'): More than ',i3,
     *         ' high/low centres'/' Skipping to next period.'/)
              return
            endif

            iopc(k) = ier2
            iq(k) = i
            jq(k) = j

            xc(k) = x
            yc(k) = y
            xv(k) = xh
            yv(k) = yh
            fc(k) = fx
            cc(k) = ca
            cxc(k) = cx
            rdc(k) = radius
            dpc(k) = depth
            zsc(k) = zsx

c           Compute cyclone velocity
c           ------------------------

            if (itabc4.eq.1 .and. upredf) then
              call steer1(scaltc,f,fxx,fyy,fxxyy,z,x,y,fxp,
     *         upc(k),vpc(k),diag7)
              if (highs) then
                upc(k) = -upc(k)
                vpc(k) = -vpc(k)
              endif
            endif

c           Determine whether to do a parallel search for an open depression
c           ----------------------------------------------------------------

c             An open depression is sought when either a closed depression
c           is not found or a depression is found but is more than a certain 
c           distance from the grid point delsq p maximum.  

            if (iopsrch.gt.0 .and. iop.eq.0 .and. ier2.lt.90) then
              drsq = scaltcsq*rsq(xd,xq,yd,yq)
c             write (6,'(a3,10f7.3)') ' X ',x,xq,y,yq,scaltcsq,
c    *         rsq(x,xq,y,yq),rsq(x,xq,y,yq)*scaltcsq,drsq,diflt2sq
              if (drsq.lt.diflt2sq) go to 470
            endif
  450       continue
            if ((ier1.ge.70).and.(ier1.le.79).and.iconcv.ge.2)
     *       go to 470
  460     continue
  470     continue
  480   continue
  490   continue
  500 continue

      nk = k

c-------------------------------------------------------------------------------
c     Reduction of numbers of centres and conversion to lats/lons.
c-------------------------------------------------------------------------------

      if (diag1) write (6,610)' Summary of systems before renumbering'
  610 format (/a//'  k   iop     (xc,yc)           f       cv',
     * '       (lonc,latc)'/)

      knew = klast
      do 670 k = klast+1,nk

c       Eliminate identical systems
c       ---------------------------

c         In some cases the elimination of a system position may be necessary
c       where searches from two grid points arrive at near positions; this
c       is flagged by values of iopc(k) > iopmxc.

        if (mod(iopc(k),10).gt.iopmxc) go to 660
        if (iopc(k).gt.istmxc) go to 660

c       Calculate positions as latitudes and longitudes
c       -----------------------------------------------

        call pstoll(xcen,ycen,rproj,fhem,xc(k),yc(k),lonc,latc)
        call pstoll(xcen,ycen,rproj,fhem,xv(k),yv(k),lonv,latv)

c       Exclude systems outside area of interest
c       ----------------------------------------

        if (latc.lt.latmnc) go to 660
        if (latc.gt.latmxc) go to 660
        if ((lonmnc.gt.0.).or.((lonmxc.gt.0.).and.(lonmxc.lt.360.)))
     *   then
          if (lonmxc.gt.lonmnc) then
            if ((lonc.gt.lonmxc).or.(lonc.lt.lonmnc)) go to 660
          else
            if ((lonc.gt.lonmxc).and.(lonc.lt.lonmnc)) go to 660
          endif
        endif

        knew = knew + 1

        if (diag1) write (6,620) k,iopc(k),xc(k),yc(k),fc(k),
     *   cc(k),lonc,latc
  620   format (i3,3x,i2,'  (',f6.2,',',f6.2,')  ',2f9.3,'  (',f6.1,
     *   ',',f6.1,')')

c       Renumber cyclones and convert positions to lat.-lon.
c       ----------------------------------------------------

        if (icendp.ge.3) then
          xc(knew) = lonv
          yc(knew) = latv
        else
          xc(knew) = lonc
          yc(knew) = latc
        endif
c       xc(knew)   = lonc
c       yc(knew)   = latc
        xv(knew)   = lonv
        yv(knew)   = latv
        fc(knew)   = fc(k)
        cc(knew)   = cc(k)
        iopc(knew) = iopc(k)
        cxc(knew)  = cxc(k)
        rdc(knew)  = rdc(k)
        dpc(knew)  = dpc(k)
        zsc(knew)  = zsc(k)
        upc(knew)  = upc(k)
        vpc(knew)  = vpc(k)

  660   continue
  670 continue

      nk = knew

      if (diag1) then
        write (6,680)' Summary of systems after renumbering'
  680   format (/a//'  k   iop     (xc,yc)           f       cv'/)
        do 700 k = klast+1,nk
          write (6,690) k,iopc(k),xc(k),yc(k),fc(k),cc(k)
  690     format (i3,3x,i2,'  (',f6.2,',',f6.2,')  ',2f9.3)
  700   continue
      endif

c-------------------------------------------------------------------------------
c     Eliminate of identical systems from different projections
c-------------------------------------------------------------------------------

      if (klast.eq.0) then
        klast = nk
        return
      endif

c     If klast > 0, i.e., if cyclones have also been found on another,
c     overlapping projection, a check is made for cyclones which may be
c     identical

      if (diag3) write (6,710) ' Elimination of identical systems',
     * ' from different projections','k','iop','x','y','f','c','cx',
     * 'rd','dp','zs','wt'
  710 format (/a,a//a3,1x,a3,1x,12a9/)

      do 715 k2 = 1,klast
        sink2(k2) = sin(yc(k2)*rrad)
        cosk2(k2) = cos(yc(k2)*rrad)
  715 continue

      do 770 k1 = klast+1,nk
        if (abs(yc(k1)).gt.alatlt+diflt1) go to 760
        sink1 = sin(yc(k1)*rrad)
        cosk1 = cos(yc(k1)*rrad)
        do 740 k2 = 1,klast
          if (abs(yc(k2)).gt.alatlt+diflt1) go to 730
          ydif = abs(yc(k1) - yc(k2))
          if (ydif.gt.diflt1) go to 730
          if (abs(yc(k1)).lt.85.) then
            diflon = abs(xc(k1) - xc(k2))
            if (diflon.gt.180.) diflon = abs(diflon-360.)
            xdif = diflon*amin1(cosk1,cosk2(k2))
            if (xdif.gt.diflt1) go to 730
          endif

crmwdbg
crmw          write(*,'(f32.28)') cosk1*cosk2(k2)+sink1*sink2(k2)
crmw          write(*,*)'foo'
crmw          write(*,'(f32.28)') cos((xc(k1)-xc(k2))*rrad)

          crmwarray = cos((xc(k1)-xc(k2))*rrad)
     *            *cosk1*cosk2(k2)+sink1*sink2(k2)

          if (abs(crmwarray).gt.1.0) then
             if (crmwarray.gt.0.) then
                crmwarray = 1.0
             else
                crmwarray = -1.0
             endif
          endif
 
           rdif = rad*acos( crmwarray )
crmw          rdif = rad*acos( cos((xc(k1)-xc(k2))*rrad)
crmw     *            *cosk1*cosk2(k2)+sink1*sink2(k2) )
crmwdbg

          rdifsq = rdif*rdif
          if (rdifsq.gt.diflt1sq) then
            go to 730
          else
            if (iopc(k1).gt.iopc(k2)) then 
              wt1 = 1.
            else if (iopc(k1).lt.iopc(k2)) then
              wt1 = 0.
            else
              yav = (yc(k1) + yc(k2))*0.5
              if (yav*fhem.gt.alatlt) then
                wt1 = 1.
              else if (yav*fhem.gt.alatlt) then
                wt1 = 0.
              else
                wt1 = (yav*fhem+alatlt)/(2.*alatlt)
              endif
              wt2 = 1. - wt1
            endif
            ioptmp = max0(iopc(k1),iopc(k2))
            xtmp   = wt1*xc(k1)  + wt2*xc(k2)
            ytmp   = wt1*yc(k1)  + wt2*yc(k2)
            ftmp   = wt1*fc(k1)  + wt2*fc(k2)
            ctmp   = wt1*cc(k1)  + wt2*cc(k2)
            cxtmp  = wt1*cxc(k1) + wt2*cxc(k2) 
            rdtmp  = wt1*rdc(k1) + wt2*rdc(k2)
            dptmp  = wt1*dpc(k1) + wt2*dpc(k2)
            zstmp  = wt1*zsc(k1) + wt2*zsc(k2)
            uptmp  = wt1*upc(k1) + wt2*upc(k2)
            vptmp  = wt1*vpc(k1) + wt2*vpc(k2)
            if (diag1) then
              write (6,720) k1,iopc(k1),xc(k1),yc(k1),fc(k1),
     *         cc(k1),cxc(k1),rdc(k1),dpc(k1),zsc(1),
     *         upc(k1),vpc(k1),wt1
              write (6,720) k2,iopc(k2),xc(k2),yc(k2),fc(k2),
     *         cc(k2),cxc(k2),rdc(k2),dpc(k2),zsc(2),
     *         upc(k2),vpc(k2),wt2
              write (6,720) k1,ioptmp,xtmp,ytmp,ftmp,ctmp,cxtmp,
     *         rdtmp,dptmp,zstmp,uptmp,vptmp
              write (6,*) ' '
  720         format (i3,1x,i3,1x,12f9.3)
            endif
            iopc(k1) = 90
            iopc(k2) = ioptmp
            xc(k2)  = xtmp
            yc(k2)  = ytmp
            fc(k2)  = ftmp
            cc(k2)  = ctmp
            cxc(k2) = cxtmp
            rdc(k2) = rdtmp
            dpc(k2) = dptmp
            zsc(k2) = zstmp
            upc(k2) = uptmp
            vpc(k2) = vptmp
            go to 750
          endif
  730     continue
  740   continue
  750   continue

  760 continue
  770 continue

c     Renumber cyclones
      
      knew = klast
      do 800 k1 = klast+1,nk
        if (iopc(k1).ne.90) then
          knew = knew + 1
          xc(knew)   = xc(k1)
          yc(knew)   = yc(k1)
          fc(knew)   = fc(k1)
          cc(knew)   = cc(k1)
          iopc(knew) = iopc(k1)
          cxc(knew)  = cxc(k1)
          rdc(knew)  = rdc(k1)
          dpc(knew)  = dpc(k1)
          zsc(knew)  = zsc(k1)
          upc(knew)  = upc(k1)
          vpc(knew)  = vpc(k1)
        endif
  800 continue
      nk = knew
      klast = nk

      return
      end
c     scosd(xx,yy,zz) = acos (cos(xx)*cos(yy)*cos(zz)+sin(yy)*sin(zz))
