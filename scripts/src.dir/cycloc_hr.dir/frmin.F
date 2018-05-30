      Subroutine frmin(xdep,ydep,iL,iR,jD,jU,exgdmx,drmx,sdrmx,
     * itmx,xnew,ynew,fnew,gnew,fcva,fggg,laxis,err,diag,iopmxc)

c       A subroutine which searches iteratively for a point of inflexion 
c     within a trough by seeking a true minimum of the magnitude of the 
c     gradient vector of a bicubic function defined by the values of the 
c     function and its fifteen derivatives to third order on a grid of unit 
c     spacing in x and y, by the method of conjugate directions.  

c     Written 21st Feb. 1991;
c     Last revised 11th Jan. 1996.

c-------------------------------------------------------------------------------
c     Explanation.
c-------------------------------------------------------------------------------

c     (xdep,ydep)      Point of departure at start of iteration
c     (x,y)            Current estimate of the minimum position, in rectangular
c                        coordinates in which the integral positions (x=i,y=j)
c                        are the grid points at which function (f) is specified
c                        function, f, is defined at the integral values (i,j)
c     iL,iR,jD,jU      Left,right,lower, & upper edges of f array
c     (xfit,yfit)      The grid point about at which the derivatives are
c                        reckoned (normally lower left of (x,y)) = (ifit,jfit)
c     (xr,yr)          The fractional grid point location (xr = x - xfit, &c.)
c     f,fx,fxx,fxy,&c. Function and it partial derivatives of function w.r.t. 
c                        x,x**2,xy,&c. at point (x,y)
c     g                Magnitude of the gradient vector of f at (x,y)
c     fmn0(m,n)        f and its derivatives w.r.t x**m and y**n (m,n <= 3) at 
c                        (ifit,jfit), used in Taylor's expansion for finding
c                        the local values of f and its derivatives.  The
c                        coefficients of fmn0 are found from the grid point
c                        values of f,fxx,fyy, & fxxyy by subroutine `sint', to
c                        which they are passed from the main program by common 
c                        blocks.

c     (u,v)            Coordinates extending along the minor and major axes of
c                        the ellipsoid of best fit to the function f 
c     laxis,saxis      Orientations of the minor & major (u,v) axes of f
c     fu,fv,fuu,fvv    Partial derivatives of f w.r.t. u,v.&c.  fuu and fvv
c                        are the min. and max. 2nd derivatives, respectively.
c                        Rotation renders fuv = 0.
c     fcva             The average 2nd derivative f = (fxx + fyy)/2
c     fcvd             Amplitude of the 2nd derivative of f with orientation.
c                        fuu = cvav - cvdf, fvv = cvav + cvdf.
c     fcvax,feccx,fuux Derivatives of fcva,fecc,fuu &c. w.r.t. x
c     fcvay,feccy,fuuy Derivatives of fcva,fecc,fuu &c. w.r.t. y
c     fggg             Third derivative of f in u direction

c     (du,dv)(dx,dy)   Displacement of the centre of the ellipsoid of best fit
c                        from the point (x,y) in (x,y) and (u,v) coordinates
c     dr,iphi          Displacement radius and orientation

c     drmx             Maximum distance of movement in one iteration.  If a
c                        larger displacement be predicted, the vector will be
c                        scaled down to have this magnitude.
c     sdrmx            Maximum total distance of new position from starting
c                        position
c     flat(-pos,-neg)  Minimum (+ or -) curvature (2nd derivative value) 
c                        considered to be distinct from a flat surface
c     exgdmx           Maximum distance in the + or - x or y direction outside a
c                        grid square (in grid units) before a new fit be needed
c     itmx             The maximum number of new grid squares to be traversed.

c-------------------------------------------------------------------------------
c     Declarations and limiting values
c-------------------------------------------------------------------------------

      parameter (ni=61,nj=61)
      parameter (rad=57.29578)
      parameter (flatpos=1.E-7,flatneg=-flatpos)

      parameter (ioscmx=10)

      integer err1,err2,err
      logical slow,limit,diag,ltlast

#include "cyc6.h"    

      sdrmxsq = sdrmx*sdrmx
      xrpos = 1. + exgdmx
      xrneg =     -exgdmx
      yrpos = 1. + exgdmx
      yrneg =     -exgdmx

      iRm1 = iR - 1
      jUm1 = jU - 1
      xlo = float(iL) - exgdmx
      xhi = float(iR) + exgdmx
      ylo = float(jD) - exgdmx
      yhi = float(jU) + exgdmx

c-------------------------------------------------------------------------------
c     Iteration
c-------------------------------------------------------------------------------

      x  = xdep
      y  = ydep

      if (diag) write (6,10) xdep,ydep,
     *  'iter','err','f','fx','fy','fxx','fyy','fxy',
     * 'fxxx','fxxy','fxyy','fyyy','fcva','fgg','fcc','fggg',
     * 'gax','swv','g','gx','gy','gxx','gyy','gxy',
     * 'gcva','guu','gvv','gu','gv','lax','drc','du','dv',
     * 'dx','dy','dr','iphi','x','y','gnew'
 10   format (/' Approach to min.: xdep = ',f8.3,', ydep = ',f8.3//
     * a4,1x,a3,1x,a8,13a6,1x,a4,1x,5a6/
     * 11x,7a6,1x,a4,a6,1x,'(',a6,',',a6,') (',a6,',',a6,
     * ') (',a5,',',a5,') (',a5,',',a5,') ',a7/)

      iosc  = 0
      iter3 = 0
      !ltlast = .false.
      slow = .false.
      drlt = drmx

      dxlast = 0.
      dylast = 0.

      do 380 iter1 = 1,itmx

c         The counter, iter1, is incremented when the search extends outside 
c       the current grid square, necessitating the evaluation of the 
c       derivatives, fmn0(m,n), at a new (lower left) grid point, (ifit,jfit) 
c       from the spline coefficients, fxx,fyy,fxxyy, by subroutine sdiv.

        if (iter1.gt.3) then
          xrpos = 1. + exgdmx*4.
          xrneg =     -exgdmx*4.
          yrpos = 1. + exgdmx*4.
          yrneg =     -exgdmx*4.
        endif

c       Initialise coordinates for fitting
c       ----------------------------------

        ifit = x
        jfit = y
        xfit = ifit
        yfit = jfit

        if (ifit.lt.iL)   ifit = iL
        if (ifit.gt.iRm1) ifit = iRm1
        if (jfit.lt.jD)   jfit = jD
        if (jfit.gt.jUm1) jfit = jUm1

c       Compute function f and its derivatives at (ifit,jfit)
c       -----------------------------------------------------

        call sdiv(ifit,jfit,ie)

c       Return if a corner have a missing value
c       ---------------------------------------

        if (ie.ne.0) then
          err = 50
          return
        endif

c       Carry out the iteration with the grid square (ifit,jfit)
c       --------------------------------------------------------

        do 360 iter2 = 1,41

c           The counter, iter2, is the number of iterations performed within 
c         the current grid square; iter3 is the total number of iterations 
c         in all squares.

          iter3 = iter3 + 1

c         Compute the function and its derivatives at (x,y)
c         -------------------------------------------------

c           If (x,y) be on a grid point (as may be the case at the start of 
c         an iteration, the required derivatives are already available in 
c         the array, fmn0(m,n); otherwise they are evaluated using bicubic 
c         interpolation by the subroutine sint.

          xr = x - xfit
          yr = y - yfit
          xrold = xr
          yrold = yr

          if ((xr.eq.0.).and.(yr.eq.0.)) then
            f    = fmn0(0,0)
            fx   = fmn0(1,0)
            fy   = fmn0(0,1)
            fxx  = fmn0(2,0)
            fxy  = fmn0(1,1)
            fyy  = fmn0(0,2)
            fxxx = fmn0(3,0)
            fxxy = fmn0(2,1)
            fxyy = fmn0(1,2)
            fyyy = fmn0(0,3)
          else
            call sint(xr,yr,0,0,f)
            call sint(xr,yr,1,0,fx)
            call sint(xr,yr,0,1,fy)
            call sint(xr,yr,2,0,fxx)
            call sint(xr,yr,1,1,fxy)
            call sint(xr,yr,0,2,fyy)
            call sint(xr,yr,3,0,fxxx)
            call sint(xr,yr,2,1,fxxy)
            call sint(xr,yr,1,2,fxyy)
            call sint(xr,yr,0,3,fyyy)
          endif

          fcva = (fxx + fyy)/2.
          g = sqrt(fx*fx + fy*fy)

c         Initialise the error flags
c         --------------------------

          err1 = 0
          err2 = 0

c         Compute the orientations of the long gradient (gaxis) and 
c         cross gradient (caxis) axes
c         ---------------------------------------------------------

          if (g.lt.flatpos) then
            icaxis = 90
            caxis = 90./rad
            igaxis = 0
            ccax = 0.
            scax = 1.
            err = 90
            return
          else
            caxis = atan2(fx,-fy)
            icaxis= caxis*rad
            igaxis = icaxis - 90
            if (igaxis.lt.0.) igaxis = igaxis + 360
            ccax = cos(caxis)
            scax = sin(caxis)
          endif

c         Compute the 2nd and 3rd derivatives along the long
c         gradient (g) and cross gradient (c) axes
c         --------------------------------------------------

          fgg = scax*scax*fxx - 2.*scax*ccax*fxy + ccax*ccax*fyy
          fcc = ccax*ccax*fxx + 2.*scax*ccax*fxy + scax*scax*fyy
          fggg = scax*scax*scax*fxxx+3.*scax*ccax*(-scax*fxxy+ccax*fxyy)
     *          -ccax*ccax*ccax*fyyy
          swv = fggg/g

c         Calculate the derivatives of the gradient
c         -----------------------------------------

          rg  = 1./g
          gx  = (fx*fxx + fy*fxy)*rg
          gy  = (fx*fxy + fy*fyy)*rg
          gxx = (fxx*fxx + fx*fxxx + fxy*fxy + fy*fxxy - gx*gx)*rg
          gxy = (fxy*(fxx + fyy)   + fx*fxxy + fy*fxyy - gx*gy)*rg
          gyy = (fxy*fxy + fx*fxyy + fyy*fyy + fy*fyyy - gy*gy)*rg

c           Using the local derivatives, gxx,gyy, and gxy, the second 
c         derivatives (guu & gvv) along the minor (u) and major (v) axes of 
c         the ellipsoidal surface of best fit.

          gecc = (gxx - gyy)/2.
          gcva = (gxx + gyy)/2.
          gcvd = sqrt(gecc*gecc + gxy*gxy)
          guu  = gcva - gcvd
          gvv  = gcva + gcvd

          aguu = abs(guu)
          agvv = abs(gvv)

c         Compute the orientations of and first derivatives along the 
c         principal axes
c         -----------------------------------------------------------

c           The orientation of the u and v axes (laxis & saxis) are now
c         into (u,v) coordinates, as (gu,gv).

          if (gcvd.lt.flatpos) then
            isaxis = 90
            saxis = 90./rad
            laxis = 0
            csax = 0.
            ssax = 1.
            gu = gx
            gv = gy
          else
            saxis = atan2(gxy,gecc)/2.
            isaxis= saxis*rad
            laxis = isaxis - 90
            if (laxis.le.-360) laxis = laxis + 360
            if (laxis.gt.360)  laxis = laxis - 360
            csax = cos(saxis)
            ssax = sin(saxis)
            gu = gx*ssax - gy*csax
            gv = gx*csax + gy*ssax
          endif

          agu = abs(gu)
          agv = abs(gv)

c         Calculate the displacement (du,dv) towards the minimum
c         ------------------------------------------------------

c           The displacement in each of the axial directions calculated by 
c         the Newton-Raphson algorithm, as follows:
c               dv = -gv/gvv
c               du = -guu/guuu
c         The first equation estimates the displacement in the direction of
c         greatest positive curvature to the (major) axis of the trough.
c         The second equation estimates the displacement along the trough to 
c         the point of inflection.

c           The axial displacements are transformed into (x,y) coordinates.  
c         If the magnitude of the displacement exceed the appropriate limit 
c         (drlt or dumx) for the time being in force, the displacement vector 
c         is reduced to this value.

          if (agvv.lt.flatpos) then
            dv = 0.
          else if (agv.lt.flatpos) then
            dv = 0.
          else if (gvv.lt.flatneg) then
            if (gv.gt.flatpos) dv = -drlt
            if (gv.lt.flatneg) dv =  drlt
          else
            dv = -gv/gvv
          endif

          if (aguu.lt.flatpos) then
            du = 0.
          else if (agu.lt.flatpos) then
            du = 0.
          else if (guu.lt.flatneg) then
            if (gu.gt.flatpos) du = -drlt
            if (gu.lt.flatneg) du =  drlt
          else
            du = -gu/guu
          endif

c         Transform the displacement into (x,y) coordinates
c         -------------------------------------------------

          dr =  sqrt(du*du + dv*dv)
          drcalc = dr

          if (dr.lt.flatpos) then
            du = 0.
            dv = 0.
            dx = 0.
            dy = 0.
            iphi = 0.
            go to 300
          else if (gcvd.lt.flatpos) then
            dx = du  
            dy = dv
            iphi = 0.
          else
            dx =  du*ssax + dv*csax
            dy = -du*csax + dv*ssax
            iphi = atan2(dy,dx)*rad
          endif

c         Limit displacements
c         -------------------

c         (a) Oscillations
c             ------------
              
c           If the displacement be in (roughly) the opposite direction to 
c         the previous displacement, the magnitude of the displacement is 
c         revised to arrive at a point about which the oscillation is 
c         assumed to be amplifying or decaying exponentially.

          limit = .false.
          if (iter2.gt.1 .and. iter3.ge.2) then
            idphi = abs(iphi - iphilast)
            if (idphi.gt.180) idphi = 360 - idphi
            if (idphi.gt.145) then
              limit = .true.
              dr = dr*drlast/(drlast+dr)
              err2 = 9
              iosc = iosc + 1
              drlt = drlt*0.5
            endif
          endif

c         (b) Large displacements
c             -------------------

          if (dr.gt.drlt) then
            limit = .true.
            dr = drlt
          endif

c         (c) Final approach to minimum
c             -------------------------

c           If displacements have fallen below 0.01 grid spaces,
c         subsequent displacements will be limited to this amount.

          if (iter1.ge.3 .and. iter2.gt.1 .and. drlast.lt.0.01 .and.
     *     dr.gt.0.01 .and. (.not.ltlast)) then
            limit = .true.
            dr = amin1(dr,0.01)
            slow = .true.
          endif

c         (d) Scale du,dv,dx,dy to dr
c             -----------------------

          if (limit) then
            ratio = dr/drcalc
            du = du*ratio
            dv = dv*ratio
            dx = dx*ratio
            dy = dy*ratio
          endif

  300     continue

c         Add displacement to find new estimate of minimum position
c         ---------------------------------------------------------

c           The displacement at this iteration is added to the sum of the 
c         previous displacements (xr,yr) and the new position (x,y) is found.
c         New values of the function (fnew) and its gradient (gnew) are 
c         interpolated.  gnew should be lower than the previous value (g).
c         The total displacement from the beginning of the iteration 
c         is also found (sumdx,sumdy): its magnitude should not exceed sdrmx.

          xr = xrold + dx
          yr = yrold + dy

          xnew = xfit + xr
          ynew = yfit + yr

          sumdx = xnew - xdep
          sumdy = ynew - ydep
          sumdrsq = sumdx*sumdx + sumdy*sumdy

c         Calculate function and gradient at new position
c         -----------------------------------------------

          call sint(xr,yr,0,0,fnew)
          call sint(xr,yr,1,0,fxnew)
          call sint(xr,yr,0,1,fynew)
          gnew = sqrt(fxnew*fxnew + fynew*fynew)
          gcrit = g + abs(g*1.E-3)

c         Error conditions
c         ----------------

c           The following err2 conditions indicate a curvature of the
c         functional surface which does not imply a minimum: an error
c         condition of this type is not immediately fatal.  The physio-
c         graphic analogues imply the search for a minimum.  The negatives
c         of the function values must be supplied by the calling programme
c         if a maximum be sought.  The conditions diagnosed are:

c         1  A col (a concave-convex surface) in g
c         2  A dome (a concave surface) in g
c         3  A level valley in g (a surface concave in one direction 
c              and level in the perpendicalar direction)
c         4  A level surface in g
c         5  An incorrect inflexion in f

cc        1  A cirque (a maximum gradient inflection in a trough)
cc        2  A shute (a straight sloping concave surface)
cc        3  An arete (a maximum gradient inflection on a ridge)
cc        4  A buttress (a straight sloping convex surface)
cc        5  A hump

c         7  Function value increased at iteration
c         9  Revision of displacement needed for amplifying oscillation
          
          if (fcc.lt.flatpos) then
            err2 = 5
            if (err2lst.ne.5 .and. iopmxc.ge.2) then
              xprv1   = xlast
              yprv1   = ylast
              fprv1   = flast
              gprv1   = glast
              fcvprv1 = fcvlast
              fccprv1 = fcclast
              fgggprv1= fggglast
              xprv2   = x
              yprv2   = y
              fprv2   = f
              gprv2   = g
              fcvprv2 = fcva
              fccprv2 = fcc
              fgggprv2= fggg
            endif
          endif
          err2lst = err2

          if (swv.lt.flatpos) err2 = 6
          if (gnew.gt.gcrit)  err2 = 7
          if (guu.lt.flatpos) then
            if ((gvv.ge.flatpos).and.(guu.le.flatneg))   err2 = 1
            if ((gvv.le.flatneg).and.(guu.le.flatneg))   err2 = 2
            if ((gvv.ge.flatpos).and.(aguu.lt.flatpos))  err2 = 3
            if ((agvv.lt.flatpos).and.(aguu.lt.flatpos)) err2 = 4
          endif
c         if ((swv.lt.flatpos).or.(fvv.lt.flatpos)) then
c           if ((fvv.ge.flatpos).and.(swv.le.flatneg))  err2 = 1
c           if ((fvv.ge.flatpos).and.(aswv.lt.flatpos)) err2 = 2
c           if ((fvv.lt.flatpos).and.(swv.le.flatneg))  err2 = 3
c           if ((fvv.lt.flatpos).and.(aswv.lt.flatpos)) err2 = 4
c           if ((fvv.lt.flatpos).and.(swv.ge.flatneg))  err2 = 5
c         endif

c           The following err1 conditions necessitate a fit to a new grid
c         square (err1 = 10) or discontinuation of the search (err1 > 10)

c         10  New position outside grid square (exit loop 360)
c         20  A horizontal trough or a plane surface (to 2nd order in dr),
c               upon which a local minimum cannot be sought
c         30  Too slow a convergence of displacements 
c         40  Distance from starting position greater than allowed
c         50  New position outside allowed area of data array
c               or in square with missing value
c         60  Two successive increases in g or continued oscillation of 
c               positions
c         70  Error (err2=1, or iter3=1.and.err=5)
c         80  LaPlacian of f <= 0
c         90  No clear trough axis (gcvd = 0, so guu = gvv)

          if ((xr.lt.xrneg).or.(xr.gt.xrpos).or.(yr.lt.yrneg).or.
     *     (yr.gt.yrpos)) err1 = 10
          if (((aguu.lt.flatpos).or.(agvv.lt.flatpos)).and.
     *     ((gnew.lt.flatpos).or.(drcalc.lt.0.001))) err1 = 20
C         if (((iter2.gt.20).and.(drcalc.gt.hdrlast).and.(.not.slow)
C    *          .and.(err2.ne.9))
C    *     .or.((iter3.gt.20).and.(drcalc.ge.drlast).and.(.not.slow)
C    *          .and.(err2.ne.9).and.(iter2.gt.1))
C    *     .or.(iter3.gt.30 .and. (.not.slow))
          if (((iter2.gt.20).and.(drcalc.gt.drlast).and.
     *     (drlast.lt.drltlast).and.(.not.slow).and.(err2.ne.9))
     *     .or.(iter3.gt.40)) err1 = 30
          if (sumdrsq.gt.sdrmxsq) err1 = 40
          if ((xnew.lt.xlo).or.(xnew.gt.xhi).or.
     *        (ynew.lt.ylo).or.(ynew.gt.yhi)) err1 = 50
          if ((iter1.ge.2).and.(iter2.gt.2).and.(gnew.gt.1.2*g).and.
     *     (g.gt.1.2*glast).and.(drcalc.gt.0.1)) err1 = 60
c         if (err2.eq.1) err1 = 70
c         if (err2.eq.5 .and. iter3.eq.1) err1 = 70
          if (fcva.lt.0 .and. err1.gt.0 .and. err1.lt.7) err1 = 80
          if (iosc.ge.ioscmx) err1 = 90

          err = err1 + err2

c         Summary of variables in iteration
c         ---------------------------------

          if (diag) then
            write (6,310) iter1,iter2,err,f,fx,fy,fxx,fyy,fxy,fxxx,
     *       fxxy,fxyy,fyyy,fcva,fgg,fcc,fggg,igaxis,swv,g,gx,
     *       gy,gxx,gyy,gxy,gcva,guu,gvv,gu,gv,laxis,drcalc,du,dv,
     *       dx,dy,dr,iphi,xnew,ynew,gnew
 310        format (2i2,2x,i2,1x,f8.2,13f6.2,1x,i4,1x,5f6.2/11x,7f6.2,
     *       1x,i4,f6.2,1x,'(',f6.3,',',f6.3,') (',f6.3,',',f6.3,
     *       ') (',f5.3,',',i5,') (',f5.1,',',f5.1,') ',f7.3/)
          endif

c         Return control
c         --------------

          dxlast = dx
          dylast = dy
          dulast = du
          dvlast = dv
          iphilast = iphi

          if (err.ge.20) return
          if (drcalc.lt.0.001) then
            if (err.ne.5 .or. iopmxc.lt.2) return
            rfccdif = 1./(fccprv1 - fccprv2)
            xnew = (xprv2*fccprv1   - xprv1*fccprv2)  *rfccdif
            ynew = (yprv2*fccprv1   - yprv1*fccprv2)  *rfccdif
            fnew = (fprv2*fccprv1   - fprv1*fccprv2)  *rfccdif
            gnew = (gprv2*fccprv1   - gprv1*fccprv2)  *rfccdif
            fcva = (fcvprv2*fccprv1 - fcvprv1*fccprv2)*rfccdif
            fggg = (fgggprv2*fccprv1- fgggprv1*fccprv2)*rfccdif
            laxis = 0
            if (diag) then
              write (6,'(/10a8)') 'x','y','f','g','fcv','fggg','fcc'
              write (6,'(10f8.3)') xprv1,yprv1,fprv1,gprv1,fcvprv1,
     *         fgggprv1,fccprv1
              write (6,'(10f8.3)') xprv2,yprv2,fprv2,gprv2,fcvprv2,
     *         fgggprv2,fccprv2
              write (6,'(10f8.3)') xnew,ynew,fnew,gnew,fcva,fggg
            endif
            return
          endif

          glast   = g
          if (iopmxc.ge.2) then
            xlast   = x
            ylast   = y
            flast   = f
            fcvlast = fcva
            fggglast = fggg
            fcclast = fcc
          endif
          x = xnew
          y = ynew
          f = fnew
          g = gnew

          drlast  = drcalc
          drltlast = drlt
          ltlast  = limit
          hdrlast = drlast/2.
 
          if (err.ge.10) go to 370

 360    continue

c         An error condition results if there have been too many 
c       iterations in the one grid square.

        err1 = 30
        err  = err1 + err2
        return

 370    continue
 380  continue

      return
      end
