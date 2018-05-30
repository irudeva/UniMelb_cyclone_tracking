      Subroutine fmin(xdep,ydep,iL,iR,jD,jU,exgdmx,drmx,
     * sdrmx,itmx,x,y,fnew,fcva,fuu,laxis,err,diag,iconcv)

c       A subroutine which searches iteratively for a true minimum of
c     a bicubic function defined by the values of the function and its 
c     fifteen derivatives to third order in x and y on a grid of unit 
c     spacing by the method of conjugate positions.  

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
c     f,fx,fxx,fxy,&c. Function and partial derivatives of function w.r.t. 
c                        x,x**2,xy,&c. (from bicubic spline interpolation)
c                        at point (x,y)
c     fmn0(m,n)        f and its derivatives w.r.t x**m and y**n (m,n <= 3) at 
c                        (ifit,jfit) used in Taylor's expansion for finding
c                        the local values of f and its derivatives.  The
c                        coefficients of fmn0 are found from the grid point
c                        values of f,fxx,fyy, & fxxyy by subroutine `sint', to
c                        which they are passed by common blocks.
c     (u,v)            Coordinates extending along the minor and major axes of
c                        the ellipsoid of best fit to the function surface at
c                        (x,y) and having origin at its centre
c     laxis,isaxis     Orientations of the minor & major (u&v) axes w.r.t. (x,y)
c     fu,fv,fuu,fvv    Partial derivatives of f w.r.t. u,v.&c.  fuu and fvv
c                        are the min. and max. 2nd derivatives, respectively.
c                        Rotation renders fuv = 0.
c     fcva             The average 2nd derivative = fxx + fyy = 1/2 Laplacian f
c     fcvd             Amplitude of the 2nd derivative with orientation.
c                        fuu = fcva - fcvd, fvv = fcva + fcvd.
c     (du,dv),(dx,dy)  Displacement of the centre of the ellipsoid of best fit
c                        from the point (x,y) in (x,y) and (u,v) coordinates
c     dr,iphi          Displacement radius and orientation
c     drmx             Maximum distance of movement in one iteration.  If a
c                        larger displacement be predicted, the vector will be
c                        scaled down to have this magnitude but the same
c                        direction
c     sdrmx            Maximum total distance of new position from starting
c                        position
c     exgdmx           Maximum distance in the + or - x or y direction outside a
c                        grid square (in grid units) before a new fit be needed
c     itmx             The maximum number of new grid squares to be traversed.
c     iconcv           If >0 flags that the condition fuu > 0 must be rigidly
c                        fulfilled at the first step of the iteration.

c-------------------------------------------------------------------------------
c     Declarations values.
c-------------------------------------------------------------------------------

      parameter (rad=57.29578)
      parameter (flatpos=1.E-7,flatneg=-flatpos)

      integer err1,err2,err
      logical diag

#include "cyc6.h"  

c-------------------------------------------------------------------------------
c     Limiting values.
c-------------------------------------------------------------------------------

      sdrmxsq  =  sdrmx*sdrmx
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
c     Initialisation of coordinates, function, and its derivatives.
c-------------------------------------------------------------------------------

      x = xdep
      y = ydep

      iosc  = 0
      iter3 = 0

      do 380 iter1 = 1,itmx

c         The counter, iter1, is incremented when the search extends outside 
c       the current grid square, necessitating the evaluation of the 
c       derivatives, fmn0(m,n), at a new (lower left) grid point, (ifit,jfit) 
c       from the spline coefficients, fxx,fyy,fxxyy, by subroutine sdiv.

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

        dxlast = 0.
        dylast = 0.

c       Carry out the iteration with the grid square (ifit,jfit)
c       --------------------------------------------------------

        do 360 iter2 = 1,20

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

          if ((x.eq.xfit).and.(y.eq.yfit)) then
            xr = 0.
            yr = 0.

            fx   = fmn0(1,0)
            fy   = fmn0(0,1)
            fxx  = fmn0(2,0)
            fyy  = fmn0(0,2)
            fxy  = fmn0(1,1)
          else
            xr = x - xfit
            yr = y - yfit

            call sint(xr,yr,1,0,fx)
            call sint(xr,yr,0,1,fy)
            call sint(xr,yr,2,0,fxx)
            call sint(xr,yr,1,1,fxy)
            call sint(xr,yr,0,2,fyy)
          endif

          if (iter3.eq.1) then
            if ((x.eq.xfit).and.(y.eq.yfit)) then
              flast1 = fmn0(0,0)
            else
              call sint(xr,yr,0,0,flast1)
            endif
            if (diag) write (6,10) xdep,ydep,flast1
   10       format (/' Approach to min.:  xdep = ',f8.3,', ydep = ',
     *       f8.3,' fdep = ',f10.3//
     *       ' it err  fx    fy   fxx   fyy   fxy  fcva  fcvd   fuu',
     *       '   fvv   fu    fv  laxis   (dx,dy)        (dr,phi)',
     *       '       (x,y)          f'/)
          endif

c         Initialise the error flags
c         --------------------------

          err1 = 0
          err2 = 0

c         Compute the second derivatives along the principal axes
c         -------------------------------------------------------

c           Using the local derivatives, fxx,fyy, and fxy, the second 
c         derivatives (fuu & fvv) along the minor (u) and major (v) axes of 
c         the ellipsoidal surface of best fit.

          fcva = (fxx + fyy)/2.
          fecc = (fxx - fyy)/2.
          fcvd = sqrt(fecc*fecc + fxy*fxy)
          fuu  = fcva - fcvd
          fvv  = fcva + fcvd

          afxy  = abs(fxy)
          afuu  = abs(fuu)
          afvv  = abs(fvv)

c         Compute the orientations of and first derivatives along the 
c         principal axes
c         -----------------------------------------------------------

c           The orientations of the principal axes (laxis & isaxis) are now
c         found.  If fuu=fvv, the contours are circular and the orientation 
c         of the minor axis is arbitrarily designated 0.  From a knowledge 
c         of these orientations, the local gradient vector is transformed 
c         into (u,v) coordinates, as (fu,fv); its magnitude, fr, is also 
c         found.

          if (fcvd.lt.flatpos) then
            isaxis = 90
            saxis = 90./rad
            laxis = 0
            csax = 0.
            ssax = 1.
            fu = fx
            fv = fy
          else
            saxis = atan2(fxy,fecc)/2.
            isaxis= saxis*rad
            laxis = isaxis + 90
            if (laxis.eq.180) laxis = 0
            csax = cos(saxis)
            ssax = sin(saxis)
            fu = fx*ssax - fy*csax
            fv = fx*csax + fy*ssax
          endif

          afu = abs(fu)
          afv = abs(fv)

          fr = sqrt(fx*fx + fy*fy)

c         Calculate the displacement (du,dv) towards the minimum
c         ------------------------------------------------------

c           If the curvature of the surface be zero, then
c         (a) if the gradient be non-zero the displacement is taken to be
c             the maximum allowed (drmx) and in the direction of the 
c             gradient vector;
c         (b) if the gradient be zero a fatal error results.

          if (((afuu.lt.flatpos).and.(afvv.lt.flatpos)).
     *     and.(fr.ge.flatpos)) then
            dx = drmx*fx/fr
            dy = drmx*fy/fr
            go to 80
          endif
          if (((afuu.lt.flatpos).or.(afvv.lt.flatpos)).and.
     *     (fr.lt.flatpos)) then
            dx = 0.
            dy = 0.
            dr = 0.
            go to 110
          endif

c           In the non-trivial case, the displacement in each of the axial 
c         directions to the centre of the ellipsoid is the negative of the 
c         ratio of the first and second derivatives (the Newton-Raphson 
c         algorithm).  If fuu or fvv < 0, a downslope movement is still
c         allowed at this stage, by applying the algorithm in reverse.

          if (afuu.lt.flatpos) then
            du = 0.
          else
            du = -fu/afuu
          endif
          if (fuu.lt.flatneg) then
            if (du.gt.drmx)  du = drmx
            if (du.lt.-drmx) du = -drmx
          endif

          if (afvv.lt.flatpos) then
            dv = 0.
          else
            dv = -fv/afvv
          endif
          if (fvv.lt.flatneg) then
            if (dv.gt.drmx) dv = drmx
            if (dv.lt.-drmx) dv = -drmx
          endif

c         Amplifying oscillations
c         -----------------------

c           If the displacement be in (roughly) the opposite direction to 
c         and larger than the previous displacement, the magnitude of the 
c         displacement is revised to arrive at a point about which the 
c         oscillation is assumed to be amplifying exponentially.

          if (iter3.ge.2) then
            idsaxis = abs(isaxis - isaxislast)
            ialign = 0
            if (idsaxis.lt.30) ialign = 1
            if (idsaxis.gt.150) ialign = -1
            if (ialign.ne.0) then
              dufact = du/dulast*float(ialign)
              dvfact = dv/dvlast*float(ialign)
              if (dufact.lt.-1.) du = du*dulast/(dulast-du)
              if (dvfact.lt.-1.) dv = dv*dvlast/(dvlast-dv)
              if ((dufact.lt.-1.).or.(dvfact.lt.-1.)) then
                err2 = 9
                iosc = iosc + 1
              endif
            endif
          endif

c         Transform the displacement into (x,y) coordinates
c         -------------------------------------------------

          if (fcvd.lt.flatpos) then
            dx = du
            dy = dv
          else
            dx =  du*ssax + dv*csax
            dy = -du*csax + dv*ssax
          endif

 80       continue

c         Non-amplifying oscillations
c         ---------------------------

c           The following is a check for a non-amplifying oscillation, 
c         which may be discovered by comparing the magnitude of the 
c         displacement in this iteration with that covered in two (this 
c         and the last) iterations.  The adopted position is taken 
c         to be the mid-point of the oscillation.

          dr =  sqrt(dx*dx + dy*dy)
          if (dr.ge.1.E-10) then
            dx2 = dx + dxlast
            dy2 = dy + dylast
            dr2 = sqrt(dx2*dx2 + dy2*dy2)
            dr2fact = dr2/dr
            if (dr2fact.lt.0.1) then
              dx = dx*0.5
              dy = dy*0.5
              dr = dr*0.5
              err2 = 8
              iosc = iosc + 1
            endif
            iphi = atan2(dy,dx)*rad
          else
            iphi = 0.
          endif

c         Limit magnitude of displacement
c         -------------------------------

c           Large displacements are reduced in magnitude to a value
c         drmx, but are unchanged in direction.

          drcalc = dr
          if (dr.gt.drmx) then
            redfact = drmx/dr
            dx = dx*redfact
            dy = dy*redfact
            dr = drmx
          endif

 110      continue

c         Add displacement to find new estimate of minimum
c         ------------------------------------------------

c           The displacement at this iteration is added to the sum of the 
c         previous displacements (xr,yr) and the new position (x,y) is found.
c         A new value of the function (fnew) is interpolated.  fnew should 
c         be lower than the previous value (f).
c         The total displacement from the beginning of the iteration 
c         is also found (sumdx,sumdy): its magnitude should not exceed sdrmx.

          xr = xr + dx
          yr = yr + dy
  
          x = xfit + xr
          y = yfit + yr

          call sint(xr,yr,0,0,fnew)
          fcrit = flast1 + abs(flast1*1.E-6) + 0.0001

          sumdx = x - xdep
          sumdy = y - ydep
          sumdrsq = sumdx*sumdx + sumdy*sumdy

c         Error conditions
c         ----------------

c           The following err2 conditions indicate a curvature of the
c         functional surface which does not imply a minimum: an error
c         condition of this type is not immediately fatal.  The physio-
c         graphic analogues imply the search for a minimum.  The negatives
c         of the function values must be supplied by the calling programme
c         if a maximum be sought.  The conditions diagnosed are:

c         1  A col (a concave-convex surface)
c         2  A dome (a concave surface)
c         3  A level valley (a surface concave in one direction and level 
c              in the perpendicalar direction)
c         4  A level surface

c         7  Function value increased at iteration
c         8  Revision of displacement needed for non-amplifying oscillation
c         9  Revision of displacement needed for amplifying oscillation

          if (fnew.gt.fcrit) err2 = 7
          if (fuu.lt.flatpos) then
            if ((fvv.ge.flatpos).and.(fuu.le.flatneg))   err2 = 1
            if ((fvv.le.flatneg).and.(fuu.le.flatneg))   err2 = 2
            if ((fvv.ge.flatpos).and.(afuu.lt.flatpos))  err2 = 3
            if ((afvv.lt.flatpos).and.(afuu.lt.flatpos)) err2 = 4
          endif
 
c           The following err1 conditions necessitate a fit to a new grid
c         square (err1 = 10) or discontinuation of the search (err1 > 10)

c         10  New position outside grid square (exit loop 360)
c         20  A horizontal trough or a plane surface (to 2nd order in dr),
c               upon which a local minimum cannot be sought
c         30  Too slow a convergence of displacements 
c         40  Distance from starting position greater than allowed
c         50  New position outside allowed area of data array
c               or in square with missing value
c         60  Two successive increases in fr or continued oscillation of 
c               positions
c         70  Error at first iteration
c         80  LaPlacian of f <= 0
c         90  Iterative displacements increasing in magnitude

          if ((xr.lt.xrneg).or.(xr.gt.xrpos).or.(yr.lt.xrneg).or.
     *     (yr.gt.xrpos)) err1 = 10
          if (((afuu.lt.flatpos).or.(afvv.lt.flatpos)).and.
     *     ((frnew.lt.flatpos).or.(dr.lt.0.001))) err1 = 20
          if (((iter2.gt.10).and.(dr.gt.hdrlast)).or.
     *     ((iter3.gt.20).and.(dr.ge.drlast)).or.
     *     (iter3.gt.30)) err1 = 30
          if (sumdrsq.gt.sdrmxsq) err1 = 40
          if ((x.lt.xlo).or.(x.gt.xhi).or.(y.lt.ylo).or.(y.gt.yhi))
     *     err1 = 50
          if ((iter3.ge.2).and.(fnew.gt.flast1).and.
     *     (flast1.gt.flast2)) err1 = 60
          if (iosc.ge.3) err1 = 60
          if ((iter3.eq.1).and.(iconcv.ge.1).and.(err2.ne.0)) 
     *     err1 = 70
          if (fcva.lt.0.) err1 = 80
          if ((iter3.gt.1).and.(fuulast.lt.0.).and.(fuu.lt.fuulast))
     *     err1 = 90
c         if ((iter3.gt.1).and.(drcalc.gt.drcalclast)) err1 = 90

          err = err1 + err2

c         Summary of variables in iteration
c         ---------------------------------

          if (diag) then
            if (err.eq.0) then
              write (6,300) iter2,fx,fy,fxx,fyy,fxy,fcva,
     *         fcvd,fuu,fvv,fu,fv,laxis,dx,dy,dr,iphi,x,y,fnew
 300          format (i2,3x,11f6.2,1x,i3,1x,'(',f6.3,',',f6.3,') (',
     *         f5.3,',',i4,') (',f6.2,',',f6.2,') ',f9.4)
            else
              write (6,310) iter2,err,fx,fy,fxx,fyy,fxy,fcva,
     *         fcvd,fuu,fvv,fu,fv,laxis,dx,dy,dr,iphi,x,y,fnew
 310          format (i2,1x,i2,11f6.2,1x,i3,1x,'(',f6.3,',',f6.3,') (',
     *         f5.3,',',i4,') (',f6.2,',',f6.2,') ',f9.4)
            endif
          endif

c         Return control
c         --------------

          dxlast = dx
          dylast = dy
          dulast = du
          dvlast = dv
          isaxislast = isaxis

          if (err.ge.20) return
          if (dr.lt.0.001) return
          if (err.ge.10) go to 370

          flast2  = flast1
          flast1  = fnew
          fuulast = fuu
          drcalclast = drcalc
          drlast  = dr
          hdrlast = drlast/2.

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
