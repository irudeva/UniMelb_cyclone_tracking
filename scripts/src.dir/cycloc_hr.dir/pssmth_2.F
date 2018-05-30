      Subroutine pssmth(ndiff,ak0,clatmx,a,b,akx,aky,idiag)

c       This subroutine smoothes a regular polar stereographic array
c     used by programme cycloc by applying ndiff diffusive passes.
c     The calling programme calculates the number of passes and the
c     diffusive coefficient required to ensure stability and a 
c     reasonable degree of accuracy.  The coefficient supplied to 
c     the programme is the coefficient in grid units that applies to
c     the projection pole (ak0); this is scaled by a numerical factor 
c     to allow for the change of grid size across the projection.

      parameter (rad=57.2957,rrad=1./rad,r2rad=1./(2.*rad))
      dimension a(ni,nj),b(ni,nj),akx(ni,nj),aky(ni,nj)
#include "cyc3.h"

c     Compute diffusive coefficients 
c     ------------------------------

c     ak = K/dx**2 = ak0/dx0**2 * (dx0/dx)**2

      rprojsq = rproj*rproj
      rsmx = rproj*tan(r2rad*clatmx)
      rssqmx = rsmx*rsmx

c     (a) Diffusive coefficients in x-direction at (i+1/2,j)
c         --------------------------------------------------

      do 120 j = 1,nj
        ys = float(j)-ycen
        yssq = ys*ys
        do 110 i = 1,ni-1
          xs = float(i)+0.5-xcen
          rssq = xs*xs + yssq
          if (rssq.gt.rssqmx .or. a(i,j).eq.spval 
     *     .or. a(i+1,j).eq.spval) then
            akx(i,j) = 0.
          else
            akx(i,j) = ak0 * ((rprojsq + rssq)/rprojsq)**2
          endif
  110   continue
        akx(ni,j) = 0.
  120 continue

c     (b) Diffusive coefficients in y-direction at (i,j+1/2)
c         --------------------------------------------------

      do 140 j = 1,nj-1
        ys = float(j)+0.5-ycen
        yssq = ys*ys
        do 130 i = 1,ni
          xs = float(i)-xcen
          rssq = xs*xs + yssq
          if (rssq.gt.rssqmx .or. a(i,j).eq.spval 
     *     .or. a(i,j+1).eq.spval) then
            aky(i,j) = 0.
          else
            aky(i,j) = ak0 * ((rprojsq + rssq)/rprojsq)**2
          endif
  130   continue
  140 continue

      do 150 i = 1,ni
        aky(i,nj) = 0.
  150 continue

c     Perform ndiff diffusive passes
c     ------------------------------
        
      do 400 idiffus = 1,ndiff
        do 200 j = 2,nj-1
          do 180 i = 2,ni-1
            b(i,j) =  (a(i+1,j)-a(i,j))*akx(i  ,j)
     *               +(a(i-1,j)-a(i,j))*akx(i-1,j)
     *               +(a(i,j+1)-a(i,j))*aky(i,j  )
     *               +(a(i,j-1)-a(i,j))*aky(i,j-1)
  180     continue
  200   continue
        
        do 300 j = 2,nj-1
          do 280 i = 2,ni-1
            a(i,j) = a(i,j) + b(i,j)
  280     continue
  300   continue
  400 continue

      if (idiag.ge.2) then
        imax = 0
        jmax = 0
        bmax = 0.
        do 500 j = 2,nj-1
          do 480 i = 2,ni-1
            if (abs(b(i,j)).gt.bmax) then
              bmax = abs(b(i,j))
              imax = i
              jmax = j
            endif
  480     continue
  500   continue
        xs = float(imax)-xcen
        ys = float(jmax)-ycen
        rssq = xs*xs + ys*ys
        alon = -fhem*atan2(xs,ys)*rad
        alat =  fhem*(90-2.*rad*atan(sqrt(rssq/rprojsq)))
        write (6,*) ' bmax = ',bmax,imax,jmax,alon,alat
        write (6,*) a(imax,jmax),a(imax+1,jmax),
     *   a(imax-1,jmax),a(imax,jmax+1),a(imax,jmax-1)
      endif

      return
      end
