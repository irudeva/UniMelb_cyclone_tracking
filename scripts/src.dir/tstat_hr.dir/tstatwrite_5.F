      Subroutine statwrite(nstat,f,lons,lats,g)

#include "tstat1.h"

c     parameter (nfun=40)
c     parameter (rad=57.295779,rrad=1./rad,r2rad=1./(2.*rad))
c     parameter (nilt=81,njlt=81)
c     parameter (ni=81,nj=81)
c     parameter (icen=(ni+1)/2,jcen=(nj+1)/2)
c     parameter (xcen=icen,ycen=jcen)
c     parameter (rproj=30.)
c     parameter (spval=99999.9)

c     character shead*960,llhead*80
c     character fname(nfun)*22,funit(nfun)*15,fn(nfun)*6
c     character funitx*15,fnx*6
c     dimension finc(nfun)

c     common /blhead/shead,llhead
c     common /blfn/fname,funit,fn
c      common /blilts/llwrit,fhem,nlons,nlats,idiagt
c     common /blfinc/finc

      dimension f(nilt,njlt)
      real lons(nlons),lats(nlats)
      dimension g(nlons*nlats)


      flo =  1.e+10
      fhi = -1.e+10
      do 110 j = 1,nj
        do 100 i = 1,ni
          if (f(i,j).ne.spval) then
            if (f(i,j).lt.flo) flo = f(i,j)
            if (f(i,j).gt.fhi) fhi = f(i,j)
          endif
 100    continue
 110  continue

      if (llwrit) then
        call lintpgr(f,lons,lats,g)

        fnx = fn(nstat)
        funitx = funit(nstat)
        llhead(1:3)   = fnx(1:3)
        llhead(58:69) = funitx(1:13)
        write (6,'(a80)') llhead
 
        write (29) nlats
        write (29) lats
        write (29) nlons
        write (29) lons
        write (29) llhead
        write (29) g
      else
        write (29) shead,nstat,fname(nstat),fn(nstat),finc(nstat),
     *   funit(nstat),flo,fhi,1,ni,1,nj
        write (29) f
      endif

      write (6,500) nstat,fname(nstat),fn(nstat),flo,fhi,funit(nstat)
 500  format (4x,i2,': ',a22,', ',a3,', (',f8.3,' TO ',
     * f8.3,a15)

      if (idiagt.ge.1)
     * write (6,'(/7(7f10.4/))') ((f(i,j),i=28,34),j=34,28,-1)
      return
      end

c-----------------------------------------------------------------------

      Subroutine lintpgr(f,lons,lats,g)

#include "tstat1.h"

      dimension f(nilt,njlt)
      real lons(nlons),lats(nlats)
      dimension g(nlons,nlats)

      parameter (nhlt=1441) !ckk 31/10/2005
      integer h
      dimension sini(nhlt),cosi(nhlt)

      if (nlons.gt.nhlt) then
        write (6,*) ' intpgr: nlons (',nlons,') > nhlt (',nhlt,').'
        stop
      endif

      do 30 h = 1,nlons
        alam    = lons(h)/rad
        sini(h) =       sin(alam)
        cosi(h) = -fhem*cos(alam)
   30 continue

      scalefact = 2.*rad*rproj
      do 80 k = 1,nlats
        r = rproj*tan(r2rad*(90.-fhem*lats(k)))
        do 60 h = 1,nlons
          x = sini(h)*r + xcen
          y = cosi(h)*r + ycen

          if (x.gt.float(ni) .or. x.lt.float(1) .or.
     *        y.gt.float(nj) .or. y.lt.float(1)) go to 40

crmw      min0 -> min
          ilo = min(ni-1,x)
          jlo = min(nj-1,y)
          ihi = ilo + 1
          jhi = jlo + 1
          A2 = x - float(ilo)
          A1 = 1. - A2
          B2 = y - float(jlo)
          B1 = 1. - B2

          if (f(ilo,jlo).eq.spval) go to 40
          if ((A2.ne.0.).and.(f(ihi,jlo).eq.spval)) go to 40
          if ((B2.ne.0.).and.(f(ilo,jhi).eq.spval)) go to 40
          if ((A2.ne.0.).and.(B2.ne.0.).and.(f(ihi,jhi).eq.spval)) 
     *     go to 40

          g(h,k) = (f(ilo,jlo)*A1 + f(ihi,jlo)*A2)*B1 +
     *             (f(ilo,jhi)*A1 + f(ihi,jhi)*A2)*B2
          go to 50
   40     continue
          g(h,k) = spval
          ierr     = 1
   50     continue
   60   continue
   80 continue

      return
      end
