      Subroutine intpgr(ni,nj,xcen,ycen,rproj,fhem,f,fxx,fyy,fxxyy,
     * intopt,nlon,nlat,lon,lat,fhk,klo,khi,spval,ierr)

      parameter (rad=57.295779,rrad=1./rad,r2rad=1./(2.*rad))
      parameter (nhlt=1466) !ckk 362
      
      integer h,hlo,hhi
      real lambda,lon(nlon),lat(nlat)
      dimension sini(nhlt),cosi(nhlt)
      dimension f(ni,nj),fxx(ni,nj),fyy(ni,nj),fxxyy(ni,nj)
      dimension fhk(nlon,nlat)

      ierr = 0
      if (ni.gt.nhlt) then
        write (6,*) ' intpgr: ni (',ni,') > nhlt (',nhlt,').'
        stop
      endif

      do 40 h = 1,nlon
        lambda  = lon(h)/rad
        sini(h) =       sin(lambda)
        cosi(h) = -fhem*cos(lambda)
   40 continue

      scalefact = 2.*rad*rproj
      do 80 k = klo,khi
        r = rproj*tan(r2rad*(90.-fhem*lat(k)))
        do 60 h = 1,nlon
          x = sini(h)*r + xcen
          y = cosi(h)*r + ycen

          if ((x.gt.float(ni)).or.(x.lt.1.).or.(y.gt.float(nj)).or.
     *     (y.lt.1.)) then
            fhk(h,k) = spval
            ierr     = 1
            go to  50
          endif

          ilo = x
          jlo = y 
          ihi = ilo + 1
          jhi = jlo + 1
          A2 = x - float(ilo)
          A1 = 1. - A2
          B2 = y - float(jlo)
          B1 = 1. - B2

          if (f(ilo,jlo).eq.spval) go to 50
          if ((A2.ne.0.).and.(f(ihi,jlo).eq.spval)) go to 50
          if ((B2.ne.0.).and.(f(ilo,jhi).eq.spval)) go to 50
          if ((A2.ne.0.).and.(B2.ne.0.).and.(f(ihi,jhi).eq.spval)) 
     *     go to 50

          if (intopt.eq.1) then
            fhk(h,k) = (f(ilo,jlo)*A1 + f(ihi,jlo)*A2)*B1 +
     *                 (f(ilo,jhi)*A1 + f(ihi,jhi)*A2)*B2
          else
            if (A2.eq.0.) then
              if (B2.eq.0.) then
                fhk(h,k) = f(ilo,jlo)
              else
                B3 = B1*(B1*B1 - 1.)/6.
                B4 = B2*(B2*B2 - 1.)/6.
                fhk(h,k) = B1*f(ilo,jlo)        + B2*f(ilo,jhi)   +
     *                     B3*fyy(ilo,jlo)      + B4*fyy(ilo,jhi)
              endif
            else 
              A3 = A1*(A1*A1 - 1.)/6.
              A4 = A2*(A2*A2 - 1.)/6.
              if (B2.eq.0.) then
                fhk(h,k) = A1*f(ilo,jlo)        + A2*f(ihi,jlo)   +
     *                     A3*fxx(ilo,jlo)      + A4*fxx(ihi,jlo)
              else
                B3 = B1*(B1*B1 - 1.)/6.
                B4 = B2*(B2*B2 - 1.)/6.
                fhk(h,k) = B1*(A1*f(ilo,jlo)    + A2*f(ihi,jlo)   +
     *                         A3*fxx(ilo,jlo)  + A4*fxx(ihi,jlo))    +
     *                     B2*(A1*f(ilo,jhi)    + A2*f(ihi,jhi)   +
     *                         A3*fxx(ilo,jhi)  + A4*fxx(ihi,jhi))    +
     *                     B3*(A1*fyy(ilo,jlo)  + A2*fyy(ihi,jlo) +
     *                         A3*fxxyy(ilo,jlo)+ A4*fxxyy(ihi,jlo))  +
     *                     B4*(A1*fyy(ilo,jhi)  + A2*fyy(ihi,jhi) +
     *                         A3*fxxyy(ilo,jhi)+ A4*fxxyy(ihi,jhi)) 
              endif
            endif
          endif
   50     continue
   60   continue
   80 continue

      return
      end
