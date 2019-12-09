      Program fderiv

c       This programme uses bicubic spline interpolation to calculate the
c     derivatives of a function; these may be:

c     deriv = 'g'  -  E and N directed gradients of the function
c     deriv = 'd'  -  the Laplacian of the function

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

c     nillt,njllt  = dimensions of array for storing f data
c     lon,lat      = dimensions of f data
c     margin       = margin for wrapping data for spline fitting
c     iunit        = logical unit for f input
c     dax,hrx      = date and time read
c     da ,hr       = date and time required
c     idiag        = diagnostic write level
c     i,i+1      = / i and j coordinates of lat.-lon. grid
c     j,j+1      = \ surrounding latc,lonc
c     lonlo,lonhi  = / longitudes and latitudes surrounding
c     latlo,lathi  = \ latc,lonc
c     A1,..,B4     = bicubic spline interpolating coefficients
c     fx,fy        = gradients 

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------
      
      character quantx*8,levelx*9,lunitx*10,sourcex*10,unitx*12,
     * dmodex*6,llgrid*17
      character*80 file1,file2,file3,file4,head1,head2,head3,head4

      parameter (r6=1./6.,r180=1./180.)
      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      logical grad,cext,divg,vort,dpur,vect,sqar
      character*80 optarg

      character*10 aa,bb,lnunit
      integer da,hr,dax,hrx,optind

      parameter (nlonlt=361,nlatlt=181,marglt=10)
      parameter (nlon2lt=nlonlt+2*marglt,nlat2lt=nlatlt+2*marglt)
      real lon,lat,lon2,lat2
      common /blltln/lon(nlonlt),lat(nlatlt),lon2(nlon2lt),lat2(nlon2lt)

c     parameter (nlllt=10585,nll2lt=14329)     ! 10585 = 145*73, 14329 = 161*89
c     parameter (nlllt=16471,nll2lt=22311)     ! 16471 = 181*91, 22311 = 201*111
      parameter (nlllt=65341,nll2lt=77081)     ! 65341 = 361*181,77081 = 381*201

      common /blfdat/fhk(nlllt)
      common /blgdat/ghk(nlllt)

      common /blfxy1/f(nll2lt)
      common /blfxy2/fxx(nll2lt)
      common /blfxy3/fyy(nll2lt)
      common /blfxy4/fxxyy(nll2lt)
      common /blfxy5/fx(nll2lt)
      common /blfxy6/fy(nll2lt)

      common /blgxy1/g(nll2lt)
      common /blgxy2/gxx(nll2lt)
      common /blgxy3/gyy(nll2lt)
      common /blgxy4/gxxyy(nll2lt)
      common /blgxy5/gx(nll2lt)
      common /blgxy6/gy(nll2lt)

c-------------------------------------------------------------------------------
c     Read command line arguments
c-------------------------------------------------------------------------------

      grad = .false.
      sqar = .false.
      cext = .false.
      divg = .false.
      vort = .false.
      dpur = .false.
      lnunit = 'DL'
      idiag = 0
      da = 0
      hr = 0
      nmap = 0
      sign = -1.
   10 continue
      nopt = ngtopt("gDVPScme:F:d:h:LH",optind,optarg)
        if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'g') grad = .true.
        if (char(nopt).eq.'S') sqar = .true.
        if (char(nopt).eq.'C') cext = .true.
        if (char(nopt).eq.'L') cext = .true.
        if (char(nopt).eq.'H') then
          cext = .true.
          sign = +1.
        endif
        if (char(nopt).eq.'D') divg = .true.
        if (char(nopt).eq.'V') vort = .true.
        if (char(nopt).eq.'P') dpur = .true.
        if (char(nopt).eq.'m') lnunit = 'M'
        if (char(nopt).eq.'c') lnunit = 'CM'
        if (char(nopt).eq.'e') read (optarg,*) idiag
        if (char(nopt).eq.'F') read (optarg,*) nmap
        if (char(nopt).eq.'d') read (optarg,*) da
        if (char(nopt).eq.'h') read (optarg,*) hr
      go to 10
   20 continue

      vect = divg.or.vort.or.dpur
      nsels = grad+divg+vort+dpur
      nargs = optind
      if (vect) nargs = optind + 1
      if (iargc().lt.nargs .or. nsels.gt.1) then
        write (6,*) ' Usage: fderivx [-i -g -m -d date -h hour] file'
        write (6,*) 
     *   '        fderivx [-i -D/V/P -m -d date -h hour] fileu,filev'
        write (6,*) '   -g        E & N gradient components of function'
        write (6,*) '               - otherwise Laplacian of function'
        write (6,*) '   -S        function squared'
        write (6,*) '   -C        minimum second derivative'
        write (6,*) '   -D        horizontal divergence'
        write (6,*) '   -V        vorticity'
        write (6,*) '   -P        pure horizontal shear'
        write (6,*) '   -m        length units in m.  \\ otherwise'
        write (6,*) '   -c        length units in cm. / deg.lat.'
        write (6,*) '   -i idiag  diagnostic level'
        write (6,*) '   -F nmap   record (map) number'
        write (6,*) '   -d date   date'
        write (6,*) '   -h hour   hour'
        write (6,*) '   file      function data file'
        stop
      endif

      call getarg(optind,file1)
      if (vect) call getarg(optind+1,file2)

      do 100 nn = lnblnk(file1),1,-1
        if (file1(nn:nn).eq.'/') go to 110
  100 continue
      nn = 0
  110 continue
      nst = nn + 1

      if (grad) then
        file3 = file1(nst:lnblnk(file1)) // '.grdx'
        file4 = file1(nst:lnblnk(file1)) // '.grdy'
      else
        if (cext) then
          if (sign.lt.0) then
          file3 = file1(nst:lnblnk(file1)) // '.cmin'
          else
          file3 = file1(nst:lnblnk(file1)) // '.cmax'
          endif
        else if (divg) then
          file3 = 'divg' //file1(nst:lnblnk(file1)) 
        else if (vort) then
          file3 = 'vort' //file1(nst:lnblnk(file1)) 
        else if (dpur) then
          file3 = 'dpur' //file1(nst:lnblnk(file1)) 
        else
          file3 = file1(nst:lnblnk(file1)) // '.dlsq'
        endif
      endif

      open (unit=1,file=file1,form='unformatted',status='old')
      if (vect) 
     *open (unit=2,file=file2,form='unformatted',status='old')
      open (unit=3,file=file3,form='unformatted',status='unknown')
      if (grad) 
     *open (unit=4,file=file4,form='unformatted',status='unknown')

      if (lnunit(1:2).eq.'M ') then
        deglat = 111111.
      else if (lnunit(1:2).eq.'CM') then
        deglat = 11111100.
      else 
        deglat = 1. 
      endif

      spval = 99999.9

c-------------------------------------------------------------------------------
c     Compute velocity at cyclone centre from geopotential heights
c-------------------------------------------------------------------------------

      if (nmap.gt.0) then
        nmaps = nmap
        nmape = nmap
      else
        nmaps = 1
        nmape = 1000
      endif

      do 400 imap = nmaps,nmape

c     Read function array
c     -------------------

  200 continue
      if (nmap.gt.1) then
        do 205 iread = 1,(nmap-1)*6
          read (1) dummy
  205   continue
      endif
      call llmaprd(1,head1,nlonlt,nlatlt,nlllt,nlat,lat,nlon,lon,
     * fhk,quantx,levelx,lunitx,sourcex,dmodex,dax,hrx,unitx,llgrid,ie1)
      if (ie1.ge.1) go to 410
      if (da.eq.0) go to 210
      if (dax.gt.da .or. (dax.eq.da .and. hrx.gt.hr)) then
        write (6,*) 'Field not present for ',da,hr
        stop
      endif
      if (dax.lt.da .or. (dax.eq.da .and. hrx.lt.hr)) go to 200
  210 continue
      write (6,*) ' Function array read.'
      write (6,*) file1
      write (6,*) head1

      if (vect) then
  220 continue
      if (nmap.gt.1) then
        do 225 iread = 1,(nmap-1)*6
          read (2) dummy
  225   continue
      endif
      call llmaprd(2,head2,nlonlt,nlatlt,nlllt,nlat,lat,nlon,lon,
     * ghk,quantx,levelx,lunitx,sourcex,dmodex,dax,hrx,unitx,llgrid,ie1)
      if (ie1.ge.1) go to 410
      if (da.eq.0) go to 230
      if (dax.gt.da .or. (dax.eq.da .and. hrx.gt.hr)) then
        write (6,*) 'Field not present for ',da,hr
        stop
      endif
      if (dax.lt.da .or. (dax.eq.da .and. hrx.lt.hr)) go to 220
  230 continue
      write (6,*) ' Second function array read.'
      write (6,*) file2
      write (6,*) head2
      endif

c     Expand array
c     ------------

      mgL = 8
      mgR = 8
      mgD = 8
      mgU = 8
      write (6,*) ' lon:',(lon(i),i=1,nlon)
      call llexpand(nlllt,nll2lt,nlon,nlat,lon,lat,nlon2,nlat2,
     * lon2,lat2,0,mgL,mgR,mgD,mgU,fhk,f,head1,file1,88,idiag,
     * ie3)
      if (vect)
     *call llexpand(nlllt,nll2lt,nlon,nlat,lon,lat,nlon2,nlat2,
     * lon2,lat2,0,mgL,mgR,mgD,mgU,ghk,g,head2,file2,88,idiag,
     * ie3)
      write (6,*) ' Array expanded.'
      
c     Obtain bicubic spline coefficient for f field
c     ---------------------------------------------

      call bisplsv(nlon2,nlat2,lon2,lat2,
     * f,fxx,fyy,fxxyy,1,nlon2,1,nlat2,spval,idiag,ie4)
      if (vect)
     *call bisplsv(nlon2,nlat2,lon2,lat2,
     * g,gxx,gyy,gxxyy,1,nlon2,1,nlat2,spval,idiag,ie4)
      write (6,*) ' Splines done'

c     Compute gradient components
c     ---------------------------

      nlonx = nlon
      write (6,*) ' lon:',(lon(i),i=1,nlon)
      if (abs(lon(nlon)-lon(1)-360.).lt.0.01) nlonx = nlon - 1
      if (abs(lon(nlon-1)-lon(1)-360.).lt.0.01) nlonx = nlon - 2
      write (6,*) 'lon(nlon),lon(1),lon(nlon)-lon(1)-360.,nlon,nlonx'
     * ,lon(nlon),lon(1),lon(nlon)-lon(1)-360.,
     * nlon,nlonx
      call fderiv2(f,fxx,fyy,fxxyy,nlon2,nlat2,lon2,lat2,spval,
     * vect.or.grad,cext,fx,fy,deglat,nlonx,sign,idiag)
      if (vect)
     *call fderiv2(g,gxx,gyy,gxxyy,nlon2,nlat2,lon2,lat2,spval,
     * vect,cext,gx,gy,deglat,nlonx,sign,idiag)
      write (6,*) ' Derivatives calculated'

c     Recast data onto original array
c     -------------------------------

      do 250 mgy = 1,nlat2
c       if (abs(lat2(mgy)+90.).lt.0.001) go to 260
        if (abs(lat2(mgy)-lat(1)).lt.0.001) go to 260
c       if (lat2(mgy).ge.-90.01) go to 260
  250 continue
  260 continue
      mgy = mgy-1

      do 270 mgx = 1,nlon2
c       if (abs(lon2(mgx)-0.).lt.0.001) go to 280
        if (abs(lon2(mgx)-lon(1)).lt.0.001) go to 280
c       if (lon2(mgx).ge.-0.01) go to 280
  270 continue
  280 continue
      mgx = mgx-1

      do 300 j1 = 1,nlat
        j2 = j1 + mgy
        j2add = (j2-1)*nlon2
        j1add = (j1-1)*nlon

        do 290 i1 = 1,nlon
          i2 = i1 + mgx
          ij2 = j2add + i2
          ij1 = j1add + i1
          if (vect) then
            f(ij1) = spval
            if (divg) then
              if (fx(ij2).ne.spval .and. gy(ij2).ne.spval)
     *         f(ij1) = fx(ij2) + gy(ij2)
            else if (vort) then
              if (fx(ij2).ne.spval .and. gy(ij2).ne.spval)
     *         f(ij1) = fy(ij2) + gx(ij2)
            else if (dpur.and.fx(ij2).ne.spval
     *       .and.gy(ij2).ne.spval) then
              DT = fx(ij2) - gy(ij2)
              DS = fx(ij2) + gy(ij2)
              f(ij1) = sqrt(DT*DT + DS*DS)
            endif
          else
            if (sqar) then
              f(ij1)   = fx(ij2)*fx(ij2)
              if (grad) f(ij1) = f(ij1) + fy(ij2)*fy(ij2)
            else
              f(ij1)   = fx(ij2)
              if (grad) fxx(ij1) = fy(ij2)
            endif
          endif
  290   continue

        ij1 = j1add + 1
        ijn = j1add + nlon
        if (nlon-nlonx.eq.1) then
          fend = (f(ij1)+f(ijn))*0.5
          f(ij1) = fend
          f(ijn) = fend
          gend = (fxx(ij1)+fxx(ijn))*0.5
          fxx(ij1) = gend
          fxx(ijn) = gend
        else if (nlon-nlonx.eq.2) then
          f(ij1) = f(ijn-1)
          f(ijn) = f(ij1+1)
          fxx(ij1) = fxx(ijn-1)
          fxx(ijn) = fxx(ij1+1)
        endif
  300 continue

c     Manipulate and write gradient arrays
c     ------------------------------------

      head3 = head1
      aa = ' '
      aa = head1(1:6)
      bb = head1(58:67)
      if (vect.or.grad) then
        head3(58:70) = bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit))
        if (sqar) head3(58:70) = '(' // bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) // ')**2'
        if (divg) then
          head3(1:9) = 'DIVG'
        else if (vort) then
          head3(1:9) = 'VORT'
        else if (dpur) then
          head3(1:9) = 'DPUR'
        else
          head3(1:9) = aa(1:lnblnk(aa)) // '_X'
          if (sqar) head3(1:9) = aa(1:lnblnk(aa)) // '_R**2'
        endif
      else if (cext) then
        if (sign.lt.0) then
        head3(1:9) = aa(1:lnblnk(aa)) // '_CMN'
        if (sqar) head3(1:9) = aa(1:lnblnk(aa)) // '_CMN**2'
        else
        head3(1:9) = aa(1:lnblnk(aa)) // '_CMX'
        if (sqar) head3(1:9) = aa(1:lnblnk(aa)) // '_CMX**2'
        endif
        head3(58:70) = bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) // '**2'
        if (sqar) head3(58:70) = '(' // bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) // '**2)**2'
      else
        head3(1:9) = aa(1:lnblnk(aa)) // '_DSQ'
        if (sqar) head3(1:9) = aa(1:lnblnk(aa)) // '_DSQ**2'
        head3(58:70) = bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) // '**2'
        if (sqar) head3(58:70) = '(' // bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) // '**2)**2'
      endif
      write (6,*) file3
      write (6,*) head3

      write (3) nlat
      write (3) (lat(j),j=1,nlat)
      write (3) nlon
      write (3) (lon(i),i=1,nlon)
      write (3) head3
      write (3) (f(ij1),ij1=1,nlon*nlat)

      if (grad.and.(.not.sqar)) then
        head4 = head1
        head4(1:9) = aa(1:lnblnk(aa)) // '_Y'
        head4(58:70) = bb(1:lnblnk(bb)) // '/' // 
     *   lnunit(1:lnblnk(lnunit)) 

        write (6,*) file4
        write (6,*) head4

        write (4) nlat
        write (4) (lat(j),j=1,nlat)
        write (4) nlon
        write (4) (lon(i),i=1,nlon)
        write (4) head4
        write (4) (fxx(ij1),ij1=1,nlon*nlat)
      endif

  400 continue
  410 continue

      close (3)
      close (4)

      stop
      end

c=======================================================================

      Subroutine fderiv2(f,fxx,fyy,fxxyy,nlon,nlat,lon,lat,spval,
     * grad,cext,fx,fy,deglat,nlonx,sign,idiag)

      dimension f  (nlon,nlat),fxx  (nlon,nlat),
     *          fyy(nlon,nlat),fxxyy(nlon,nlat),
     *          fx (nlon,nlat),fy   (nlon,nlat)
      real lon(nlon),lat(nlat)
      logical diag,grad,cext,spv

      parameter (r6=1./6.,r180=1./180.)
      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.,c180=180.,c360=360.)
      parameter (pi=3.1415926535898)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      diag = idiag.ge.2

      spv = .false.
      do 100 j = 1,nlat-1
        if (abs(lat(j)).gt.89.99) then
          fx(i,j) = spval
          fy(i,j) = spval
          convx = 0.
          go to 82
        else
          convx = 1./(deglat*cos(lat(j)*rrad))
        endif
        convy = 1./deglat
        if (grad) then
          dlat = lat(j+1) - lat(j)
          B2 = 1./dlat
          B1 = -B2
          B3 = -2.*dlat*r6
          B4 = -1.*dlat*r6
        else
          convxx = convx*convx
          convyy = convy*convy
          convxy = convx*convy
        endif

        do 80 i = 1,nlon-1
          if (grad) then
c           if  (f(i,j).eq.spval) go to 60
            if  (f(i,j  ).eq.spval.or.f(i+1,j  ).eq.spval
     *       .or.f(i,j+1).eq.spval.or.f(i+1,j+1).eq.spval) go to 60
c  rewrite to be less restrictive

            dlon = lon(i+1) - lon(i)
            A2 = 1./dlon
            A1 = -A2
            A3 = -2.*dlon*r6
            A4 = -1.*dlon*r6

            fy(i,j) = (B1*f(i,j)   + B2*f(i,j+1) +
     *                 B3*fyy(i,j) + B4*fyy(i,j+1))*convy
            if (abs(lat(j)).ne.90.) 
     *      fx(i,j) = (A1*f(i,j)   + A2*f(i+1,j)   +
     *                 A3*fxx(i,j) + A4*fxx(i+1,j))*convx
c           if (diag) write (97,'(2i3,20f8.3)') i,j,
c    *       lon(i),lat(j),f(i,j),fxx(i,j),fyy(i,j),
c    *       fxxyy(i,j),B1,B2,B3,B4,dlat,fy(i,j)
          else if (cext) then
            if  (f(i,j).eq.spval) go to 60
            if (abs(lat(j)).ne.90.) then
              fxx0 = fxx(i,j)*convxx 
              fyy0 = fyy(i,j)*convyy
              fxy0 =(B2*(A2*f    (i+1,j+1) + A1*f    (i,j+1)
     *                  +A4*fxx  (i+1,j+1) + A3*fxx  (i,j+1))
     *             + B1*(A2*f    (i+1,j  ) + A1*f    (i,j  )
     *                  +A4*fxx  (i+1,j  ) + A3*fxx  (i,j  ))
     *             + B4*(A2*fyy  (i+1,j+1) + A1*fyy  (i,j+1)
     *                  +A4*fxxyy(i+1,j+1) + A3*fxxyy(i,j+1))
     *             + B3*(A2*fyy  (i+1,j  ) + A1*fyy  (i,j  )
     *                  +A4*fxxyy(i+1,j  ) + A3*fxxyy(i,j  )))
     *             *convxy

              fcva = (fxx0 + fyy0)/2.
              fecc = (fxx0 - fyy0)/2.
              fcvd = sqrt(fecc*fecc + fxy0*fxy0)
c             fuu  = fcva - fcvd
c             fvv  = fcva + fcvd
              fx(i,j)  = fcva + sign*fcvd

              if (diag) write (97,'(2i4,20f10.4)') 
     *         i,j,lat(j),convxx,convyy,convxy,
     *         f(i,j),fxx(i,j),fyy(i,j),fx(i,j)
            else
              fx(i,j) = spval
            endif
          else
            if  (f(i,j).eq.spval) go to 60
            if (abs(abs(lat(j))-90.).ge.0.001) 
     *       fx(i,j) = fxx(i,j)*convxx + fyy(i,j)*convyy
            if (diag) write (97,'(2i4,20f10.4)') 
     *       i,j,lat(j),convxx,convyy,
     *       f(i,j),fxx(i,j),fyy(i,j),fx(i,j)
          endif

          go to 70
   60     continue
          fx(i,j) = spval
          fy(i,j) = spval
          spv = .true.
   70     continue
   80   continue
   82   continue

        if ((abs(abs(lat(j))-90.)).lt.0.001) then
          write (6,*) ' J = ',j
          iq = nlonx/4
          if (abs(lat(j)+90.).lt.0.001) iq = -iq
          if (grad) then
            do 95 i = 1,nlon
              ix = i - iq
              if (ix.lt.1)    ix = ix + nlonx
              if (ix.gt.nlon) ix = ix - nlonx
              fx(i,j) = fy(ix,j)
   95       continue
          else if (cext) then
            fx(i,j) = spval
          else
            write (6,*) iq,fyy(1,j),fyy(1+iq,j)
            if (fyy(1,j).eq.spval .or. fyy(1+iq,j).eq.spval) then
              spv = .true.
              fdlsq = spval
            else
              fdlsq = fyy(1,j) + fyy(1+iq,j)
            endif
            do 96 i = 1,nlon
              fx(i,j) = fdlsq
   96       continue
          endif
        endif
   98   continue
  100 continue

c     write (6,*) 'f'
c     write (6,*) f(16+8,32+8),f(17+8,32+8),f(18+8,32+8)
c     write (6,*) f(16+8,31+8),f(17+8,31+8),f(18+8,31+8)
c     write (6,*) f(16+8,30+8),f(17+8,30+8),f(18+8,30+8)
c     write (6,*) 'fxx'
c     write (6,*) fxx(16+8,32+8),fxx(17+8,32+8),fxx(18+8,32+8)
c     write (6,*) fxx(16+8,31+8),fxx(17+8,31+8),fxx(18+8,31+8)
c     write (6,*) fxx(16+8,30+8),fxx(17+8,30+8),fxx(18+8,30+8)
c     write (6,*) 'fyy'
c     write (6,*) fyy(16+8,32+8),fyy(17+8,32+8),fyy(18+8,32+8)
c     write (6,*) fyy(16+8,31+8),fyy(17+8,31+8),fyy(18+8,31+8)
c     write (6,*) fyy(16+8,30+8),fyy(17+8,30+8),fyy(18+8,30+8)
c     write (6,*) 'fxxyy'
c     write (6,*) fxxyy(16+8,32+8),fxxyy(17+8,32+8),fxxyy(18+8,32+8)
c     write (6,*) fxxyy(16+8,31+8),fxxyy(17+8,31+8),fxxyy(18+8,31+8)
c     write (6,*) fxxyy(16+8,30+8),fxxyy(17+8,30+8),fxxyy(18+8,30+8)
c     write (6,*) 'fx'
c     write (6,*) fx(16+8,32+8),fx(17+8,32+8),fx(18+8,32+8)
c     write (6,*) fx(16+8,31+8),fx(17+8,31+8),fx(18+8,31+8)
c     write (6,*) fx(16+8,30+8),fx(17+8,30+8),fx(18+8,30+8)
c     write (6,*) 'fy'
c     write (6,*) fy(16+8,32+8),fy(17+8,32+8),fy(18+8,32+8)
c     write (6,*) fy(16+8,31+8),fy(17+8,31+8),fy(18+8,31+8)
c     write (6,*) fy(16+8,30+8),fy(17+8,30+8),fy(18+8,30+8)

      if (spv) write (6,*) ' Special values found.'

      return
      end
