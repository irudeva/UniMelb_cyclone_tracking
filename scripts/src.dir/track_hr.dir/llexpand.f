      Subroutine llexpand(nhklt,nhk2lt,nh,nk,lon,lat,nh2,nk2,lon2,lat2,
     * ivec,mgL,mgR,mgD,mgU,fhk,f,head,infile,iurcst,idiag)

c         A routine for expanding a global lat-lon array by wrapping
c     around the end meridians and over the N and S poles, where
c     possible.

c     R.J. Murray, 9th Dec. 1992.

      real lon(nh),lat(nk),lon2(nh2),lat2(nk2)
      dimension fhk(nh,nk),f(nhk2lt)
      integer h,hhL,hhR,hhL1,hhR1,nh2
      dimension kkpole(2)
      character*80 head,headrcst,infile

      common /blrcst/hhL1,hhL,hhR,hhR1,
     *               kkD1,kkD,kkU,kkU1,kkpole,
     *               nhx,iwrapE,iwrapS,iwrapN

      nhk = nh*nk
      if (nhk.gt.nhklt) then
          write (6,*) ' Input to llexpand ',nh,'x',nk,
     *     'exceeds array bounds,',nhklt,'.'
          stop
      endif
      if (ivec.eq.-1) stop ' Vector components not allowed.'

c-----------------------------------------------------------------------
c     Check whether array is cyclic in longitude &/or latitude
c-----------------------------------------------------------------------

c     iwrapE      = 1 (not cyclic in longitude)
c                 = 2 (continuous, without repeated longitudes)
c                 = 3 (continuous, with repeated longitudes)
c     iwrap{N,S}  = 1 (not reaching to N or S pole)
c                 = 2 (extending to less than a grid point from the pole)
c                 = 3 (containing a grid point at the pole)
c     nhx         = no. of longitudes, not counting any repeated end
c                     longitudes

      call globtest(nk,nh,nhx,iwrapE,iwrapS,iwrapN,lat,lon,nk,nh)

c-----------------------------------------------------------------------
c     Limits of new array.
c-----------------------------------------------------------------------

c     x limits.
c     ---------

      if (iwrapE.lt.2) then
        mgL = 0
        mgR = 0
      endif
      hhL = 1  + mgL
      hhR = nh + mgL
      nh2  = hhR + mgR
      hhL1 = mgL
      hhR1 = hhR + 1

c     y limits.
c     ---------

      if (iwrapS.lt.2) mgD = 0
      if (iwrapN.lt.2) mgU = 0

      kkD = 1  + mgD
      kkU = nk + mgD
      nk2 = kkU + mgU

      kkD1 = mgD
      kkpole(1) = 0
      if (iwrapS.eq.2) then
        kkD1 = mgD - 1
        kkpole(1) = mgD
      endif

      kkU1 = kkU + 1
      kkpole(2) = 0
      if (iwrapN.eq.2) then
        kkU1 = kkU + 2
        kkpole(2) = kkU + 1
      endif

      nhk2 = nh2*nk2

      if (idiag.ge.3) then
        write (6,*) 'nhklt,nh,nk,nhk,mgL,mgR,mgD,mgU,',
     *    'iwrapE,iwrapS,iwrapN,nhx,',
     *    'hhL1,hhL,hhR,hhR1,nh2,',
     *    'kkD1,kkD,kkU,kkU1,nk2,kkpole,nhk2,nhk2lt'
        write (6,*) nhklt,nh,nk,nhk,mgL,mgR,mgD,mgU,
     *    iwrapE,iwrapS,iwrapN,nhx,
     *    hhL1,hhL,hhR,hhR1,nh2,
     *    kkD1,kkD,kkU,kkU1,nk2,kkpole,nhk2,nhk2lt
        write (6,*) 'lat',lat
        write (6,*) 'lon',lon
      endif

      if (nhk2.gt.nhk2lt) then
        write (6,*) ' Output from llexpand ',
     *   nh2,'x',nk2,'exceeds array bounds,',nhk2lt,'.'
        stop
      endif

c-----------------------------------------------------------------------
c     Recasting of array.
c-----------------------------------------------------------------------

c         The data are then recast on the expanded array.  This work is
c     performed by a separate subroutine (`recast'), since the dimens-
c     ions of the new array are not known when `llexpand' is called.

      call recast(nh,nk,fhk,nh2,nk2,f,lon,lat,lon2,lat2,
     * mgL,mgR,mgD,mgU,idiag)

c-----------------------------------------------------------------------
c     Write recast array (if required) to unit "iurcst"
c-----------------------------------------------------------------------

      if (idiag.eq.3) then
        open (iurcst,file=infile(1:lnblnk(infile))//
     *   '_recast',form='unformatted')
        write (iurcst) nk2
        write (iurcst) (lat2(k),k=1,nk2)
        write (iurcst) nh2
        write (iurcst) (lon2(h),h=1,nh2)
        headrcst = head
        headrcst(49:56) = 'EXPANDED'
        write (iurcst) head
        write (iurcst) (f(hk),hk=1,nhk2)
        close (iurcst)
      endif

      return 
      end

c=======================================================================

      Subroutine recast(nh,nk,fhk,nh2,nk2,f,lon,lat,lon2,lat2,
     * mgL,mgR,mgD,mgU,idiag)

      parameter (nhhsp=5,nkksp=5,nkksp1=nkksp+1,nspllt=2*nkksp)

      integer h,hhL,hhR,hhL1,hhR1,nh2
      integer hh,hhh(100),hhmnhx,hhpnhx
      real kksp(nspllt),ll(nspllt),ff(nspllt),cc(nspllt),gam(nspllt)
      dimension kkpole(2)
      real lon(nh),lat(nk),lon2(nh2),lat2(nk2)
      dimension fhk(nh,nk),f(nh2,nk2)

      common /blrcst/hhL1,hhL,hhR,hhR1,
     *               kkD1,kkD,kkU,kkU1,kkpole,
     *               nhx,iwrapE,iwrapS,iwrapN

c     Translation of given array.
c     ---------------------------

      kk = kkD
      do 60 k = 1,nk
        lat2(kk) = lat(k)
        hh = hhL
        do 55 h = 1,nh
          f(hh,kk) = fhk(h,k)
          hh = hh + 1
 55     continue
        kk = kk + 1
 60   continue

      hh = hhL
      do 70 h = 1,nh
        lon2(hh) = lon(h)
        hh = hh + 1
 70   continue

c     Extension E-W.
c     --------------

      if (mgL.gt.0) then
        do 80 hh = 1,hhL1
          hhpnhx = hh + nhx
          lon2(hh) = lon2(hhpnhx) - 360.
          do 75 kk = kkD,kkU
            f(hh,kk) = f(hhpnhx,kk)
 75       continue
 80     continue
      endif

      if (mgR.gt.0) then
        do 90 hh = hhR1,nh2
          hhmnhx = hh - nhx
          lon2(hh) = lon2(hhmnhx) + 360.
          do 85 kk = kkD,kkU
            f(hh,kk) = f(hhmnhx,kk)
 85       continue
 90     continue
      endif

c     Extension N-S.
c     --------------

      if ((mgD.gt.0).or.(mgU.gt.0)) then

c       Calculate longitude indices ("hhh") for transpolar longitudes

        nhxd2 = nhx/2 
        nhmid = nhxd2 + hhL
        do 100 hh = 1,nh2
          if (hh.ge.nhmid) then
            hhh(hh) = hh - nhxd2
          else
            hhh(hh) = hh + nhxd2
          endif
 100    continue
        if (idiag.eq.3) write (6,*) 'hhh',hhh
        if (idiag.ge.3) write (6,*) 'lon2',lon2

c       Calculate transpolar latitude indices ("kkk") and latitudes
c       and place data values in array

        if (mgD.gt.0) then
          if (iwrapS.eq.2) lat2(kkpole(1)) = -90.
          kk = kkD
          if (iwrapS.eq.3) kk = kkD + 1
          do 120 kkk = kkD1,1,-1
            lat2(kkk) = -180. - lat2(kk)
            do 110 hh = 1,nh2
              f(hh,kkk) = f(hhh(hh),kk)
 110        continue
            kk = kk + 1
 120      continue
        endif

        if (mgU.gt.0) then
          if (iwrapN.eq.2) lat2(kkpole(2)) = 90.
          kk = kkU
          if (iwrapN.eq.3) kk = kkU - 1
          do 140 kkk = kkU1,nk2
            lat2(kkk) = 180. - lat2(kk)
            do 130 hh = 1,nh2
              f(hh,kkk) = f(hhh(hh),kk)
 130        continue
            kk = kk - 1
 140      continue
        endif

        if (idiag.ge.3) write (6,*) 'lat2',lat2

c       Calculate values at Poles.

        do 220 ipole = 1,2
          ihinc=(nh-1)/8
          ipoleSN = 0
          if (kkpole(ipole).ne.0) then
            kk0   = kkpole(ipole)
            polelat = lat2(kk0)
            ipoleSN = 1
            if (ipole.eq.1) then
              kkend = kkD
              if (kk0.ge.3) ipoleSN = 2
              kksign = -1
              nspline = 5 + (mgD-1)
            else
              kkend = kkU
              if (kk0.le.nk2-2) ipoleSN = 2
              kksign = 1
              nspline = 5 + (mgU-1)
            endif
            if (nspline.gt.nspllt) nspline = nspllt

            if (ipoleSN.ge.2) then
              do 160 kkk = 1,nkksp
                kksp(kkk) = kk0 - kksign*(nkksp+1-kkk)
                ll(kkk) = float(kksign)*(lat2(kksp(kkk)) - polelat)
  160         continue
              do 170 kkk = nkksp1,nspline
                kksp(kkk) = kk0 - kksign*(nkksp-kkk)
                ll(kkk) = float(kksign)*(lat2(kksp(kkk)) - polelat)
  170         continue
              if (idiag.ge.3) then
                write (6,*) ' '
                write (6,*) ' Pole ',ipole,' kk0=',kk0,
     *           ' ipoleSN=',ipoleSN
                write (6,'(a12,20f10.2)') ' Latitudes: ',
     *           (ll(kkk),kkk=1,nspline)
              endif
              sum = 0.
              do 180 ihh = 1,nhhsp
                hh = hhL + ((ihh-1)*nhx)/nhhsp
                do 175 kkk = 1,nspline
                  ff(kkk) = f(hh,kksp(kkk))
  175           continue
                call isplcf(ll,ff,cc,gam,nspline,1,nspline,0.,0.)
                A1 = 0.5
                A3 = -ll(nkksp1)*ll(nkksp1)*0.25
                f0h = A1*(ff(nkksp) + ff(nkksp1)) +
     *                A3*(cc(nkksp) + cc(nkksp1))
                sum = sum + f0h
                if (idiag.ge.3) write (6,'(a6,i3,3x,21f10.2)')
     *           ' hh = ',hh,(ff(kkk),kkk=1,nspline),f0h
     *           
 180          continue
              f0 = sum/float(nhhsp)
            else if (ipoleSN.eq.1) then
              sum = 0.
              do 200 hh = hhL,hhR
                sum = sum + f(hh,kkend)
 200          continue
              f0  = sum/float(hhR - hhL + 1)
            endif

            do 210 h = 1,nh2
              f(h,kk0) = f0
 210        continue
            if (idiag.ge.3) write (6,*) ' Polar value = ',f0
          endif

 220    continue

      endif

      return
      end
