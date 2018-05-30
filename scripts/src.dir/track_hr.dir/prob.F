      Subroutine prob

c       This routine calculates the probability of association of each 
c     predicted cyclone position and each actual position, based on their 
c     distance apart and difference in central pressure.

c     Written by Ross Murray
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      real ndtref
      dimension xr(nklt),yr(nklt),sinphir(nklt),cosphir(nklt)
      logical addpres,diag2,diag3,wrline

      parameter (c0=0.,c1=1.,c2=2.,r2=0.5,c90=90.,c180=180.,c360=360.)
      parameter (c1440=1440.)
      parameter (pi=3.1415926535898,p5pi=0.5*pi)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

      scos(xx,yy,zz) = acos( cos(xx)*sin(yy)*sin(zz) + cos(yy)*cos(zz) )
      stan(xx,yy,zz) = atan2(sin(xx),(sin(zz)/tan(yy)-cos(zz)*cos(xx)))
      angd(xx) = amod(xx+c1440,c360)

c-----------------------------------------------------------------------
c     Adjust and limit probability constants
c-----------------------------------------------------------------------

      ierr = 0
      diag2 = idiagt.ge.2
      diag3 = idiagt.ge.3

c       The analysis is skipped if all tracks be new

      n = 0
      if (newtks) then
        write (6,*) ' NO CONTINUATION OF EXISTING TRACKS'
        go to 210
      endif

      if (diag2) then
        write (6,*) ' '
        write (6,*) ' LOG(PROBABILITIES) OF ASSOCIATED POSITIONS: ',
     *   dac,hrc
        wrline = .false.
      endif

      scalt0  = 2.*57.2957/rproj
      rprojsq = rproj*rproj

c       dequiv is the number of deg.lat. of equivalent separation which 
c     gives the same diminution in association probability to a change
c     of one unit of the function.

c       rcprob is the search radius in deg.lat. appropriate to a 
c     reference analysis interval (refdt).  The actual search radius
c     is scaled to rcprob as the square root of the time relative
c     time (ndtref=ndt/refdt, both ndt and refdt being in days) on 
c     the assumption of diffusive movement away from the predicted 
c     position).

      ndtref = ndt/refdt
c     dc   = rcprob*sqrt(amax0(ndtref,0.01))
crmw  amin0 -> min amax0 _> max
      dc   = rcprob*sqrt(max(min(1.,ndtref),0.01))
      dcsq = dc*dc

c       rpbell is a measure of the radius (squared) of the bell (i.e. the
c     point of inflection) of the probability funtion of radius.

c     rpbell = 1   gives an inverted parabolic curve
c     rpbell < 1   gives a curve tightened near the origin and flatter
c                   (approx 1/r**2) away from the origin
c     rpbell > 1   gives a curve flattened at the top but falling
c                   steeply near r = rc

      if ((rpbell.lt.0.01).or.((rpbell.ne.1.).and.
     * (abs(rpbell-1.).lt.0.01))) then
        write (6,*) ' rpbell = ',rpbell,':  reset to 1.'
        wrline = .true.
        rpbell = 1.
      endif

c       Max. q for open depressions should be less than for closed 
c     depressions (1) but ge 0.

      if ((qmxopn.lt.0.).or.(qmxopn.gt.1.)) then
        write (6,*) ' Resetting qmxopn, 0.<qmxopn<1.'
        wrline = .true.
        if (qmxopn.lt.0.) qmxopn = 0.
        if (qmxopn.gt.1.) qmxopn = 1.
      endif

c       Max. q for weak depressions should be less than for closed 
c     and (strong) open depressions (qmxopn) but ge 0.

      if ((qmxwek.lt.0.).or.(qmxwek.gt.qmxopn)) then
        write (6,*) ' Resetting qmxwek, 0.<qmxwek<qmxopn'
        wrline = .true.
        if (qmxwek.lt.0.)     qmxwek = 0.
        if (qmxwek.gt.qmxopn) qmxwek = qmxopn
      endif

c       Max. q for new cyclones should be < for existing cyclones, but
c     not so small that they cannot form.

      if ((qmxnew.lt.0.2).or.(qmxnew.gt.1.)) then
        write (6,*) ' Resetting qmxnew, 0.2<qmxnew<1.'
        wrline = .true.
        if (qmxnew.lt.0.2) qmxnew = 0.2
        if (qmxnew.gt.1.) qmxnew = 1.
      endif
      if (diag2.and.wrline) write (6,*) ' '

      uconv = 1./1.286      ! 1 deg.lat./day = 1/1.286 m./s.
      factS = ndt*wsteer*fsteer*(1.-asteer)*uconv

c-----------------------------------------------------------------------
c     Seek possible associations between predicted and new positions
c-----------------------------------------------------------------------

c     Write headers for diagnostic output, if required
c     ------------------------------------------------

      if (diag2) write (6,20) refdt,ndt,ndtref,scalt0,
     * uconv,factS,dc,dcsq,dequiv,rpbell,qmxopn,qmxwek,qmxnew
   20 format(/5x,'refdt   = ',f7.4,5x,'ndt     = ',f7.4/
     *        5x,'ndtref  = ',f7.4,5x,'scalt0  = ',f7.4/
     *        5x,'uconv   = ',f7.4,5x,'factS   = ',f7.4/
     *        5x,'dc      = ',f7.4,5x,'dcsq    = ',f7.2/
     *        5x,'dequiv  = ',f7.4,5x,'rpbell  = ',f7.4/
     *        5x,'qmxopn  = ',f7.4,5x,'qmxwek  = ',f7.4/
     *        5x,'qmxnew  = ',f7.4)

c     Find regressed positions for time tc
c     ------------------------------------

      if (wsteer.gt.0. and .asteer.gt.0.) then
        if (pstrak) then
          if (diag2) then
            write (6,'(/20a8)') 'ndt','wsteer','fsteer','scalt',
     *       'asteer','factS'
            write (6,'(20f8.3)') ndt,wsteer,fsteer,scalt,
     *       asteer,factS
            write (6,'(/a3,20a8)') 'kc','xc(kc)','yc(kc)','xcs',
     *       'ycs','the','upc(kc)','vpc(kc)','upcx','vpcy',
     *       'factSsc','-dxS','-dyS','xr(kc)','yr(kc)'
            write (6,*) ' '
          endif
          do 110 kc = 1,nkc
            xs = xc(kc) - xcen
            ys = yc(kc) - ycen
            rssq = xs*xs + ys*ys
            rs = sqrt(rssq)
            sinthe = -fhem*xs/rs
            costhe = -fhem*ys/rs
            upcx =  upc(kc)*costhe + vpc(kc)*sinthe
            vpcy = -upc(kc)*sinthe + vpc(kc)*costhe
            scalt = scalt0*rprojsq/(rprojsq + rssq)
            factSsc = factS/scalt
            dxS = upcx*factSsc
            dyS = vpcy*factSsc
            xr(kc) = xc(kc) - dxS
            yr(kc) = yc(kc) - dyS
            call pstoll(xcen,ycen,rproj,fhem,xr(kc),yr(kc),
     *       alonr,alatr)
            call pstoll(xcen,ycen,rproj,fhem,xc(kc),yc(kc),
     *       alonc,alatc)
c           if (diag2) write (6,'(i3,20f8.3)') kc,xc(kc),yc(kc),
c    *       xs,ys,atan2(xs,ys)*rad,upc(kc),vpc(kc),upcx,vpcy,
c    *       factSsc,-dxS,-dyS,xr(kc),yr(kc)
            if (diag2) write (6,'(i3,20f8.3)') kc,alonc,alatc,
     *       upc(kc),vpc(kc),-dxS,-dyS,alonr,alatr
  110     continue
        else
          if (diag2) then
            write (6,'(/20a8)') 'ndt','wsteer','fsteer','factS'
            write (6,'(20f8.3)') ndt,wsteer,fsteer,factS
            write (6,'(/a3,20a8/)') 'kc','xc(kc)','yc(kc)',
     *       'upc(kc)','vpc(kc)','-dxS','-dyS',
     *       'xr(kc)','yr(kc)'
          endif
          do 120 kc = 1,nkc
            dxS = upc(kc)*factS
            dyS = vpc(kc)*factS
            chicr = rrad*sqrt(dxS*dxS + dyS*dyS)
            thecr = atan2(-dxS,-dyS)
            phidc = rrad*(c90 - yc(kc))
            xr(kc) = angd(xc(kc) + rad*stan(thecr,chicr,phidc))
            yr(kc) = c90         - rad*scos(thecr,chicr,phidc)
            if (diag2) write (6,'(i3,20f8.3)') kc,xc(kc),yc(kc),
     *       upc(kc),vpc(kc),-dxS,-dyS,xr(kc),yr(kc)
  120     continue
        endif
      else
        do 130 kc = 1,nkc
          xr(kc) = xc(kc)
          yr(kc) = yc(kc)
  130   continue
      endif

c     Loop through predicted and new positions
c     ----------------------------------------

      if (diag2) write (6,'(/''  n  kp  kc'',
     * '' iopb     (xp,yp)         pp iopc     (xr,yr)'',
     * ''         pc  scalt   rcp   ppr  drpr  dppr  '',
     * ''sepsq ratio   qmx     q''/)')

      if (dequiv.eq.0.) dppr = 0.

      if (.not.pstrak) then
        do 160 kc = 1,nkc
          phir = rrad*yr(kc)
          sinphir(kc) = sin(phir)
          cosphir(kc) = cos(phir)
  160   continue
      endif

      n = 0
      do 200 kp = 1,nkb
        if (.not.pstrak) then
crmw      amin0 -> min
          latc = min(89.9,max( abs(yp(kp)+dc),abs(yp(kp)-dc) ))
          dlonc = dc/cos(rrad*latc)
          phip = rrad*yp(kp)
          sinphip = sin(phip)
          cosphip = cos(phip)
        endif

        qmxnew1 = 1.
        if ((kab(kp).eq.0).and.(ic.gt.2)) qmxnew1 = qmxnew
        qmxopn1 = 1.
        if (iopb(kp).eq.1) qmxopn1 = qmxopn
        if (iopb(kp).gt.1) qmxopn1 = qmxwek

        addpres = (dequiv.ne.0.)
c    *   .and.(ic.gt.2)
c    *   .and.(kab(k).ne.0)

        do 180 kc = 1,nkc

c         Check that new cyclone be within a square of sides 2*rc
c         around the predicted position
c         -------------------------------------------------------

          ypr = abs(yr(kc) - yp(kp))
          if (pstrak) then
            xs = (xp(kp)+xr(kc))*0.5 - xcen 
            ys = (yp(kp)+yr(kc))*0.5 - ycen
            rssq = xs*xs + ys*ys
            scalt = scalt0*rprojsq/(rprojsq + rssq)
            scaltsq = scalt*scalt
            rc   = dc/scalt
            if (ypr.ge.rc) go to 170
            xpr = abs(xr(kc) - xp(kp))
            if (xpr.ge.rc) go to 170
          else
            if (ypr.ge.dc) go to 170
            xpr = xr(kc) - xp(kp)
            axpr1 = abs(xpr)
            axpr2 = abs(360. - axpr1)
crmw        amin0 -> min
            axpr = min(axpr1,axpr2)
            if (axpr.ge.dlonc) go to 170
          endif

c         Calculate the square of the distance in degrees between
c         the predicted and new positions
c         -------------------------------------------------------

          if (pstrak) then
            rprsq = (xpr*xpr + ypr*ypr)
            drprsq = rprsq*scaltsq
          else
            dlampr = rrad*xpr
            drpr = rad*acos(cos(dlampr)*cosphir(kc)*cosphip
     *                                 +sinphir(kc)*sinphip)
            drprsq = drpr*drpr
          endif

c         Calculate an effective 3-D square of the distance by adding
c         the square of the weighted pressure difference 
c         -----------------------------------------------------------

          if (addpres) then
            ppr   = (pc(kc) - pp(kp))
            dppr  = ppr*dequiv
            sepsq = drprsq + dppr*dppr
          else
            sepsq = drprsq
          endif

c         Determine whether this distance is less than the minimum
c         allowable distance, dc.
c         ----------------------------------------------------------

          if (sepsq.ge.dcsq) go to 170

c         Scale the separation
c         --------------------

          sepsqrat = sepsq/dcsq

c         Calculate the maximum q for the probability function
c         ----------------------------------------------------

          qmxopn2 = 1.
          if (iopc(kc).eq.1) qmxopn2 = qmxopn
          if (iopc(kc).gt.1) qmxopn2 = qmxwek
crmw      amin0 -> min
          qmxopn3 = min(qmxopn1,qmxopn2)
c         qmx = amin0(qmxopn3,qmxnew1)
          qmx = qmxopn3*qmxnew1

c         Calculate the probability function 
c         ----------------------------------

c           The probability function, q, is taken to be a bell-shaped
c         function of the effective width squared, sepsqrat; it decreases 
c         from a maximum at sep=0 to zero at sep ~ rc.  The shape of the 
c         curve is determined by rpbell.  The maximum value of q
c         (at sep=0) is the product of two factors, which penalise the
c         the formation of new tracks and the inclusion of open depressions.
c
c             qmxopn2 = 1. (closed depressions), qmxopn (open depressions),
c             qmxnew1 = 1. (existing systems),   qmxnew (new systems)

          if (rpbell.eq.1.) then
            q = qmx - sepsqrat
          else
            q = qmx - sepsqrat/(rpbell + (1.-rpbell)*sepsqrat)
          endif
          if (q.le.0.) go to 170

c         Record an association
c         ---------------------

c           If the probability function be greater than the background
c         value (0), record the predicted and new cyclone numbers and
c         the probability value, q, for the incremented association
c         number n.

          n = n + 1

          if (n.gt.nasslt) then
            write (6,*) ' prob: nass = ',nass,': limited to ',nasslt,'.'
            ierr = 1
            return
          endif

          qn(n) = q
          kpn(n) = kp
          kcn(n) = kc

c         Write diagnostics, if required
c         ------------------------------

          rsep = sqrt(rsepsq)
          if (diag2) then
            if (pstrak) then
            call pstoll(xcen,ycen,rproj,fhem,xr(kc),yr(kc),
     *       alonr,alatr)
            call pstoll(xcen,ycen,rproj,fhem,xp(kp),yp(kp),
     *       alonp,alatp)
            write (6,150) n,kp,kc,iopb(kp),alonp,alatp,pp(kp),
     *       iopc(kc),alonr,alatr,pc(kc),scalt,sqrt(rprsq),ppr,
     *       sqrt(drprsq),dppr,sepsq,0.,qmx,q
            write (6,*)
            else
            write (6,150) n,kp,kc,iopb(kp),xp(kp),yp(kp),pp(kp),
     *       iopc(kc),xr(kc),yr(kc),pc(kc),scalt,sqrt(rprsq),ppr,
     *       sqrt(drprsq),dppr,sepsq,0.,qmx,q
            endif
 150        format (i3,2(1x,i3),2(3x,i2,'  (',f5.1,',',f5.1,')',
     *       f8.2),1x,5f6.2,f7.2,f6.3,2f6.3)
cXXX
cXXX
          endif

 170      continue
 180    continue
 200  continue

      nass = n

 210  continue

c     Set remaining elements of the association array to zero
c     -------------------------------------------------------

      if (nass.le.nasslt) then
        nass1 = nass + 1
        do 220 n = nass1,nasslt
          kpn(n) = 0
          kcn(n) = 0
          qn(n)  = 0.
 220    continue
      endif

      return
      end
