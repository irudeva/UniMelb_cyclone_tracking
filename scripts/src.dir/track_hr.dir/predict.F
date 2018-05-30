      Subroutine predict(iowtst)

c       This routine predicts a new cyclone position based on a 
c     weighting of displacements calculated from climatological
c     average velocities and from previous movement.  Cyclone
c     central pressures are calculated from a weighting of
c     persistence and previous tendency.

c     Written by Ross Murray.
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

      parameter (c0=0.,c1=1.,c2=2.,r2=0.5,c90=90.,c180=180.,c360=360.)
      parameter (c1440=1440.)
      parameter (pi=3.1415926535898,p5pi=0.5*pi)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      logical secondt,wstatdet,nomove,diag2,diag3
      real mdtr,ndtr

      scos(xx,yy,zz) = acos( cos(xx)*sin(yy)*sin(zz) + cos(yy)*cos(zz) )
      stan(xx,yy,zz) = atan2(sin(xx),(sin(zz)/tan(yy)-cos(zz)*cos(xx)))
      angd(xx) = amod(xx+c1440,c360)

c-----------------------------------------------------------------------

      ierr = 0

      secondt = (ic.eq.2)
      if (secondt) mdt = 0.

      diag2 = idiagt.ge.2
      diag3 = idiagt.ge.3

      if (diag2) write (6,'(/'' PREDICTED POSITIONS ('',i6,x,i4,
     * '').''/)') dac,hrc

      wstatdet = ((wdata).and.(mdt.gt.0.).and.(.not.secondt).
     * and.(wmotn.gt.-0.001))

      if (refdt.gt.0.) then
        mdtr = mdt/refdt
        ndtr = ndt/refdt
      else
        mdtr = mdt
        ndtr = ndt
      endif

c-----------------------------------------------------------------------
c     Predicted pressures
c-----------------------------------------------------------------------

c     Pressure tendency factor
c     ------------------------
c     
      if ((secondt).or.(wpten.eq.0.)) then
        wPmn = 0.
      else
        if (refdt.gt.0.) then
          wPmmdt = wpten**(-mdtr)
          if (wPmmdt.lt.1.1) then
            wPmn = ndtr/mdtr*(1. + (ndtr + mdtr)*0.5*log(wpten))
          else
            wPmn = (1. - wpten**ndtr)/(wPmmdt - 1.)
          endif
        else
          wPmn = wpten
        endif
      endif
      if (wPmn.gt.3.) wPmn = 3.

      if (diag2) write (6,20) refdt,ic,mdt,ndt,mdtr,ndtr,
     * wpten,wPmn,wmotn,iadvtp
   20 format (5x,'refdt = ',f7.4,5x,'ic    = ',i7/
     *        5x,'mdt   = ',f7.4,5x,'ndt   = ',f7.4/
     *        5x,'mdtr  = ',f7.4,5x,'ndtr  = ',f7.4/
     *        5x,'wpten = ',f7.4,5x,'wPmn  = ',f7.4/
     *        5x,'wmotn = ',f7.4,5x,'iadvtp= ',i7)

c     Predicted pressure calculation
c     ------------------------------

      do 40 k = 1,nkb
        if ((wPmn.eq.0.).or.(kab(k).eq.0)) then
          pp(k) = pb(k)
        else
          pp(k) = pb(k) + wPmn*(pb(k) - pa(k))
        endif
   40 continue

c-----------------------------------------------------------------------
c     Prediction of no movement
c-----------------------------------------------------------------------

      if (((wmotn.lt.-0.001).or.((iadvtp.eq.0).and.(secondt.or.
     * (wmotn.eq.0.)))).and.(wsteer.eq.0.)) then
        if (diag2) write (6,'(/a)') ' No motion predicted.'
        do 50 k = 1,nkb
          xp(k) = xb(k)
          yp(k) = yb(k)
   50   continue
        return
      endif

c-----------------------------------------------------------------------
c     Precalculations for prediction of movements
c-----------------------------------------------------------------------

c     Precalculations
c     ---------------

      wML = 1. - wsteer
      if ((secondt).or.(wmotn.lt.0.01).or.(wML.eq.0.)) then
        wM  = 0.
        wMmn = 0.
      else
        wM = wmotn
        if (iadvtp.eq.0) wM = 1.
        if ((wM.eq.1.).or.(ndtr.eq.0)) then
          wMmn = 1.
        else
          if (refdt.gt.0.) then
            wMmmdt = wM**(-mdtr)
            if (wMmmdt.lt.1.1) then
              wMmn = 1. + (ndtr + mdtr)*0.5*log(wM)
            else
              wMmn = mdtr/ndtr*(1. - wM**ndtr)/(wMmmdt - 1.)
            endif
          else
            wMmn = wmotn
          endif
        endif
      endif

      if (secondt .or. mdt.eq.0.) then
        factM = 0.
        factL = ndt
      else
        factM = ndt/mdt
        factL = ndt*(1. - wMmn)
      endif

      uconv = 1./1.286      ! 1 deg.lat./day = 1/1.286 m./s.
      factS = ndt*wsteer*fsteer*(1.-asteer)*uconv

      if (diag2) write (6,60) wM,wMmn,wsteer,wML,uconv,factS,
     * factM,factL
   60 format (5x,'wM    = ',f7.4,5x,'wMmn  = ',f7.4/
     *        5x,'wsteer= ',f7.4,5x,'wML   = ',f7.4/
     *        5x,'uconv = ',f7.4,5x,'factS = ',f7.4/
     *        5x,'factM = ',f7.4,5x,'factL = ',f7.4)
      if (pstrak) then
        scalt0  = 2.*rad/rproj
        if (diag2) write (6,61) xcen,ycen,rproj,scalt0
   61   format (5x,'xcen  = ',f7.4,5x,'ycen  = ',f7.4/
     *          5x,'rproj = ',f7.4,5x,'scalt0= ',f7.4)
      endif

c-----------------------------------------------------------------------
c     Predicted positions and pressures
c-----------------------------------------------------------------------

      if (diag2) then
        write (6,70)
   70   format (/'  k   xa(k)  xb(k)   dxM   dxL   dxS   dxK  xp(k) ',
     *              '  ya(k)  yb(k)   dyM   dyL   dyS   dyK  yp(k) ',
     *              '  pa(k)  pb(k)  dp(k)  pp(k)')
        if (wstatdet) write (6,80)
   80   format ('   xnM  ynM  dxKM  dyKM   xM    yM',
     *          '   xnL  ynL  dxKL  dyKL   xL    yL     pM  ')
        write (6,*) ' '
      endif

      if (wstatdet) 
     * write (iowtst) dac,hrc,nkb,xcen,ycen,rproj,projn

      do 200 k = 1,nkb

        if (diag3) write (6,'(/a,i3)') ' Cyclone ',k

c       Prediction velocities based on position
c       ---------------------------------------

        if (iadvtp.ge.1) then
          call intdx(xb(k),yb(k),uLb,vLb,iadvuvb)
        else
          iadvuvb = 0
        endif

c       Increment based on previous movement
c       ------------------------------------

        if (.not.pstrak) phidb = p5pi - rrad*yb(k)
        nomove = ((kab(k).eq.0).or.(secondt).or.(wMmn.eq.0.))
        if (nomove) then
          dxM = 0.
          dyM = 0.
        else
          wMmnk =  wMmn
          if (iadvuvb.eq.0) wMmnk = 1.
          factMw = factM*wMmnk
          if (pstrak) then
            xab = xb(k) - xa(k)
            yab = yb(k) - ya(k)
          else
            dlamab = rrad*(xb(k) - xa(k))
            phida = p5pi - rrad*ya(k)
            chiba = scos(dlamab,phida,phidb)
            theba = stan(dlamab,phida,phidb)
            xab =  rad*chiba*sin(theba)
            yab = -rad*chiba*cos(theba)
            if (diag3) then
              write (6,'(20a8)') 'xb(k)','xa(k)','dlamab','ya(k)',
     *         'yb(k)','chiba','theba','xab','yab'
              write (6,'(20f8.3)') xb(k),xa(k),dlamab*rad,ya(k),
     *         yb(k),chiba*rad,theba*rad,xab,yab
            endif
          endif
          dxM = factMw*xab*wML
          dyM = factMw*yab*wML
        endif

c       Increment based on climatological movement
c       ------------------------------------------

        if (((wMmn.eq.1.).and.(.not.nomove)).or.(iadvuvb.eq.0)
     *   .and.(wML.ne.0.)) then
          dxL = 0.
          dyL = 0.
        else if (nomove) then
          dxL = uLb*ndt*wML
          dyL = vLb*ndt*wML
        else
          dxL = factL*uLb*wML
          dyL = factL*vLb*wML
        endif

c       Increment based on steering velocity
c       ------------------------------------

        if (wsteer.gt.0.) then
          if (pstrak) then
            xs = xb(k) - xcen
            ys = yb(k) - ycen
            rssq = xs*xs + ys*ys
            rs = sqrt(rssq)
            sinthe = -fhem*xs/rs
            costhe = -fhem*ys/rs
            upbx =  upb(k)*costhe + vpb(k)*sinthe
            vpby = -upb(k)*sinthe + vpb(k)*costhe
            rprojsq = rproj*rproj
            scalt = scalt0*rprojsq/(rprojsq + rssq)
            factSsc = factS/scalt
            dxS = upbx*factSsc
            dyS = vpby*factSsc
            if (diag3) then
              write (6,'(20a8)') 'ndt','wsteer','fsteer','scalt',
     *         '1-asteer','factS','factSsc'
              write (6,'(20f8.3)') ndt,wsteer,fsteer,scalt,
     *         1.-asteer,factS,factSc
              write (6,'(20a8)') 'xb(k)','yb(k)','xs','ys',
     *         'the','upb(k)','vpb(k)','upbx','vpby','dxS','dyS'
              write (6,'(20f8.3)') xb(k),yb(k),xs,ys,
     *         atan2(xs,ys)*rad,upb(k),vpb(k),upbx,vpby,dxS,dyS
            endif
          else
            dxS = upb(k)*factS
            dyS = vpb(k)*factS
            if (diag3) then
              write (6,'(20a8)') 'ndt','wsteer','fsteer','factS',
     *         'upb(k)','vpb(k)','dxS','dyS'
              write (6,'(20f8.3)') ndt,wsteer,fsteer,factS,
     *         upb(k),vpb(k),dxS,dyS
            endif
          endif
        else
          dxS = 0.
          dyS = 0.
        endif

c       Predicted position from sum of 1st order terms
c       ----------------------------------------------

        if (pstrak) then
          xn = xb(k) + dxM + dxL + dxS
          yn = yb(k) + dyM + dyL + dyS
          if (diag3) then
            write (6,'(20a8)') 'uLb','dxM','dxL','dxS','xn',
     *                         'vLb','dyM','dyL','dyS','yn'
            write (6,'(20f8.3)') uLb,dxM,dxL,dxS,xbn,
     *                           vLb,dyM,dyL,dyS,ybn
          endif
        else
          xbn = dxM + dxL + dxS
          ybn = dyM + dyL + dyS
          if (diag3) then
            write (6,'(20a8)') 'uLb','dxM','dxL','dxS','xbn',
     *                         'vLb','dyM','dyL','dyS','ybn'
            write (6,'(20f8.3)') uLb,dxM,dxL,dxS,xbn,
     *                           vLb,dyM,dyL,dyS,ybn
          endif
        endif

c       Second order increment based on previous movement
c       -------------------------------------------------

C       Rotate dxM,dyM to allow for curvature of great circles on PS
C       based on provisional positions (xbn,ybn). NOT YET IMPLEMENTED
C
C       if (pstrak) then
C         if (nomove) then
C           dxC = 0.
C           dyC = 0.
C         else
C           dxC = factM*wMmnk* ....
C           dyC = factM*wMmnk* ....
C         endif
C       endif

c       Second order increment based on climatological movement
c       -------------------------------------------------------

        dxK = 0.
        dyK = 0.
        if (iadvuvb.eq.1) then
          if (pstrak) then
            call intdx(xn,yn,uLn,vLn,iadvuvp)
          else
            chibn = rrad*sqrt(xbn*xbn + ybn*ybn)
            thebn = atan2(xbn,ybn)
            alonn = angd(xb(k) + rad*stan(thebn,chibn,phidb))
            phidn = scos(thebn,chibn,phidb)
            alatn = 90. - rad*phidn
            call intdx(alonn,alatn,uLnn,vLnn,iadvuvp)
            dlambn = rrad*(alonn-xb(k))
            thenb = stan(dlambn,phidb,phidn)
            betnb = pi - thenb - thebn
            cosbetnb = cos(betnb)
            sinbetnb = sin(betnb)
            uLn = uLnn*cosbetnb - vLnn*sinbetnb
            vLn = uLnn*sinbetnb + vLnn*cosbetnb
            if (diag3) then
              write (6,'(20a8)') 'xb','yb','xbn','ybn','chibn',
     *         'thebn','alonn','alatn','uLnn','vLnn'
              write (6,'(20f8.3)') xb(k),yb(k),xbn,ybn,chibn*rad,
     *         thebn*rad,alonn,alatn,uLnn,vLnn
              write (6,'(20a8)') 'thenb','betnb','uLn','vLn'
              write (6,'(20f8.3)') thenb*rad,betnb*rad,uLn,vLn
            endif
          endif

          if (iadvuvp.eq.0) then
            dxK = 0.
            dyK = 0.
          else
            hndt = 0.5*ndt
            dxK = hndt*(uLn-uLb)
            dyK = hndt*(vLn-vLb)
            if (.not.nomove) then
              if (pstrak) then
                call intdx(xa(k),ya(k),uLa,vLa,iadvuva)
              else
                call intdx(xa(k),ya(k),uLaa,vLaa,iadvuva)
                theab = stan(dlamab,phidb,phida)
                betab = -(pi - theab - theba)
                cosbetab = cos(betab)
                sinbetab = sin(betab)
                uLa = uLaa*cosbetab - vLaa*sinbetab
                vLa = uLaa*sinbetab + vLaa*cosbetab
                if (diag3) then
                  write (6,'(20a8)') 'xa(k)','ya(k)','uLaa','vLaa',
     *             'theab','betab','uLa','vLa'
                  write (6,'(20f8.3)') xa(k),ya(k),uLaa,vLaa,theab,
     *             betab,uLa,vLa
                endif
              endif
              if (iadvuva.eq.1) then
                dxK = dxK +hndt*wMmn*(uLb-uLa)
                dyK = dyK +hndt*wMmn*(vLb-vLa)
              endif
            endif
          endif
          dxK = dxK*wML
          dyK = dyK*wML
        endif

c       Final predicted position
c       ------------------------

        if (pstrak) then
          xp(k) = xn + dxK     ! + dxC
          yp(k) = yn + dyK     ! + dyC
        else
          xbp = xbn + dxK
          ybp = ybn + dyK
          chibp = rrad*sqrt(xbp*xbp + ybp*ybp)
          thebp = atan2(xbp,ybp)
          xp(k) = angd(xb(k) + rad*stan(thebp,chibp,phidb))
          yp(k) = 90.        - rad*scos(thebp,chibp,phidb)
          if (diag3) then
            write (6,'(20a8)') 'xbn','dxK','xbp',
     *       'ybn','dyK','ybp'
            write (6,'(20f8.3)') xbn,dxK,xbp,ybn,dyK,ybp
            write (6,'(20a8)') 'xb(k)','yb(k)','chibp','thebp',
     *       'xp(k)','yp(k)'
            write (6,'(20f8.3)') xb(k),yb(k),chibp*rad,thebp*rad,
     *       xp(k),yp(k)
            write (6,*) ' '
          endif
        endif
        if (diag3) write (6,*) ' '

c       Summary
c       -------

        if (diag2) then
          if (pstrak) then
            write (6,120) k,
     *       xa(k),xb(k),dxM,dxL,dxS,dxK,xp(k),
     *       ya(k),yb(k),dyM,dyL,dyS,dyK,yp(k),
     *       pa(k),pb(k),pp(k)-pb(k),pp(k)
 110        format (i3,2x,2(7(f5.2,1x),1x),4(1x,f6.1))
            call pstoll(xcen,ycen,rproj,fhem,xa(k),ya(k),
     *       alona,alata)
            call pstoll(xcen,ycen,rproj,fhem,xb(k),yb(k),
     *       alonb,alatb)
            call pstoll(xcen,ycen,rproj,fhem,xp(k),yp(k),
     *       alonp,alatp)
            xs = xb(k) - xcen
            ys = yb(k) - ycen
            rssq = xs*xs + ys*ys
            rs = sqrt(rssq)
            sinlam = -fhem*xs/rs
            coslam = -fhem*ys/rs
            rprojsq = rproj*rproj
            scalt = scalt0*rprojsq/(rprojsq + rssq)
            dxMr = (dxM*coslam - dyM*sinlam)*scalt
            dyMr = (dxM*sinlam + dyM*coslam)*scalt
            dxLr = (dxL*coslam - dyL*sinlam)*scalt
            dyLr = (dxL*sinlam + dyL*coslam)*scalt
            dxSr = (dxS*coslam - dyS*sinlam)*scalt
            dySr = (dxS*sinlam + dyS*coslam)*scalt
            dxKr = (dxK*coslam - dyK*sinlam)*scalt
            dyKr = (dxK*sinlam + dyK*coslam)*scalt
            write (6,120) k,
     *       alona,alonb,dxMr,dxLr,dxSr,dxKr,alonp,
     *       alata,alatb,dyMr,dyLr,dySr,dyKr,alatp,
     *       pa(k),pb(k),pp(k)-pb(k),pp(k)
            write (6,*) ' '
          else
            write (6,120) k,
     *       xa(k),xb(k),dxM,dxL,dxS,dxK,xp(k),
     *       ya(k),yb(k),dyM,dyL,dyS,dyK,yp(k),
     *       pa(k),pb(k),pp(k)-pb(k),pp(k)
 120        format (i3,1x,2(2f7.2,4f6.2,f7.2,
     *       1x),4(1x,f6.1))
          endif
        endif

c       Calculations for weighting statistics
c       -------------------------------------

        if (wstatdet.and.pstrak) then
          if (nomove) then
            write (iowtst) k,iopb(k),0,0.,0.,0.,0.,0.,0.,0.,0.,0.
          else
            xnM = xb(k) + ndt/mdt*(xb(k) - xa(k))
            ynM = yb(k) + ndt/mdt*(yb(k) - ya(k))
            call intdx(xnM,ynM,uLpM,vLpM,iadvuvpM)
            call intdx(xa(k),ya(k),uLa,vLa,iadvuva)
            if ((iadvuvb*iadvuvpM*iadvuva.eq.1)) then
              dxKM = hndt*(uLpM - uLa)
              dyKM = hndt*(vLpM - vLa)
            else
              dxKM = 0.
              dyKM = 0.
            endif
            xM   = xnM + dxKM
            yM   = ynM + dyKM

            xnL = xb(k) + ndt*uLb
            ynL = yb(k) + ndt*vLb
            call intdx(xnL,ynL,uLpL,vLpL,iadvuvpL)
            if (iadvuvb*iadvuvpL.eq.1) then
              dxKL = hndt*(uLpL - uLb)
              dyKL = hndt*(vLpL - vLb)
            else
              dxKL = 0.
              dyKL = 0.
            endif
            xL = xnL + dxKL
            yL = ynL + dyKL
            pM = pb(k) + ndt/mdt*(pb(k) - pa(k))
            if (diag2) write (6,'(2x,12f6.1,f8.1)') xnM,ynM,
     *       dxKM,dyKM,xM,yM,xnL,ynL,dxKL,dyKL,xL,yL,pM
            write (iowtst) k,iopb(k),1,xM,yM,xL,yL,xp(k),yp(k),pb(k),
     *       pM,pp(k)
          endif
        endif

 200  continue

      return
      end
