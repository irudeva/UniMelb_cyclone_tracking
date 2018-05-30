      Subroutine match

c         This routine uses takes estimations of the probability
c     of association of predicted and actual cyclone positions
c     and matches them by an optimisation of these probabilities.

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

ckk      parameter (nasglt=13,nnglt=200)
      parameter (nasglt=800,nnglt=2000) ! ckk
      integer gtot,gpp,gcc,g,gold,gpk,gck,gp,gc
      integer statn,statnn,statg,statp,statc
      integer flagp,flagc,flagpp,flagcc
      dimension gp(0:nklt),gc(0:nklt),
     * kpasg(0:nklt),kcasg(0:nklt),nasg(0:nklt),nng(nnglt,nklt),
     * statn(nasslt),
crmw     * statnn,
     * statg(0:nklt),
     * statp(nklt),
     * statc(nklt),
     * flagp(nklt),flagc(nklt)

c-----------------------------------------------------------------------
c     Preliminary settings
c-----------------------------------------------------------------------

      if (newtks) then
        do 10 k = 1,nklt
          kcb(k) = 0
          kbc(k) = 0
          qa(k) = 0.
          qf(k) = 0.
 10     continue
        return
      endif

      kasglt = nsort + irevmx
      ierr = 0

      if (idiagt.ge.2) then
        write (6,179) dac,hrc
 179    format (/' SYSTEM MATCHES: ',i6,i4,'.')
      endif

      if (merget.ne.0) write (6,'(/'' Subroutine match not yet'',
     * ''programmed to merge systems.''/'' Execution terminated.''/)')

c-----------------------------------------------------------------------
c     Grouping of associated predicted & new positions.
c-----------------------------------------------------------------------

      do 20 k = 0,nklt
        gp(k) = 0
        gc(k) = 0
 20   continue

      newg = 0

      do 100 n = 1,nass
        kp = kpn(n)
        kc = kcn(n)
        gpp = gp(kp)
        gcc = gc(kc)

        if (gpp.eq.0) then
          if (gcc.eq.0) then
            newg = newg + 1
            g = newg
            gp(kp) = g
            gc(kc) = g
            go to 90
          endif
            gp(kp) = gcc
            go to 90
        endif
          if (gcc.eq.0) then
            gc(kc) = gpp
            go to 90
        endif

        if (gpp.eq.gcc) go to 90

        if (gcc.gt.gpp) then
          g    = gp(kp)
          gold = gc(kc)
        else
          g    = gc(kc)
          gold = gp(kp)
        endif

        do 50 kp = 1,nkb
          gpk = gp(kp)
          if (gpk.ge.gold) then
            if (gpk.eq.gold) then
              gp(kp) = g
            else
              gp(kp) = gp(kp) - 1
            endif
          endif
 50     continue

        do 60 kc = 1,nkc
          gck = gc(kc)
          if (gck.ge.gold) then
            if (gck.eq.gold) then
              gc(kc) = g
            else
              gc(kc) = gc(kc) - 1
            endif
          endif
 60     continue

        newg = newg - 1
 90     continue
 100  continue

      gtot = newg

c-----------------------------------------------------------------------
c     Determination of group sizes, reference numbers, and
c     status variables.
c-----------------------------------------------------------------------

      do 140 g = 0,gtot
        kpasg(g) = 0
        kcasg(g) = 0
        nasg(g)  = 0
        statg(g) = 0
 140  continue

      do 150 kp = 1,nkb
        statp(kp) = 0
        g = gp(kp)
        kpasg(g) = kpasg(g) + 1
 150  continue

      do 180 kc = 1,nkc
        statc(kc) = 0
        g = gc(kc)
        kcasg(g) = kcasg(g) + 1
 180  continue

      do 170 n = 1,nass
        kp = kpn(n)
        kc = kcn(n)
        g  = gp(kp)
        nasg(g) = nasg(g) + 1
        nasgg = nasg(g)
        nng(nasgg,g) = n
 170  continue

      do 175 n = 1,nass
        kp = kpn(n)
        kc = kcn(n)
        g = gp(kp)
        nasgg = nasg(g)
        statg(g) = 2
        if (nasgg.eq.1) statg(g) = 1
        statp(kp) = statg(g)
        statc(kc) = statg(g)
        statn(n)  = statg(g)
 175  continue

      if (idiagt.ge.3) then
        write (6,185)
 185    format (/'  n  kp  kc    g kpasgg kcasgg nasgg  statgg',
     *   '   ng     qn(n)'/)
        do 200 g = 1,gtot
          nasgg = nasg(g)
          do 200 ng = 1,nasgg
            n = nng(ng,g)
            if (ng.eq.1) then
              write (6,190) n,kpn(n),kcn(n),g,kpasg(g),
     *         kcasg(g),nasg(g),statg(g),ng,qn(n)
 190          format (i3,1x,i3,1x,i3,3x,i2,3x,5(i2,5x),f5.3)
            else
              write (6,195) n,kpn(n),kcn(n),ng,qn(n)
 195          format (i3,1x,i3,1x,i3,36x,i2,5x,f5.3)
            endif
 200    continue
      endif

c-----------------------------------------------------------------------
c     Sorting of predicted-new associations.
c-----------------------------------------------------------------------

      if (idiagt.ge.4) then
        write (6,206)
 206    format (/'  g   n1 n2 n3 n4 n5 n6     qn(n) values',
     *   15x,'qtot     qmax'/)
      endif

      do 380 g = 1,gtot

        irevis = 0

 207    continue

c       Sorting within each group.
c       --------------------------

        nasgg = nasg(g)
        if (nasgg.eq.1) go to 370
        if (nasgg.gt.nasglt) go to 218

        kpasgg = kpasg(g)
        kcasgg = kcasg(g)
        if (kpasgg.ge.kcasgg) then
          kasgmin = kcasgg
        else
          kasgmin = kpasgg
        endif

        if (kasgmin.gt.kasglt) go to 218

        if (idiagt.ge.4) then
          write (6,209) g
 209      format (i3)
        endif

        do 210 k = 1,nklt  
          flagp(k) = 0
          flagc(k) = 0
 210    continue

        qmax = 0.
        n1max = 0
        n2max = 0
        n3max = 0
        n4max = 0
        n5max = 0
        n6max = 0

      do 350 ng1 = 1,nasgg

        n1 = nng(ng1,g)
        qtot1 = qn(n1)
        qtot = qtot1

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = 0
          n3max = 0
          n4max = 0
          n5max = 0
          n6max = 0
        endif
        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
            write (6,211) n1,
     *       qn(n1),qtot,qmax
          else
            write (6,211) n1,
     *       qn(n1)
          endif
 211      format (5x,i3,20x,f5.3,21x,f5.3,4x,f5.3)
        endif

        if (kasgmin.lt.2) go to 340

        kp1 = kpn(n1)
        kc1 = kcn(n1)
        flagp(kp1) = 1
        flagc(kc1) = 1

        ng1p1 = ng1 + 1
      do 330 ng2 = ng1p1,nasgg

        n2 = nng(ng2,g)
        kp2 = kpn(n2)
        kc2 = kcn(n2)
        flagpp = flagp(kp2)
        flagcc = flagc(kc2)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 320
        if (nsort.eq.1) go to 218

        qtot2 = qn(n2) + qtot1
        qtot = qtot2

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = n2
          n3max = 0
          n4max = 0
          n5max = 0
          n6max = 0
        endif

        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
            write (6,212) n1,n2,
     *       qn(n2),qtot,qmax
          else
            write (6,212) n1,n2,
     *       qn(n2),qtot
          endif
 212            format (5x,2i3,20x,f5.3,18x,f5.3,4x,f5.3)
        endif

        if ((kasgmin.eq.2).or.(ng2.eq.nasgg)) go to 320

        flagp(kp2) = 1
        flagc(kc2) = 1

        ng2p1 = ng2 + 1
      do 310 ng3 = ng2p1,nasgg

        n3 = nng(ng3,g)
        kp3 = kpn(n3)
        kc3 = kcn(n3)
        flagpp = flagp(kp3)
        flagcc = flagc(kc3)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 300
        if (nsort.eq.2) go to 218

        qtot3 = qn(n3) + qtot2
        qtot = qtot3

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = n2
          n3max = n3
          n4max = 0
          n5max = 0
          n6max = 0
        endif

        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
            write (6,213) n1,n2,n3,
     *       qn(n3),qtot,qmax
          else
            write (6,213) n1,n2,n3,
     *       qn(n3),qtot
          endif
 213      format (5x,3i3,20x,f5.3,15x,f5.3,4x,f5.3)
        endif

        if ((kasgmin.eq.3).or.(ng3.eq.nasgg)) go to 300

        flagp(kp3) = 1
        flagc(kc3) = 1

        ng3p1 = ng3 + 1
      do 290 ng4 = ng3p1,nasgg

        n4 = nng(ng4,g)
        kp4 = kpn(n4)
        kc4 = kcn(n4)
        flagpp = flagp(kp4)
        flagcc = flagc(kc4)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 280
        if (nsort.eq.3) go to 218

        qtot4 = qn(n4) + qtot3
        qtot = qtot4

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = n2
          n3max = n3
          n4max = n4
          n5max = 0
          n6max = 0
        endif

        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
           write (6,214) n1,n2,n3,n4,
     *      qn(n4),qtot,qmax
          else
           write (6,214) n1,n2,n3,n4,
     *      qn(n4),qtot
          endif
 214      format (5x,4i3,20x,f5.3,12x,f5.3,4x,f5.3)
        endif

        if ((kasgmin.eq.4).or.(ng4.eq.nasgg)) go to 280

        flagp(kp4) = 1
        flagc(kc4) = 1

        ng4p1 = ng4 + 1
      do 270 ng5 = ng4p1,nasgg

        n5 = nng(ng5,g)
        kp5 = kpn(n5)
        kc5 = kcn(n5)
        flagpp = flagp(kp5)
        flagcc = flagc(kc5)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 260
        if (nsort.eq.4) go to 218

        qtot5 = qn(n5) + qtot4
        qtot = qtot5

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = n2
          n3max = n3
          n4max = n4
          n5max = n5
          n6max = 0
        endif

        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
            write (6,215) n1,n2,n3,n4,n5,
     *       qn(n5),qtot,qmax
          else
            write (6,215) n1,n2,n3,n4,n5,
     *       qn(n5),qtot
          endif
 215      format (5x,5i3,20x,f5.3,9x,f5.3,4x,f5.3)
        endif

        if ((kasgmin.eq.5).or.(ng5.eq.nasgg)) go to 260

        flagp(kp5) = 1
        flagc(kc5) = 1

        ng5p1 = ng5 + 1
      do 250 ng6 = ng5p1,nasgg

        n6 = nng(ng6,g)
        kp6 = kpn(n6)
        kc6 = kcn(n6)
        flagpp = flagp(kp6)
        flagcc = flagc(kc6)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 240
        if (nsort.eq.5) go to 218

        qtot6 = qn(n6) + qtot5
        qtot = qtot6

        if (qtot.gt.qmax) then
          qmax = qtot
          n1max = n1
          n2max = n2
          n3max = n3
          n4max = n4
          n5max = n5
          n6max = n6
        endif

        if (idiagt.ge.4) then
          if (qtot.eq.qmax) then
            write (6,216) n1,n2,n3,n4,n5,n6,
     *       qn(n6),qtot,qmax
          else
            write (6,216) n1,n2,n3,n4,n5,n6,
     *       qn(n6),qtot
          endif
 216      format (5x,6i3,20x,f5.3,6x,f5.3,4x,f5.3)
        endif

        if ((kasgmin.eq.6).or.(ng6.eq.nasgg)) go to 240

        flagp(kp6) = 1
        flagc(kc6) = 1

        ng6p1 = ng6 + 1
      do 230 ng7 = ng6p1,nasgg

        n7 = nng(ng7,g)
        kp7 = kpn(n7)
        kc7 = kcn(n7)
        flagpp = flagp(kp7)
        flagcc = flagc(kc7)
        if ((flagpp.eq.1).or.(flagcc.eq.1)) go to 225
        go to 218

c       Resetting of flags 
c       ------------------

 225    continue
 230  continue
        flagp(kp6) = 0
        flagc(kc6) = 0
 240    continue
 250  continue
        flagp(kp5) = 0
        flagc(kc5) = 0
 260    continue
 270  continue
        flagp(kp4) = 0
        flagc(kc4) = 0
 280    continue
 290  continue
        flagp(kp3) = 0
        flagc(kc3) = 0
 300    continue
 310  continue
        flagp(kp2) = 0
        flagc(kc2) = 0
 320    continue
 330  continue
        flagp(kp1) = 0
        flagc(kc1) = 0
 340    continue
 350  continue

c       Revision of status variables
c       ----------------------------

        if (n1max.gt.0) statn(n1max) = 1
        if (n2max.gt.0) statn(n2max) = 1
        if (n3max.gt.0) statn(n3max) = 1
        if (n4max.gt.0) statn(n4max) = 1
        if (n5max.gt.0) statn(n5max) = 1
        if (n6max.gt.0) statn(n6max) = 1

        do 360 n = 1,nass
          kp = kpn(n)
          kc = kcn(n)
          statnn = statn(n)
          if (statnn.eq.1) then
            statp(kp) = 1
            statc(kc) = 1
          else
            statn(n)  = 0
            if (statp(kp).ne.1) statp(kp) = 0
            if (statc(kc).ne.1) statc(kc) = 0
          endif
          statg(g) = 0
 360    continue

        go to 370

c       Artificial reduction of intractable groups and
c       revision of reference numbers and status variables.
c       ---------------------------------------------------

 218    continue

        if (irevis.ge.irevmx) then
          if (idiagt.ge.1) then
            write (6,219) g,irevis
          endif
 219      format (/' match: Group g = ',i2,' unsorted after',i2,
     *       ' sorting revisions.'/' Execution terminated.')
          write (6,185)
          nasgg = nasg(g)
          do 220 ng = 1,nasgg
            n = nng(ng,g)
            if (ng.eq.1) then
              write (6,190) n,kpn(n),kcn(n),g,kpasg(g),
     *         kcasg(g),nasg(g),statg(g),ng,qn(n)
            else
              write (6,195) n,kpn(n),kcn(n),ng,qn(n)
            endif
 220      continue
          ierr = 1
          return
        endif

        irevis = irevis + 1

        statg(g) = 2
        do 221 ng = 1,nasgg
          n = nng(ng,g)
          kp = kpn(n)
          kc = kcn(n)
          statp(kp) = 2
          statc(kc) = 2
          statn(n)  = 2
 221    continue

ckk          write (6,222) g
ckk 222      format (/' match: Exit sort of g = ',i2,
ckk     *       ': too many associated lows.')
          if (kasgmin.gt.kasglt) then
            write (6,*) 'kasgmin:',kasgmin,', kasglt: ',kasglt
            write (6,*) ' Execution terminated.'
            ierr = 1
            return
          endif

        qmax = 0.
        nasgg = nasg(g)
        do 372 ng = 1,nasgg
          n = nng(ng,g)
          q = qn(n)
          if (q.gt.qmax) then
            qmax = q
            nmax = n
          endif
 372      continue

        kpmax = kpn(nmax)
        kcmax = kcn(nmax)
        statn(nmax) = 1
        statp(kpmax) = 1
        statc(kcmax) = 1

        ngnew = 0
        do 375 ng = 1,nasgg
          n = nng(ng,g)
          kp = kpn(n)
          kc = kcn(n)
          if (n.eq.nmax) go to 374
          if ((statp(kp).eq.1).or.(statc(kc).eq.1)) then
            statn(n) = 0
            go to 374
          endif

          ngnew = ngnew + 1
          nng(ngnew,g) = n

 374      continue
 375    continue

        ngnew1 = ngnew + 1
        do 376 ng = ngnew1,nasgg
          nng(ng,g) = 0
 376    continue

        kpasg(g) = 0
        do 377 kp = 1,nkb
            if ((gp(kp).eq.g).and.(statp(kp).eq.2)) kpasg(g) = kpasg(g) 
     *       + 1
 377    continue

        kcasg(g) = 0
        do 378 kc = 1,nkc
          if ((gc(kc).eq.g).and.(statc(kc).eq.2)) kcasg(g) = kcasg(g)
     *       + 1
 378    continue

        nasg(g) = ngnew

        if (idet.ge.1) then
          write (6,381) nmax,kpn(nmax),kcn(nmax),nasg(g),kpasg(g),
     *     kcasg(g)
 381      format (/' nmax = ',i3,', kp    = ',i3,', kc    = ',i3/
     *             ' nasg = ',i3,', kpasg = ',i3,', kcasg = ',i3)
        endif

        if (idiagt.ge.3) then
          write (6,185)
          nasgg = nasg(g)
          do 383 ng = 1,nasgg
            n = nng(ng,g)
            if (ng.eq.1) then
              write (6,190) n,kpn(n),kcn(n),g,kpasg(g),
     *         kcasg(g),nasg(g),statg(g),ng,qn(n)
            else
              write (6,195) n,kpn(n),kcn(n),ng,qn(n)
            endif
 383      continue
        endif

        go to 207

 370    continue
 380  continue

c-----------------------------------------------------------------------
c     Sorting of matched associations.
c-----------------------------------------------------------------------

      do 490 k = 1,nklt
        kcb(k) = 0
        kbc(k) = 0
        qa(k) = 0.
        qf(k) = 0.
 490  continue

      do 500 n = 1,nass
        statnn = statn(n)
        kp = kpn(n)
        kc = kcn(n)
        if (statnn.eq.1) then
          kbc(kc) = kp
          kcb(kp) = kc
          qa(kc) = qn(n)
          qf(kp) = qn(n)
        endif
 500  continue

      if (idiagt.ge.2) then
        write (6,570) 
 570    format (/' kb   kcb      q       kc   kbc      q'/)

        nkmax = max0(nkb,nkc)
        do 600 k= 1,nkmax
          if (k.le.nkb) then
            if (k.le.nkc) then
              write (6,580) k,kcb(k),qf(k),k,kbc(k),qa(k)
            else
              write (6,580) k,kcb(k),qf(k)
 580          format (2(i3,2x,i3,5x,f5.3,4x))
            endif
          else
            write (6,590) k,kbc(k),qa(k)
 590        format (22x,i3,2x,i3,5x,f5.3)
          endif
 600    continue
      endif

      return
      end
