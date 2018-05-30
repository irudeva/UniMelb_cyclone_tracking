      Subroutine update(nwhist)

c       This routine updates the track history files after each time
c     step of the tracking programme "track_z".

c     thist1.1     File contains summary of tracking to date
c     thist2.[12]  Alternating files for writing track histories of
c                    completed track.  Last used file transferred to
c                    thist2.1 at the end of tracking.
c     thist3.[12]  Alternating files for writing track histories of
c                    current tracks (all tracks from the oldest track
c                    still unfinished, including subsequent completed
c                    tracks).  Last used file transferred to thist3.1
c                    at the end of tracking.

c     Written by Ross Murray.
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
C     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      integer trk,trkdash,statf,statl
crmw      integer fseek
      logical firstt,diag1,diag5
      character thist3a*10,thist3b*10,thist1b*10
      character trform*11
      logical ltrue,lfals
      data ltrue,lfals/.true.,.false./
      logical nwhist

c-----------------------------------------------------------------------
c     Switches
c-----------------------------------------------------------------------

      ierr = 0

      firstt = (ic.eq.1)
      diag1  = (idiagt.ge.1)
      diag5  = (idiagt.ge.5)

      ihzb = 1
      if (ihza.eq.1) ihzb = 2
      write (thist1b,'(''thist1.'',i1)') ihzb
      write (thist3a,'(''thist3.'',i1)') ihza
      write (thist3b,'(''thist3.'',i1)') ihzb

      trform = 'unformatted'
      if (trfm) trform = 'formatted'

c-----------------------------------------------------------------------
c     First tracking period (no additions to histories)
c-----------------------------------------------------------------------

      if (firstt) then
        if (diag5) write (6,20) dac,hrc,dac,hrc,tc,tc,1,0,0,0,0
 20     format (/' HISTORY FILE : ',i6,1x,i4,' to ',i6,1x,i4,
     *   ' (t = ',f7.2,',',f7.2,'; i = 1,',i3,')'/
     *   '     (lstrk1 = ',i4,', lstrk2 = ',i4,
     *   ', mdt = ',f7.3,', nkb = ',i2,').')
        if (nwhist) then
          open (unit=26,file=thist3b,status='unknown',form=trform)
        else
          open (unit=26,file=thist3b,status='new',form=trform,err=250)
        endif
        ic = 1
        trk = 0
        lstrk1b = 0
        lstrk2b = 0
        go to 140
      endif

c-----------------------------------------------------------------------
c     Additions to existing tracks
c-----------------------------------------------------------------------

c     Reading and checking of old accounting file
c     -------------------------------------------

      if (diag5) write (6,20) da1,hr1,dac,hrc,t1,tc,ic,lstrk1,
     * lstrk2,mdt,nkb

      lstrk1b = lstrk1
      lstrk2b = lstrk2
      if (lstrk2.lt.lstrk1) then 
        write (6,'(/'' lstrk2 ('',i5,'') < lstrk1 ('',i5,''.'')
     *   ') lstrk2,lstrk1
        stop ' update'
      endif
      if (lstrk2.eq.lstrk1) then
        trk = lstrk2b
        open (unit=26,file=thist3b,status='unknown',form=trform)
        go to 140
      endif

c     Checking of status of later tracks
c     ----------------------------------

      ihf = 25
      open (unit=24,file=thist3a,status='unknown',form=trform)
      open (unit=26,file=thist3b,status='unknown',form=trform)
      if (lstrk1.eq.0) then
        if (nwhist) then
          open (unit=25,file='thist2.1',status='unknown',form=trform)
        else
          open (unit=25,file='thist2.1',status='new',form=trform,
     *     err=260)
        endif
      else
        open (unit=25,file='thist2.1',status='old',form=trform,err=270)
crmw        ifs = fseek(25,0,2)
crmw        if (ifs.ne.0) stop ' update: fseek unsuccessful.'
         call fseek(25,0,2)
crmw         if (ifs.ne.0) stop ' update: fseek unsuccessful.'
      endif

      do 130 trkdash = lstrk1+1,99999

c       Read track history
c       ------------------

        call tr2rd(24,trfm,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z,ierd)
        if (ierd.eq.1) go to 135
        if (ierd.eq.2) go to 240
        if (ierd.eq.3) then
          write (6,*) ' ', thist3a
          stop ' update: Not a track history auxiliary file.'
        endif
        if ((itabt3z.ne.itabt3).or.(itabt4z.ne.itabt4)) then
          write (6,*) ' Tabulation styles differ:'
          write (6,*) '   prescribed itabt  = ',itabt3,itabt4
          write (6,*) '   read       itabtz = ',itabt3z,itabt4z
          stop ' update'
        endif
        if (trkdash.ne.trk) write (6,*) ' Warning: trk (counted) = ',
     *   trkdash,', trk (read) = ',trk

        call tr3rd(24,trfm,lfals,itabt3,itabt4,nvarlt,nitlt,
     * nit,vart,ierd)
        if (ierd.eq.1) go to 135
        if (ierd.eq.2) go to 241
        if (statl.le.2) go to 100

c       Additions to unfinished tracks.
c       -------------------------------

        kb = kt(nit)
        kc = kcb(kb)

        if (kc.eq.0) then
          statl       = 2
          stat2t(nit) = 2
        else if (nit.eq.nitlt) then
          statl       = 2
          stat2t(nit) = 2
          kbc(kc)      = 0
          write (6,*) ' Track ',trk,' age = ',nitlt,
     *     ', New position assigned to new track.'
          write (6,*) ' it = ',nitlt,' k = ',kb,' at (',xt(nit),
     *     ',',yt(nit),')'
          write (6,*) ' it = ',1,' k = ',kc,' at (',xc(kc),
     *     ',',yc(kc),')'
        else
          ihf = 26
          if (kc.lt.0) then
            statl     = 1
            kc        = -kc
          else
            statl     = 3
          endif
          ilst        = ic
          itb         = nit
          nit         = nit + 1
          tt(nit)     = tc
          dat(nit)    = dac
          hrt(nit)    = hrc
          stat2t(itb) = 4
          stat1t(nit) = 4
          stat2t(nit) = statl
          kt(nit)     = kc
          iopt(nit)   = iopc(kc)
          if (pstrak) then
            call pstoll(xcen,ycen,rproj,fhem,xc(kc),yc(kc),
     *       xt(nit),yt(nit))
          else
            xt(nit)   = xc(kc)
            yt(nit)   = yc(kc)
          endif
          pt(nit)     = pc(kc)
          ct(nit)     = cc(kc)
          qt(nit)     = qa(kc)
          upt(nit)    = upc(kc)
          vpt(nit)    = vpc(kc)
c         cxt(nit)    = cxc(kc)
          rdt(nit)    = rdc(kc)
          dpt(nit)    = dpc(kc)
          idalst      = dac
          ihrlst      = hrc
        endif

 100    continue

c       Writing of updated histories to file.
c       -------------------------------------

        call tr2wr(ihf,trfm,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
        call tr3wr(ihf,trfm,itabt3,itabt4,nvarlt,nitlt,nit,vart)
        if (diag5) then
          call tr2wr(6,ltrue,trk,statf,statl,ifst,ilst,nit,
     *     idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          call tr3wr(6,ltrue,itabt3,itabt4,nvarlt,nitlt,nit,vart)
        endif

        if (ihf.eq.25) lstrk1b = trk
        lstrk2b = trk
 130  continue
 135  continue

      close (unit=24,status='keep')

c-----------------------------------------------------------------------
c     New tracks
c-----------------------------------------------------------------------

 140  continue

      do 200 kc = 1,nkc

        kb = kbc(kc)
        if (kb.gt.0) go to 195

c       Status of new tracks.
c       ---------------------

        trk         = trk + 1
        if (kb.lt.0) then
          statf     = 1
          kb        = -kb
          nit       = 2
          ifst      = ib
          tt(1)     = tb
          dat(1)    = dab
          hrt(1)    = hrb
          stat1t(1) = 1
          stat2t(1) = 4
          stat1t(2) = 4
          stat2t(2) = 3
          kt(1)     = kb
          iopt(1)   = iopc(kb)
          if (pstrak) then
            call pstoll(xcen,ycen,rproj,fhem,xc(kb),yc(kb),
     *       xt(1),yt(1))
          else
            xt(1)   = xc(kb)
            yt(1)   = yc(kb)
          endif
          pt(1)     = pc(kb)  
          ct(1)     = cc(kb)  
          qt(1)     = qa(kb)
          upt(1)    = upc(kb)
          vpt(1)    = vpc(kb)
c         cxt(1)    = cxc(kb)
          rdt(1)    = rdc(kb)
          dpt(1)    = dpc(kb)
        else
          if (firstt) then
            statf   = 3
          else
            statf   = 2
          endif
          nit       = 1 
          statl     = 3
          ifst      = ic
          stat1t(1) = statf
          stat2t(1) = 3 
        endif 
        ilst      = ic
        tt(nit)   = tc
        dat(nit)  = dac
        hrt(nit)  = hrc
        kt(nit)   = kc
        iopt(nit) = iopc(kc)
        if (pstrak) then
          call pstoll(xcen,ycen,rproj,fhem,xc(kc),yc(kc),
     *     xt(nit),yt(nit))
        else
          xt(nit)  = xc(kc)
          yt(nit)  = yc(kc)
        endif
        pt(nit)    = pc(kc)  
        ct(nit)    = cc(kc)  
        qt(nit)    = 0.
        upt(nit)   = upc(kc)
        vpt(nit)   = vpc(kc)
c       cxt(nit)   = cxc(kc)
        rdt(nit)   = rdc(kc)
        dpt(nit)   = dpc(kc)

c       Writing of details of new tracks to file.
c       -----------------------------------------

        call tr2wr(26,trfm,trk,statf,statl,ifst,ilst,nit,
     *   dat(1),hrt(1),dac,hrc,itabt3,itabt4)
        call tr3wr(26,trfm,itabt3,itabt4,nvarlt,nitlt,nit,vart)
        if (diag5) then
          call tr2wr(6,ltrue,trk,statf,statl,ifst,ilst,nit,
     *     dat(1),hrt(1),dac,hrc,itabt3,itabt4)
          call tr3wr(6,ltrue,itabt3,itabt4,nvarlt,nitlt,nit,vart)
        endif

        lstrk2b = trk
 195  continue
 200  continue

      lstrk1 = lstrk1b
      lstrk2 = lstrk2b

c-----------------------------------------------------------------------
c     Updating of content file and closure of files.
c-----------------------------------------------------------------------

c     Write content file (unit 21)
c     ----------------------------

      open (unit=21,file=thist1b,status='unknown',form=trform)
      call tr1wr(21,19,trfm,trnml)
      close (unit=21,status='keep')
      if (diag1) write (6,'(/3a)') ' Updated to ',thist1b(1:8),'.'

      if (lstrk1.eq.0) then
        close (unit=25,status='delete',err=210)
      else
        close (unit=25,status='keep',err=210)
        if (diag1) write (6,*) ' Updated to thist2.1: tracks ',
     *   1,'-',lstrk1,'.'
      endif
 210  continue

c     Close track data logical units
c     ------------------------------

      if (lstrk2.eq.lstrk1) then
        close (unit=26,status='delete',err=220)
      else
        close (unit=26,status='keep',err=220)
        ifstrk2 = 1 + lstrk1
        if (diag1) write (6,*) ' Updated to ',thist3b(1:8),
     *   ': tracks ',ifstrk2,'-',lstrk2,'.'
      endif
 220  continue

      return

 240  write (6,*) ' Error/premature end in track history',
     * ' auxiliary file: ',thist3a,'  (tr2rd)'
      stop ' update'
 241  write (6,*) ' Error in reading track history auxiliary file: ',
     * thist3a,'  (tr3rd)'
      stop ' update'
 250  write (6,*) ' History file ',thist3b,' exists.'
      stop ' update'
 260  stop ' update: History file thist2.1 exists.'
 270  stop ' update: History file thist2.1 not found.'
      
      end
