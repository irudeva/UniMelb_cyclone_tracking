      Subroutine lasthist

c       This routine reads a cyclone track history file and extracts 
c     cyclone positions and pressures from the last two analysis times.

c     Written by Ross Murray.
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

      integer trk,trkexp
      integer statf,statl
      character trform*11
      integer dmin,ddhm

c-----------------------------------------------------------------------
c     Instruction parameters 
c-----------------------------------------------------------------------

      ihza = 1

      trform = 'unformatted'
      if (trfmin) trform = 'formatted'

c     Read content file (unit 21)
c     ---------------------------

      write (6,*) ' Reading thist1.1'
      open (unit=21,file='thist1.1',status='old',form=trform,err=920)
      call tr1rdz(21,19,trfmin,trnmlin,idiagt,t1,da1,hr1,
     * ib,tb,dab,hrb,nkb,mdt,lstrk1,lstrk2,ierr)
      close (21)
c     if (idiagt.ge.1) then
c       write (6,'(/a/)') ' Parameters given in track programme.'
c       write (6,nmltrdata)
c     endif
      if (ierr.eq.1) go to 940
      if (ierr.eq.2) go to 950
      if (ierr.eq.3) stop ' lasthist: Not a thist1 type file.'

c     Resumption period - checking of dates
c     -------------------------------------

      if ((dastop.ne.0).or.(hrstop.ne.0)) then
        call timedif(dab,hrb,dastop,hrstop,dmode,
     *   dmin,ddhm,dday,itderr)
        if (itderr.gt.3) stop ' lasthist'
        if (ddhm.lt.ddhmt) then
          write (6,'(/'' Date/time for finish of tracking ('',
     *     i6,x,i4,'') is less than one tracking''/
     *     '' period after the last'',
     *     '' date/time ('',i6,x,i4,'') in the existing history''
     *     '' file.''/'' Execution terminated.''/)')
     *     dastop,hrstop,dab,hrb
          stop ' lasthist'
        endif
      endif
      
      if ((dastrt.ne.0).or.(hrstrt.ne.0)) then
        call timedif(dab,hrb,dastrt,hrstrt,
     *   dmode,dmin,ddhm,dday,itderr)
        if (itderr.gt.3) stop ' lasthist'
        if (ddhm.gt.ddhmmt) then
          write (6,'(/'' Date/time for resumption of tracking ('',
     *     i6,x,i4,'') is more than the maximum ''/
     *     '' tracking period after the last'',
     *     '' date/time ('',i6,x,i4,'') in the existing history''
     *     '' file.''/'' Execution terminated.''/)')
     *     dastrt,hrstrt,dab,hrb
          stop ' lasthist'
        endif
        if (ddhm.lt.ddhmt) then
          write (6,'(/'' Date/time for resumption of tracking ('',
     *     i6,x,i4,'') is earlier than''/'' one tracking interval'',
     *     '' after the last''/
     *     '' date/time ('',i6,x,i4,'') in the existing history''
     *     '' file.''/'' Tracking will resume one tracking period''
     *     '' after this date/time.''/)')
     *     dastrt,hrstrt,dab,hrb
        endif
      endif

c-----------------------------------------------------------------------
c     Extraction of cyclone data for times ta,tb.
c-----------------------------------------------------------------------

      write (6,'(/a/)') ' Reading thist3.1'
      if (lstrk2.gt.lstrk1) open (23,file='thist3.1',status='old',
     * form=trform,err=920)

      do 200 trkexp =  lstrk1+1,99999
        call tr2rd(23,trfmin,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z,ierr)
        if (trkexp.eq.lstrk1+1) call trtabhd(itabt3z,itabt4z)
        if (ierr.eq.1) go to 210
        if (ierr.eq.2) go to 960
        if (ierr.eq.3) stop ' lasthist: Not a thist2/3 type file.'

        if (statl.le.2) then
          call tr3rd(23,trfmin,.true.,itabt3z,itabt4z,nvarlt,nitlt,
     *     nit,vart,ierr)
          if (ierr.eq.1) go to 970
          if (ierr.eq.2) go to 980
          go to 170
        endif

        if (idiagt.ge.2) call tr2wr(6,.true.,trk,statf,statl,
     *   ifst,ilst,nit,idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z)
        if (trk.ne.trkexp) then
          write (6,*) ' Track read is not ',trkexp,' but ',trk,'.'
          stop ' lasthist'
        endif
        if (trk.gt.lstrk2) then
          write (6,'(/'' Last track in file ('',i5,
     *     '') > expected (''i5,'').''/'' file.'')') trk,lstrk
          stop ' lasthist'
        endif

        call tr3rd(23,trfmin,.false.,itabt3z,itabt4z,nvarlt,nitlt,
     *     nit,vart,ierr)
        if (ierr.eq.1) go to 970
        if (ierr.eq.2) go to 980
        if (idiagt.ge.2) call tr3wr(6,trfmin,itabt3z,itabt4z,nvarlt,
     *   nitlt,nit,vart)

        kb = kt(nit)
        if (nit.le.1) then
          xa(kb)   = 0.
          ya(kb)   = 0.
          pa(kb)   = 0.
          iopa(kb) = 0
          kab(kb)  = 0
        else
          ita     = nit - 1
          if (pstrak) then
            call lltops(xcen,ycen,rproj,fhem,xt(ita),
     *       yt(ita),xa(kb),ya(kb))
          else
            xa(kb)   = xt(ita)
            ya(kb)   = yt(ita)
          endif
          pa(kb)   = pt(ita)
          iopa(kb) = iopt(ita)
          kab(kb)  = kt(ita)
        endif
        itb     = nit
        if (pstrak) then
          call lltops(xcen,ycen,rproj,fhem,xt(itb),
     *     yt(itb),xb(kb),yb(kb))
        else
          xb(kb)   = xt(itb)
          yb(kb)   = yt(itb)
        endif
        pb(kb)   = pt(itb)
        iopb(kb) = iopt(itb)
 170    continue
 200  continue
 210  continue

      close (23)
      if (trk.ne.lstrk2) stop 
     * ' lasthist: Last track no. > then expected.'

c-----------------------------------------------------------------------
c     Removal of unwanted systems - first period.
c-----------------------------------------------------------------------

c     if ((iopmxt.lt.iopmxtz).or.(cmnt1.gt.cmnt1z).or.
c    * (cmnt2.gt.cmnt2z)) then
c       write (6,*) ' Removing unwanted systems'
c       k2 = 0
c       do 280 k1 = 1,nkb
c         if (iopb(k1).gt.iopmxt) go to 240
c         if ((iopb(k1).eq.0).and.(pc(k1,2).lt.cmnt1)) go to 240
c         if ((iopb(k1).eq.1).and.(pc(k1,2).lt.cmnt2)) go to 240

c         k2 = k2 + 1
c         xb(k2)   = xb(k1)
c         yb(k2)   = yb(k1)
c         pb(k2)   = pb(k1)
c         iopb(k2) = iopb(k1)

c         if (nit.eq.1) go to 210
c         if (iopb(k1).gt.iopmxt) go to 210
c         if ((iopb(k1).eq.0).and.(pc(k1,2).lt.cmnt1)) go to 210
c         if ((iopb(k1).eq.1).and.(pc(k1,2).lt.cmnt2)) go to 210

c         kab(k2)  = kab(k1)
c         xa(k2)   = xa(k1)
c         ya(k2)   = ya(k1)
c         pa(k2)   = pa(k1)
c         iopa(k2) = iopa(k1)
c         go to 220

c 210     continue
c         kab(k2)  = 0
c         xa(k2)   = 0.
c         ya(k2)   = 0.
c         pa(k2)   = 0.
c         iopa(k2) = 0.

c 220     continue
c 240     continue
c 280   continue
c       nkb = k2
c     endif

c-----------------------------------------------------------------------
c     Summary of last two periods of history file
c-----------------------------------------------------------------------

      if (idiagt.ge.1) then
        write (6,310) ib,dab,hrb
 310    format (/' EXTRACT FROM EXISTING HISTORY FILE : ib = ',
     *   i3,' (',i6,' ',i4,').')
        write (6,'(/'' ka    xa   ya      pa     iopa  '',
     *              '' kb    xb   yb      pb     iopb  ''/)')

        do 350 kb = 1,nkb
          ka = kab(kb)
          if (ka.eq.0) then
            write (6,330) kb,xb(kb),yb(kb),pb(kb),iopb(kb)
 330        format (33x,i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2)
          else
            write (6,340) ka,xa(kb),ya(kb),pa(kb),iopa(kb),
     *                    kb,xb(kb),yb(kb),pb(kb),iopb(kb)
 340        format (2(i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,3x))
          endif
 350    continue
      endif

      return

c-----------------------------------------------------------------------
c     Errors
c-----------------------------------------------------------------------

 920  stop ' lasthist: Cannot find file.'
 940  stop ' lasthist: End of track history file in track contents.'
 950  stop 
     * ' lasthist: Error in reading track history file, track contents.'
 960  write (0,*)  ' trkexp = ',trkexp
      stop 
     * ' lasthist: Error in reading track history file, track header.'
 970  write (0,*)  ' trkexp = ',trkexp
      stop 
     * ' lasthist: End of track history file in track data.'
 980  write (0,*)  ' trkexp = ',trkexp
      stop 
     * ' lasthist: Error in reading track history file in track data.'
      end
