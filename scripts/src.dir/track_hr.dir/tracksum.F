      Subroutine tracksum

c         This routine writes a summary of cyclones and their
c     properties at the previous, current, and new time steps.

c     Written by Ross Murray.
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c-----------------------------------------------------------------------
c     Tabulation
c-----------------------------------------------------------------------

      write (6,460) ib,dab,hrb
 460  format (/' SUMMARY OF ITERATION : ib = ',i5,' (',i6,' ',i4,').'/)
      write (6,461) 'ka','xa','ya','pa','iopa','kb','xb','yb','pb',
     * 'iopb','xp','yp','pp','kc','xc','yc','pc','iopc','q'
 461  format (2(a3,'    (',a2,',',a2,')    ',a8,1x,a4,2x),
     * '   (',a2,',',a2,')    ',a8,2x,a3,'    (',a2,',',a2,')    ',
     * a8,1x,a4,1x,a5)
c461  format (2(a3,' (',a4,',',a4,') ',a8,x,a4,2x),
c    * '(',a4,',',a4,') ',a8,2x,a3,' (',a4,',',a4,') ',a8,x,a4,x,a5)

      do 480 kb = 1,nkb
        ka = kab(kb)
        kc = kcb(kb)

        if (kab(kb).eq.0) then
          if (kc.eq.0) then
            write (6,465) kb,xb(kb),yb(kb),pb(kb),iopb(kb),
     *                       xp(kb),yp(kb),pp(kb)
 465        format (33x,i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,3x,
     *                    '(',f5.1,',',f5.1,') ',f8.2)
          else
            write (6,466) kb,xb(kb),yb(kb),pb(kb),iopb(kb),
     *                       xp(kb),yp(kb),pp(kb),
     *                    kc,xc(kc),yc(kc),pc(kc),iopc(kc),qf(kb)
 466        format (33x,i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,3x,
     *                    '(',f5.1,',',f5.1,') ',f8.2,2x,
     *                i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,2x,f5.3)
          endif
        else
          if (kc.eq.0) then
            write (6,468) ka,xa(kb),ya(kb),pa(kb),iopa(kb),
     *                    kb,xb(kb),yb(kb),pb(kb),iopb(kb),
     *                           xp(kb),yp(kb),pp(kb)
 468        format (2(i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,3x),
     *                    '(',f5.1,',',f5.1,') ',f8.2)
          else
            write (6,470) ka,xa(kb),ya(kb),pa(kb),iopa(kb),
     *                    kb,xb(kb),yb(kb),pb(kb),iopb(kb),
     *                       xp(kb),yp(kb),pp(kb),
     *                    kc,xc(kc),yc(kc),pc(kc),iopc(kc),qf(kb)
 470        format (2(i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,3x),
     *                    '(',f5.1,',',f5.1,') ',f8.2,2x,
     *                i3,' (',f5.1,',',f5.1,') ',f8.2,2x,i2,2x,f5.3)
          endif
        endif
 480  continue

      do 485 kc = 1,nkc
        if (kbc(kc).eq.0) write (6,'(90x,i3,'' ('',f5.1,'','',f5.1,
     *   '') '',f8.2,2x,i2)') kc,xc(kc),yc(kc),pc(kc),iopc(kc)
 485  continue

      return
      end
