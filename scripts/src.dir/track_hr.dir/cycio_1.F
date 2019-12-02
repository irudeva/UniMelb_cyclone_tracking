c     File "cycio.F"
c     --------------

c       This file contains the following subroutines and entry points
c     connected with cyclone data input and output:

c     Subroutine cychd
c       Entry cychdwr(iaux)
c       Entry cychdrd(iaux,ierr)
c     Subroutine cycio
c       Entry cycwr(iunit,nklt,iopc,xc,yc,fc,cc,dpc,rdc,zsc,upc,vpc,
c    *   sc,nsuplt)
c       Entry cyc1rd(iunit,nklt,ierr)
c       Entry cyc2rd(iunit,skip,nklt,iopc,xc,yc,fc,cc,dpc,rdc,zsc,
c    *   upc,vpc,sc,nsuplt,ierr)

c     Last revised 11 Jan., 1996, R.J. Murray.

c==============================================================================

      Subroutine cychdwr(iaux)

c       Subroutine which writes ascii buffers for namelist
c     input and output instruction parameters.

c------------------------------------------------------------------------------
c     Declarations
c------------------------------------------------------------------------------

#include "cyc1.h"     
#include "cyc2.h"     

      character*60 hdtail
      character*100 lnmlc(50)
      character*160 tabc
      common /bltabc1/nnmlc
      common /bltabc2/hdtail,tabc,lnmlc

c     Local declarations
c     ------------------

      character feats*5
c     logical cycfmz,cycnmlz

c     Namelist 
c     --------

      namelist /nmlcycdat/quant,level,lunit,source,unit,cunit,
     * dmode,rdiff,hilo,iopmxc,istmxc,latmnc,latmxc,lonmnc,lonmxc,
     * icendp,cvarad,cmnh,cmnc0,cmnc1,cmnc2,swvmn,dpmn,fccmn,
     * fmxc,frmxc,frcmxc,cmncw,dpmnw,rdincr,ftopeq,
     * zsmax,zscr1,zscr2,qsteer,rdustr,alatgv,rhoa,upfact

c-----------------------------------------------------------------------

c     Tail of header line
c     -------------------

      feats = feat(1:lnblnk(feat))
      if (nk.gt.1) feats = feats(1:lnblnk(feats)) // 'S'
      hdtail = quant(1:lnblnk(quant)) // level(1:lnblnk(level)) //' '//
     * feats(1:lnblnk(feats))
      lnbs = lnblnk(source)
      lnba = lnblnk(area)
      if (lnbs+lnba.gt.0) hdtail = hdtail(1:lnblnk(hdtail)) // '  ('
      if (lnbs.gt.0)      hdtail = hdtail(1:lnblnk(hdtail)) //
     * source(1:lnbs)
      if (lnba.gt.0) then
        if (lnbs.gt.0) then
          hdtail = hdtail(1:lnblnk(hdtail)) // ', ' // area(1:lnba)
        else
          hdtail = hdtail(1:lnblnk(hdtail)) // area(1:lnba)
        endif
      endif
      if (lnbs+lnba.gt.0) hdtail = hdtail(1:lnblnk(hdtail)) // ')'

c     Namelist lines
c     --------------

      open (iaux,status='scratch')
c     open (iaux)
      write (iaux,nmlcycdat) 
      rewind (iaux)
      do 100 inmlc = 1,20
        read (iaux,'(a100)',end=110) lnmlc(inmlc)
  100 continue
      stop ' Array "lnmlc" not large enough.'
  110 close (iaux)
      nnmlc = inmlc - 1

c     Tabulation header
c     -----------------

      if (itabc1.eq.0) write (tabc,120) 'k'
      if (itabc1.eq.1) write (tabc,120) 'k','iop'
  120 format (a3,1x,a2)

      if (itabc2.ge.1) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'lon','lat'
c     if (itabc2.eq.2) write (tabc,130) tabc(1:lnblnk(tabc)),
c    * 'lon','lat','lonv','latv'
  130 format (a,10a8)

      if (itabc3.eq.1) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'f'
      if (itabc3.eq.2) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'f','c'
      if (itabc3.eq.3) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'f','c','dp','rd'
      if (itabc3.ge.4) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'f','c','dp','rd','zs'

      if (itabc4.eq.1) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'up','vp'

      if (itabc2.eq.2) write (tabc,130) tabc(1:lnblnk(tabc)),
     * 'lonv','latv'

      if (itabc5.ge.1) write (tabc,'(a,20(a10))')
     * tabc(1:lnblnk(tabc)),(svname(isup),isup=1,itabc5)

      return
      end

c==============================================================================

      Subroutine cychdrd(iaux,ierr)

c       Subroutine which reads ascii buffers for namelist
c     input and output instruction parameters.

c------------------------------------------------------------------------------
c     Declarations
c------------------------------------------------------------------------------

#include "cyc1.h"   
#include "cyc2.h"   

      character*60 hdtail
      character*100 lnmlc(50)
      character*160 tabc
      common /bltabc1/nnmlc
      common /bltabc2/hdtail,tabc,lnmlc

c     Local declarations
c     ------------------

      character feats*5
c     logical cycfmz,cycnmlz

c     Namelist 
c     --------

      namelist /nmlcycdat/quant,level,lunit,source,unit,cunit,area,
     * dmode,rdiff,hilo,feat,iopmxc,istmxc,latmnc,latmxc,lonmnc,lonmxc,
     * nshell,mscrn,sdrmx,drmx1,drmx2,itmx1,itmx2,diflt1,diflt2,iconcv,
     * icendp,cvarad,cmnh,cmnc0,cmnc1,cmnc2,swvmn,dpmn,fccmn,
     * fmxc,frmxc,frcmxc,cmnhw,cmncw,dpmnw,swvmnw,rdincr,nrddir,sphtrg,
     * ftopeq,zsmax,zscr1,zscr2,qsteer,rdustr,npgdir,alatgv,rhoa,upfact
c    *,rdsupv,avsupv,

c-----------------------------------------------------------------------

      open (iaux,status='scratch')
c     open (iaux)
c     write (iaux,'(/a100)') (lnmlc(inmlc),inmlc = 1,nnmlc)
      write (iaux,'(a100)') (lnmlc(inmlc),inmlc = 1,nnmlc)
      rewind (iaux)

      area = ' '

      read (iaux,nmlcycdat,err=220,end=210) 
      close (iaux)

      if (hilo.eq.'h') hilo = 'H'
      if (hilo.eq.'l') hilo = 'L'
      if (hilo.eq.'H') feat = 'HIGH'
      if (hilo.eq.'L') feat = 'LOW'

      return
 210  ierr = 1
      write (6,*) ' Premature end of namelist read.'
      write (6,'(a100)') (lnmlc(idash),inmlc=1,nnmlc)
      stop
 220  ierr = 2
      write (6,*) ' cychdrd: Error in namelist read.'
      stop
      end

c=======================================================================

      Subroutine cycio

c       This subroutine writes identifying information and the details
c     of high/low positions found by cycp.  At present, only formatted
c     input and output are allowed.

c     Written 21 Feb., 1991, R.J.Murray
c     Last revised 16 Dec., 1995, R.J. Murray.

c------------------------------------------------------------------------------
c     Declarations
c------------------------------------------------------------------------------

#include "cyc1.h" 
#include "cyc2.h" 
      COMMON /BLTMP/XV(200),YV(200)

      character*60 hdtail
      character*100 chead1,lnmlc(50)
      character*160 tabc
      character ada*6,ahr*4
      common /bltabc1/nnmlc
      common /bltabc2/hdtail,tabc,lnmlc

      logical skip
      character fileid*10,line*100,fmt*80
      dimension iopc(nklt),xc(nklt),yc(nklt),fc(nklt),cc(nklt)
     * ,dpc(nklt),rdc(nklt),zsc(nklt),upc(nklt),vpc(nklt)
     * ,sc(nklt,nsuplt)

c-----------------------------------------------------------------------

      Entry cycwr(iunit,nklt,iopc,xc,yc,fc,cc,dpc,rdc,zsc,
     * upc,vpc,nsuplt,sc)

c     Compose header line
c     -------------------

      write (ada,'(i6)') da
      if (ada(1:1).eq.' '.and.ada(2:2).ne.' ') ada(1:1) = '0'
      write (ahr,'(i4)') hr
      if (ahr(1:1).eq.' ') ahr(1:1) = '0'
      if (ahr(2:2).eq.' ') ahr(2:2) = '0'
      if (ahr(3:3).eq.' ') ahr(3:3) = '0'
      write (chead1,240) ada,ahr,nnmlc,nnmlc+4,itabc1,itabc2,itabc3,
     * itabc4,itabc5,nk,hdtail(1:lnblnk(hdtail))
  240 format (11x,a6,1x,a4,7x,i2,1x,i2,7x,
     * 2i1,i2,i1,i2,3x,i3,1x,a)
      
      chead = chead1(1:80)

c     Write header information
c     ------------------------

      write (iunit,'(/a/)') chead
      do 250 inmlc = 1,nnmlc
        line = lnmlc(inmlc)
        write (iunit,'(a)') line(1:lnblnk(line))
  250 continue
      write (iunit,'(/a/)') tabc(1:lnblnk(tabc))

c     Write cyclone arrays
c     --------------------

      if (itabc3.eq.0) then
        write (fmt,'(''(i3,x,i2,'',i1,''(2f8.2),'',i1,
     *   ''f8.3,20(1pe10.3.1))'')') itabc2,2*itabc4
      else
        itabc3x = itabc3
        if (itabc3.ge.3) itabc3x = itabc3x + 1
        write (fmt,'(''(i3,x,i2,'',i1,''(2f8.2),f8.2,'',i1,
     *   ''f8.3,20(1pe10.3.1))'')') itabc2,itabc3x-1+2*itabc4
      endif
c     write (iunit,*) fmt(1:lnblnk(fmt))

      do 300 k = 1,nk
        if (itabc3.eq.0) then
          write (iunit,fmt) k,iopc(k),xc(k),yc(k),
     *     (upc(k),vpc(k),ii=1,itabc4),
     *     (xv(k),yv(k),ii=2,itabc2),(sc(k,isup),isup=1,nsuplt)
        else if (itabc3.eq.1) then
          write (iunit,fmt) k,iopc(k),xc(k),yc(k),
     *     fc(k),(upc(k),vpc(k),ii=1,itabc4),
     *     (xv(k),yv(k),ii=2,itabc2),(sc(k,isup),isup=1,nsuplt)
        else if (itabc3.eq.2) then
          write (iunit,fmt) k,iopc(k),xc(k),yc(k),
     *     fc(k),cc(k),(upc(k),vpc(k),ii=1,itabc4),
     *     (xv(k),yv(k),ii=2,itabc2),(sc(k,isup),isup=1,nsuplt)
        else if (itabc3.eq.3) then
          write (iunit,fmt) k,iopc(k),xc(k),yc(k),
     *     fc(k),cc(k),dpc(k),rdc(k),(upc(k),vpc(k),ii=1,itabc4),
     *     (xv(k),yv(k),ii=2,itabc2),(sc(k,isup),isup=1,nsuplt)
        else if (itabc3.eq.4) then
          write (iunit,fmt) k,iopc(k),xc(k),yc(k),
     *     fc(k),cc(k),dpc(k),rdc(k),zsc(k),
     *     (upc(k),vpc(k),ii=1,itabc4),
     *     (xv(k),yv(k),ii=2,itabc2),(sc(k,isup),isup=1,nsuplt)
        endif
c       if (itabc3.eq.0) then
c         write (iunit,fmt) k,iopc(k),xc(k),yc(k),
c    *     (xv(k),yv(k),ii=2,itabc2),
c    *     (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c       else if (itabc3.eq.1) then
c         write (iunit,fmt) k,iopc(k),xc(k),yc(k),
c    *     (xv(k),yv(k),ii=2,itabc2),fc(k),
c    *     (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c       else if (itabc3.eq.2) then
c         write (iunit,fmt) k,iopc(k),xc(k),yc(k),
c    *     (xv(k),yv(k),ii=2,itabc2),fc(k),cc(k),
c    *     (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c       else if (itabc3.eq.3) then
c         write (iunit,fmt) k,iopc(k),xc(k),yc(k),
c    *     (xv(k),yv(k),ii=2,itabc2),fc(k),cc(k),dpc(k),rdc(k),
c    *     (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c       endif
  300 continue

      return

c-----------------------------------------------------------------------

      Entry cyc1rd(iunit,nklt,ierr)

c     Read header and namelist information

      ierr = 0
  400 continue
      read (iunit,'(a80)',end=410) chead
      if (lnblnk(chead).eq.0) go to 400

      read (chead,'(a10)') fileid
      if (fileid(1:8).ne.' CENTRES') go to 420

      read (chead,240,err=440,end=430) ada,ahr,nnmlc,nnmlcp4,
     * itabc1,itabc2,itabc3,itabc4,itabc5,nk
      read (ada,'(i6)') da
      read (ahr,'(i4)') hr

      if (nk.gt.nklt) go to 450
      read (iunit,'()',end=460)
      read (iunit,'(a100)',end=470) (lnmlc(inmlc),inmlc=1,nnmlc)
      return
  410 ierr = 1
      return
  420 write (6,*) ' Not a cyclone data file.'
      write (6,*) chead(1:lnblnk(chead))
      stop
  430 stop ' cyc1rd: End of internal read of cyclone header.'
  440 stop ' cyc1rd: Error in internal read of cyclone header.'
  450 stop ' cyc1rd: Cyclone data arrays not large enough.'
  460 stop ' cyc1rd: End of line following cyclone header.'
  470 stop ' cyc1rd: Premature end of file in namelist lines.'

c-----------------------------------------------------------------------

      Entry cyc2rd(iunit,skip,nklt,iopc,xc,yc,fc,cc,dpc,rdc,
     * zsc,upc,vpc,nsuplt,sc,ierr)

c     Read cyclone array

c     If skip=.true.: skip appropriate number of lines

crmw      print*,itabc3
      ierr = 0
      if (skip) then
        do 490 k = 1,nk+3
          read (iunit,'()',end=510)
 490    continue
      else
        if (itabc3.eq.0) then
          write (fmt,'(''(i3,x,i2,'',i1,''(2f8.2),'',i1,
     *     ''f8.3,20(1pe10.3.1))'')') itabc2,2*itabc4
        else
          itabc3x = itabc3

          if (itabc3.ge.3) itabc3x = itabc3x + 1
crmw_removed
crmw          write (fmt,'(''(i3,x,i2,'',i1,''(2f8.2),f8.2,'',i1,
crmw     *     ''f8.3,20(1pe10.3.1))'')') itabc2,itabc3x-1+2*itabc4
crmw_removed
crmw_added
          write (fmt,'(''(i3,1x,i2,2f8.2,f8.2,'',i1,
     *     ''f8.3,4f8.2)'')') itabc3x-1+2*itabc4
crmw_added
        endif
crmw       write (6,*) fmt(1:lnblnk(fmt))

        read (iunit,'(//)',end=510)
crmw    k -> kk
        do 500 kk = 1,nk
          if (itabc3.eq.0) then
            read (iunit,fmt,err=520,end=510) k,iopc(kk),xc(kk),yc(kk),
     *       (upc(kk),vpc(kk),ii=1,itabc4),
     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
          else if (itabc3.eq.1) then
            read (iunit,fmt,err=520,end=510) k,iopc(kk),xc(kk),yc(kk),
     *       fc(kk),(upc(kk),vpc(kk),ii=1,itabc4),
     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
          else if (itabc3.eq.2) then
            read (iunit,fmt,err=520,end=510) k,iopc(kk),xc(kk),yc(kk),
     *       fc(kk),cc(kk),(upc(kk),vpc(kk),ii=1,itabc4),
     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
          else if (itabc3.eq.3) then
            read (iunit,fmt,err=520,end=510) k,iopc(kk),xc(kk),yc(kk),
     *       fc(kk),cc(kk),dpc(kk),rdc(kk),
     *       (upc(kk),vpc(kk),ii=1,itabc4),
     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
          else if (itabc3.eq.4) then

            read (iunit,fmt,err=520,end=510) k,iopc(kk),xc(kk),yc(kk),
     *       fc(kk),cc(kk),dpc(kk),rdc(kk),zsc(kk),
     *       (upc(kk),vpc(kk),ii=1,itabc4),
     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
crmw
crmw          write(6,fmt) k,iopc(kk),xc(kk),yc(kk),
crmw     *       fc(kk),cc(kk),dpc(kk),rdc(kk),zsc(kk),
crmw     *       (upc(kk),vpc(kk),ii=1,itabc4),
crmw     *       (xv(kk),yv(kk),ii=2,itabc2),(sc(kk,isup),isup=1,nsuplt)
crmw          stop
          endif

c         if (itabc3.eq.0) then
c           read (iunit,fmt,err=520,end=510) k,iopc(k),xc(k),yc(k),
c    *       (xvx,yvx,ii=2,itabc2),
c    *       (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c         else if (itabc3.eq.1) then
c           read (iunit,fmt,err=520,end=510) k,iopc(k),xc(k),yc(k),
c    *       (xvx,yvx,ii=2,itabc2),fc(k),
c    *       (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c         else if (itabc3.eq.2) then
c           read (iunit,fmt,err=520,end=510) k,iopc(k),xc(k),yc(k),
c    *       (xvx,yvx,ii=2,itabc2),fc(k),cc(k),
c    *       (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c         else if (itabc3.eq.3) then
c           read (iunit,fmt,err=520,end=510) k,iopc(k),xc(k),yc(k),
c    *       (xvx,yvx,ii=2,itabc2),fc(k),cc(k),dpc(k),rdc(k),
c    *       (upc(k),vpc(k),ii=1,itabc4),(sc(k,isup),isup=1,nsuplt)
c         endif
 500    continue
      endif
      return
 510  stop 'cyc2rd: Premature end of cyclone data.'
 520  stop 'cyc2rd: Error in cyclone data.'
      end
