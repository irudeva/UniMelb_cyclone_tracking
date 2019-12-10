      Subroutine trackio(nitlt,nvarlt)

c       Subroutine which writes and reads the data for a single track

c     tr1wr      Write of track content file 
c     tr1rd      Read  of track content file 
c     tr1rdz     Read  of track content file (for progress summary
c                  only in "lasthist"; tracking parameters not used)
c     tr2wr      Write of individual track (header)
c     tr2rd      Read  of individual track (header)
c     trtabhd    Tabulation heading for formatted writes
c     tr3wr      Write of individual track (point data)
c     tr3rd      Read  of individual track (point data)

c     Written by Ross Murray
c     Last revised 10th Aug., 1995.


c-------------------------------------------------------------------------------
c     Modified 09/12/2019, Ahmad Galea (Science IT University of Melbourne)
c     
c     - Made integer conversions for file output explicit.
c     - Date format changed from yymmdd to yyyymmdd.
c
c-------------------------------------------------------------------------------



c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"

c     Local declarations
c     ------------------

      logical skip,trfmz,trnmlz
      character*10 fileid
      character*160 trtab,trkhd
      character*100 lnmlt(20)
      dimension isw(20)
      character*80 fmt
      save trtab,lntrtab,nvarc,nvart,nvar,isw,fmt
      integer trk,statf,statl
      dimension vart(nitlt,nvarlt)

c     Edit: Ahmad Galea, 2019

c     gfortran does not implicitly do conversions 
c     to output format, so they must be explicit

c     Values to point to explicitly converted
c     int data retrieved from 'vart'
c     -------------------------- 
      integer dai,hri,stat1i,stat2i,ki,iopi
      real dar,hrr,stat1r,stat2r,kr,iopr
  
      equivalence (dai,dar)
      equivalence (hri,hrr)
      equivalence (stat1i,stat1r)
      equivalence (stat2i,stat2r)
      equivalence (ki,kr)
      equivalence (iopi,iopr)


c     Namelist for formatted i/o
c     --------------------------

      namelist /nmltrdata/
     * quant,level,lunit,source,dmode,unit,cunit,area,
     * hilo,feat,latmnc,latmxc,lonmnc,lonmxc,
     * sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cvarad,
     * fmxc,frmxc,frcmxc,da0,hr0,ddhmt,iopmxt,cmnt1,cmnt2,
     * iadvtp,afile,refdt,wsteer,fsteer,asteer,wmotn,wpten,
     * dequiv,rcprob,rpbell,qmxopn,qmxwek,qmxnew,
     * nsort,irevmx,merget,qmerge,itabt3,itabt4,rsttks,
     * t1,da1,hr1,ic,tc,dac,hrc,nkc,ndt,lstrk1,lstrk2

c-----------------------------------------------------------------------

      Entry tr1wr(iunit,iaux,trfmz,trnmlz)

      if (trfmz) then
        write (iunit,'(a/)') ' TRACK HISTORY FILE'
        write (iunit,nmltrdata) 
        write (iunit,'()') 
      else if (trnmlz) then
        open (iaux,status='scratch')
c       open (iaux)
        write (iaux,nmltrdata) 
        rewind (iaux)
        do 60 inmlt = 1,20
          read (iaux,'(a100)',end=70) lnmlt(inmlt)
   60   continue
        close (iaux)
   70   nnmlt = inmlt - 1
        fileid = 'thnml'
        write (iunit) fileid,nnmlt
        do 80 inmlt = 1,nnmlt
          write (iunit) lnmlt(inmlt)
   80   continue
      else
        fileid = 'thist1'
        write (iunit) fileid,
     *   quant,level,lunit,source,dmode,unit,cunit,area,
     *   hilo,feat,latmnc,latmxc,lonmnc,lonmxc,
     *   sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cvarad,
     *   fmxc,frmxc,frcmxc,da0,hr0,ddhmt,
     *   iopmxt,cmnt1,cmnt2,iadvtp,afile,refdt,wsteer,fsteer,
     *   asteer,wmotn,wpten,dequiv,rcprob,rpbell,qmxopn,qmxnew,
     *   nsort,irevmx,merget,qmerge,itabt3,itabt4,rsttks
        write (iunit) t1,da1,hr1,ic,tc,dac,hrc,nkc,ndt,
     *   lstrk1,lstrk2
      endif

      return

c-----------------------------------------------------------------------

      Entry tr1rd(iunit,iaux,trfmz,trnmlz,ierd)

      ierd = 0
      if (trfmz) then
        read (iunit,'(a/)',err=180,end=170) fileid
        if (fileid.ne.' TRACK HIS') go to 190
        read (iunit,nmltrdata,err=180,end=180) 
        read (iunit,'()',err=180,end=180)
      else if (trnmlz) then
        read (iunit) fileid,nnmlt
        if (fileid.ne.'thnml') go to 150
        open (iaux,status='scratch')
c       open (iaux)
        do 120 inmlt = 1,nnmlt
          read (iunit,end=150) lnmlt(inmlt)
          write (iaux,'(a100)') lnmlt(inmlt)
  120   continue
        rewind (iaux)
        read (iaux,nmltrdata) 
        close (iaux)
      else
        read (iunit,err=180,end=170) fileid,
     *   quant,level,lunit,source,dmode,unit,cunit,area,
     *   hilo,feat,latmnc,latmxc,lonmnc,lonmxc,
     *   sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cvarad,
     *   fmxc,frmxc,frcmxc,da0,hr0,ddhmt,
     *   iopmxt,cmnt1,cmnt2,iadvtp,afile,refdt,wsteer,fsteer,
     *   asteer,wmotn,wpten,dequiv,rcprob,rpbell,qmxopn,qmxnew,
     *   nsort,irevmx,merget,qmerge,itabt3,itabt4,rsttks
        read (iunit) t1,da1,hr1,ic,tc,dac,hrc,nkc,ndt,
     *   lstrk1,lstrk2
        if (fileid(1:6).ne.'thist1') go to 190
      endif

      return
  150 write (6,*) ' Fileid = ',fileid
      stop ' Not an embedded namelist file.'
  160 stop ' Error in namelist read.'
  170 ierd = 1
      return
  180 ierd = 2
      return
  190 ierd = 3
      write (6,*) ' fileid = ',fileid
      return

c-----------------------------------------------------------------------

      Entry tr2wr(iunit,trfmz,trk,statf,statl,ifst,ilst,nit,
     * idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z)

      if (trfmz) then
        write (iunit,310) trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
  310   format (' Track',i5,': stat = ',i1,i1,', ifst = ',i4,
     *       ', ilst = ',i4,', nit = ',i3,1x,i8,i4,' - ',i8,i4,
     *       '.  (itab=',2i2,')')
      else
        fileid = 'thist2'
        write (iunit) fileid,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
      endif

      return

c-----------------------------------------------------------------------

      Entry tr2rd(iunit,trfmz,trk,statf,statl,ifst,ilst,nit,
     * idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z,ierd)

      ierd = 0
      if (trfmz) then
        read (iunit,'(a160)',err=370,end=370) trkhd
        read (trkhd,310,err=380,end=370) trk,statf,statl,ifst,
c       read (iunit,310,err=380,end=370) trk,statf,statl,ifst,
     *   ilst,nit,idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
      else
        read (iunit,err=380,end=370) fileid,trk,statf,statl,ifst,
     *   ilst,nit,idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
        if (fileid(1:6).ne.'thist2') go to 390
      endif

      return

  370 ierd = 1
      return
  380 ierd = 2
      write (6,'(a)') trkhd
      return
  390 ierd = 3
      write (6,*) ' fileid = ',fileid
      return

c-----------------------------------------------------------------------

      Entry trtabhd(itabt3z,itabt4z,nvarlt,idiagt)

      if (itabt3z.eq.0) nvarc = 3
      if (itabt3z.eq.1) nvarc = 4
      if (itabt3z.eq.2) nvarc = 5
      if (itabt3z.ge.3) nvarc = 7
      if (itabt4z.eq.0) nvart = 0
      if (itabt4z.eq.1) nvart = 2
      nvar = 7+nvarc+nvart
      if (nvar.gt.nvarlt) stop ' Track variable array not large enough.'
      do 410 ivar = 1,nvarlt
        isw(ivar) = 0
  410 continue
      do 420 ivar = 1,7+nvarc
        isw(ivar) = ivar
  420 continue
      if (itabt4z.gt.0) then
        isw(nvar-1) = 15
        isw(nvar)   = 16
c       do 430 ivar = 7+nvarc+1,nvar
c         isw(ivar) = ivar
c 430   continue
      endif
      write (fmt,'(''(f10.4,2x,i8,1x,i4,3x,i1,i1,2x,i3,2x,i2,2x,'',
     * '''',i2,''f9.3)'')') nvarc+nvart
        
      if (itabt4z.eq.0) then
        if (itabt3z.eq.0) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y'
        else if (itabt3z.eq.1) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p'
        elseif (itabt3z.eq.2) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p','c'
        elseif (itabt3z.ge.3) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p','c','dp','rd'
        else
          write (6,*) ' itabt3z = ',itabt3z
          stop ' Tabulation style unknown.'
        endif
      else if (itabt4z.eq.1) then
        if (itabt3z.eq.1) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p','up','vp'
        elseif (itabt3z.eq.2) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p','c','up','vp'
        elseif (itabt3z.ge.3) then
          write (trtab,440) 't','da','hr','stat','k','iop',
     *     'q','x','y','p','c','dp','rd','up','vp'
        else
          write (6,*) ' itabt3z = ',itabt3z
          stop ' Tabulation style unknown.'
        endif
      else
        write (6,*) ' itabt4z = ',itabt4z
        stop ' Tabulation style unknown.'
      endif
  440 format (2x,a8,2x,a8,1x,a4,1x,a4,2x,a3,1x,a3,2x,20a9)
      lntrtab = lnblnk(trtab)

      if (idiagt.ge.1) then
        write (6,'(/a)') ' Tabulated variables:' 
        write (6,*) '   Style   : itabt3 = ',itabt3z,', itabt4 = ',
     *   itabt4z
        write (6,*) '   No. of real variables: ',nvarc,'+',nvart,'=',
     * nvar,'):'
        write (6,*) trtab(1:lntrtab)
        write (6,*) '   Switches: ', (isw(ivar),ivar=1,nvar)
        write (6,*) '   Format  : ',fmt(1:lnblnk(fmt))
        write (6,*)
      endif

      return

c-----------------------------------------------------------------------

      Entry tr3wr(iunit,trfmz,itabt3z,itabt4z,nvarlt,nitlt,nit,vart)

      if (trfmz) then
        write (iunit,'(/a)') trtab(1:lntrtab)

c       Loop over track data and write to file
        do m=1, nit

c         Retrieve values from vart in real format
          dar = vart(m,2)
          hrr = vart(m,3)
          stat1r = vart(m,4)
          stat2r = vart(m,5)
          kr = vart(m,6)
          iopr = vart(m,7)

c         Write data to file
          write (iunit,fmt) vart(m,1),
c         seperate out int vars (using int conversions)
     *    dai,hri,stat1i, stat2i,ki,iopi,
c         rest of data
     *    ((vart(m,isw(ivar))),ivar=8,nvar)

        end do

        write (iunit,'()')
      else
        write (iunit) ((vart(m,isw(ivar)),ivar=1,nvar),m=1,nit)
      endif

      return

c-----------------------------------------------------------------------

      Entry tr3rd(iunit,trfmz,skip,itabt3z,itabt4z,nvarlt,nitlt,
     * nit,vart,ierd)

      ierd = 0
      if (trfmz) then
        if (skip) then
          do 460 i=1,nit+3
            read (iunit,'()',err=480,end=470)
  460     continue
        else
          read (iunit,'(/)',err=480,end=470)
          read (iunit,fmt,err=480,end=480) 
     *     ((vart(m,isw(ivar)),ivar=1,nvar),m=1,nit)
          read (iunit,'()')
        endif
      else
        if (skip) then
          read (iunit)
        else
          read (iunit,err=480,end=480) 
     *     ((vart(m,isw(ivar)),ivar=1,nvar),m=1,nit)
        endif
      endif

      return

  470 ierd = 1
      return
  480 ierd = 2
      return

      end

c=======================================================================

      Subroutine tr1rdz(iunit,iaux,trfmz,trnmlz,idiagt,t1z,da1z,hr1z,
     * icz,tcz,dacz,hrcz,nkcz,ndtz,lstrk1z,lstrk2z,ierd)

      logical trfmz,trnmlz
      character*100 lnmlt(20)
      character*10 fileid
      character quant*8,level*9,lunit*10,source*10,unit*12,
     * area*17,dmode*6,hilo*1,feat*4,cunit*17
      real latmnc,latmxc,lonmnc,lonmxc
      real ndt
      real ndtz
      integer da0,hr0,da1,hr1,dac,hrc
      integer da1z,hr1z,dacz,hrcz
      character*80 afile
      logical rsttks

      namelist /nmltrdata/
     * quant,level,lunit,source,dmode,unit,cunit,area,
     * hilo,feat,latmnc,latmxc,lonmnc,lonmxc,
     * sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cvarad,
     * fmxc,frmxc,frcmxc,da0,hr0,ddhmt,
     * iopmxt,cmnt1,cmnt2,iadvtp,afile,refdt,wsteer,fsteer,
     * asteer,wmotn,wpten,dequiv,rcprob,rpbell,qmxopn,qmxnew,
     * nsort,irevmx,merget,qmerge,itabt3,itabt4,rsttks,
     * t1,da1,hr1,ic,tc,dac,hrc,nkc,ndt,lstrk1,lstrk2

      ierd = 0
      if (trfmz) then
        read (iunit,'(a/)',err=280,end=270) fileid
        if (fileid.ne.' TRACK HIS') go to 290
        read (iunit,nmltrdata,err=280,end=280) 
        read (iunit,'()',err=280,end=280)
      else if (trnmlz) then
        read (iunit) fileid,nnmlt
        if (fileid.ne.'thnml') go to 250
        open (iaux,status='scratch')
c       open (iaux)
        do 220 inmlt = 1,nnmlt
          read (iunit,end=260) lnmlt(inmlt)
          write (iaux,'(a100)') lnmlt(inmlt)
  220   continue
        rewind (iaux)
        read (iaux,nmltrdata) 
        close (iaux)
      else
        read (iunit,err=280,end=270) fileid
     *   quant,level,lunit,source,dmode,unit,cunit,area,
     *   hilo,feat,latmnc,latmxc,lonmnc,lonmxc,
     *   sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cvarad,
     *   fmxc,frmxc,frcmxc,da0,hr0,ddhmt,
     *   iopmxt,cmnt1,cmnt2,iadvtp,afile,refdt,wsteer,fsteer,
     *   asteer,wmotn,wpten,dequiv,rcprob,rpbell,qmxopn,qmxnew,
     *   nsort,irevmx,merget,qmerge,itabt3y,itabt4y,rsttks
        read (iunit) t1z,da1z,hr1z,icz,tcz,dacz,hrcz,nkcz,
     *   ndtz,lstrk1z,lstrk2z
        if (fileid(1:6).ne.'thist1') go to 290
      endif

      if (trfmz.or.trnmlz) then
        t1z    = t1
        da1z   = da1
        hr1z   = hr1
        icz    = ic
        tcz    = tc
        dacz   = dac
        hrcz   = hrc
        nkcz   = nkc
        ndtz   = ndt
        lstrk1z= lstrk1
        lstrk2z= lstrk2
      endif

      if (idiagt.ge.1) then
        write (6,'(/a/)') ' Parameters given in existing history file:'
        write (6,nmltrdata)
      endif

      return

  250 write (6,*) ' Fileid = ',fileid
      stop ' Not an embedded namelist file.'
  260 stop ' Error in namelist read.'
  270 ierd = 1
      return
  280 ierd = 2
      return
  290 ierd = 3
      write (6,*) ' fileid = ',fileid
      return

      end

