      Subroutine trackioa

c       Subroutine which reads track data from a 1996 style
c     track file to a 1993 style analysis programme.

c     tr1rda     Read  of track file header
c     tr2rda     Read  of individual track (header)
c     trtabhda   Tabulation heading for formatted writes
c     tr3rda     Read  of individual track (point data)

c     Written by Ross Murray
c     Last revised 12th May, 1996.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

c     New format
c     ----------

      character dmode*6,quant*8,level*9,lunit*10,source*10,unit*12
      character hilo*1,feat*4,cunit*17,area*10

      real latmnc,latmxc,lonmnc,lonmxc
      logical sphtrg

      integer da0,hr0,ddhmt
      logical rsttks

      integer da1,hr1,dab,hrb,dac,hrc
      real mdt,ndt

      character*50 afile

c     Old format
c     ----------

      character dmode1*6
      character hilo1*1

      real latmnc1,latmxc1,lonmnc1,lonmxc1
      logical sphtrg1

      integer da01,hr01,ddhmt1
      logical rsttks1

      integer da11,hr11,dab1,hrb1,dac1,hrc1
      real mdt1,ndt1

      character*50 afile1

      character progmt1*20
      character source1*20,progmc1*20,projn1*1,level1*6
      character trakid1*20,advfil1*20
      integer ddhmmt1,ddhmc1

c     Local declarations
c     ------------------

      logical skip,trfmz,trnmlz
      character*10 fileid
      character*100 lnmlt(20)
      dimension isw(20)
      character*80 fmt

      save nvarc,nvart,nvar,isw,fmt

      integer trk,statf,statl
      dimension vart(nitlt,nvarlt)

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

c       call tr1rda(20,40,trfmz,trnmlz,progmt,irun,source,
c    *   level,projn,fhem,rproj,ni1,nj1,progmc,hilo,ddhmc,
c    *   kmax,methdc,flat,drlt,latmnc,latmxc,lonmnc,lonmxc,
c    *   prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2,
c    *   ddhmt,ddhmmt,dmodet,dyepoc,hrepoc,t1,tc,dy1,hr1,dyc,hrc,
c    *   methdt,iopmxt,cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
c    *   c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,itabt3,itabt4,
c    *   ic,lstrk1,lstrk2,ndt,kcycc,ierd)

      Entry tr1rda(iunit,iaux,trfmz,trnmlz,progmt1,irun1,source1,
     * level1,projn1,fhem1,rproj1,ni1,nj1,progmc1,hilo1,ddhmc1,
     * nkc1,methdc1,flat1,sdrmx1,latmnc1,latmxc1,lonmnc1,lonmxc1,
     * frmxc1,frcvmxc1,drmx11,drmx21,itmx11,itmx21,
     * ddhmt1,ddhmmt1,dmode1,da01,hr01,t11,tc1,da11,hr11,dac1,hrc1,
     * methdt1,iopmxt1,cvmnt11,cvmnt21,trakid1,iadvtp1,advfil1,wmotn1,
     * wpten1,
     * dequiv1,rcprob1,rpbell1,qmxopn1,qmxnew1,c61,nsort1,irevmx1,
     * merget1,qmerge1,itabt31,itabt41,
     * ic1,lstrk11,lstrk21,ndt1,nkc2,ierd)

      ierd = 0
      if (trfmz) then
        read (iunit,'(a10)',err=180,end=170) fileid
        if (fileid.ne.' TRACK HIS') go to 190
        read (iunit,'()',err=180,end=170)
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

c     Convert to old style quantities
c     -------------------------------

      progmt1 = 'track'
      irun1   = 0
      read (source,*,err=123,end=123) irun1
  123 continue
      source1 = source
      level1  = quant(1:lnblnk(quant)) // level(1:lnblnk(level))
      avlat = 0.5*(latmnc+latmxc)
      if (avlat.gt.0) then
        projn1  = 'N'
        fhem1   = 1.
      else
        projn1  = 'S'
        fhem1   = -1.
      endif
      rproj1  = 30.
      ni1     = 61
      nj1     = 61
      progmc1 = 'cycloc'
      ddhmc1  = ddhmt
      methdc1 = 2
      flat1   = 0.
      frcvmxc1= frcmxc*2.0
      ddhmmt1 = ddhmt
      methdt1 = 2
      cvmnt11 = 0.5*cmnt1
      cvmnt21 = 0.5*cmnt2
      trakid1 = ' '
      advfil1 = afile(1:20)
      c61     = 0
      nkc1    = nkc
      nkc2    = nkc

      dmode1  = dmode
      hilo1   = hilo
      latmnc1 = latmnc
      latmxc1 = latmxc
      lonmnc1 = lonmnc
      lonmxc1 = lonmxc
      sdrmx1  = sdrmx
      drmx11  = drmx1
      drmx21  = drmx2
      itmx11  = itmx1
      itmx21  = itmx2
      nshell1 = nshell
      cvarad1 = cvarad
      frmxc1  = frmxc
      da01    = da0
      hr01    = hr0
      ddhmt1  = ddhmt
      iopmxt1 = iopmxt
      iadvtp1 = iadvtp
      refdt1  = refdt
      wmotn1  = wmotn
      wpten1  = wpten
      dequiv1 = dequiv
      rcprob1 = rcprob
      rpbell1 = rpbell
      qmxopn1 = qmxopn
      qmxnew1 = qmxnew
      nsort1  = nsort
      irevmx1 = irevmx
      merget1 = merget
      qmerge1 = qmerge
      itabt31 = itabt3
      itabt41 = itabt4
      rsttks1 = rsttks
      t11     = t1
      da11    = da1
      hr11    = hr1
      ic1     = ic
      tc1     = tc
      dac1    = dac
      hrc1    = hrc
      nkc1    = nkc
      ndt1    = ndt
      lstrk11 = lstrk1
      lstrk21 = lstrk2

      return

c     Errors
c     ------

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

      Entry tr2rda(iunit,trfmz,trk,statf,statl,ifst,ilst,nit,
     * idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z,ierd)

      ierd = 0
      if (trfmz) then
        read (iunit,310,err=380,end=370) trk,statf,statl,ifst,
     *   ilst,nit,idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
  310   format (' Track ',i4,': stat = ',i1,i1,', ifst = ',i4,
     *       ', ilst = ',i4,', nit = ',i3,1x,i6,i4,' - ',i6,i4,
     *       '.  (itab=',2i2,')')
      else
        read (iunit,err=380,end=370) fileid,trk,statf,statl,ifst,
     *   ilst,nit,idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z
        if (fileid(1:6).ne.'thist2') go to 390
      endif

      return

  370 ierd = 1
      return
  380 ierd = 2
      return
  390 ierd = 3
      write (6,*) ' fileid = ',fileid
      return

c-----------------------------------------------------------------------

      Entry trtabhda(itabt3z,itabt4z,nvarlt,idiag)

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
        do 430 ivel = 1,nvart
          ivar = 7+nvarc+ivel
          isw(ivar) = 14+ivel
  430   continue
      endif
      write (fmt,'(''(2x,f8.4,2x,i6,x,i4,3x,i1,i1,2x,i3,2x,i2,2x,'',
     * ''f5.3,t45,'',i2,''f9.3)'')') nvarc+nvart-1
        
      if (idiagt.ge.1) then
        write (6,'(/a)') ' Tabulated variables:' 
        write (6,*) '   Style   : itabt3 = ',itabt3z,', itabt4 = ',
     *   itabt4z
        write (6,*) '   No. of real variables: ',nvarc,'+',nvart,'=',
     * nvar,'):'
        write (6,*) '   Switches: ', (isw(ivar),ivar=1,nvar)
        write (6,*) '   Format  : ',fmt(1:lnblnk(fmt))
        write (6,*)
      endif

      return

c-----------------------------------------------------------------------

      Entry tr3rda(iunit,trfmz,skip,nvarlt,nitlt,
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

