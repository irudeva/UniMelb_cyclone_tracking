      Program tstat
c
c * Author: Ross Murray
c
c * Modified by Richard Wardle and Kevin Keay 2003
c
c * Notes
c   (1) 13/12/2003: Read of nlats and nlons from llfile (llstat file)
c       was modified to give original (Sun f77) behaviour.
c       Introduced extra namelist (nmltstat) parameter ssd to output
c       scaled SD e.g. ssd=1000., SD x 1000
c
      parameter (sixth=1./6.)

#include "tstat1.h"

c     parameter (nisglt=120,njsglt=600)
c     parameter (nlonlt=182,nlatlt=92,nbuflt=nlonlt*nlatlt)
c     parameter (lsmax=45)
c     parameter (nfun=40)
c     parameter (rad=57.295779,rrad=1./rad,r2rad=1./(2.*rad))
c     parameter (pi=3.1415927)
c     parameter (nilt=31,njlt=31)
c     parameter (ni=81,nj=81)
c     parameter (icen=(ni+1)/2,jcen=(nj+1)/2)
c     parameter (xcen=icen,ycen=jcen)
c     parameter (rproj=30.)
c     parameter (niwt=81,njwt=81)
c     parameter (spval=99999.9)

c     character shead*960
c     character fname(nfun)*22,funit(nfun)*15,fn(nfun)*6
c     character funitx*15,fnx*6
c     dimension finc(nfun)

c     common /blhead/shead,llhead
c     common /blfn/fname,funit,fn
c     common /blilts/llwrit,fhem,nlons,nlats,idiagt
c     common /blfinc/finc

      character fileidtd*10,fileidh3*10,progmt*20
      character fileidsd*10,fileidvd*10,progms*20
      character source*20,progmc*20,projn*1,level*6,dmodet*6,hilo*1
      character trakid*20,advfil*20
      real latmnc,latmxc
      real lonmnc,lonmxc
      integer ddhmt,ddhmmt,ddhmc,ddhms

      integer dy1,hr1,dyc,hrc,dyepoc,hrepoc
      real mdt,ndt
      integer trk,trkdash
      integer statt1,statt2
      character trakidz*20
      character feat*12,hemis*4
      character addhm*7
      integer dystrts,hrstrts,dystops,hrstops
      integer dystrtx,dystopx

c Include file 'trk3.h'
c     Track arrays
      parameter (nvarlt=16)
      integer stat1,stat2,dy,hr
      COMMON /bltrk05/t(nisglt),dy(nisglt),hr(nisglt)
     *, stat1(nisglt),stat2(nisglt),k(nisglt),iop(nisglt)
     *, q(nisglt),x(nisglt),y(nisglt),p(nisglt),cv(nisglt)
     *, depi(nisglt),r0i(nisglt),gxi(nisglt),gyi(nisglt)
      dimension vart(nisglt,nvarlt)
      equivalence (t(1),vart(1,1))

      integer ji(nisglt)
      dimension gamma(nisglt)
      dimension pcv(nisglt)
      dimension ax(nisglt),ay(nisglt),ap(nisglt),ac(nisglt)
      dimension au(nisglt),av(nisglt)
      dimension ac2(nisglt)
      dimension ac4(nisglt)

      dimension xj(njsglt),yj(njsglt),pj(njsglt),cj(njsglt)
      dimension gxj(njsglt),gyj(njsglt)
      dimension r0j(njsglt),depj(njsglt)

      dimension u(nilt,njlt)
      dimension v(nilt,njlt)
      dimension w(nilt,njlt)
      dimension wt(nilt,njlt)
      dimension sd(nilt,njlt)
      dimension fs(nilt,njlt)
      dimension fx(nilt,njlt)
      dimension fy(nilt,njlt)
      dimension pc(nilt,njlt)
      dimension pt(nilt,njlt)
      dimension cc(nilt,njlt)
      dimension ct(nilt,njlt)
      dimension fg(nilt,njlt)
      dimension fl(nilt,njlt)
      dimension gx(nilt,njlt)
      dimension gy(nilt,njlt)
      dimension gs(nilt,njlt)
      dimension r0(nilt,njlt)
      dimension dep(nilt,njlt)
      dimension pm(nilt,njlt)
      dimension cm(nilt,njlt)

      dimension wtdn(niwt,njwt),icount(niwt,njwt)

      character*80 sshead(12)
      equivalence (shead,sshead(1))

      real lons(nlonlt),lats(nlatlt)
      dimension g(nbuflt)

      logical trfm,trnml,skip,diag
      logical cvsdet,pretrk,prestt,helpdet,newform
      logical vdet,adet,zdet,sdet,anydet,zadet,vzadet
      logical pmdet,zmdet
crmw      logical sector,crossgm,insect,llwrit,llform
      logical sector,crossgm,insect,llform
      real lonj,lon1,lon2

      integer ngtopt,optind
      character*80 optarg,statfile,infile,trackfile
      character*80 cvefile,zonefile,advfile,pmfile,zmfile
crmw      character*80 llfile,llhead
      character*80 llfile
      character*40 exfmt
crmw
      character*80 head2
crmw
      character*80 ch80   !KK

      common /blztstat/ifglz(lsmax),ifllz(lsmax),flz(lsmax),
     * ulz(lsmax),vlz(lsmax),plz(lsmax),ptlz(lsmax),wlz(lsmax),
     * clz(lsmax),ctlz(lsmax)
c    * sflz,isfglz,isfllz
      common /blcs/trk

      namelist /nmltstat/idiags,dlat,iperdv,ifc,cvmns2,
     * alatlt,wwqmin,ddhms,dystrts,hrstrts,dystops,hrstops,
     * Rcdnlt,Rkdn,Rcwqlt,Rkwq,Rcgllt,Rkgl,projn,alatlt2,
     * ssd !KK 13/12/2003

      clatt(xx,yy) = 2.*rad*atan2(xx,yy)

      data fn/
     * 'TN','UG','VG','PC','PT', 'CC','CT','SD','FS','FX'
     *,'FY','US','UX','UY','FG', 'FL','FE','FN','FV','UE'
     *,'UN','UV','GS','GX','GY', 'GE','GN','GV','PM','CM'
     *,'HX','HY','PD','HE','HN', 'HV','C0','R0','CL','DP'/
      data funit/'/GRID UNIT',' GRID UNIT/DAY',' GRID UNIT/DAY',
     * ' MB.',' MB./DAY', ' MB./DLSQ',' MB./DLSQ/DAY',
     * '/DEG.LAT.SQ','/DEG.LAT./DAY','/DEG.LAT./DAY',
     * '/DEG.LAT./DAY',' M./SEC.',' M./SEC.',' M./SEC.',
     * '/1000 DLSQ./DAY','/1000 DLSQ./DAY',
     * '/DEG.LAT./DAY','/DEG.LAT./DAY','/DEG.LAT./DAY',
     * ' M./SEC.',' M./SEC.',' M./SEC.',
     * ' M./SEC.',' M./SEC.',' M./SEC.',
     * ' M./SEC.',' M./SEC.',' M./SEC.',
     * ' MB.',' MB./DLSQ',' M./SEC.',' M./SEC.',
     * ' MB.',' M./SEC.',' M./SEC.',' M./SEC.'
     * ,'MB./DLSQ.','DEG.LAT.','MB.','MB.'/
      data fname/'TOT. TRACK DENSITY','AVE. GRID U VEL.',
     * 'AVE. GRID V VEL.',
     * 'AVE. CENTRAL PRESSURE','AVE.PRESSURE TENDENCY',
     * 'AVE. CENTRAL DELSQ P','AVE.DELSQ P TENDENCY',
     * 'SYSTEM DENSITY','SCALAR AVE''D FLUX','AVE. X FLUX',
     * 'AVE. Y FLUX','AVE. SPEED','AVE. X VEL.','AVE. Y VEL.',
     * 'CYCLOGENESIS','CYCLOLYSIS',
     * 'EASTWARD FLUX','NORTHWARD FLUX','NETT FLUX',
     * 'EASTWARD VELOCITY','NORTHWARD VELOCITY','NETT VELOCITY',
     * 'SCALAR GEOS. VEL.','X GEOST. VELOCITY','Y GEOST. VELOCITY',
     * 'EASTWD GEOS. VEL.','NORTHWD GEOS. VEL.','NETT GEOS. VEL.',
     * 'MEAN PRESSURE','MEAN DELSQ P','X 500 MB. WIND','Y 500 MB. WIND',
     * 'CENTRAL P DIFF.','EASTWD 500 MB. WIND','NORTHWD 500 MB. WIND',
     * 'NETT 500 MB. WIND','CENTRAL DELSQP.','CENTRAL RADIUS',
     * 'INTEG.DELSQP.','DEPTH'/

      data finc/1.,0.2,0.2,5.,1.,0.1,0.1,0.001,0.005,0.005,0.005,1.,
     * 1.,1.,0.1,0.1,0.005,0.005,0.005,1.,1.,1.,1.,1.,1.,1.,1.,1.,
     * 5.,0.1,1.,1.,2.,1.,1.,1.,0.5,0.1,10.,1./

c-----------------------------------------------------------------------
c       (1)  File assignments.
c-----------------------------------------------------------------------

      trackfile = 'trackdat'
      infile  = 'intstat'
      statfile  = ' '
      cvefile   = ' '
      advfile   = ' '
      zonefile  = ' '
      pmfile  = ' '
      zmfile  = ' '
      newform = .false.
      trfm = .false.
      trnml = .false.
      pmdet = .false.
      zmdet = .false.
      exfmt     = ' '
      sector    = .false.
      vdet      = .false.
      sdet      = .false.
      zdet      = .false.
      adet      = .false.
      helpdet   = .false.
      llwrit    = .false.
      llform    = .false.
      idiagsx   = 0
      dystrtx   = 0
      dystopx   = 0
c
c * Help screen
c
      if (iargc().eq.0) then 
c     Usage
c     -----

      write (0,*)'Usage: tstatx [-t:i:s:z:a:b:g:AFNx:d:D:S:P:Z:l:L:]'
      write (0,*) ' '
      write (0,*) '   -t tfile  instruction file (default intrack)'
      write (0,*) '   -i ifile  instruction file (default intrack)'
      write (0,*) '   -s sfile  2-D statistical output file'
      write (0,*) '   -z zfile  zonal average data file'
      write (0,*) '   -a afile  zonal average velocity file'
      write (0,*) '   -b afile  regional average velocity file'
      write (0,*) '   -g gfile  P.S. formatted grid file'
      write (0,*) '   -A, -F          (no longer used)'
      write (0,*) '   -U        unformatted track data' 
      write (0,*) '   -N 	track parameters in namelist form'
      write (0,*) '               (only relevant with -U option)'
      write (0,*) '   -x exfmt  extension to zonal average heading'   
      write (0,*) '   -d idiag  diagnostic output level'
      write (0,*) '   -D d1,d2  Starting and ending dates for analysis'
      write (0,*) '   -S l1,l2  W and E longitude limits for zonal aves'
      write (0,*) '   -P pmfile mean PMSL file'
      write (0,*) '   -Z zmfile mean z500 file'
      write (0,*) '   -l llfile unformatted lat-lon file for analysis'        
      write (0,*) '   -L llfile formatted lat-lon file for analysis'
      stop 
      endif

 10   continue
      nopt = ngtopt("t:i:s:v:z:a:AFNx:d:D:S:P:Z:?l:L:",optind,optarg)
      if (nopt.eq.-1) go to 20
      if (char(nopt).eq.'t') trackfile = optarg
      if (char(nopt).eq.'i') infile = optarg
      if (char(nopt).eq.'s') statfile = optarg
      if (char(nopt).eq.'v') cvefile = optarg
      if (char(nopt).eq.'z') zonefile = optarg
      if (char(nopt).eq.'a') advfile = optarg
      if (char(nopt).eq.'A') newform = .true.
      if (char(nopt).eq.'F') trfm = .true.
      if (char(nopt).eq.'N') trnml = .true.
      if (char(nopt).eq.'P') then
        pmfile = optarg
        pmdet = .true.
      endif
      if (char(nopt).eq.'Z') then
        zmfile = optarg
        zmdet = .true.
      endif
      if (char(nopt).eq.'x') exfmt = optarg
      if (char(nopt).eq.'d') read (optarg,*) idiagsx
      if (char(nopt).eq.'D') read (optarg,*) dystrtx,dystopx
      if (char(nopt).eq.'S') then
        sector = .true.
        read (optarg,*) lon1,lon2
        crossgm = lon2.lt.lon1
      endif
      if (char(nopt).eq.'l') then
        llfile = optarg
        llwrit = .true.
      endif
      if (char(nopt).eq.'L') then
        llfile = optarg
        llwrit = .true.
        llform = .true.
      endif
      go to 10
 20   continue
      if (iargc().eq.optind) then 
          write (6,*) ' Incorrect usage.'
          helpdet = .true.
      endif

      if (cvefile(1:5).ne.'     ')  vdet = .true.
      if (statfile(1:5).ne.'     ') sdet = .true.
      if (zonefile(1:5).ne.'     ') zdet = .true.
      if (advfile(1:5).ne.'     ')  adet = .true.
      zadet = (zdet.or.adet)
      vzadet = (vdet.or.zadet)
      anydet = (vzadet.or.sdet)
      if (.not.anydet) then
          write (6,*) ' No output options selected.'
          helpdet = .true.
      endif
      if (helpdet) go to 95

      if (newform.and.trfm) then
        open (unit=20,file=trackfile,status='old',form='formatted',
     *   err=40)
      else
        open (unit=20,file=trackfile,status='old',form='unformatted',
     *   err=40)
      endif
      go to 45
 40   write (6,*) ' Cannot find file ',trackfile(1:60)
      helpdet = .true.
 45   continue
      open (unit=28,file=infile,status='old',err=90)
      go to 95
 90   write (6,*) ' Cannot find file ',infile(1:60)
      helpdet = .true.
 95   continue

      if (helpdet) stop ' For help type ''tstatx'' ' !ckk

      fileidvd = 'icnvqevduf'
      fileidsd = 'icnvqesduf'
      write (progms,'(''tstat'',i2)') nj

c-----------------------------------------------------------------------
c       (2)  Instruction parameters and history file heading.
c-----------------------------------------------------------------------

c           (a)  Track instruction parameters.
c                -----------------------------

      write (6,*) ' Track history file: ',trackfile(1:lnblnk(trackfile))
      if (newform) then
        call tr1rda(20,40,trfm,trnml,progmt,irun,source,
     *   level,projn,fhem,rprojx,ni1,nj1,progmc,hilo,ddhmc,
     *   kmax,methdc,flat,drlt,latmnc,latmxc,lonmnc,lonmxc,
     *   prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2,
     *   ddhmt,ddhmmt,dmodet,dyepoc,hrepoc,t1,tc,dy1,hr1,dyc,hrc,
     *   methdt,iopmxt,cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
     *   c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,itabt1,itabt2,
     *   ic,lstrk1,lstrk2,ndt,kcycc,ierd)
        if (ierd.eq.1) stop ' Error in track history file.'
        if (ierd.eq.2) stop 
     *   ' Error in/premature end of track history file.'
        if (ierd.eq.3) stop ' Not a track history file.'
        call trtabhda(itabt1,itabt2,nvarlt,idiags)
      else
        read (20,err=50,end=60) fileidtd,progmt,irun,source,
     *   level,projn,rprojx,ni1,nj1,progmc,hilo,ddhmc,
     *   kmax,methdc,flat,drlt,latmnc,latmxc,lonmnc,lonmxc,
     *   prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2,
     *   ddhmt,ddhmmt,dmodet,dyepoc,hrepoc,t1,tc,dy1,hr1,dyc,hrc,
     *   methdt,iopmxt,cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
     *   c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,itabt,
     *   ic,lstrk1,lstrk2,ndt,kcycc

        if (fileidtd.ne.'icnvqetduf') stop ' Not a track history file.'
        go to 70
 50     stop ' Error in reading track history file.'
 60     stop ' End of track history file.'
 70     continue
      endif

c           (b)  Tstat instruction parameters.
c                -----------------------------

      if (newform) then
        idiags  = 1      ! Diagnostic output option               
        dlat    = 5.0    ! Selection of stat. arrays              
        iperdv  = 5      ! Interpolation intervals per period     
        ifc     = 1      ! Centres of pressure (1) or p curv. (2)
        cvmns2  = 0.     ! Min. cv. open dep. at ends of tracks 
        alatlt  = 15.    ! Minimum latitude (N. or S.)         
        wwqmin  = 0.     ! Min. wt.ing for weighted quantities
        ddhms   = 01200  ! Min. track duration (dddhhmm)     
        dystrts = 0      ! Start date(yymmdd)
        dystops = 0      ! Finishing date
        hrstrts = 0      ! Start time(hhmm)
        hrstops = 0      ! Finishing time                  
        Rcdnlt  = 5.     ! Pass radius (deg.lat.) for cyc. density
        Rkdn    = 0.2    ! Pinching of prob. cve. (parabola=1)   
        Rcwqlt  = 5      ! Pass radius (deg.lat.) for wt.'d funcs.
        Rkwq    = 0.2    ! Pinching of prob. cve. for wt.'d funcs.
        Rcgllt  = 0.     ! Pass radius (deg.lat.) for c'genesis funcs.
        Rkgl    = 0.     ! Pinching of prob. cve. for c'genesis funcs.
        ssd     = 1.     ! Scale factor for output SD (default: 1);
                         ! x 1000 convenient for plotting

        write (6,*) ' Tstat instruction file: ',infile(1:lnblnk(infile))
        read (28,nmltstat,err=100,end=110)
      else
        read (28,80,err=100,end=110) idiags,dlat,iperdv,ifc,cvmns2,
     *   alatlt,wwqmin,ddhms,dystrts,hrstrts,dystops,hrstops,Rcdnlt,
     *   Rkdn,Rcwqlt,Rkwq
 80     format (//i3/f7.3/2(i3/),3(f7.3/),i7/2(i6,i4/),3(f9.5/),f9.5)
      endif
      close (unit=28,status='keep')

      go to 120
 100  stop ' Error in reading file.'
 110  stop ' End of file.'
 120  continue

      if (Rcgllt.eq.0.) then
        Rcgllt = Rcwqlt
        Rkgl   = Rkwq
      endif

c           (c)  Summary of Track instruction parameters.
c                ----------------------------------------

      write (6,140) irun,source,level,projn,rproj,ni,nj
 140  format (/' Rect parameters.'/
     * 4x,' irun   = ',i4,   8x,' source = ',a20/
     * 4x,' level  = ',a6,   6x,' projn  = ',2x,a1/ 
     * 4x,' rproj  = ',f7.3, 5x,' ni,nj  = ',i3,1x,i3)
      write (6,150) hilo,progmc,ddhmc,
     * kmax,methdc,flat,drlt,latmnc,latmxc, 
     * lonmnc,lonmxc,
     * prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2
 150  format (/' Cycloc parameters.'/
     * 4x,' hilo   = ',2x,a1,9x,' progmc = ',a20/
     * 4x,' ddhmc  = ',i6,   6x,' kmax   = ',i3/
     * 4x,' methdc = ',i3/
     * 4x,' flat   = ',f9.7, 3x,' drlt   = ',f7.3/
     * 4x,' latmnc = ',f7.3, 5x,' latmxc = ',f7.3/
     * 4x,' lonmnc = ',f7.2, 5x,' lonmxc = ',f7.2/
     * 4x,' prmxc2 = ',f7.3, 5x,' axrmax = ',f7.3/
     * 4x,' drmax1 = ',f7.3, 5x,' drmax2 = ',f7.3/
     * 4x,' itmax1 = ',i3,   9x,' itmax2 = ',i3/)
      write (6,160) ddhmt,ddhmmt,dmodet,dyepoc,hrepoc,
     * dy1,hr1,dyc,hrc,t1,tc,methdt,iopmxt,
     * cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
     * c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,itabt1,
     * itabt2,itabt,progmt
 160  format (' Track parameters.'/
     * 4x,' ddhmt  = ',i6,   6x,' ddhmmt = ',i6/
     * 4x,' dmodet = ',a6,   6x,' epoch  = ',i6,i4/
     * 4x,' strtt  = ',i6,i5,1x,' stopt  = ',i6,i5/
     * 4x,' t1     = ',f9.4, 3x,' tc     = ',f9.4/
     * 4x,' methdt = ',i3,   9x,' iopmxt = ',i3/
     * 4x,' cvmnt1 = ',f7.3, 5x,' cvmnt2 = ',f7.3/
     * 4x,' trakid = ',a20/
     * 4x,' iadvtp = ',i3,   9x,' advfil = ',a20/
     * 4x,' wtmov1 = ',f7.3, 5x,' pmem1  = ',f7.3/
     * 4x,' c1     = ',f7.3, 5x,' c2     = ',f7.3/
     * 4x,' c3     = ',f7.3, 5x,' c4     = ',f7.3/
     * 4x,' c5     = ',f7.3, 5x,' c6     = ',f7.3/
     * 4x,' nsort  = ',i3,   9x,' irevmx = ',i3/
     * 4x,' merget = ',i3,   9x,' qmerge = ',f7.3/
     * 4x,' itabt1 = ',i3,   9x,' itabt2 = ',i3/
     * 4x,' itabt  = ',i3,   9x,' progmt = ',a20)

c           (d)  Checking of tstat instruction parameters.
c                -----------------------------------------

      if (.not.newform) then
      if (ifc.ne.2) ifc = 1
      if ((ifc.eq.2).and.(itabt.eq.1)) stop ' ifc must=1 if itabt=1.'
          cvsdet = .false.
      endif
      if ((cvmns2.gt.cvmnt2).and.(methdc.ge.2)) cvsdet = .true.
      if (iperdv.le.0) stop 'iperdv = 0: execution terminated. '
      if (iperdv.gt.10) then
          write (6,'(/'' iperdv reduced from '',i3,'' to 5.'')')
     *     iperdv
          iperdv = 5
      endif
      if (wwqmin.lt.0.01) wwqmin = 0.01
      if (idiagsx.ne.0) idiags = idiagsx
      if (dystrtx.ne.0) then
        dystrts = dystrtx
        dystops = dystopx
        hrstrts = 0
        hrstops = 0
      endif
      if ((dystrts.lt.dy1).or.((dystrts.eq.dy1).and.
     *     (hrstrts.lt.hr1))) then
          dystrts = dy1
          hrstrts = hr1
      endif
      if ((dystops.eq.0).and.(hrstops.eq.0)) then
          dystops = dyc
          hrstops = hrc
      else if ((dystops.gt.dyc).or.((dystops.eq.dyc).and.
     *     (hrstops.gt.hrc)).or.((dystops.eq.0).and.
     *     (hrstops.eq.0))) then
          dystops = dyc
          hrstops = hrc
      endif
      if ((dystrts.gt.dystops).or.((dystrts.eq.
     *     dystops).and.(hrstrts.gt.hrstops))) stop
     *     ' Starting period later than stopping.'
      if (Rcdnlt.gt.Rcwqlt) then
          write (6,'(/'' Rcdnlt ('',f7.3,'') > Rcwqlt ('',
     *     f7.3,'').  Execution terminated.''/)')
     *     Rcdnlt,Rcwqlt
          stop
      endif

      Rrdnsq = (1./Rkdn - 1.)
      Rrwqsq = (1./Rkwq - 1.)
      Rrglsq = (1./Rkgl - 1.)

      write (6,200) idiags,dlat,iperdv,ifc,cvmns2,
     * alatlt,wwqmin,ddhms,dystrts,hrstrts,dystops,hrstops,
     * Rcdnlt,Rkdn,Rcwqlt,Rkwq,Rcgllt,Rkgl,ssd
 200  format (/' Stat. parameters.'/
     * 4x,' idiags = ',i3,   9x,' dlat   = ',f7.3/
     * 4x,' iperdv = ',i3,   9x,' ifc    = ',i3/
     * 4x,' cvmns2 = ',f7.3, 5x,' alatlt = ',f7.3/
     * 4x,' wwqmin = ',f7.3, 5x,' ddhms  = ',i7/
     * 4x,' strts  = ',i6,i5,1x,' stops  = ',i6,i5/
     * 4x,' Rcdnlt = ',f7.3, 5x,' Rkdn   = ',f7.3/
     * 4x,' Rcwqlt = ',f7.3, 5x,' Rkwq   = ',f7.3/
     * 4x,' Rcgllt = ',f7.3, 5x,' Rkgl   = ',f7.3/
     * 4x,' ssd    = ',f7.1) ! KK 13/12/2003
      if (newform) write (6,201) projn
 201  format (4x,' projn = ',a1)

c           (e)  Quantity for cve file.
c                ----------------------

      if (hilo.eq.'l') hilo = 'L'
      if (hilo.eq.'h') hilo = 'H'
      if (.not.newform.and.ifc.eq.2) then
          if (hilo.eq.'L') feat = 'CURV. MAXIMA' 
          if (hilo.eq.'H') feat = 'CURV. MINIMA'
          fn(3)    = 'CCV'
          fn(4)    = 'CVT'
          funit(3) = ' MB./DLSQ.'
          funit(4) = ' MB./DLSQ./DAY'
          fname(3) = ' AVE. PRESSURE CURV.'
          fname(4) = ' AVE. P.CURV. TENDENCY'
      else
          if (hilo.eq.'L') feat = 'CYCLONES'
          if (hilo.eq.'H') feat = 'ANTICYCLONES'
      endif

c           (f)  Hemisphere
c                ----------

          if ((projn.eq.'n').or.(projn.eq.'N')) then
            projn = 'N'
            hemis = 'N.H.'
            fhem = 1.
          endif
          if ((projn.eq.'s').or.(projn.eq.'S')) then
            projn = 'S'
            hemis = 'S.H.'
            fhem = -1.
          endif

c-----------------------------------------------------------------------
c       (3)  Initial values for statistical routine.
c-----------------------------------------------------------------------

c            (a)  Initialisation of summations.
c            ----------------------------------

      do 220 j = 1,njlt
      do 220 i = 1,nilt
          u(i,j) = 0.
          v(i,j) = 0.
          w(i,j) = 0.
          wt(i,j) = 0.
          sd(i,j) = 0.
          fs(i,j) = 0.
          fx(i,j) = 0.
          fy(i,j) = 0.
          pc(i,j) = 0.
          pt(i,j) = 0.
          cc(i,j) = 0.
          ct(i,j) = 0.
          fg(i,j) = 0.
          fl(i,j) = 0.
          gx(i,j) = 0.
          gy(i,j) = 0.
          gs(i,j) = 0.
 220  continue

      do 240 ls = 1,lsmax
          ifglz(ls) = 0
          ifllz(ls) = 0
          flz(ls) = 0.
          isfglz = 0
          isfllz = 0
          sflz = 0.
          ulz(ls) = 0.
          vlz(ls) = 0.
          wlz(ls) = 0.
          plz(ls) = 0.
          ptlz(ls) = 0.
          clz(ls) = 0.
          ctlz(ls) = 0.
 240  continue

c            (b)  Constants derived from instruction parameters.
c            ---------------------------------------------------

      scalefact = 2.*rad*rproj
      rprojsq = rproj*rproj
      rslt = rproj*tan((90. - alatlt)*0.5/rad)
      rsltsq = rslt*rslt
      rdlat = 1./dlat
      lslt =  int(rdlat*(90. - alatlt))
      if (lslt.gt.lsmax) lslt = lsmax

      write (addhm,'(i7)') ddhmt
      read (addhm,'(i3,2i2)') iday,ihour,imint
      dtt = float(iday) + float(ihour)/24. + float(imint)/1440.
      dtj = dtt/float(iperdv)

      write (addhm,'(i7)') ddhms
      read (addhm,'(i3,2i2)') iday,ihour,imint
      dts = float(iday) + float(ihour)/24. + float(imint)/1440.

      call timedif(dystrts,hrstrts,dystops,hrstops,dmodet,idmin,
     * iddhm,dtss,ierr)

      djss1 = dtss/dtj
      rdjss1 = 1./djss1
c     diss2 = dtss/dtt
      diss2 = dtss   ! NOTE CORRECTION
      rdiss2th = 1000./diss2

      if (idiags.ge.1) write (6,250) scalefact,rprojsq,rslt,rsltsq,
     * dtj,lslt,dtt,dts,dtss1,djss1,dtss2,diss2
 250  format (/' Constants derived from instruction parameters.'/
     * 4x,' scalefact = ',f7.1, 2x,' rprojsq   = ',f7.3/
     * 4x,' rslt      = ',f7.3, 2x,' rsltsq    = ',f7.3/
     * 4x,' dtj       = ',f7.3, 2x,' lslt      = ',i3/
     * 4x,' dtt       = ',f7.3, 2x,' dts       = ',f7.3/
     * 4x,' dtss1     = ',f7.3, 2x,' djss1     = ',f7.3/
     * 4x,' dtss2     = ',f7.3, 2x,' diss2     = ',f7.3)

c           (c)  Headings for data files.
c                ------------------------

      if (vdet) then
      open (unit=30,file=cvefile,status='unknown',form='unformatted')
      write (30) fileidvd,irun,source,level,projn,rproj,ni,nj,
     * progmc,hilo,ddhmc,kmax,methdc,flat,drlt,latmnc,latmxc, 
     * lonmnc,lonmxc,prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2,
     * ddhmt,ddhmmt,dmodet,t1,tc,
     * dy1,hr1,1,dyc,hrc,ic,dyepoc,hrepoc,methdt,iopmxt,
     * cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
     * c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,
     * progmt,iperdv,ifc,cvmns2,alatlt,wwqmin,progms,
     * ddhms,dystrts,hrstrts,dystops,hrstops,
     * hemis,feat,icen,jcen,1,nilt,1,njlt,lstrk2,dtt,dtj
      endif

      if (sdet) then

      if (prmxc2.gt.999.) prmxc2 = 999.
      if (axrmax.gt.999.) axrmax = 999.
c     if (X.gt.999.) X = 999.

      if (llwrit) then
        write (6,'('' Taking new lats-lons. from  : '',a)') 
     *   llfile(1:lnblnk(llfile))
        if (llform) then
          open (36,file=llfile,status='unknown',err=205)
          read (36,'(a80)') head2
crmw          read (36,'(/'' Latitudes  ('',i3,''):'')' ) nlats
ckk          read (36,'(/13x,i3)') nlats !KK 13/12/2003
          read (36,'(a80)') ch80       ! "
          read (36,'(A)')ch80          !KK 30/10/2005
          i1= index(ch80,'(') +1       ! "
          i2= index(ch80,')') -1       ! "
          read(ch80(i1:i2),*)nlats     ! "
          read (36,*) (lats(j),j=1,nlats)
crmw          read (36,'(/'' Longitudes ('',i3,''):'')') nlons
ckk          read (36,'(/13x,i3)') nlons !KK 13/12/2003
          read (36,'(a80)') ch80       ! "
          read (36,'(A)')ch80          !KK 30/10/2005
          i1= index(ch80,'(') +1       ! "
          i2= index(ch80,')') -1       ! "
          read(ch80(i1:i2),*)nlons     ! "
          read (36,*) (lons(i),i=1,nlons)
        else 
          open (36,file=llfile,status='unknown',form='unformatted',
     *     err=205)
          read (36) nlats
          read (36) (lats(j),j=1,nlats)
          read (36) nlons
          read (36) (lons(i),i=1,nlons)
          read (36) head2
        endif
        go to 206
  205   continue
        stop ' Cannot find file.'
  206   continue
        close (36)

        if (diag) then
          write (6,'(/a)') 
     *     ' Longitudes and latitudes:'
          write (6,'(/a80)') head2
          write (6,'(/'' Latitudes  ('',i3,''):'')') nlats
          write (6,207) (lats(j),j=1,nlats)
  207      format (8f10.3)
          write (6,'(/'' Longitudes ('',i3,''):'')') nlons
          write (6,207) (lons(i),i=1,nlons)
          write (6,*)
          write (6,*) ' Lats. and lons. for output read.'
        endif

        llhead = ' '
        if (hilo.eq.'H') then
          llhead(5:6)   = 'HI'
        else
          llhead(5:6)   = 'LO'
        endif
        llhead(8:20)  = level(1:lnblnk(level))
        llhead(31:40) = source(1:10)
        write (llhead(43:48),'(i6)') dystrts
        llhead(49:49) = '-'
        write (llhead(50:55),'(i6)') dystops

        dellat = (lats(nlats-1)-lats(2))/float(nlats-3)
        dellon = (lons(nlons-1)-lons(2))/float(nlons-3)
        write (llhead(71:80),'(f3.1,''x'',f3.1,''DEG'')') dellat,dellon
        write (6,'(a80)') llhead
      else

      write (shead,210) fileidsd,irun,source,level,projn,rproj,ni,
     * nj,progmc,hilo,ddhmc,kmax,methdc,flat,drlt,latmnc,latmxc, 
     * lonmnc,lonmxc,prmxc2,axrmax,drmax1,drmax2,itmax1,itmax2,
     * ddhmt,ddhmmt,dmodet,t1,tc,dy1,hr1,1,dyc,hrc,ic,methdt,iopmxt,
     * cvmnt1,cvmnt2,trakid,iadvtp,advfil,wtmov1,pmem1,
     * c1,c2,c3,c4,c5,c6,nsort,irevmx,merget,qmerge,
     * progmt,iperdv,ifc,cvmns2,alatlt,wwqmin,progms,
     * ddhms,dystrts,hrstrts,dystops,hrstops,Rcdnlt,Rkdn,Rcwqlt,Rkwq,
     * hemis,feat,lslt,jslt,icen,jcen,1,nilt,1,njlt
 210  format (a10,i10,a20,a6,4x,a1,9x,f10.5,2i10,
     * a20,a1,9x,3i10,10f10.5,4i10,
     * a6,4x,2f10.5,8i10,2f10.5,a20,i10,a20,8f10.5,3i10,f10.5,a20,
     * 2i10,3f10.3,a20,5i10,4f10.5,a4,6x,a12,8x,8i10)
      endif
c     write (6,'(a80)') sshead

      endif

c-----------------------------------------------------------------------
c       (4)  Statistical summations.
c-----------------------------------------------------------------------

      do 480 trkdash = 1,99999

c           (a)  Reading of track histories.
c                ---------------------------

      if (newform) then
c       write (6,*) ' tr2rda ',trfm,trk,statt1,statt2,ifst,
c    *   ilst,nisg,idyfst,ihrfst,idylst,ihrlst,itabt1z,itabt2z
        call tr2rda(20,trfm,trk,statt1,statt2,ifst,ilst,nisg,
     *   idyfst,ihrfst,idylst,ihrlst,itabt1z,itabt2z,ierd)
        if (ierd.eq.1) then
          if (trkdash.gt.lstrk2) then
            write (6,'(a,a,i5)') ' All tracks processed.',
     *       '  Last track was ',trk
            go to 490
          else
            write (6,*)
     *       ' tr2rda: Unexpected end of track history file.'
            write (6,*) ' Track ',trk,trkdash
            go to 490
c           stop ' tr2rda: Unexpected end of track history file.'
          endif
        endif
        if (ierd.gt.1) write (6,*) ' Track ',trk,trkdash
        if (trkdash.gt.lstrk2) then
          write (6,*) ' File goes beyond track ',lstrk2
          go to 490
        endif
        if (ierd.eq.2) stop 
     *   ' tr2rda: Error in of track history file.'
        if (ierd.eq.3) then
          write (6,'(/'' Not a track (histz3 or 4) file '',a80/)')
     *     trackfile
          stop
        endif
      else
        read (20,err=260,end=490) fileidh3,trakidz,trk,statt1,statt2,
     *   ifst,ilst,nisg,idyfst,ihrfst,idylst,ihrlst,itabty
        if (fileidh3.ne.'icnvqeh3uf') then
          write (6,'(/'' Not a track (histz3 or 4) file '',a80/)')
     *     trackfile
          stop
        endif
        go to 270
  260   stop ' Error in reading track history file.'
  270   continue
      endif
      if (nisg.gt.nisglt) stop 'isg exceeds array bounds'
c     write (99,*) trk,idyfst,ihrfst,dystops,hrstops
      if ((idyfst.gt.dystops).or.((idyfst.eq.dystops).and.
     * (ihrfst.gt.hrstops))) go to 490
      skip = .false.
      if ((idylst.lt.dystrts).or.((idylst.eq.dystrts).and.
     * (ihrlst.lt.hrstrts))) then
          if (newform) then
            skip = .true.
          else
            read (20) dummy
            go to 475
          endif
      endif

      if (newform) then
c       if (ifc.ne.1) stop ' ifc must = 1.'
        call tr3rda(20,trfm,skip,nvarlt,nisglt,nisg,vart,ierd)
        if (ierd.ge.1) write (6,*) ' Track ',trk
        if (ierd.eq.1) stop 
     *   ' tr3rda: Unexpected end of track history file.'
        if (ierd.eq.2) stop 
     *   ' tr3rda: Error in of track history file.'
        if (ierd.eq.3) then
          write (6,'(/'' Not a track (histz3 or 4) file '',a80/)')
     *     trackfile
          stop
        endif
        if (skip) go to 475
        do 280 isg=1,nisg
          if (projn.eq.'N' .and. y(isg).lt.-40. .or.
     *        projn.eq.'S' .and. y(isg).gt. 40.) go to 475
          call lltops(xcen,ycen,rproj,fhem,x(isg),y(isg),xx,yy)
          x(isg) = xx
          y(isg) = yy
  280   continue
      else
          if (itabty.eq.1) then
          read (20) (t(isg),dy(isg),hr(isg),
     *     stat1(isg),stat2(isg),k(isg),iop(isg),x(isg),y(isg),
     *     p(isg),q(isg),gxi(isg),gyi(isg),
     *     depi(isg),r0i(isg),
     *     isg=1,nisg)
          else if (ifc.eq.2) then
          read (20) (t(isg),dy(isg),hr(isg),
     *     stat1(isg),stat2(isg),k(isg),iop(isg),xc,yc,p(isg),
     *     x(isg),y(isg),cv(isg),q(isg),gxi(isg),gyi(isg),
     *     depi(isg),r0i(isg),
     *     isg=1,nisg)
          else
          read (20) (t(isg),dy(isg),hr(isg),
     *     stat1(isg),stat2(isg),k(isg),iop(isg),x(isg),y(isg),
     *     p(isg),xh,yh,cv(isg),q(isg),gxi(isg),gyi(isg),
     *     depi(isg),r0i(isg),
     *     isg=1,nisg)
          endif
      endif

c     write (6,*) ' Track read: ',trk,' nisg = ',nisg

c           (b)  Removal of open depressions, if required.
c                -----------------------------------------
 
          irreg = 0
          pretrk = .true.
          prestt = .true.
      if (cvsdet) then
          isgg = 0
          isgy = 0
      else
          isgg = 1
          isgy = nisg
      endif
          isgf = 0
          isgl = 0
          ibegin = 1
          if (statt1.eq.1) ibegin = 2
          iend = nisg
          if (statt1.eq.1) iend = nisg - 1
          jsg = 1
          mdt = 0.
          cvmxf = 0.
          cvmxl = 0.

      do 300 isg = 1,nisg
          if ((itabty.eq.2).and.(ifc.eq.2)) then
              pcv(isg) = cv(isg)
          else
              pcv(isg) = p(isg)
          endif
          if (isg.gt.1) then
              mdt = t(isg) - t(isgm1)
              difft = abs(mdt - dtt)
              if (difft.ge.0.001) irreg = 1
          endif
              njmod = nint(mdt/dtj)
              jsg = jsg + njmod
              ji(isg) = jsg

          if (cvsdet) then
          if ((pretrk).and.(isg.ge.ibegin).and.((iop(isg).eq.0).or.
     *     (cv(isg).gt.cvmns2))) then
              isgg = isg
              pretrk = .false.
          endif
          if ((isg.le.iend).and.((iop(isg).eq.0).or.
     *     (cv(isg).gt.cvmns2)))
     *     isgy = isg
          endif

          if (prestt) then
          if ((dy(isg).gt.dystrts).or.
     *     ((dy(isg).eq.dystrts).and.(hr(isg).ge.hrstrts))) then
              prestt = .false.
              isgf = isg
          endif
              if (cv(isg).gt.cvmxf) cvmxf = cv(isg)
          endif
          if ((dy(isg).lt.dystops).or.((dy(isg).eq
     *     .dystops).and.(hr(isg).le.dystops))) then
              isgl = isg
          else
              if (cv(isg).gt.cvmxl) cvmxl = cv(isg)
          endif

          isgm1 = isg
 300  continue

          njsg = ji(nisg)
          if (njsg.gt.njsglt) stop ' jsg exceeds array bounds'

          jsgg = ji(isgg)
          jsgy = ji(isgy)
          jsgf = ji(isgf)
          jsgl = ji(isgl)
          jsggf = max0(jsgg,jsgf)
          jsgyl = min0(jsgy,jsgl)

      tgl = t(isgy) - t(isgg)

c     if (idiags.ge.2)
c    *     write (6,310) trk,statt1,statt2,ifst,ilst,nisg,
c    *     ibegin,iend,isgg,isgy,jsgg,jsgy,dy(1),hr(1),
c    *     dy(nisg),hr(nisg),tgl,irreg,isgf,isgl
c310      format (/' Track ',i4,': stat = ',i1,i1,', ifst = ',i4,
c    *     ', first pd. = ',i6,x,i4,', last pd. = ',i6,x,i4,',',
c    *     ', ilst = ',i4,', nisg = ',i4,
c    *     ', ibegin = ',i4,', iend = ',i4,
c    *     ', isgg = ',i4,', isgy = ',i4,','/' jsgg = ',i3,
c    *     ', jsgy = ',i3,
c    *     ', first pd. = ',i6,x,i4,', last pd. = ',i6,x,i4,',',
c    *     ' tgl = ',f7.3,', irreg = ',i1,','/' isgf = ',
c    *     i4,', isgl = ',i4,'.'//
c    *     ' isg          day     date  time  stat   k  iop',
c    *     '      (x,y)         p     cvh    q',
c    *     '           (ax,ay)         ap   mdt'/)
c          write (6,*) 't = ',(t(isg),isg=1,nisg),
c    *     ', ji = ',(ji(isg),jsg=1,nisg)

c           ( )  Double cv to get laplacian in mb/deglatsq
c                -----------------------------------------

          if (.not.newform) then
            do 315 isg = 1,nisg
              cv(isg) = 2.*cv(isg)
  315       continue
          endif
            
c           (c)  Spline coefficients for cyclone track.
c                --------------------------------------

          call cspline(0,0.,t,x,ax,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,y,ay,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,pcv,ap,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,cv,ac,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,gxi,au,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,gyi,av,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,r0i,ac2,gamma,nisg,0,0.,0.,0)
          call cspline(0,0.,t,depi,ac4,gamma,nisg,0,0.,0.,0)

c           (d)  Constants for each isg.
c                -----------------------

          isgy1 = isgy
          if (isgy.eq.1) isgy1 = 2

      do 450 isg = 1,nisg

      if (isg.eq.1) then
          mdt = 0.
          xj(1) = x(1)
          yj(1) = y(1)
          pj(1) = pcv(1)
          cj(1) = cv(1)
          gxj(1) = gxi(1)
          gyj(1) = gyi(1)
          r0j(1) = r0i(1)
          depj(1) = depi(1)
          go to 420
      endif
      if ((.not.vdet).and.((isg.lt.isgg).or.(isg.gt.isgy1)))
     * go to 420

      mdt = t(isg) - t(isgm1)
      if (mdt.eq.0.) then
          write (6,'(/'' mdt = 0. (trk = '',i5,'', isg = '',i3,
     *     '')  Execution terminated.''/)') trk,isg
          stop
      endif

      sdd = mdt*sixth
      sddsq = mdt*sdd
      gxav = (x(isg) - x(isgm1))/mdt
      gyav = (y(isg) - y(isgm1))/mdt
      gpav = (pcv(isg) - pcv(isgm1))/mdt
      gcav = (cv(isg) - cv(isgm1))/mdt
      jmodstt = 1
      if (isg.eq.2) jmodstt = 0
      njmod = ji(isg) - ji(isgm1)

c            (e)  Position and velocity at each stat. period.
c            ------------------------------------------------

      do 410 jmod = jmodstt,njmod

          jsg = ji(isgm1) + jmod

      if ((.not.vdet).and.((jsg.lt.jsggf).or.(jsg.gt.jsgyl)))
     * go to 400
      ainc = 1.
      if ((jsg.eq.jsggf).or.(jsg.eq.jsgyl)) ainc = 0.5
      if (jsggf.eq.jsgyl) ainc = 0.

      if ((jmod.eq.0).or.(jmod.eq.njmod)) then

          if (jmod.eq.0) then
              uj = gxav - (2.*ax(1) + ax(2))*sdd
              vj = gyav - (2.*ay(1) + ay(2))*sdd
              ptj = gpav - (2.*ap(1) + ap(2))*sdd
              ctj = gcav - (2.*ac(1) + ac(2))*sdd
          else
              xj(jsg) = x(isg)
              yj(jsg) = y(isg)
              pj(jsg) = pcv(isg)
              cj(jsg) = cv(isg)
              gxj(jsg) = gxi(isg)
              gyj(jsg) = gyi(isg)
              r0j(jsg) = r0i(isg)
              depj(jsg) = depi(isg)
              uj = gxav + (ax(isgm1) + 2.*ax(isg))*sdd
              vj = gyav + (ay(isgm1) + 2.*ay(isg))*sdd
              ptj = gpav + (ap(isgm1) + 2.*ap(isg))*sdd
              ctj = gcav + (ac(isgm1) + 2.*ac(isg))*sdd
          endif

      else

          B = float(jmod)/float(njmod)
          A = 1. - B

          AA = A*A
          BB = B*B

          xj(jsg) = A*x(isgm1) + B*x(isg) + (A*(AA - 1.)*ax(isgm1) +
     *     B*(BB - 1.)*ax(isg))*sddsq
          yj(jsg) = A*y(isgm1) + B*y(isg) + (A*(AA - 1.)*ay(isgm1) +
     *     B*(BB - 1.)*ay(isg))*sddsq
          pj(jsg) = A*pcv(isgm1) + B*pcv(isg) + (A*(AA - 1.)*ap(isgm1) +
     *     B*(BB - 1.)*ap(isg))*sddsq
          cj(jsg) = A*cv(isgm1) + B*cv(isg) + (A*(AA - 1.)*ac(isgm1) +
     *     B*(BB - 1.)*ac(isg))*sddsq
          gxj(jsg) = A*gxi(isgm1) + B*gxi(isg) + (A*(AA - 1.)*au(isgm1)+
     *     B*(BB - 1.)*au(isg))*sddsq
          gyj(jsg) = A*gyi(isgm1) + B*gyi(isg) + (A*(AA - 1.)*av(isgm1)+
     *     B*(BB - 1.)*av(isg))*sddsq
          r0j(jsg) = A*r0i(isgm1) + B*r0i(isg) + 
     *    (A*(AA - 1.)*ac2(isgm1)+
     *     B*(BB - 1.)*ac2(isg))*sddsq
          depj(jsg) = A*depi(isgm1) + B*depi(isg) + 
     *    (A*(AA - 1.)*ac4(isgm1)+
     *     B*(BB - 1.)*ac4(isg))*sddsq
          uj = gxav - ((3*AA - 1.)*ax(isgm1) - (3*BB - 1.)*ax(isg))*sdd
          vj = gyav - ((3*AA - 1.)*ay(isgm1) - (3*BB - 1.)*ay(isg))*sdd
          wj = sqrt(uj*uj + vj*vj)
          ptj = gpav - ((3*AA - 1.)*ap(isgm1) - (3*BB - 1.)*ap(isg))*sdd
          ctj = gcav - ((3*AA - 1.)*ac(isgm1) - (3*BB - 1.)*ac(isg))*sdd

      if (tgl.lt.dts) go to 400
      if ((jsg.lt.jsggf).or.(jsg.gt.jsgyl)) go to 400
      if (xj(jsg).lt.1. .or. xj(jsg).gt.float(ni) .or.
     *    yj(jsg).lt.1. .or. yj(jsg).gt.float(nj)) go to 400

      if (idiags.ge.3) then
          if (ifc.eq.1) then
          write (6,320) jsg,xj(jsg),yj(jsg),pj(jsg),cj(jsg)
 320      format (i5,41x,'   (',f5.2,',',f5.2,') ',f8.2,1x,f5.2)
          else
          write (6,330) jsg,xj(jsg),yj(jsg),pj(jsg)
 330      format (i5,41x,'   (',f5.2,',',f5.2,') ',8x,1x,f5.2)
          endif
      endif

      endif

c            (f)  Latitude scaling.
c                 -----------------

      xs = xj(jsg) - xcen
      ys = yj(jsg) - ycen
      rssq = xs*xs + ys*ys
      rs = sqrt(rssq)
c     if (rs.gt.rproj) write (6,'(/'' Warning: rs = '',f7.3,
c    * '', rproj = '',f7.3)') rs,rproj

      clat = clatt(rs,rproj)
      alat = 90. - clat
      if (alat.lt.alatlt2) go to 400
      scallat = scalefact/(rprojsq + rssq)
      Rcdn = Rcdnlt/scallat
      if (Rcdn.lt.0.8) Rcdn = 0.8
      Rcdnsq = Rcdn*Rcdn
      Rcwq = Rcwqlt/scallat
      if (Rcwq.lt.0.8) Rcwq = 0.8
      Rcwqsq = Rcwq*Rcwq

c            (g)  Distribution of zonal and meridional
c                 components of velocity for each zone.
c                 -------------------------------------

      if (rs.gt.0.01) then
          costheta = ys/rs
          sintheta = xs/rs
          sintheta1 = sintheta
          if ((projn.eq.'n').or.(projn.eq.'N')) then
              costheta = -costheta
              sintheta = -sintheta
          endif
          ujdash = uj*costheta - vj*sintheta
          vjdash = uj*sintheta + vj*costheta
          if (sector) then
            lonj = atan2(sintheta1,costheta)*rad
            if (lonj.lt.0.) lonj = lonj + 360.
            if ((lonj.gt.360.).or.(lonj.lt.0.)) stop
     *       ' Longitudes not allowable '
            if (crossgm) then
              insect = ((lonj.gt.lon1).or.(lonj.lt.lon2))
            else
              insect = ((lonj.gt.lon1).and.(lonj.lt.lon2))
            endif
          else
            insect = .true.
          endif
      else
          ujdash = 0.
          vjdash = 0.
          insect = .not.sector
      endif
c     if (jsg.eq.1) write (99,*)
c    *     xj(jsg),yj(jsg),xs,ys,lonj,insect

      ls = 1 + int(rdlat*(90. - alat))
      if (insect.and.(ls.le.lslt).and.(ls.ge.1)) then
          sflz = sflz + ainc
          flz(ls) = flz(ls) + ainc
          ulz(ls) = ulz(ls) + ujdash*ainc
          vlz(ls) = vlz(ls) + vjdash*ainc
          wlz(ls) = wlz(ls) + wj*ainc
          plz(ls) = plz(ls) + pj(jsg)*ainc
          ptlz(ls) = ptlz(ls) + ptj*ainc
          clz(ls) = clz(ls) + cj(jsg)*ainc
          ctlz(ls) = ctlz(ls) + ctj*ainc
      endif

c            (h)  Distribution of weighted x and y components
c                 of velocity for each grid point.
c                 -------------------------------------------

          ilowq = int(xj(jsg) - Rcwq)
          ihiwq = int(xj(jsg) + Rcwq) + 1
          jlowq = int(yj(jsg) - Rcwq)
          jhiwq = int(yj(jsg) + Rcwq) + 1
          if (ilowq.gt.nilt .or. ihiwq.lt.1 .or. 
     *        jlowq.gt.njlt .or. jhiwq.lt.1) go to 360

      do 350 j = jlowq,jhiwq
      do 340 i = ilowq,ihiwq
          xdif = float(i) - xj(jsg)
          ydif = float(j) - yj(jsg)
          rdifsq = xdif*xdif + ydif*ydif
          if (rdifsq.lt.Rcwqsq) then
          if ((i.ge.1).and.(i.le.nilt).and.(j.ge.1).and.
     *     (j.le.njlt)) then
          wtwq    = ainc*(Rcwqsq - rdifsq)/(Rcwqsq + rdifsq*Rrwqsq)
          wt(i,j) = wt(i,j) + wtwq
          u(i,j)  = u(i,j)  + uj*wtwq
          v(i,j)  = v(i,j)  + vj*wtwq
          w(i,j)  = w(i,j)  + wj*wtwq
          pc(i,j) = pc(i,j) + pj(jsg)*wtwq
          pt(i,j) = pt(i,j) + ptj*wtwq
          cc(i,j) = cc(i,j) + cj(jsg)*wtwq
          ct(i,j) = ct(i,j) + ctj*wtwq
          gx(i,j) = gx(i,j) + gxj(jsg)*wtwq
          gy(i,j) = gy(i,j) + gyj(jsg)*wtwq
          r0(i,j) = r0(i,j) + r0j(jsg)*wtwq
          dep(i,j) = dep(i,j) + depj(jsg)*wtwq
          endif
          endif
 340  continue
 350  continue
 360  continue

c            (hh)  Distribution of frequency tallies
c                 ---------------------------------

          ilodn = int(xj(jsg) - Rcdn)
          ihidn = int(xj(jsg) + Rcdn) + 1
          jlodn = int(yj(jsg) - Rcdn)
          jhidn = int(yj(jsg) + Rcdn) + 1

          if (ihidn-ilodn+1.gt.niwt .or. jhidn-jlodn+1.gt.njwt) then
            write (6,*) 'xj(jsg),yj(jsg),scallat,Rcdnlt,Rcdn,',
     *       'ilodn,ihidn,jlodn,jhidn,niwt,njwt'
     *       ,'scalefact,xcen,ycen,xs,ys,rs,rssq'
            write (6,*) xj(jsg),yj(jsg),scallat,Rcdnlt,Rcdn,
     *       ilodn,ihidn,jlodn,jhidn,niwt,njwt
     *       ,scalefact,xcen,ycen,xs,ys,rs,rssq
            stop 'wtdn(niwt,njwt) array not large enough.'
          endif
          ilodnm1 = ilodn - 1
          jlodnm1 = jlodn - 1
          sumwt = 0.
      do 380 j = jlodn,jhidn
      do 370 i = ilodn,ihidn
          xdif = float(i) - xj(jsg)
          ydif = float(j) - yj(jsg)
          rdifsq = xdif*xdif + ydif*ydif
            idash = i - ilodnm1
            jdash = j - jlodnm1

            if (rdifsq.lt.Rcdnsq) then
              icount(idash,jdash) = 1
              wtdn(idash,jdash) = (Rcdnsq - rdifsq)/
     *         (Rcdnsq + rdifsq*Rrdnsq)
              sumwt = sumwt + wtdn(idash,jdash)
            else
              wtdn(idash,jdash)   = 0.
              icount(idash,jdash) = 0
            endif
 370  continue
 380  continue

c            (i)  Normalisation of frequency distributions for each
c                 grid point.
c                 -------------------------------------------------

      if (sumwt.gt.0.) then
          rsumwt = ainc/sumwt
          wrsumwt = wj*rsumwt
          ursumwt = uj*rsumwt
          vrsumwt = vj*rsumwt
          istrt = max0(1,ilodn)
          istop = min0(nilt,ihidn)
          jstrt = max0(1,jlodn)
          jstop = min0(njlt,jhidn)
          do 390 j = jstrt,jstop
          do 390 i = istrt,istop
              idash = i - ilodnm1
              jdash = j - jlodnm1
              if (icount(idash,jdash).eq.1) then
              sd(i,j) = sd(i,j) + wtdn(idash,jdash)*rsumwt
              fs(i,j) = fs(i,j) + wtdn(idash,jdash)*wrsumwt
              fx(i,j) = fx(i,j) + wtdn(idash,jdash)*ursumwt
              fy(i,j) = fy(i,j) + wtdn(idash,jdash)*vrsumwt
              endif
 390      continue
      endif

 400  continue
 410  continue
 420  continue

      if (idiags.ge.2) then
c         if ((tgl.ge.dts).and.(isg.ge.isgg).and.(isg.le.isgy).and.
          if ((isg.ge.isgg).and.(isg.le.isgy).and.
     *     (isg.ge.isgf).and.(isg.le.isgl))
     *     write (6,440) isg,t(isg),dy(isg),hr(isg),
     *     stat1(isg),stat2(isg),k(isg),iop(isg),x(isg),y(isg),
     *     p(isg),cv(isg),q(isg),ax(isg),ay(isg),ap(isg),mdt
c    *     p(isg),cv(isg),q(isg),sflz,isfglz,isfllz,mdt
 440      format (i3,2x,4x,2x,f8.4,2x,i6,1x,i4,3x,i1,i1,2x,i3,2x,i2,
     *     '   (',f5.2,',',f5.2,') ',f8.2,1x,f5.2,2x,f5.3,
     *     '    ',f7.1,1x,f7.1,1x,f7.1,1x,f4.2)
c    *     '    ',f7.1,x,i7,x,i7,x,f4.2)
      endif
cXX to add cc,ct

          isgm1 = isg

 450  continue

c            (j)  Positions of cyclogenesis and cyclolysis.
c                 -----------------------------------------

          xgen = 0.
          ygen = 0.
          xlys = 0.
          ylys = 0.
      if (tgl.ge.dts) then
      if ((isgg.ne.0).
     * and.((dystrts.lt.dy(isgg)).or.((dystrts.eq.dy(isgg)).and
     * .(hrstrts.lt.hr(isgg))))) then
          xgen = x(isgg)
          ygen = y(isgg)
          xs = xgen - xcen
          ys = ygen - ycen
          rs = sqrt(xs*xs + ys*ys)
          ls = 1 + int(rdlat*clat)
          if (ls.le.lslt) ifglz(ls) = ifglz(ls) + 1
          if (ls.le.lslt) isfglz = isfglz + 1
      endif
      if ((isgy.ne.0).
     * and.((dystops.gt.dy(isgy)).or.((dystops.eq.dy(isgy)).and
     * .(hrstops.gt.hr(isgy))))) then
          xlys = x(isgy)
          ylys = y(isgy)
          xs = xlys - xcen
          ys = ylys - ycen
          rs = sqrt(xs*xs + ys*ys)
          ls = 1 + int(rdlat*clat)
          if (ls.le.lslt) ifllz(ls) = ifllz(ls) + 1
          if (ls.le.lslt) isfllz = isfllz + 1
      endif
      endif

      do 460 ngl = 1,2
      if ((ngl.eq.1).and.(xgen.ge.1.).and.(ygen.ge.1.)) then
          xgl = xgen
          ygl = ygen
      else if ((ngl.eq.2).and.(xlys.ge.1.).and.(ylys.ge.1.)) then
          xgl = xlys
          ygl = ylys
      else
          go to 461
      endif

          xs = xgl - xcen
          ys = ygl - ycen
          rssq = xs*xs + ys*ys
          rs = sqrt(rssq)
c         if (rs.gt.rproj) write (6,'(/'' Warning: rs = '',f7.3,
c    *     '', rproj = '',f7.3)') rs,rproj
          scallat = scalefact/(rprojsq + rssq)
          Rcgl = Rcgllt/scallat
          if (Rcgl.lt.0.8) Rcgl = 0.8
          Rcglsq = Rcgl*Rcgl

          ilogl = int(xgl - Rcgl)
          ihigl = int(xgl + Rcgl) + 1
          jlogl = int(ygl - Rcgl)
          jhigl = int(ygl + Rcgl) + 1
          if (ihigl-ilogl+1.gt.niwt .or. jhigl-jlogl+1.gt.njwt) then
            write (6,*) 'xj(jsg),yj(jsg),scallat,Rcgllt,Rcgl,',
     *       'ilogl,ihigl,jlogl,jhigl,niwt,njwt'
     *       ,'scalefact,xcen,ycen,xs,ys,rs,rssq'
            write (6,*) xj(jsg),yj(jsg),scallat,Rcgllt,Rcgl,
     *       ilogl,ihigl,jlogl,jhigl,niwt,njwt
     *       ,scalefact,xcen,ycen,xs,ys,rs,rssq
            stop 'wtdn(niwt,njwt) array not large enough.'
          endif
          iloglm1 = ilogl - 1
          jloglm1 = jlogl - 1
          sumwt = 0.

      do 457 j = jlogl,jhigl
      do 456 i = ilogl,ihigl

          idash = i - iloglm1
          jdash = j - jloglm1
          xdif = float(i) - xgl
          ydif = float(j) - ygl
          rdifsq = xdif*xdif + ydif*ydif

            if (rdifsq.lt.Rcglsq) then
              icount(idash,jdash) = 1
              wtdn(idash,jdash) = (Rcglsq - rdifsq)/
     *         (Rcglsq + rdifsq*Rrglsq)
              sumwt = sumwt + wtdn(idash,jdash)
            else
              wtdn(idash,jdash)   = 0.
              icount(idash,jdash) = 0
            endif
 456  continue
 457  continue

      if (sumwt.gt.0.) then
          istrt = max0(1,ilogl)
          istop = min0(nilt,ihigl)
          jstrt = max0(1,jlogl)
          jstop = min0(njlt,jhigl)
          iloglm1 = ilogl - 1
          jloglm1 = jlogl - 1
          rsumwt = 1./sumwt
          do 458 j = jstrt,jstop
          do 458 i = istrt,istop
              idash = i - iloglm1
              jdash = j - jloglm1
              if (icount(idash,jdash).eq.1) then
              if (ngl.eq.1) then
                  fg(i,j) = fg(i,j) + wtdn(idash,jdash)*rsumwt
              else
                  fl(i,j) = fl(i,j) + wtdn(idash,jdash)*rsumwt
              endif
              endif
 458      continue
      endif

 460  continue
 461  continue

      if (vdet) then
      write (30) trk,dy(1),hr(1),dy(nisg),hr(nisg),nisg,njsg,ifst,ilst,
     * statt1,statt2,tgl,irreg,isgf,isgl,jsgf,jsgl,cvmxf,cvmxl,
     * dy(isgf),hr(isgl),dy(isgl),hr(isgl)
      write (30) (x(isg),y(isg),p(isg),cv(isg),t(isg),dy(isg),
     * hr(isg),ji(isg),iop(isg),isg=isgf,isgl)
      write (30) (xj(jsg),yj(jsg),pj(jsg),jsg=jsgf,jsgl)
      endif

 475  continue
 480  continue
 490  continue

      if (vdet) write (6,*) ' Cve data written to file: ',cvefile
     * (1:lnblnk(cvefile))

c-----------------------------------------------------------------------
c        (5)  Normalisation of grid point statistics.
c-----------------------------------------------------------------------

c           (a)  Grid scaled velocities, pressures.
c                ----------------------------------

      if (.not.sdet) go to 650
      open (unit=29,file=statfile,status='unknown',form='unformatted')

c     write (6,'(a80)') sshead
C     write (6,*) 'A'
      call statwrite(1,fs,lons,lats,g)
C     write (6,*) 'B'
c     write (6,'(a80)') sshead

      do 510 j = 1,njlt
        ys = j - jcen
      do 510 i = 1,nilt
        xs = i - icen
        rssq = xs*xs + ys*ys
        if ((wt(i,j).lt.wwqmin).or.(rssq.gt.rsltsq)) then
          u(i,j) = spval
          v(i,j) = spval
          w(i,j) = spval
          pc(i,j) = spval
          pt(i,j) = spval
          cc(i,j) = spval
          ct(i,j) = spval
          gx(i,j) = spval
          gy(i,j) = spval
          r0(i,j) = spval
          dep(i,j) = spval
        else
          u(i,j) = u(i,j)/wt(i,j)
          v(i,j) = v(i,j)/wt(i,j)
          w(i,j) = w(i,j)/wt(i,j)
          pc(i,j) = pc(i,j)/wt(i,j)
          pt(i,j) = pt(i,j)/wt(i,j)
          cc(i,j) = cc(i,j)/wt(i,j)
          ct(i,j) = ct(i,j)/wt(i,j)
          gx(i,j) = gx(i,j)/wt(i,j)
          gy(i,j) = gy(i,j)/wt(i,j)
          r0(i,j) = r0(i,j)/wt(i,j)
          dep(i,j) = dep(i,j)/wt(i,j)

          if ((gx(i,j).eq.spval).or.(gy(i,j).eq.spval)) then
            gx(i,j) = spval
            gy(i,j) = spval
c           gs(i,j) = spval
          endif
        endif
 510  continue

      call statwrite(2,u,lons,lats,g)
      call statwrite(3,v,lons,lats,g)
      call statwrite(4,pc,lons,lats,g)
      call statwrite(5,pt,lons,lats,g)
      call statwrite(6,cc,lons,lats,g)
      call statwrite(7,ct,lons,lats,g)

c           (b)  Density functions, zonal & meridional velocities.
c                -------------------------------------------------

      do 520 j = 1,njlt
      do 520 i = 1,nilt
        xs = i - icen
        ys = j - jcen
        rssq = xs*xs + ys*ys
        scallat = scalefact/(rprojsq + rssq)
        recsl = 1./scallat
        recslsq = recsl*recsl

        sd(i,j) = sd(i,j)*recslsq*rdjss1*ssd !KK 13/12/2003
        fs(i,j) = fs(i,j)*recsl*rdjss1
        fx(i,j) = fx(i,j)*recsl*rdjss1
        fy(i,j) = fy(i,j)*recsl*rdjss1
        fg(i,j) = fg(i,j)*recslsq*rdiss2th
        fl(i,j) = fl(i,j)*recslsq*rdiss2th

        if (u(i,j).eq.spval) then
          u(i,j) = spval
          v(i,j) = spval
          w(i,j) = spval
        else
          u(i,j) = u(i,j)*scallat*1.286
          v(i,j) = v(i,j)*scallat*1.286
          w(i,j) = w(i,j)*scallat*1.286
        endif
 520  continue

      call statwrite( 8,sd,lons,lats,g)
      call statwrite( 9,fs,lons,lats,g)
      call statwrite(10,fx,lons,lats,g)
      call statwrite(11,fy,lons,lats,g)
      call statwrite(12,w ,lons,lats,g)
      call statwrite(13,u ,lons,lats,g)
      call statwrite(14,v ,lons,lats,g)
c     call statwrite(23,gs,lons,lats,g)
      call statwrite(24,gx,lons,lats,g)
      call statwrite(25,gy,lons,lats,g)
      call statwrite(38,r0,lons,lats,g)
      call statwrite(40,dep,lons,lats,g)
      call statwrite(15,fg,lons,lats,g)
      call statwrite(16,fl,lons,lats,g)

c           (b)  Flux and velocity transformations
c                ---------------------------------

      sgnhem = 1.
      if ((projn.eq.'n').or.(projn.eq.'N')) sgnhem=-1.

      do 620 j = 1,njlt
        ys = j - jcen
      do 620 i = 1,nilt
        xs = i - icen
        rs = sqrt(xs*xs + ys*ys)
        costheta = ys/rs*sgnhem
        sintheta = xs/rs*sgnhem

        fs(i,j) = sqrt(fx(i,j)*fx(i,j) + fy(i,j)*fy(i,j))
        if (rs.lt.0.0001) then
          fx(i,j) = 0.
          fy(i,j) = 0.
        else
          fxx = fx(i,j)
          fyy = fy(i,j)
          fx(i,j) = fxx*costheta - fyy*sintheta
          fy(i,j) = fxx*sintheta + fyy*costheta
        endif

        if ((gx(i,j).eq.spval).or.(gy(i,j).eq.spval)) then
          gx(i,j) = spval
          gy(i,j) = spval
          gs(i,j) = spval
        else
          gs(i,j) = sqrt(gx(i,j)*gx(i,j) + gy(i,j)*gy(i,j))
          if (rs.lt.0.0001) then
            gx(i,j) = 0.
            gy(i,j) = 0.
          else
            gxx = gx(i,j)
            gyy = gy(i,j)
            gx(i,j) = gxx*costheta - gyy*sintheta
            gy(i,j) = gxx*sintheta + gyy*costheta
c           if (gx(i,j).gt.100.) write (6,*) i,j,xs,ys,costheta,
c    *       sintheta,gxx,gyy,gx(i,j),gy(i,j)
          endif
        endif

        if (u(i,j).eq.spval) then
          u(i,j) = spval
          v(i,j) = spval
          w(i,j) = spval
        else
          w(i,j) = sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
          if (rs.lt.0.0001) then
            u(i,j) = 0.
            v(i,j) = 0.
          else
            uxx = u(i,j)
            uyy = v(i,j)
            u(i,j) = uxx*costheta - uyy*sintheta
            v(i,j) = uxx*sintheta + uyy*costheta
          endif
        endif
 620  continue

      call statwrite(17,fx,lons,lats,g)
      call statwrite(18,fy,lons,lats,g)
      call statwrite(19,fs,lons,lats,g)
      call statwrite(20,u ,lons,lats,g)
      call statwrite(21,v ,lons,lats,g)
      call statwrite(22,w ,lons,lats,g)
      call statwrite(26,gx,lons,lats,g)
      call statwrite(27,gy,lons,lats,g)
      call statwrite(28,gs,lons,lats,g)

      if (.not.(pmdet.or.zmdet)) go to 645

c           (c)  Mean quantities and differences
c                -------------------------------

      open (unit=34,file=pmfile,status='old',form='unformatted')
      open (unit=35,file=zmfile,status='old',form='unformatted')
      call meanij(34,35,1,nilt,1,njlt,xcen,ycen,
     * rproj,projn,pm,cm,gx,gy,idiag)
      close (34)
      close (35)

      call statwrite(29,pm,lons,lats,g)
      call statwrite(30,cm,lons,lats,g)
      call statwrite(31,gx,lons,lats,g)
      call statwrite(32,gy,lons,lats,g)

      do 640 j = 1,njlt
      ys = j - jcen
      do 640 i = 1,nilt
      xs = i - icen
      rs = sqrt(xs*xs + ys*ys)
      costheta = ys/rs*sgnhem
      sintheta = xs/rs*sgnhem
      
      if (pc(i,j).eq.spval) then
        pm(i,j) = spval
      else
        pm(i,j) = pm(i,j) - pc(i,j)
      endif
      
      if ((gx(i,j).eq.spval).or.(gy(i,j).eq.spval)) then
        gx(i,j) = spval
        gy(i,j) = spval
        gs(i,j) = spval
      else
        gs(i,j) = sqrt(gx(i,j)*gx(i,j) + gy(i,j)*gy(i,j))
        if (rs.lt.0.0001) then
          gx(i,j) = 0.
          gy(i,j) = 0.
        else
          gxx = gx(i,j)
          gyy = gy(i,j)
          gx(i,j) = gxx*costheta - gyy*sintheta
          gy(i,j) = gxx*sintheta + gyy*costheta
        endif
      endif
  640 continue
      
      call statwrite(33,pm,lons,lats,g)
      call statwrite(34,gx,lons,lats,g)
      call statwrite(35,gy,lons,lats,g)
      call statwrite(36,gs,lons,lats,g)
      
  645 continue
      close (unit=29,status='keep')
      write (6,*) ' Stat data written to file: ', statfile
     * (1:lnblnk(statfile))

  650 continue

c-----------------------------------------------------------------------
c       (6) Zonal averages.
c-----------------------------------------------------------------------

      if (.not.zadet) stop 
      if (zdet) open (unit=32,file=zonefile,status='unknown')
      if (adet) open (unit=33,file=advfile,status='unknown')

      call tstatzone(irun,dystrts,dystops,hrstrts,hrstops,hemis,level,
     * feat,dts,dlat,lslt,rproj,projn,diss2,djss1,dmodet,zdet,adet,
     * exfmt)

      if (zdet) write (6,*) ' Zone data written to file: ',zonefile
     * (1:lnblnk(zonefile))
      if (adet) write (6,*) ' Adv  data written to file: ',advfile
     * (1:lnblnk(advfile))

      stop
      end
