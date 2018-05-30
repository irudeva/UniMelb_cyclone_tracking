      Program track

c       This programme tracks cyclones from day to day from positions
c     given in a cyclone position file.  

c     Written by Ross Murray.
c     Last revised 27th Dec., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "cyc2.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      character*50 cfile,ifile,jfile,wfile,zfile
      character*50 optarg
      integer optind,jdata,rename
crmw      integer optind,fseek,jdata,rename
      logical strtdet,zdata,remove,diag,oldio
      character addhm*6
      integer dmin,ddhm,dmint
      logical nwhist
      real latmxt,latmnt,lonmxt,lonmnt
      logical skip
      integer dastrtt,hrstrtt,dastopt,hrstopt

c     Namelists
c     ---------

      namelist /nmlcycdat/quant,level,lunit,source,unit,cunit,
     * area,dmode,hilo,feat,iopmxc,latmnc,latmxc,lonmnc,lonmxc,
     * sdrmx,drmx1,drmx2,itmx1,itmx2,nshell,cmnc1,cmnc2,
     * dpmn,cvarad,rdincr,nrddir,sphtrg,rdustr,npgdir,alatgv,
     * rhoa,upfact,fmxc,frmxc,frcmxc

      namelist /nmltrack/ ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,
     * da0,hr0,iopmxt,latmnt,latmxt,lonmnt,lonmxt,cmnt1,cmnt2,cmnt3,
     * refdt,wsteer,fsteer,asteer,wmotn,wpten,dequiv,rcprob,rpbell,
     * qmxopn,qmxwek,qmxnew,nsort,irevmx,merget,qmerge,itabt3,itabt4,
     * rsttks,pstrak,hemis,xcen,ycen,rproj,requiv

c-----------------------------------------------------------------------
c     File assignments
c-----------------------------------------------------------------------

      afile = ' '
      zfile = ' '
      cfile = 'cycdat'
      ifile = 'intrack'
      jdata = 0
      wdata = .false.
      zdata = .false.
      iadvtp= 0
      trfm   = .false.
      trfmin = .false.
      trnml  = .false.
      trnmlin= .false.
      nwhist = .false.
      oldio  = .false.
      idiagt = 0
      dastrtt = -999
   10 continue
      nopt = ngtopt("a:b:c:d:i:hj:w:z:FNOol:",optind,optarg)
      if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'i') ifile = optarg
        if (char(nopt).eq.'c') cfile = optarg
        if (char(nopt).eq.'d') read (optarg,*) idiagt
        if (char(nopt).eq.'a') then
          afile = optarg
          if (lnblnk(afile).ne.0) iadvtp = 1
        endif
        if (char(nopt).eq.'b') then
          afile = optarg
          if (lnblnk(afile).ne.0) iadvtp = 2
        endif
        if (char(nopt).eq.'z') then
          zdata = .true.
          zfile = optarg
        endif
        if (char(nopt).eq.'w') then
          wfile   = optarg
          wdata  = .true.
        endif
        if (char(nopt).eq.'h') jdata = 2
        if (char(nopt).eq.'j') then
          jdata = 1
          jfile = optarg
        endif
        if (char(nopt).eq.'F') then
          trfmin = .true.
          trfm   = .true.
        endif
        if (char(nopt).eq.'N') then
          trnmlin = .true.
          trnml   = .true.
        endif
        if (char(nopt).eq.'O') nwhist = .true.
        if (char(nopt).eq.'o') oldio  = .true.
        if (char(nopt).eq.'l') read (optarg,*) dastrtt,dastopt,
     *   hrstrtt,hrstopt
      go to 10
   15 write (0,'(a/)') ' Usage: trackx [-i:c:a:b:z:w:hH:j:J:FNOol:]'
      write (0,*) ' '
      write (0,*) '   -i ifile  instruction file (default intrack)'
      write (0,*) '   -c cfile  cyclone data file (default cycdat)'
      write (0,*) '   -a afile  zonal average prediction vel. file '
      write (0,*) '   -b afile  grid point prediction vel. file '
      write (0,*) '   -z zfile  z500 file for geostrophic velocities'
      write (0,*) '   -w wfile  parameter weighting stats. output file'
      write (0,*) '   -h        continue tracks from thist?.1 files '
      write (0,*) '   -j jfile  continue from concatenated history file'
      write (0,*) '   -d idiagt diagnostic output level'
      write (0,*) '   -F        formatted files to be used'
      write (0,*) '   -N        unformatted files with namelist'
      write (0,*) '   -O        force overwriting of thist files'
      write (0,*) '   -o        read old format cyclone files'
      write (0,*) '   -l ''dastrt dastop hrstrt hrstop'' '
      write (0,'(/a)') ' Output is to "thist[123].1"'
      stop
   20 continue
      if (iargc().eq.0) go to 15
c     if (iargc().lt.optind) go to 15

      diag = idiagt.ge.1
c-----------------------------------------------------------------------
c     Open input files
c-----------------------------------------------------------------------

      write (6,'(/a/)') ' PROGRAMME TRACK'

      write (6,*) ' Instruction file                   : ',
     * ifile(1:lnblnk(ifile))
      open (unit=11,file=ifile,status='old',err=910)

      write (6,*) ' Cyclone data file                  : ',
     * cfile(1:lnblnk(cfile))
      open (unit=12,file=cfile,status='old',err=910)

      if (iadvtp.eq.0) then
        write (6,*) ' No cyclone prediction velocities used.'
      else if (iadvtp.eq.1) then
        write (6,*) ' Zonal ave. prediction vel. file    : ',
     *   afile(1:lnblnk(afile))
        open (13,file=afile,status='old',err=910)
      else if (iadvtp.eq.2) then
        write (6,*) ' Grid pt. prediction vel. file      : ',
     *   afile(1:lnblnk(afile))
        open (13,file=afile,status='old',form='unformatted',err=910)
      endif

      if (jdata.eq.1) then
        write (6,*) ' Joined track file                  : ',
     *   jfile(1:lnblnk(jfile))
        open (unit=14,file=jfile,status='old',form='unformatted',
     *   err=910)
      endif

      if (zdata) then
        write (6,*) ' 500 mb geopotential height file    : ',
     *   zfile(1:lnblnk(zfile))
        open (15,file=zfile,status='old',form='unformatted',
     *   err=910)
      endif

c-----------------------------------------------------------------------
c     Track instruction parameters
c-----------------------------------------------------------------------

c     Defaults
c     --------

      ddhmmn  = 010000 ! Days/hrs./mins. anal. interval (ddhhmm)
      ddhmmx  = 020000 ! Maximum interval between anals (ddhhmm)
      da0     = 000000 ! Epoch date (decimal dates)                  
      hr0     = 0000   ! Epoch time
      dastrt  = 000000 ! Start date(yymmdd)/date(ddd)
      hrstrt  = 0000   ! Start time(hhmm)
      dastop  = 000000 ! Finishing date                    
      hrstop  = 000000 ! Finishing time                    
      iopmxt  = 99     ! Open depressions allowed (=1), or not  
      latmxt  = 90.    ! Max. latitude 
      latmnt  = -90.   ! Min. latitude 
c     lonmxt  = 0.     ! Max. longitude 
c     lonmnt  = 360.   ! Min. longitude 
      cmnt1   = 0.     ! Max. Laplacian for closed depressions  
                       ! (0=use cmnc1)
      cmnt2   = 0.     ! Max. Laplacian for open depressions    
                       ! (0=use cmnc2)
      refdt   = 1.     ! Reference time interval for wmotn,wpten,
                       !   dequiv,rcprob,rpbell,qmxopn,qmxwek,qmxnew
      wsteer  = 0.     ! Weighting factor for steering velocity
      fsteer  = 1.     ! Scaling factor for steering velocity
      asteer  = 0.5    ! Weighting (0. or 0.5) for steering velocity 
                       !   at time tc (weighting at tb is 1-asteer)
      wmotn   = 0.36   ! Weighting factor for movement relative
                       ! to the total of movement and latitude
                       ! applicable to an interval of refdt 
      wpten   = 0.3    ! Weighting factor for p tendency (1 day)
      dequiv  = 0.35   ! Deg. lat. equiv to 1 mb. pressure diff.
      requiv  = -1.    ! (formerly reciprocal of dequiv - not used)
      rcprob  = 12.5   ! Pass radius (deg.lat.) for prob. fun.  
      rpbell  = 1.     ! c2/R (where P = R2/(R2 + r2) in prob.f.
      qmxopn  = 0.80   ! Max. value of prob.fn.(q) open depress.
      qmxwek  = 0.60   ! Max. value of prob.fn.(q) weak depress.
      qmxnew  = 0.75   ! Max. value of prob.fn. for new cyclones
      nsort   = 6      ! Max.no. of simultaneous associations   
      irevmx  = 9      ! Max. no. of artificial sorting revisns.
      merget  = 0      ! Merging of tracks (not yet available (unused)
      qmerge  = 0.     ! Min. prob. fn. for merging/dividing (unused)
      itabt3  = 100    ! Tabuln. of cyclone variables in history file 
                       ! 0=use (itabt3=itabc))
      itabt4  = 100    ! Tabuln. of additional variables in history file
      rsttks  = .true. ! Start new tracks after data gap(1)     
      pstrak  = .true. ! Calculate tracks on PS projection
      xcen    = 31     ! Position on array of pole of projection.  (This
      ycen    = 31     ! will normally be at the centre of the array, but
                       ! may be at any grid point or intermediate location.)
      rproj   = 30.    ! No. of grid point spaces between pole and equator
      hemis   = ' '    ! The hemisphere in which the projection is done will 
                       ! depend on the latitude range of the cyclone data
                       ! unless specified 'N' or 'S'.
  
c     Read instruction file
c     ---------------------

      read (11,nmltrack,err=930,end=920) 
      close (unit=11,status='keep')
      if (diag) write (6,*) ' Instruction file read'

      if (dastrtt.ne.-999) then
        dastrt = dastrtt
        dastop = dastopt
        hrstrt = hrstrtt
        hrstop = hrstopt
      endif

c     Consistency of track instruction parameters
c     -------------------------------------------

c     if ((dastop.gt.0).or.(hrstop.gt.0)) then
c       call timedif(dastrt,hrstrt,dastop,hrstop,dmode,
c    *   idmin,iddhm,dt,itderr)
c       if (itderr.gt.3) stop 'track'
c       if (iddhm.lt.0) stop 'track'
c    *   ' Starting time later than finishing time.'
c     endif

      if (((dastop.gt.0).or.(hrstop.gt.0)).and.
     * ((dac.gt.dastop).or.((dac.eq.dastop).and.
     * (hrc.gt.hrstop)))) then
        write (6,*) ' First day/time in file (',dac,hrc,
     *   ') was after finishing time (',dastop,hrstop,').'
        stop 'track'
      endif

      write (addhm,'(i6)') ddhmmn
      read (addhm,'(3i2)') iday,ihour,iminute
      ihm = 60*ihour + iminute
      dmint = 1440*iday + ihm
      dtt = float(iday) + float(ihm)/1440.
      if ((ddhmmx.gt.0).and.(ddhmmx.lt.ddhmmn)) then
        write (6,'(/'' ddhmmx < ddhmmn.  Set to ddhmmn ('',i6,'').'')')
     *   ddhmmn
        ddhmmx = ddhmmn
      endif
      ddhmt = ddhmmn

      if (requiv.ne.-1.) stop ' track: requiv out of use; use dequiv.'

      if (diag) write (6,*) ' Tracking consistency checks done'

c-----------------------------------------------------------------------
c     Cyclone file instruction parameters
c-----------------------------------------------------------------------

c     Reading of first entry in cyclone file
c     --------------------------------------

      call cyc1rd(12,nklt,ierr)
      if (ierr.ge.1) stop 
     * ' track: Error/end in first cyclone data record.'
      rewind (12)

      call cychdrd(19,ierr)
      if (ierr.ge.1) stop ' track: Error/end in namelist read.'
      if (diag) write (6,*) ' First entry in cyclone file read'

c     Consistency of trackinst and cycloc parameters
c     ----------------------------------------------

      itabt1 = 1
      itabt2 = 1
      itabt3 = min0(itabc3,itabt3)
      itabt4 = min0(itabc4,itabt4)
      itabt5 = 0

      if (iopmxt.gt.iopmxc) iopmxt = iopmxc
      if (latmxt.gt.latmxc) latmxt = latmxc
      if (latmnt.lt.latmnc) latmnt = latmnc
c     if (lonmxt.gt.lonmxc) lonmxt = lonmxc
c     if (lonmnt.lt.lonmnc) lonmnt = lonmnc
      if (cmnt1.lt.cmnc1) cmnt1 = cmnc1
      if (cmnt2.lt.cmnc2) cmnt2 = cmnc2

      remove = iopmxt.lt.iopmxc .or.
     *         latmxt.lt.latmxc .or. latmnt.gt.latmnc .or.
c    *         lonmxt.lt.lonmxc .or. lonmnt.gt.lonmnc .or.
     *         cmnt1.gt.cmnc1 .or.
     *         ((iopmxt.ge.1).and.(cmnt2.gt.cmnc2))

      if (dmode(6:6).eq.'a') 
     * stop ' track: Average files not suitable for tracking.'
c     if (da.ge.1000) then
c       write (6,'(/'' Day no. > 999.  Possibly an average file.''/
c    *   '' Execution terminated.''/)')
c       stop 'track'
c     endif

      if (((dastrt.gt.0).or.(hrstrt.gt.0)).and.
     * ((da.gt.dastrt).or.((dac.eq.dastrt).and.
     * (hr.gt.hrstrt))))
     * write (6,*) ' First day/time in file was ',dac,hrc,'.'

c     if ((itabt3.gt.itabc).or.(itabt3.eq.0)) itabt3 = itabc

      if (diag) write (6,*) ' Cyclone file consistent with',
     * ' track instruction file'

c     Hemisphere for projection (if tracked in PS coordinates)
c     -------------------------

      fhem = 0.
      if (pstrak) then
        if (latmnc.lt.-20) then
          if (latmxc.gt.20.) stop
     *     ' track: Tracking programme cannot handle global cyclones.'
          hemis = 'S'
        else if (latmxc.gt.20) then
          hemis = 'N'
        else
          if (hemis.eq.' ') then
          alatav = (latmnc + latmxc)*0.5
            if (alatsum.gt.0.) then 
              hemis = 'N'
            else
              hemis = 'S'
            endif
          endif
        endif
        if (hemis.eq.'N') fhem = 1.
        if (hemis.eq.'S') fhem = -1.
        if (diag) write (6,*) ' Hemisphere for projection: ',hemis
     *,  fhem
      endif

c-----------------------------------------------------------------------
c     Read prediction velocity data
c-----------------------------------------------------------------------

      if (iadvtp.ge.1) call advgrd(13)
      close (13)
      if (diag) write (6,*) ' Prediction velocities read'

c-----------------------------------------------------------------------
c     Initialise track window
c-----------------------------------------------------------------------

      mdt = 0.
      ib  = 0
      dab = 0
      hrb = 0
      tb  = 0
      do 100 k = 1,nklt
        kab(k)   = 0
        iopa(k)  = 0
        xa(k)    = 0.
        ya(k)    = 0.
        pa(k)    = 0.
C?      ca(k)    = 0.
        iopb(k)  = 0
        xb(k)    = 0.
        yb(k)    = 0.
        pb(k)    = 0.
C?      cb(k)    = 0.
        upb(k)   = 0.
        vpb(k)   = 0.
  100 continue
      if (diag) write (6,*) ' Arrays initialised'

c-----------------------------------------------------------------------
c     Read existing history file(s) if required
c-----------------------------------------------------------------------

      ihza = 0

      if (jdata.eq.1) call splithist(14)
      if (jdata.ge.1) call lasthist
      if (jdata.ge.1.and.diag) write (6,*)' Existing history files read'

      if (ihza.eq.1) then
        ihzb = 2
      else
        ihzb = 1
      endif
      if (diag) write (6,*) ' Permuting counters set'

c-----------------------------------------------------------------------
c     Set up headers for tabulation
c-----------------------------------------------------------------------

      call trtabhd(itabt3,itabt4,nvarlt,idiagt)
      if (diag) write (6,*) ' Tabulation heads set'

c-----------------------------------------------------------------------
c     Open ancilliary output files
c-----------------------------------------------------------------------

crmw      if (wdata) then
crmw        write (6,*) ' Predict. stats. file               : ',
crmw     *   wfile(1:lnblnk(wfile))
crmw        if (jdata.gt.0) then 
crmw          open (16,file=wfile,status='old',form='unformatted',
crmw     *     err = 910)
crmw          ifs = fseek(16,0,2)
crmw          if (ifs.eq.-1) stop ' track: fseek failed, unit 16.'
crmw        else
crmw          open (16,file=wfile,status='unknown',form='unformatted')
crmw        endif
crmw      endif

c-----------------------------------------------------------------------
c     Summary of instruction parameters (as adjusted)
c-----------------------------------------------------------------------

      write (6,'(/a/)') ' Cycloc data parameters:'
      write (6,nmlcycdat)
      write (6,'(/a/)') ' Track parameters :'
      write (6,nmltrack)
      write (6,'()')

c-----------------------------------------------------------------------
c     Tracking from sequential cyclone files
c-----------------------------------------------------------------------

      strtdet = .true.
      newtks = ib.eq.0
      do 630 itime = 1,99999

c       Read headers and parameters for cyclone data period
c       ---------------------------------------------------

        call cyc1rd(12,nklt,ierr)
        dac = da
        hrc = hr
        nkc = nk
        if (ierr.ge.1) go to 670
        if (idiagt.eq.1) write (6,'(a)') chead
        if (idiagt.gt.1) write (6,'(/80(''-'')//a)') chead

c       Checking dates 
c       --------------

c       if (dac.ge.1000) then
c         write (6,'(/'' Day no. > 999.  Possibly an average file.''/
c    *     '' Execution terminated.''/)')
c         go to 670
c       endif

        skip = .false.
        if ((dac.lt.dastrt).or.((dac.eq.dastrt).and.
     *   (hrc.lt.hrstrt))) then
          skip = .true.
        else if (ib.gt.0) then
          call timedif(dab,hrb,dac,hrc,dmode,idmin,ddhm,dt,itderr)
          if (ddhm.lt.0) then
            write (6,'(/'' Data periods out of order '',
     *       ''('',i6,x,i4'' followed by '',i6,x,i4'')''/
     *       '' Execution terminated.''/)') dab,hrb,
     *       dac,hrc
            go to 670
          else if (ddhm.eq.0) then
            write (6,'(/'' Two copies of data for same day/time.'',
     *       i6,x,i4/
     *       '' Skipping to next data period.'')') dac,hrc
            skip = .true.
          else if (ddhm.lt.ddhmmn) then
            write (6,'(/'' Data interval '',i6,'' < '',i6,
     *       '' Skipping to next data period.'')') ddhm,ddhmmn
            skip = .true.
          else if (ddhm.gt.ddhmmn) then
            if ((ddhmmx.ne.0).and.(ddhm.gt.ddhmmx)) then
              write (6,'(/'' Spacing of data periods too great'',
     *         ''('',i6,x,i4'' to '',i6,x,i4'').''/)') dab,hrb,
     *         dac,hrc
              if (rsttks) then
                write(6,*)' Finishing existing tracks at ',dab,' ',hrb
                write(6,*)' Starting new tracks at       ',dac,' ',hrc
                newtks = .true.
              else
                write (6,*) ' Execution terminated.'
                go to 670
              endif
            else if (ddhmmn.gt.0) then
              write (6,'(/'' Data period missing.''/
     *         '' Reading next data array in file ('',i6,x,i4,
     *         '').'')') dac,hrc
            endif
          endif
        endif

c       Read point data for cyclone data period
c       ---------------------------------------

        nsuplt= 0 ! KK 13/12/2003 Doesn't appear to be set anywhere;
                  ! causes segmentation fault under g77 / Cygwin
        if(diag)write(*,*)'track:  nsuplt=',nsuplt
        if (oldio) then
          call cyc2rd(12,skip,nklt,iopc,xc,yc,pc,cc,cxc,rdc,
     *     dpc,upc,vpc,nsuplt,sc,ierr)
        else
          call cyc2rd(12,skip,nklt,iopc,xc,yc,pc,cc,dpc,rdc,
     *     zsc,upc,vpc,nsuplt,sc,ierr)
        endif

        if (ierr.ge.1) go to 670
        if (skip)  go to 570

c       Compute time differences
c       ------------------------

        if (ib.eq.0) then
          call timedif(da0,hr0,dac,hrc,dmode,dmin,ddhm,tc,itderr)
          da1 = dac
          hr1 = hrc
          t1  = tc
        else 
          call timedif(da1,hr1,dac,hrc,dmode,dmin,ddhm,dt,itderr)
          tc  = t1 + dt
          ndt = tc - tb
          if (ddhmmn.eq.0) then
            ddhmmn = ddhm
            dmint = dmin
          else
            modd  = mod(dmin,dmint)
            if (modd.ne.0) write (6,'(/'' Warning: present'',
     *       '' period ('',
     *       i6,x,i4,'') is not a multiple''/'' of the tracking'',
     *       '' period ('',
     *       i6,'') since the first period ('',i6,x,i4,'').''/
     *       '' (Time difference is '',f7.3,'' days.)''/)')
     *       dac,hrc,ddhmmn,da1,hr1,dt
          endif
        endif

        ic  = ib + 1

c       Initialise arrays if tracks restarted
c       -------------------------------------

        if (newtks) then
          mdt = 0.
          ndt = 0.
          dab = 0
          hrb = 0
          tb  = 0
          do 400 k = 1,nklt
            kab(k)   = 0
            iopa(k)  = 0
            xa(k)    = 0.
            ya(k)    = 0.
            pa(k)    = 0.
C?          ca(k)    = 0.
            iopb(k)  = 0
            kbc(k)   = 0
            kcb(k)   = 0
            xb(k)    = 0.
            yb(k)    = 0.
            pb(k)    = 0.
C?          cb(k)    = 0.
            upb(k)   = 0.
            vpb(k)   = 0.
  400     continue
          if (diag) write (6,*) ' Arrays initialised'
        endif

c       Removal of unwanted systems
c       ---------------------------

        if (remove) then
          k2 = 0
          do 420 k1 = 1,nkc
            iopk = mod(iopc(k1),10)
            if (iopk.gt.iopmxt) go to 410
            if (pstrak) then
              call pstoll(xcen,ycen,rproj,fhem,xc(k1),yc(k1),
     *         alonc,alatc)
              if (alatc.gt.latmxt .or. alatc.lt.latmnt) go to 410
c             if (alonc.gt.lonmxt .or. alonc.lt.lonmnt) go to 410
            else
              if (yc(k1).gt.latmxt .or. yc(k1).lt.latmnt) go to 410
c             if (xc(k1).gt.lonmxt .or. xc(k1).lt.lonmnt) go to 410
            endif
            if ((iopk.eq.0).and.(cc(k1).lt.cmnt1)) go to 410
            if ((iopk.ge.1).and.(cc(k1).lt.cmnt2)) go to 410
              k2 = k2 + 1
              xc(k2) = xc(k1)
              yc(k2) = yc(k1)
              pc(k2) = pc(k1)
              cc(k2) = cc(k1)
              iopc(k2) = iopc(k1)
c             cxc(k2) = cxc(k1)
              rdc(k2) = rdc(k1)
              dpc(k2) = dpc(k1)
              upc(k2) = upc(k1)
              vpc(k2) = vpc(k1)
 410        continue
 420      continue
          nkc = k2
        endif

c       Convert to polar stereographic coordinates
c       ------------------------------------------

        if (pstrak) then
          do 450 k = 1,nkc
            call lltops(xcen,ycen,rproj,fhem,xc(k),yc(k),xx,yy)
            xc(k) = xx
            yc(k) = yy
  450     continue
        endif

c       Tracking to period = tc
c       -----------------------

        if (strtdet) strtdet = .false.
        if (.not.newtks) then
          if (idiagt.eq.1) write (6,'(/'' Tracking to '',i6,'':'',
     *     i4,''.'')') dac,hrc

          call predict(16)
          if (idiagt.eq.1) write (6,*) ' Predictions completed.'
          if (ierr.eq.1) go to 670

          call prob
          if (idiagt.eq.1) write (6,*) ' Probabilities completed.'
          if (ierr.eq.1) go to 670

          call match
          if (idiagt.eq.1) write (6,*) ' Matchings completed.'
          if (ierr.eq.1) go to 670

          if (idiagt.ge.2) call tracksum
          if ((wdata).and.(ic.gt.2)) then
            do 530 kb = 1,nkb
              kc = kcb(kb)
              if (kc.eq.0) then
                write (16) 0,0.,0.,0.
              else
                write (16) 1,xc(kc),yc(kc),pc(kc)
              endif
 530        continue
          endif
        endif

c       Update history files
c       --------------------

        call update(nwhist)
        if (ierr.eq.1) go to 670

        if (idiagt.eq.0) write (6,'(i3,'': '',i6,x,i4,'', TRK '',
     *   i5)') ic,dac,hrc,lstrk2

c       Reallocate variables and commute history file indices
c       -----------------------------------------------------

        mdt = ndt
        newtks = .false.
        ib  = ic
        dab = dac
        hrb = hrc
        tb  = tc
        nkb = nkc
        do 550 kc = 1,nkc
          kb       = kbc(kc)
          if (kb.ne.0) then
            iopa(kc) = iopb(kb)
            xa(kc)   = xb(kb)
            ya(kc)   = yb(kb)
            pa(kc)   = pb(kb)
C?          ca(kc)   = cb(kb)
            kab(kc)  = kbc(kc)
          else
            iopa(kc) = 0
            xa(kc)   = 0.
            ya(kc)   = 0.
            pa(kc)   = 0.
C?          ca(kc)   = 0.
            kab(kc)  = 0
          endif
 550    continue
        do 560 kc= 1,nkc
          iopb(kc) = iopc(kc)
          xb(kc)   = xc(kc)
          yb(kc)   = yc(kc)
          pb(kc)   = pc(kc)
C?        cb(kc)   = cc(kc)
          upb(kc)  = upc(kc)
          vpb(kc)  = vpc(kc)
 560    continue

        ihza = ihzb
        if (ihza.eq.1) then
          ihzb = 2
        else
          ihzb = 1
        endif

 570    continue

c       Date/time of last period
c       ------------------------

        if ((dastop.gt.0).or.(hrstop.gt.0)) then
          call timedif(dac,hrc,dastop,hrstop,dmode,idmin,ddhm,
     *     dt,itderr)
          if (ddhm.lt.ddhmmn) then
            if (diag) write (6,*) 'Date: ',dac,hrc,', stop:',
     *       dastop,hrstop,', diff:',ddhm
            go to 670
          endif
        endif

 630  continue
 670  continue

      close (unit=12,status='keep')
      if (wdata) close (unit=16,status='keep')

c-----------------------------------------------------------------------
c     Exit if no tracking accomplished.
c-----------------------------------------------------------------------

      if (strtdet) then
        write (6,'(/'' End of data file: '',a/)') cfile(1:lnblnk(cfile))
        if (it.gt.1) write (6,'('' Last data: '',i6,x,i4,'').''/)') 
     *   dac,hrc
        stop 'track'
      endif

c-----------------------------------------------------------------------
c     Renaming final history files to thist?.1
c-----------------------------------------------------------------------

      if (ib.gt.1) then
        if (ihza.eq.1) then
          open (unit=22,file='thist1.2',status='old',form='unformatted')
          open (unit=26,file='thist3.2',status='old',form='unformatted')
          close (unit=22,status='delete')
          write (6,*) ' thist1.2 deleted'
          close (unit=26,status='delete')
          write (6,*) ' thist3.2 deleted'
        else
          irn1 = rename('thist1.2','thist1.1')
          if (irn1.ne.0) then
            write (6,*) ' thist1.2 could not be renamed.'
            stop 'track'
          endif
          if (diag) write (6,*) ' thist1.2 renamed thist1.1'
          irn3 = rename('thist3.2','thist3.1')
          if (irn3.ne.0) then
            write (6,*) ' thist3.2 could not be renamed.'
            stop 'track'
          endif
          if (diag) write (6,*) ' thist3.2 renamed thist3.1'
          close (unit=22,status='keep')
          close (unit=26,status='keep')
        endif
      endif

      stop

c-------------------------------------------------------------------------------
c     Error conditions
c-------------------------------------------------------------------------------

  910 stop ' track: Error in opening file'
  920 stop ' track: End of input file.'
  930 stop ' track: Error in input file.'
  960 stop ' track: Cannot find cyclone data file'
  970 stop ' track: Cannot find velocity prediction file'
  980 stop ' track: Cannot find last track history file'
  990 stop ' track: Cannot find weight file'

      end
