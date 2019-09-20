      Program cycloc

c       This programme calls routines which search for maxima or minima of 
c     a function fitted to a NH or SH polar stereographic array by a bicubic 
c     spline coefficients (closed highs or lows) and also, if required,
c     points of inflexion along the axes of ridges or troughs (open highs
c     or lows).

c       The search is carried out on a polar stereographic (PS) array.  The
c     data may be supplied in this format or on a lat.-lon. grid.  In the 
c     latter case, a conversion to a polar stereographic array is carried
c     out first using bicubic spline interpolation.  This may be saved for 
c     later direct use with the cyclone finding part of the programme.

c       The programme has been combined from two separate programmes,
c     "trangp.f", which created a PS array, and "cyclocp.f", which found
c     the cyclones.  It is now no longer necessary to store the PS data
c     as disk files; however, this may still be done and may be useful for
c     testing a number of parameter settings with the one set of data.
c     PS data is only written and read in unformatted form at present: it
c     may be formatted using programme "psfm".

c       Another change from the previous programmes is that cyclones can
c     now be sought in both hemispheres simultaneously.  The searches are
c     carried out on separate overlapping PS arrays, and a discrimination
c     of the duplicated positions in the equatorial zone is performed at
c     the end of the second call to "cycp".  The hemisphere(s) used will
c     be decided from the values of "latmnc" and "latmxc" (and to a degree
c     by that of "hemis").  If used in global mode, the PS data for the
c     NH array should be read first and the SH data second for each time.

c       The input data may be taken as sequential reads from a single file
c     as a single read from multiple files, or from a combination of both.
c     The output is directed to a single file.  The syntax is as follows:

c     For finding cyclones
c     from lat.-lon. data:   cyclocx -i infile -c cycfile latlonfiles
c     For obtaining a PS 
c     data file only:        cyclocx -i infile -p fpfile latlonfiles
c     For obtaining PS 
c     and cyclone files:     cyclocx -i infile -p fpfile -c cycfile latlonfiles
c     For finding cyclones 
c     from a PS data file:   cyclocx -P -i infile -c cycfile fpfiles

c       Diagnostics may be obtained by specifying idiagx and idiagy by the
c     option '-d idiagx [idiagy]'

c     idiagx = 1:  Reports main stages of programme flow
c     idiagy = 5:  Reports diagnostics of brsplsv depending on value of idiagy
c     idiagx = 6:  Reports diagnostics of cycp depending on value of idiagy
c       idiagy = 1,2,3  progressively more detailed diagnostics on cycp
c       idiagy = 4      diagnostics of "fmin"
c       idiagy = 5      diagnostics of "frmin"
c       idiagy = 6      diagnostics of "cvave"
c       idiagy = 7      diagnostics of "steer1"
c     idiagx = 7:  Reports diagnostics of steer2 depending on value of idiagy
c     idiagx = 8:  Reports diagnostics of supvar depending on value of idiagy

c     Last modified 11th Jan., 1996., R.J. Murray.

c-------------------------------------------------------------------------------

c * Additional Modifications
c
c (1) 21/2/2007: As a temporary fix, namelist nmlcycloc parameter offlon 
c may be used to convert a longitude range including -ve values to a
c non-negative range. For instance, 20 W - 60 E (-20 to 60) with offlon=20 
c becomes 0 - 80. This offset is removed before the cyclone tabulation is output.
c (2) 29/5/2007: Added offlon correction to output of smoothed lat-lon (conmap) files.
c

c-------------------------------------------------------------------------------
c     Explanation
c-------------------------------------------------------------------------------

c     Instruction parameters: see `Default values' below

c     Data attributes
c     ---------------

c       The quantities quant,level,lunit,source,unit,dmode are read from
c     the data file but may be overridden if specified in the namelist 
c     file.  If a PS data file be read, the projection characteristics will
c     also be read.  Also obtained from the data file are

c     da,hr            day/date and time of day (hours,mins/decimal day)

c     Grid point arrays
c     -----------------

c     nlonlt,nlatlt,&c.Maximum allowable values of nlon,nlat,nlon*nlat,
c                        nlon2,nlat2,nlon2*nlat2,ni,nj,ni*nj
c     marglt           Maximum expansion of the (h,k) grid in each direction
c     mgL,mgR,mgD,mgU  Margins to be added to the left,right,lower,and
c                        upper edges of an expanded array 
c     lon(h),lat(k)    Longitudes and latitudes of the input array 
c     lon2(h),lat2(k)  Longitudes and latitudes of the expanded arrray

c       Since the dimensions of the data may vary, the data arrays are
c     made 1-dimensional in the main programme, but 2-dimensional in the
c     working subroutines.

c     fhk(hk)          Lat.-lon. data array
c     f(hk)            Expanded lat.-lon. data array
c     fij(ij,ihem)     Array interpolated to N (ihem=1) or S (ihem=2) PS 
c                        projection (equivalenced to fhk)
c     fxx,fyy,fxxyy    Bicubic spline coefficient arrays (ni*nj) (partial 
c                        derivatives of function w.r.t. x**2,y**2,(xy)**2.
c                        for (1) fex (for remapping array),
c                            (2) fij(i,j,ihem) (cyclone tracking)
c     q(i,j)           Working array of dimension (ni*nj) required by cycp

c     Cyclone data arrays
c     -------------------

c     nk               Number of centres found
c     klast            The last value of nk, when more than one projection
c                        is used
c     ih(nklt)         ) Arrays of starting positions required 
c     jh(nklt)         ) by subroutine cycp
c     iopc(nklt)       High/low type (closed=0, open=1) of centre kc
c     xc(nklt)         Longitude of centre
c     yc(nklt)         Latitude of centre
c     fc(nklt)         Function value at high/low centre
c     cc(nklt)         Laplacian of f (del**2 f) averaged about high/low
c                        centre by subroutine cvave
c     cxc(nklt)        Laplacian of f at cyclone centre, xc(k),yc(k)
c     rdc(nklt)       `Radius' of depression
c     dpc(nklt)       `Depth' of depression
c     zsc(nklt)       Topographic height at which depression occurs
c     upc,vpc(nklt)   E and N steering velocities computed from pmsl field

c-------------------------------------------------------------------------------
c     Declarations
c-------------------------------------------------------------------------------
 
      parameter (rad=57.2957,rrad=1./rad,r2rad=1./(2.*rad))

#include "cyc1.h"       
#include "cyc2.h"       
#include "cyc3.h"       
#include "cyc4.h"       
#include "cyc5.h"       
 

c#ifdef delsqzs
      common /blzxy2/zxx(nar2lt,2)
      common /blzxy3/zyy(nar2lt,2)
      common /blzxy4/zxxyy(nar2lt,2)
c#endif
      real lons(nlonlt),lats(nlatlt)

      integer dastrt,hrstrt,dastop,hrstop,ddhmmn,ddhmmx
      integer dalast,hrlast
      character quantx*8,levelx*9,lunitx*10,sourcex*10,unitx*12,
     * dmodex*6,llgrid*17,hemisx*1,sourcey*12
      character quantz*8,levelz*9,lunitz*10,sourcez*10,unitz*12,
     * dmodez*6,gridz*17

      character*80 ffile,fpfile,fsfile,flfile,llfile
      character*80 zfile,zsfile,zpfile,zlfile
      character*80 infile,ufile,vfile,llhead,cycfile
      character*40 svfile(nsupmx)
      character*10 svnamex
      parameter (lcard=160)
      character card*(lcard)
      integer optind
      logical diag,usage,fdata,zdata,fpdata,zpdata,fpwrit,zpwrit,
     * sfcalc,szcalc,fswrit,flwrit,zswrit,zlwrit,cycwrit,cyccalc,
     * upredg,upredu,ftop,llconv,llform
      logical expNS,noneg,noarea,nhem,shem
crmw
      logical psform
crmw
      character*80 ch80  !ckk

c * Link to cycp (cycp_2.F) for (xv,yv) KK 21/2/2007
c   Note: nklt is in cyc4.h
      common /bltmp/xv(nklt),yv(nklt)

c     Namelists
c     ---------

      namelist /nmltrangp/quant,level,lunit,source,unit,dmode,
     * ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,ni,nj,
     * xcen,ycen,hemis,rproj,rdiff0,rdiff,rdifz0,rdifz,
     * intopt,noneg,expNS,spval

      namelist /nmlcycloc/hilo,iopmxc,istmxc,area,cunit,latmnc,
     * latmxc,lonmnc,lonmxc,imnc,imxc,jmnc,jmxc,nshell,mscrn,sdrmx,
     * drmx1,drmx2,itmx1,itmx2,diflt1,diflt2,iconcv,cmnh,cmnc0,
     * cmnc1,cmnc2,dpmn,swvmn,fccmn,fmxc,frmxc,frcmxc,
     * icendp,cvarad,rdincr,nrddir,sphtrg,
     * rdpgrd, rdustr,npgdir,alatgv,rhoa,upfact,iavsup,rdsupv,
     * zsmax,zscr1,zscr2,ftopeq,itabc,itabc2,itabc3,itabc4,
     * ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,
     * cmnhw,cmncw,dpmnw,swvmnw,fmxcw,fccmnw,offlon

      namelist /nmldates/ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop

      namelist /nmltrangp2/quant,level,lunit,source,unit,dmode,
     * ni,nj,xcen,ycen,hemis,rproj,rdiff0,rdiff1,rdiff,
     * rdifz0,rdifz1,rdifz,intopt,
     * noneg,expNS,spval

      namelist /nmltrangp3/quant,level,lunit,source,unit,dmode,
     * ni,nj,xcen,ycen,hemis,rproj,rdiff0,rdiff1,rdiff,
     * rdifz0,rdifz1,rdifz,spval

      namelist /nmlcycloc2/hilo,iopmxc,istmxc,area,cunit,latmnc,
     * latmxc,lonmnc,lonmxc,imnc,imxc,jmnc,jmxc,nshell,mscrn,sdrmx,
     * drmx1,drmx2,itmx1,itmx2,diflt1,diflt2,iconcv,cmnh,cmnc0,
     * cmnc1,cmnc2,dpmn,swvmn,fccmn,fmxc,frmxc,frcmxc,
     * icendp,cvarad,rdincr,nrddir,sphtrg,
     * rdustr,npgdir,alatgv,rhoa,upfact,iavsup,rdsupv,
     * zsmax,zscr1,zscr2,ftopeq,itabc,itabc2,itabc3,itabc4,
     * cmnhw,cmncw,dpmnw,swvmnw,fmxcw,fccmnw

c     Logical units
c     -------------

      dimension iosv(nsupmx)
      data ioin,ioda,iops,iocy,ioau,iozs,ioup,iovp,iosp,iosl,ioll,
     * iosz/11,12,13,14,15,16,17,18,19,20,21,22/
      data iosv/31,32,33,34,35,36/

c     Timing functions
c     ----------------

      dimension etimes(2),dtimes(2)
      eetime() = etime(etimes)
      ddtime() = dtime(dtimes)

c------------------------------------------------------------------------------
c     File assignments and command line options
c------------------------------------------------------------------------------

      ddtimex = ddtime()

      infile    = ' '      ! instruction namelist file
      idiagx    = 0        ! Diagnostic output option (0=no diagnostics,
                           ! 1=stages reported, >1 also detailed diagnostics
                           ! for (5) splines, (6) cyclone finding
      idiagy    = 0        ! Level of detail of diagnostics
      zdata     = .false.  ! 
      fpwrit    = .false.  ! 
      fswrit    = .false.  ! 
      flwrit    = .false.  ! 
      zpwrit    = .false.  ! 
      zswrit    = .false.  ! 
      zlwrit    = .false.  ! 
      llform    = .false.  !
      fpdata    = .false.  ! PS data expected
      zpdata    = .false.  ! PS topography data expected
      cyccalc   = .false.  !
      cycwrit   = .false.  !
      fpfile    = ' '      ! filename for PS output
      fsfile    = ' '      ! filename for smoothed PS output
      flfile    = ' '      ! filename for smoothed lat-lon output
      llfile    = ' '      ! filename for lat-lons for sm. output
      cycfile   = ' '      ! filename for cyclone output
      usage     = .false.  !
      psform    = .false.  !
      zfile     = ' '      ! topography lat-lon file
      ufile     = ' '      ! u component or z steering flow file
      vfile     = ' '      ! v component steering flow file
      itabc5    = 0        ! no. of supplementary variables
      data svfile/nsupmx*' '/!supplementary variable files

   10 continue
      nopt = ngtopt("d:i:p:c:CfPz:YZs:S:FL:g:u:v:V:n:xX",optind,card)
        if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'d') read (card,'(2i1)') idiagx,idiagy
        if (char(nopt).eq.'i') read (card,'(a80)') infile
        if (char(nopt).eq.'P') fpdata = .true.
        if (char(nopt).eq.'p') read (card,'(a80)') fpfile
        if (char(nopt).eq.'s') read (card,'(a80)') fsfile
        if (char(nopt).eq.'S') read (card,'(a80)') flfile
        if (char(nopt).eq.'L') read (card,'(a80)') llfile
        if (char(nopt).eq.'F') llform = .true.
        if (char(nopt).eq.'c') read (card,'(a80)') cycfile
        if (char(nopt).eq.'C') cyccalc = .true.
c       if (char(nopt).eq.'f') psform = .true.
        if (char(nopt).eq.'z') zfile = card
        if (char(nopt).eq.'Z') zpdata = .true.
        if (char(nopt).eq.'Y') zpwrit = .true.
        if (char(nopt).eq.'x') zswrit = .true.
        if (char(nopt).eq.'X') zlwrit = .true.
        if (char(nopt).eq.'g') then
          ufile = card
          upredg = .true.
        endif
        if (char(nopt).eq.'u') then
          ufile = card
          upredu = .true.
        endif
        if (char(nopt).eq.'v') vfile = card
        if (char(nopt).eq.'V') 
     *   call getwds(card,lcard,nsupmx,svfile,itabc5,ie)
        if (char(nopt).eq.'n')
     *   call getwds(card,lcard,nsupmx,svname,nsupx,ie)
      go to 10
   20 continue

      fdata = iargc().ge.optind
      if (lnblnk(zfile).ne.0)   zdata  = .true.
      if (lnblnk(fpfile).ne.0)  fpwrit  = .true.
      if (lnblnk(fsfile).ne.0)  fswrit  = .true.
      if (lnblnk(flfile).ne.0)  flwrit  = .true.
      if (lnblnk(llfile).ne.0)  llconv  = .true.
      if (lnblnk(cycfile).ne.0) cycwrit = .true.
      fpdata  = fpdata .and. fdata
      fpwrit  = fpwrit .and. fdata
      cycwrit = cycwrit.and. fdata
      fswrit  = fswrit .and. fdata
      flwrit  = flwrit .and. fdata
      zpdata  = zpdata .and. zdata
      zpwrit  = zpwrit .and. zdata
      zswrit  = zswrit .and. zdata
      zlwrit  = zlwrit .and. zdata
      llconv  = llconv .and. (flwrit.or.zlwrit)
      cyccalc = (cyccalc.or.cycwrit).and. fdata

      if (fpwrit.and.fpdata) then
        write (0,*) ' May not read and write PS data file.'
        usage = .true.
      endif
      if (zpwrit.and.zpdata) then
        write (0,*) ' May not read and write PS topography data file.'
        usage = .true.
      endif
      if (.not.(fpwrit.or.cycwrit.or.fswrit.or.flwrit.or.zswrit.or.
     * zlwrit.or.zpwrit)) then
        write (0,*) ' Must write PS data and/or cyclone data.'
        usage = .true.
      endif
      if (upredg.and.upredu) stop 
     * ' Cannot use both geopotential and winds for steering.'
      if (upredu.and.lnblnk(vfile).eq.0)
     * vfile = 'v' // ufile(2:lnblnk(ufile))

   30 continue
      if (((iargc().lt.optind).and.(.not.(zpwrit.or.zswrit.or.zlwrit)))
     *    .or. usage) then
        write (0,*) ' Usage: cyclocpx [-d (1 or 2 digits) -i infile',
     *   '-p fpfile -c cycfile -P -C -z zfile [-x -X -Y -Z]',
     *   '-s fsfile -S flfile -F -L llfile -g zfile',
     *   ' -u ufile [-v vfile] -V ''svfiles'' -N ''svnames''] data'
        write (0,*) '   -d idiagx,idiagy    diagnostic output levels'
        write (0,*) '   -i infile           namelist input file'
        write (0,*) '   -p fpfile           PS output data file'
        write (0,*) '   -c cycfile          cyclone output data file'
        write (0,*) '   -P      input PS data (otherwise lat.-lon.)'            
        write (0,*) '   -C      cyclone diagnostics only'
        write (0,*) '   -z zfile           topography input file'
        write (0,*) '   -Z      input PS topog. (otherwise lat.-lon.)'
        write (0,*) '   -Y      write PS topography (zfile + .st)'
        write (0,*) '   -x      write smoothed PS topog. (zfile + .sl)'
        write (0,*) '   -X      write smoothed LL topog. (zfile + .sl)'
        write (0,*) '   -s fsfile           smoothed PS output file'
        write (0,*) '   -S flfile           smoothed LL output file'
        write (0,*) '   -L llfile           input file for new lat-lon'
        write (0,*) '   -F                  input file formatted (mapx)'
        write (0,*) '   -g gfile            geopot.ht. for steering'
        write (0,*) '   -u ufile            u steering vel. file'
        write (0,*) '   -v vfile            v steering vel. file'
        write (0,*) '            (otherwise as ufile but with v'
        write (0,*) '   -V svfile           supplementary var. files'
        write (0,*) '   -N svname           supplementary var. names'
        stop
      endif

c     Open logical units
c     ------------------

      write (6,'(a/)') ' PROGRAMME CYCLOC'
      if (fpdata) write (6,'(a)') ' Assuming PS input data.'
      if (zpdata) write (6,'(a)') ' Assuming PS topography input data.'
      if (.not.fpdata) write (6,'(a)') ' Assuming lat.-lon. input data.'
      if (cyccalc.and.(idiagx.gt.1)) write (6,'(a)') 
     * ' Calculating cyclone diagnostics, 1 data period only.'

      write (6,'('' Taking input from file      : '',a)')
     * infile(1:lnblnk(infile))
      open (ioin,file=infile,status='old',err=910)

      if (fpwrit) then
        write (6,'('' Writing PS data to file     : '',a)') 
     *   fpfile(1:lnblnk(fpfile))
        open (iops,file=fpfile,status='unknown',form='unformatted')
      endif

      if (fswrit) then
        write (6,'('' Writing smoothed PS to file : '',a)') 
     *   fsfile(1:lnblnk(fsfile))
        open (iosp,file=fsfile,status='unknown',form='unformatted')
      endif

      if (flwrit) then
        write (6,'('' Writing smoothed ll. data to: '',a)') 
     *   flfile(1:lnblnk(flfile))
        open (iosl,file=flfile,status='unknown',form='unformatted')
      endif

      if (llconv) then
        write (6,'('' Taking new lats-lons. from  : '',a)') 
     *   llfile(1:lnblnk(llfile))
        if (llform) then
          open (ioll,file=llfile,status='unknown',err=910)
        else 
          open (ioll,file=llfile,status='unknown',form='unformatted',
     *     err=910)
        endif
      endif

      if (cycwrit) then
        write (6,'('' Writing cyclone data to file: '',a)')
     *   cycfile(1:lnblnk(cycfile))
        open (iocy,file=cycfile,status='unknown',form='formatted')
      endif

      if (zdata) then
        write (6,'('' Taking topography input from: '',a)')
     *   zfile(1:lnblnk(zfile))
        do 35 ilet = lnblnk(zfile),1,-1
          if (zfile(ilet:ilet).eq.'/') go to 36
   35   continue
        ilet = 0
   36   continue

cli        open (iozs,file=zfile,status='old',form='unformatted')
        if (llform) then
          open (iozs,file=zfile,status='old',form='formatted',err=37)
        else
          open (iozs,file=zfile,status='old',form='unformatted',err=37)
        endif
        go to 38
  37    write (6,*) ' Cannot find data file: ',
     *   ffile(1:lnblnk(ffile))
        stop
  38    continue

        if (zpwrit) then
          zpfile = zfile(ilet+1:lnblnk(zfile)) // '.st'
          write (6,'('' Writing PS topography to    : '',a)')
     *     zpfile(1:lnblnk(zpfile))
        endif
        if (zswrit) then
          zsfile = zfile(ilet+1:lnblnk(zfile)) // '.sp'
          write (6,'('' Writing smoothed PS topog to: '',a)')
     *     zsfile(1:lnblnk(zsfile))
        endif
        if (zlwrit) then
          zlfile = zfile(ilet+1:lnblnk(zfile)) // '.sl'
          write (6,'('' Writing smoothed ll topog to: '',a)') 
     *     zlfile(1:lnblnk(zlfile))
        endif

      endif

      if (upredg) then
        write (6,'('' Taking steering heights from: '',a)')
     *   ufile(1:lnblnk(ufile))
        open (ioup,file=ufile,status='old',form='unformatted')
      else if (upredu) then
        write (6,'('' Taking steering winds from  : '',a/
     *   ''                               '',a)')
     *   ufile(1:lnblnk(ufile)),vfile(1:lnblnk(vfile))
        open (ioup,file=ufile,status='old',form='unformatted')
        open (iovp,file=vfile,status='old',form='unformatted')
      endif

      if (itabc5.ne.0) then
        do 40 isup = nsupx+1,nsupmx
          write (svname(isup),'(''s'',2i1)') isup/10,mod(isup,10)
   40   continue

        do 50 isup = 1,itabc5
          card = svfile(isup)
          card = card(1:lnblnk(card)) //','
          svnamex = svname(isup)
          write (svname(isup),'(a10)') svnamex(1:lnblnk(svnamex))
          write (6,'('' Suppl. variable file : '',a30,
     *     '' variable name: '',a10,''.'')')
     *     card(1:30),svname(isup)
          open (iosv(isup),file=svfile(isup),status='old',
     *     form='unformatted')
   50   continue
      endif

      diag = idiagx.ge.1

c------------------------------------------------------------------------------
c     Default values
c------------------------------------------------------------------------------

c     Dates
c     -----

      ddhmmn = 10000   ! Minimum time interval between successive data fields,
                       !   in days,hours,minutes (ddhhmm); e.g. 010000=1 day,
                       !   000600=6 hrs.
      ddhmmx = 10000   ! Maximum time interval between successive data fields
                       !   (0=inf, no check made)
      dastrt = 0       ! Starting date (0=first data field used; otherwise the 
                       !   meaning depends on dmode.  If dmode='DDDHM', it is 
                       !   the day no; if dmode='YMDHM', 910311= 11th Mar. 1991)
      hrstrt = 0       ! Starting time
      dastop = 0       ! Finishing date (0=last data field used)
      hrstop = 0       ! Finishing time

c     Quantity information
c     --------------------

      quant  = ' '     ! Quantity of data array
      level  = ' '     ! Level of data array 
      lunit  = ' '     ! Quantity of level of data array 
      source = ' '     ! Source or identifaction code of data 
      unit   = ' '     ! Units for data, and (lat-lon) grid/region used  
      dmode  = ' '     ! Mode for interpretation of dates and times, e.g., 
                       !  ymdhm  year,month,day,hour,minute (yymmddhhmm)
                       !  ydd    year,day(1-365) (yydddd)
                       !  ddddd  day(d),decimals of day(D) (ddddddDDDD)

c     PS projection parameters
c     ------------------------

      ni     = 61      ! Size of PS array in 90degE and GM directions 
      nj     = 61      ! Size of PS array in 90degE and GM directions
      xcen   = 0.      ! Grid coordinates of pole of projection (not
      ycen   = 0.      ! necessarily integral values).  (If not
                       ! otherwise defined, these will be set to the
                       ! float(int(1+ni)/2) and float(int(1+nj)/2).)
      rproj  = 30.     ! No. of grid point spaces between pole and equator
      hemis  = ' '     ! Projection: S or s = PS SH, N or n = PS NH
      grid   = ' '     ! Grid/region used for PS output
      rdiff0 = 0.      ! Diffusive radius (sqrt(K delta t)) previously
                       !   used for smoothing input data
      rdiff  = 0.      ! Diffusive radius (sqrt(K delta t)) for smoothing
                       !   PS fields = sqrt(rdiff0**2 + rdiff1**2)
                       !   rdiff1 is applied diffusion (rdiff in input)
      rdifz0 = 0.      ! Diffusive radii for topography
      rdifz  = 0.      ! 
      intopt = 3       ! interpolation 1=linear,3=bicubic spline,
                       !   2=bicubic to equator only
      noneg  = .false. ! negative interpolated values set to zero
      expNS  = .false. ! forces margins at north and south boundaries
      spval  = 99999.9 ! missing value indicator

c     Cyclone finding parameters
c     --------------------------

      hilo   = 'L'     ! Highs (H) or lows (L) required
      area   = ''      ! Geographical region covered by cyclone search
      iopmxc = 1       ! Closed features only (0) or both open & closed (1)?
      istmxc = 01      ! Strong closed (0) strong closed and open (1)
                       !   all strong and weak closed (10), etc.
      latmnc = -90.    ! S limit of latitude  
      latmxc = 90.     ! N limit of latitude  
      lonmnc = 0.      ! W limit of longitude  (lonmnc may be < 0.)
      lonmxc = 360.    ! E limit of longitude  (lonmxc may be < lonmnc.)
      imnc   = 0       ! / Limits of search in i direction
      imxc   = 0       ! \   (no checks if imxc = imnc)
      jmnc   = 0       ! / Limits of search in j direction
      jmxc   = 0       ! \ (no checks if jmxc = jmnc)
      nshell = 8       ! No. (4,8,12,20,24) of surrounding points for scanning
      mscrn  = 2       ! Screen with min (0), min grad (1), or max delsq (2)
      sdrmx  = 10.     ! Maximum total distance (sigma dr) of new position 
                       ! from starting position in deg.lat.
      drmx1  = 0.7     ! / Maximum distance of movement in one iteration, for
      drmx2  = 0.3     ! \ closed and open depressions, respectively(grid units)
      itmx1  = 5       ! / Maximum number of new grid squares to be traversed in
      itmx2  = 5       ! \ searching for a closed or open depression, resp.
      diflt1 = 0.5     ! Closest allowable spacing of systems (grid units)
      diflt2 = 6.0     ! Minimum separation of starting grid point and closed
                       ! system position for allowing a parallel search for an
                       ! open depression (grid units)
      iconcv = 2       ! 1=pressure surface must be concave at start of fmin
                       ! iteration,2= for frmin iteration also.
      cmnh   = 0.      ! Minimum value Laplacian at grid pt. max. (deg.lat.**2) 
      cmnc0  = 0.      ! Minimum central value of Laplacian (deg.lat.**2) 
      cmnc1  = 0.      ! Minimum area averaged Laplacian, closed depressions
      cmnc2  = 0.      ! Minimum area averaged Laplacian, open depressions
      swvmn  = 0.      ! Minimum third derivative at inflection point
      dpmn   = 0.      ! Minimum depth (if calculated) in mb.
      fccmn  = 0.      ! Minimum cross gradient 2nd deriv.
      icendp = 1       ! Use (1) cyclone centre or (2) vorticity centre
                       !   for cvave, radius, and depth calculations.
      cvarad = 4.0     ! Radius around centre over which average the Laplacian
                       !   of the function is found (deg.lat.)
      rdincr = 0.5     ! Radial increment in deg.lat. for radius/depth
      nrddir = 12      ! No. of radial directions for radius/depth
      sphtrg = .false. ! Calculate radii using (T) spherical trigonometry,
                       ! (F) map scale of projection at cyclone centre
      rdustr = -999.   ! Radius (deg.lat.) for steering velocity or
                       !   pressure gradient averaging circle
                       !   (If unaltered by namelist this is set to
                       !      0.     for use in steer2
                       !      rdpgrd for use in steer1.)
      rdpgrd = 5.      ! Default averaging radius for PMSL derived steering
      npgdir = 12      ! No. of radial directions for p gradient calculation
      alatgv = 8.      ! Characteristic latitude for reducing geostrophic
                       !   velocities near equator
      rhoa   = 1.2     ! Air density characteristic of level (Kg./cu.m.)
      upfact = 1.      ! Factor for scaling steering flow from msl values
      fmxc   = -999.   ! Maximum function value at centre
      frmxc  = 0.      ! Maximum absolute gradient of function at centre of open
                       ! depression (function units/deg.lat.) (if.ne.0.)
      frcmxc = 0.      ! Maximum ratio of gradient/laplacian(deg.lat.)(if.ne.0.)
      itabc  = 2       ! 1=calc min.p,2=also cv,3 = also cxc,dpc,rdc,
                       ! 4=also upc,vpc, 5=also sc. 
      itabc1 = 1       ! 1=include open/closed status
      itabc2 = 1       ! 1=lon. & lat. of centre,2=also vort. centre 
      itabc3 = -1      ! 1=calc min.p,2=also cv,3 = also cxc,dpc,rdc,\
                       !   4=also zs                                 | -1=use
      itabc4 = -1      ! 1=also upc,vpc.                             / itabc
      data iavsup/nsupmx*0/! 1=average supp. variables over radius rdsupv
      rdsupv = 5.      ! averaging radius for supplementary variables
      zsmax  = 1.e+10  ! Maximum topographic height for finding cyclones
      zscr1  = 1.e+10  ! / Lower and upper topographic heights for phasing
      zscr2  = 1.e+10  ! \ down cyclone velocities from PMSL data.
      ftopeq = 0.      ! Error in delsq-f units per unit of delsq-zs caused
                       !   by reduction from topographic height zs
                       !   (0.01 suitable for f in mb., zs in m.)
      cmnhw  = 0.      ! Minimum value Laplacian at grid pt. max.\ for strong
      cmncw  = 0.      ! Minimum central or ave. of Laplacian    | status
      swvmnw = 0.      ! Minimum third derivative at inflection  | (iop>=10)
      dpmnw  = 0.      ! Minimum depth (if calculated) in mb.    |
      fmxcw  = -999.   ! Maximum function value at centre        |
      fccmnw = 0.      ! Minimum cross gradient 2nd deriv.       /

      offlon= 0.       ! KK Longitude offset (if -ve longitudes present)

c------------------------------------------------------------------------------
c     Instruction file
c------------------------------------------------------------------------------

c     Read instruction file
c     ---------------------

      write (6,'()')
      if ((.not.fpdata).or.zdata.or.zpdata.or.zpwrit) then
        write (6,'(a)') ' Reading namelist "nmltrangp"'
        read (ioin,nmltrangp,err=940,end=950)

        if ((xcen.lt.1).or.(ycen.lt.1)) then
          xcen = float(int(1+ni)/2) 
          ycen = float(int(1+nj)/2)
        endif

        niexp = ni
        njexp = nj
        xcenexp = xcen
        ycenexp = ycen
        rprojexp = rproj
      endif
      if (cyccalc) then
        write (6,'(a)') ' Reading namelist "nmlcycloc"'
        read (ioin,nmlcycloc,err=940,end=950)
      endif
      if (diag) write (6,60) ' Input file read.'
     *, ddtime(),eetime()
   60 format (a,t60,2f10.2)
      close (ioin)

c     Consistency checks
c     ------------------

      itabc1 = 1
      itabc2 = max0(min0(itabc2,2),1)
      if (itabc3.eq.-1) itabc3 = min0(itabc,3)
      if (itabc4.eq.-1) itabc4 = max0(min0(itabc-3,1),0)
      itabc3 = max0(min0(itabc3,4),0)
      itabc4 = max0(min0(itabc4,1),0)
      if (upredu.or.upredg) itabc4 = 1

      if (intopt.eq.1) stop ' Linear interpolation not allowed.'
      if (intopt.eq.2) stop ' Mixed interpolation not allowed.'

      rdiff1 = rdiff
      rdifz1 = rdifz

crmw  amin0 -> min
      cmnc012 = min(cmnc0,cmnc1,cmnc2)
crmw
      if (cmnh.lt.cmnh012) cmnh = cmnh012

      if (.not.zdata) then
        if (zsmax.lt.1.e+10.or.zscr2.lt.1.e+10) then
          zscr1 = 1.e+10
          zscr2 = 1.e+10
          zsmax = 1.e+10
          write (6,'(a)') ' Warning: no topography test done.'
        endif
        if (zscr2.lt.zscr1 .or. zscr1.lt.0.) 
     *   stop ' Must have zscr2 >= zscr1 >= 0.'
        if (ftopeq.ne.0.) then
          ftopeq = 0.
          write (6,'(a)') ' Warning: no delsqzs test done.'
          write (6,'(a)') ' Warning: not topography test done.'
        endif
c     else
c#if !defined delsqzs
c       if (ftopeq.ne.0.) write (6,*) ' Warning: delsqzs option not',
c    *   ' defined; ftopeq unused.'
c#endif
      endif
      ftop = ftopeq.ne.0.

c     (No others included at this stage.)

      qsteer = ' '
      if (itabc4.eq.1) then
        if (upredu) then
          qsteer = 'U'
        else if (upredg) then
          qsteer = 'Z'
        else
          qsteer = 'F'
          if (rdustr.eq.-999.) rdustr = rdpgrd
        endif
        if (rdustr.eq.-999.) rdustr = 0.
      endif

      if ((hilo.eq.'H').or.(hilo.eq.'h')) then
        hilo = 'H'
        feat = 'HIGH'
      else
        hilo = 'L'
        feat = 'LOW'
      endif
      if (noarea) area = hemis // 'H'

c     Constants derived from instruction parameters
c     ---------------------------------------------

      if (rdiff1.gt.0. .or. rdifz1.gt.0.) then
        if (rdiff1.gt.0.) sfcalc = .true.
        if (rdifz1.gt.0.) szcalc = .true.

crmw    amax0 -> max
        xs = max(abs(float(1)-xcen),abs(float(ni)-xcen))
        ys = max(abs(float(1)-ycen),abs(float(nj)-ycen))
crmw
        rprojsq = rproj*rproj
        rssq = xs*xs + ys*ys
        clatmx = 2.*rad*atan(sqrt(rssq/rprojsq))
        if (clatmx.gt.100.) then
          clatmx = 100.
          rssq = rprojsq*tan(r2rad*clatmx)**2
        endif
        scaleconst = 2.*rproj*rad
        scalt0  = scaleconst/ rprojsq
        scaltmx = scaleconst/(rprojsq + rssq)

        write (6,'(/a/)') ' Smoothing quantities:'
        write (6,*) ' xs,ys    = ',xs,ys 
        write (6,*) ' rssq     = ',rssq  
        write (6,*) ' rprojsq  = ',rprojsq  
        write (6,*) ' R        = ',sqrt(rssq/rprojsq) 
        write (6,*) ' clatmx   = ',clatmx
        write (6,*) ' scalt0   = ',scalt0
        write (6,*) ' scaltmx  = ',scaltmx

        if (sfcalc) then
          rdfgrd0 = rdiff1/scalt0
          rdfgrdmx= rdiff1/scaltmx
          ndiff   = max0(nint(rdfgrdmx*rdfgrdmx+0.5),1)
          ak0f    = 0.25*rdfgrd0*rdfgrd0/float(ndiff)
          write (6,*) ' rdiff1   = ',rdiff1
          write (6,*) ' rdfgrd0  = ',rdfgrd0
          write (6,*) ' rdfgrdmx = ',rdfgrdmx
          write (6,*) ' ndiff    = ',ndiff
          write (6,*) ' ak0f     = ',ak0f
        endif

        if (szcalc) then
          rdzgrd0 = rdifz1/scalt0
          rdzgrdmx= rdifz1/scaltmx
          ndifz   = max0(nint(rdzgrdmx*rdzgrdmx+0.5),1)
          ak0z    = 0.25*rdzgrd0*rdzgrd0/float(ndifz)
          write (6,*) ' rdifz1   = ',rdifz1
          write (6,*) ' rdzgrd0  = ',rdzgrd0
          write (6,*) ' rdzgrdmx = ',rdzgrdmx
          write (6,*) ' ndifz    = ',ndifz
          write (6,*) ' ak0z     = ',ak0z
        endif

        ndifmx = 120
        if (ndiff.gt.ndifmx .or. ndifz.gt.ndifmx) then
          write (6,*) ' ndiff = ',ndiff,
     *               ', ndifz = ',ndifz,': Too many passes.'
          if (fswrit) close (iosp,status='delete')
          if (flwrit) close (iosl,status='delete')
          if (zswrit) close (iosz,status='delete')
          if (zlwrit) close (iosz,status='delete')
          if (llconv) close (iosz,status='keep')
          stop
        endif
      else
        if (fswrit.or.flwrit.or.zswrit) stop 
     *   ' Cannot write smoothed PS data since rdiff1 = 0.'
      endif
        
      nij = ni*nj
      noarea = lnblnk(area).eq.0
     
c     Hemisphere for projection
c     -------------------------

      if (cyccalc) then
        alatlt = 10.
        if (latmnc.lt.-alatlt) then
          if (latmxc.gt.alatlt) then
            hemis = 'G'
          else
            hemis = 'S'
          endif
        else if (latmxc.gt.alatlt) then
          hemis = 'N'
        else
          if (hemis.eq.' ') then
            alatav = (latmnc + latmxc)*0.5
            if (alatav.gt.0.) then 
              hemis = 'N'
            else
              hemis = 'S'
            endif
          endif
        endif
        if (hemis.eq.'G') alatlt = 5.
      endif
      nhem = ((hemis.eq.'N').or.(hemis.eq.'G'))
      shem = ((hemis.eq.'S').or.(hemis.eq.'G'))
      if (.not.(nhem.or.shem)) stop ' Specify hemisphere.'
      if (diag) write (6,60) ' Preliminaries done.'
     *, ddtime(),eetime()

c     Margins for extending latitude-longitude arrays
c     -----------------------------------------------

      mgL = marglt
      mgR = marglt
      mgD = 0
      mgU = 0
      if (nhem.or.expNS) mgU = marglt
      if (shem.or.expNS) mgD = marglt

c------------------------------------------------------------------------------
c     Establish lats. and lons. for writing smoothed data
c------------------------------------------------------------------------------

      if (llconv) then
        if (llform) then
          read (ioll,'(a80)') head2
ckk         read (ioll,'(/'' Latitudes  ('',i3,''):'')') nlats
          read (ioll,'(A80)')ch80 !ckk Blank line
          read (ioll,'(A80)')ch80
	  i1= index(ch80,'(') +1
	  i2= index(ch80,')') -1
          read(ch80(i1:i2),*)nlats
          read (ioll,*) (lats(j),j=1,nlats)
ckk          read (ioll,'(/'' Longitudes ('',i3,''):'')') nlons
          read (ioll,'(A80)')ch80 !ckk Blank line
          read (ioll,'(A80)')ch80
	  i1= index(ch80,'(') +1
	  i2= index(ch80,')') -1
          read(ch80(i1:i2),*)nlons
          read (ioll,*) (lons(i),i=1,nlons)
        else
          read (ioll) nlats
          read (ioll) (lats(j),j=1,nlats)
          read (ioll) nlons
          read (ioll) (lons(i),i=1,nlons)
          read (ioll) head2
        endif
        if (diag) then
          write (6,'(/a)') 
     *     ' Longitudes and latitudes for smoothed data:'
          write (6,'(/a80)') head2
          write (6,'(/'' Latitudes  ('',i3,''):'')') nlats
          write (6,80) (lats(j),j=1,nlats)
  80      format (8f10.3)
          write (6,'(/'' Longitudes ('',i4,''):'')') nlons !ckk
          write (6,80) (lons(i),i=1,nlons)
          write (6,*)
          write (6,60) ' Lats. and lons. for smoothed data read.'
     *,    ddtime(),eetime()
        endif
      endif

c------------------------------------------------------------------------------
c     Read and manipulate topography
c------------------------------------------------------------------------------

      if (zdata.and.(zsmax.lt.1.e+10 .or. zscr2.lt.1.e+10
     * .or. itabc3.ge.4 .or. ftop .or. szcalc .or.
     * zpwrit .or. zswrit .or. zlwrit)) then

        if (zpdata) then

c         PS topographic data supplied
c         ----------------------------

c         (a) Read data
c             ---------

          if (nhem) then
            call psrd(iozs,quantz,levelz,lunitz,sourcez,dmodez,unitz,
     *       gridz,ni,nj,hemisx,xcen,ycen,rproj,da,hr,zij(1,1),nijlt,
     *       spval,rdifz0,ier)

            if (ier.eq.2) stop ' Cannot read PS zs data file.'
            if (ier.eq.1) stop ' Unexpected end of PS zs file.'
            if (hemisx.ne.'N') then
              write (6,'(a)') ' Expecting N hemis. data: trying again'
              call psrd(iozs,quantz,levelz,lunitz,sourcez,dmodez,unitz,
     *         gridz,ni,nj,hemisx,xcen,ycen,rproj,da,hr,zij(1,1),nijlt,
     *         spval,rdifz0,ier)
              if (hemisx.ne.'N') stop ' Expecting N hemis. data.'
            endif
          endif
          if (shem) then
            call psrd(iozs,quantz,levelz,lunitz,sourcez,dmodez,unitz,
     *       gridz,ni,nj,hemisx,xcen,ycen,rproj,da,hr,zij(1,2),nijlt,
     *       spval,rdifz0,ier)
            if (ier.eq.2) stop ' Cannot read PS zs data file.'
            if (ier.eq.1) stop ' Unexpected end of PS zs file.'
            if (hemisx.ne.'S') then
              write (6,'(a)') ' Expecting S hemis. data: trying again'
              call psrd(iozs,quantz,levelz,lunitz,sourcez,dmodez,unitz,
     *         gridz,ni,nj,hemisx,xcen,ycen,rproj,da,hr,zij(1,2),nijlt,
     *         spval,rdifz0,ier)
              if (hemisx.ne.'S') stop ' Expecting S hemis. data.'
            endif
          endif
          if ((ni.ne.niexp.or.nj.ne.njexp.or.rproj.ne.rprojexp.or.
     *     xcen.ne.xcenexp.or.ycen.ne.ycenexp).and.
     *     (zdata.or.zpdata.or.zpwrit)) then
            write (6,*) ' ni,nj,xcen,ycen,rproj expected: ',
     *       niexp,njexp,xcenexp,ycenexp,rprojexp
            write (6,*) ' ni,nj,xcen,ycen,rproj read    : ',
     *       ni,nj,xcen,ycen,rproj
            stop
          endif
          if (diag) write (6,*) hemisx,quantz,levelz,sourcez

c         (b) Calculate smoothing radii
c             -------------------------

          if (rdifz0.eq.0.) then
            rdifz = rdifz1
          else if (rdifz1.eq.0.) then
            rdifz = rdifz0
          else
            rdifz = sqrt(rdifz0*rdifz0 + rdifz1*rdifz1)
          endif

        else

c         Lat.-lon. topographic data supplied
c         -----------------------------------

c         (a) Read lat-lon zs array
c             ---------------------

          call llmaprd(iozs,llhead,nlonlt,nlatlt,nlllt,nlat,lat,nlon,
     *     lon,fhk,quantz,levelz,lunitz,sourcez,dmodez,da,hr,unitz,
     *     llgrid,offlon,llform,ie1)
          close (iozs)
          write (6,*) ' '
          write (6,60) ' Topography read: '
     *,    ddtime(),eetime()
          write (6,'(a)') llhead

c         (b) Expand zs array
c             ---------------

          idiagy1 = idiagy * lint(idiagx.eq.4)
          if (idiagy1.ge.1) write (6,*) ' '
          if (idiagy1.ge.1) write (6,*) ' Diagnostic output ',da,hr,
     *     ': ',hemis,'H ',quant,level,feat

          call llexpand(nlllt,nll2lt,nlon,nlat,lon,lat,nlon2,nlat2,
     *     lon2,lat2,0,mgL,mgR,mgD,mgU,fhk,f,llhead,zfile,ioau,
     *     idiagy1,ie3)
          write (6,60) ' Topography arrays expanded.'
     *,    ddtime(),eetime()

c         (c) Bicubic spline coefficients for topography
c             ------------------------------------------

          idiagy1 = idiagy * lint(idiagx.eq.5)
          if (intopt.ne.1) call bisplsv(nlon2,nlat2,lon2,lat2,
     *     f,fxx,fyy,fxxyy,1,nlon2,1,nlat2,spval,idiagy1,ie4)
          write (6,60) ' Bicubic splines done for topography.'
     *,    ddtime(),eetime()

c         (d) Interpolate zs to PS points
c             ---------------------------

          if (nhem) call intgpr(nlon2,nlat2,f,fxx,fyy,fxxyy,'N',rproj,
     *     intopt,ni,nj,xcen,ycen,lon2,lat2,zij(1,1),spval,idiagx,ie5)
          if (shem) call intgpr(nlon2,nlat2,f,fxx,fyy,fxxyy,'S',rproj,
     *     intopt,ni,nj,xcen,ycen,lon2,lat2,zij(1,2),spval,idiagx,ie5)
          write (6,60) ' Topography interpolated to PS.'
     *,    ddtime(),eetime()

c         (e) Calculate smoothing radii
c             -------------------------

          if (rdifz0.eq.0.) then
            rdifz = rdifz1
          else if (rdifz1.eq.0.) then
            rdifz = rdifz0
          else
            rdifz = sqrt(rdifz0*rdifz0 + rdifz1*rdifz1)
          endif

c         (f) Write polar stereographic topography
c             ------------------------------------

          if (zpwrit) then
            open (unit=iozs,file=zpfile,form='unformatted')
            if(nhem)call pswr(iozs,quantz,levelz,lunitz,sourcez,dmodez,
     *       unitz,gridz,ni,nj,'N',xcen,ycen,rproj,0,0,zij(1,1),spval,
     *       rdifz0)
            if(shem)call pswr(iozs,quantz,levelz,lunitz,sourcez,dmodez,
     *       unitz,gridz,ni,nj,'S',xcen,ycen,rproj,0,0,zij(1,2),spval,
     *       rdifz0)
            close (iozs)
            write (6,60) ' Topography written to PS file:'
     *,      ddtime(),eetime()
            write (6,'(a)') zpfile(1:lnblnk(zpfile))
          endif

        endif

c       Smooth and write PS topography if required
c       ------------------------------------------

        if (szcalc) then
          if (nhem) call pssmth(ndifz,ak0z,clatmx,zij(1,1),fxx,fyy,
     *     fxxyy,idiagx)
          if (shem) call pssmth(ndifz,ak0z,clatmx,zij(1,2),fxx,fyy,
     *     fxxyy,idiagx)
          write (6,60) ' Topographic smoothing done.'
     *,    ddtime(),eetime()

          sourcey = sourcez(1:lnblnk(sourcex)) // '_S'
          if (zswrit) then
            open (unit=iozs,file=zsfile,form='unformatted')
            if (nhem) call pswr(iosz,quantz,levelz,lunitz,sourcey,
     *       dmodez,unitz,gridz,ni,nj,hemisx,xcen,ycen,rproj,
     *       da,hr,zij(1,1),spval,rdifz)
            if (shem) call pswr(iosz,quantz,levelz,lunitz,sourcey,
     *       dmodez,unitz,gridz,ni,nj,hemisx,xcen,ycen,rproj,
     *       da,hr,zij(1,2),spval,rdifz)
            close (iozs)
            write (6,60) ' Smoothed PS topog. written.'
     *,      ddtime(),eetime()
          endif
        endif

c       Calculate splines and reinterpolate smoothed 
c       topography if required
c       --------------------------------------------

        if (ftop .or. zlwrit) then

          do 120 ihem = 1,2
            if (ihem.eq.1) then
              if (.not.nhem) go to 110
              fhem = 1.
              hemisx = 'N'
            else
              if (.not.shem) go to 130
              fhem = -1.
              hemisx = 'S'
            endif

c           Calculate bicubic spline coefficients for PS topography
c           -------------------------------------------------------

c             Bicubic spline coefficients are evaluated for the entire array,
c           with an integral grid spacing and zero second derivatives at the
c           boundaries.  The field is checked for special values.  

            idiagy1 = idiagy * lint(idiagx.eq.5)
c#ifdef delsqzs
            call brsplsv(ni,nj,zij(1,ihem),zxx(1,ihem),zyy(1,ihem),
     *       zxxyy(1,ihem),
c#else
c           call brsplsv(ni,nj,zij(1,ihem),fxx,fyy,fxxyy,
c#endif
     *       1,ni,1,nj,spval,idiagy1,ier)
            if (ier.gt.0) stop ' Error in brsplsv.'
            write (6,'(a,a1,a,t60,2f10.2)') 
     *       ' Topographic bicubic splines done, ',hemisx,' hemisphere.'
     *,      ddtime(),eetime()

c           Reinterpolate to lat.-lon. grid
c           -------------------------------

            if (zlwrit) then
              if (.not.llconv) then
                nlons = nlon
                nlats = nlat
                do 90 i = 1,nlons
                  lons(i) = lon(i)
   90           continue
                do 100 j = 1,nlats
                  lats(j) = lat(j)
  100           continue
              endif

              klo = 1
              khi = nlats
              if (nhem.and.shem) then
                call srchls(0.,lats,nlats,keqs,keqn)
                if (ihem.eq.2) khi = keqs
                if (ihem.eq.1) klo = keqn
              endif
c             write (6,*) 'ni,nj,xcen,ycen,rproj,fhem,',
c    *         'intopt,nlons,nlats,klo,khi,spval,ierr'
c             write (6,*) ni,nj,xcen,ycen,rproj,fhem,
c    *         intopt,nlons,nlats,klo,khi,spval,ierr
              call intpgr(ni,nj,xcen,ycen,rproj,fhem,zij(1,ihem),
c#ifdef delsqzs
     *         zxx(1,ihem),zyy(1,ihem),zxxyy(1,ihem),
c#else
c    *         fxx,fyy,fxxyy,
c#endif
     *         intopt,nlons,nlats,lons,lats,fhk,klo,khi,spval,ierr)
              write (6,'(a,a1,a,t60,2f10.2)') 
     *         ' Smoothed topography reinterpolated, ',hemisx,
     *         ' hemisphere.'
     *,        ddtime(),eetime()
            endif

  110       continue
  120     continue
  130     continue

c         Write reinterpolated smoothed topography
c         ----------------------------------------

          if (zlwrit) then
            open (unit=iosz,file=zlfile,form='unformatted')
            write (iosz) nlats
            write (iosz) (lats(k),k=1,nlats)
            write (iosz) nlons
            write (iosz) (lons(n),n=1,nlons)
            llhead(31:42) = sourcey
            write (iosz) llhead
            write (iosz) (fhk(nk),nk=1,nlons*nlats)
            close (iosz)
            write (6,'(a,t60,2f10.2)') 
     *       ' Reinterpolated smoothed topography written.'
     *,      ddtime(),eetime()
          endif
        endif
      else if (zdata) then
        write (6,*) ' Topography not required.'
      else
        write (6,*) ' Topography not supplied.'
        zsmax = 1.e+10
        zscr2 = 1.e+10
        zscr1 = 1.e+10
        ftopeq = 0.
        ftop   = .false.
        szcalc = .false.
      endif

c------------------------------------------------------------------------------
c     Processing of data fields
c------------------------------------------------------------------------------

c       The data may be contained as a number of fields (imap)
c     in one data files and/or in many data files (ifile).

      ientry = 0
      istrt = 0

      eetimey = 0.

      do 400 ifile = optind,iargc()
        call getarg(ifile,ffile)
        if (diag) write (6,'(a80)') ffile(1:lnblnk(ffile))
cli        open (ioda,file=ffile,status='old',form='unformatted',err=140)
        if (llform) then
          open (ioda,file=ffile,status='old',form='formatted',err=140)
        else
          open (ioda,file=ffile,status='old',form='unformatted',err=140)
        endif
        go to 150
  140   write (6,*) ' Cannot find data file: ',
     *   ffile(1:lnblnk(ffile))
        go to 390
  150   continue

        do 380 imap = 1,10000

c         POLAR STEREOGRAPHIC DATA
c         ------------------------

          if (.not.fpdata) then

            ddtimex = ddtime()
            eetimex = eetime()

c           Read lat-lon data for one analysis time
c           ---------------------------------------

c             A user-supplied routine reads or specifies the required
c           information about the latitude-longitude data and reads the 
c           array data.  Attributes, such as data mode, quantity, level,
c           unit, etc., if not read from the data, should be supplied
c           by the routine or specified in the instruction file.

             call llmaprd(ioda,llhead,nlonlt,nlatlt,nlllt,nlat,lat,nlon,
     *       lon,fhk,quantx,levelx,lunitx,sourcex,dmodex,da,hr,unitx,
     *       llgrid,offlon,llform,ie1)
             if (diag) write (6,'(a80)') llhead(1:lnblnk(llhead))
 
            if (ie1.ge.1) go to 390

c           Checking of date and time
c           -------------------------

c             This routine checks that time and date fall within specified 
c           time limits and that if follow sequentially from previous time
c           to within specified time interval limits.

crmw            print*, 'Checking of date and time 1'
crmw            print*, 'da', da
            if ((dastrt.ne.0).or.(dastop.ne.0).or.(ddhmmn.ne.0).or.
     *       (ddhmmx.ne.0)) call checkdate(istrt,da,hr,dalast,hrlast,
     *       dmode,ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,ie2)
crmw            print*, ie2
            if (ie2.eq.1) go to 370

            ientry = ientry + 1

c           Checking of information in data file
c           ------------------------------------

c             Information relating to the data are taken from the 
c           instruction file if not supplied in the data file.
  
            if (ientry.eq.1) then
              if (quant(1:5) .eq.'     ') quant  = quantx
              if (level(1:5) .eq.'     ') level  = levelx
              if (lunit(1:5) .eq.'     ') lunit  = lunitx
              if (source(1:5).eq.'     ') source = sourcex
              if (unit(1:5)  .eq.'     ') unit   = unitx
              if (dmode(1:5) .eq.'     ') dmode  = dmodex
            endif
            cunit = unit(1:lnblnk(unit)) // '/DL**2'

            if (rdiff0.eq.0.) then
              rdiff = rdiff1
            else if (rdiff1.eq.0.) then
              rdiff = rdiff0
            else
              rdiff = sqrt(rdiff0*rdiff0 + rdiff1*rdiff1)
            endif

c           Instruction parameters (as adjusted)
c           ------------------------------------

            if (ientry.eq.1) then
              write (6,'(/a)') ' Trangp parameters:'
              write (6,nmltrangp2)
              if (cyccalc) then
                write (6,'(/a/)') ' Cycloc parameters:'
                write (6,nmlcycloc2)
              endif
              write (6,'(/a/)') ' Date and time limits:'
              write (6,nmldates)
              write (6,'()')
            endif

            if (imap.eq.1 .and. diag .and. ifile.gt.optind) write (6,*) ' '
            if (imap.eq.1)write (6,'('' Reading data from file: '',a)') 
     *       ffile(1:lnblnk(ffile))
            if (diag) write (6,'(/a,2x,i6,x,i4,t60,2f10.2/a,t50,f10.2)') 
     *       ' Start reading data: ',da,hr,eetimex-eetimey,eetimex,
     *       ' Lat.-lon. Data read: ',ddtime()

c           Checking for data errors
c           ------------------------

c           if (errdet) stop ' No error checking of lat-lon data available.'

c           Expansion of grid
c           -----------------

c             The original latitude-longitude nlon*nk array (fhk) is expanded 
c           by `marglt' grid points around each edge to an nlon2*nlat2 array (f)
c           in order that the bicubic spline derivatives used in the 
c           interpolation may be cushioned from the arbitrary prescription of 
c           boundary values.  This is done by wrapping around the W and E 
c           meridians and the N and S poles, after checking that this be 
c           possible.

c             The expansion should not be used if the quantity be the easterly
c           or northerly component of a vector quantity.  Vectors should be
c           first transformed thus:
c               udash =  u*cos(theta) + v*sin(theta)
c               vdash = -u*sin(theta) + v*cos(theta)
c           where theta = the angle clockwise from the GM (-or+ longitude in 
c           the N and S hemisphere projections, respectively).   The rotated 
c           components may then be remapped without hazard.

c             See also `Margins' above.

            idiagy1 = idiagy * lint(idiagx.eq.4)
            if (idiagy1.ge.1) write (6,*) ' '
            if (idiagy1.ge.1) write (6,*) ' Diagnostic output ',da,hr,
     *       ': ',hemis,'H ',quant,level,feat

            call llexpand(nlllt,nll2lt,nlon,nlat,lon,lat,nlon2,nlat2,
     *       lon2,lat2,0,mgL,mgR,mgD,mgU,fhk,f,llhead,ffile,ioau,
     *       idiagy1,ie3)
            if (diag) write (6,160) ' Arrays expanded.'
     *,      ddtime()
  160       format (a,t50,f10.2)

c           Bicubic spline coefficients
c           ---------------------------

c             If a bicubic spline interpolation be required, the 2nd
c           derivatives, fxx, fyy, & fxxyy, are calculated over the array
c           with zero second derivatives at the boundaries.  The field 
c           is checked for special values.  

            idiagy1 = idiagy * lint(idiagx.eq.5)
            if (intopt.ne.1) call bisplsv(nlon2,nlat2,lon2,lat2,
     *       f,fxx,fyy,fxxyy,1,nlon2,1,nlat2,spval,idiagy1,ie4)
            if (diag) write (6,160) ' Bicubic splines done.'
     *,      ddtime()

c           Bicubic spline interpolation to polar stereographic
c           ---------------------------------------------------

            if (nhem) call intgpr(nlon2,nlat2,f,fxx,fyy,fxxyy,'N',rproj,
     *       intopt,ni,nj,xcen,ycen,lon2,lat2,fij(1,1),spval,idiagx,ie5)
            if (shem) call intgpr(nlon2,nlat2,f,fxx,fyy,fxxyy,'S',rproj,
     *       intopt,ni,nj,xcen,ycen,lon2,lat2,fij(1,2),spval,idiagx,ie5)
            if (diag) write (6,160) ' Interpolation done.'
     *,      ddtime()

            if (noneg) then
              do 180 ihem = 1,2
                do 170 ij = 1,nijlt
                  if (fij(ij,ihem).lt.0.) fij(ij,ihem) = 0.
  170           continue
  180         continue
            endif

c           Smoothing of PS data
c           --------------------

c           Not implemented

c           Write to PS file
c           ----------------

c             An input-output routine is called to write the PS data.

            if (fpwrit) then
              if (nhem) call pswr(iops,quant,level,lunit,source,dmode,
     *         unit,grid,ni,nj,'N',xcen,ycen,rproj,da,hr,fij(1,1),spval,
     *         rdiff0)
              if (shem) call pswr(iops,quant,level,lunit,source,dmode,
     *         unit,grid,ni,nj,'S',xcen,ycen,rproj,da,hr,fij(1,2),spval,
     *         rdiff0)

              if (ientry.eq.1) write (6,'()')
              write (6,'(i3,'': '',a8,x,a9,x,a20,x,i6,x,i4)') ientry,
     *         quant,level,source,da,hr
            endif

          else

            ddtimex = ddtime()
            eetimex = eetime()

c           Reading of polar stereographic grid point data
c           ----------------------------------------------

c             A user-supplied subroutine reads (or inserts) information 
c           about the data and reads the data array.

            quant  = ' '
            level  = ' '
            lunit  = ' '
            source = ' '
            unit   = ' '
            dmode  = ' '

            if (nhem) then
              call psrd(ioda,quant,level,lunit,source,dmode,unit,
     *         grid,ni,nj,hemisx,xcen,ycen,rproj,da,hr,fij(1,1),nijlt,
     *         spval,rdiff0,ier)

              if (ier.eq.2) stop ' Cannot read data file.'
              if (ier.eq.1) go to 390
              if (hemisx.ne.'N') then
                write (6,'(a)') ' Expecting N hemis. data: trying again'
                call psrd(ioda,quant,level,lunit,source,dmode,unit,
     *           grid,ni,nj,hemisx,xcen,ycen,rproj,da,hr,fij(1,1),nijlt,
     *           spval,rdiff0,ier)
                if (hemisx.ne.'N') 
     *           stop ' Expecting N hemis. data.'
              endif
              idan = da
              ihrn = hr
            endif
            if (shem) then
              call psrd(ioda,quant,level,lunit,source,dmode,unit,
     *         grid,ni,nj,hemisx,xcen,ycen,rproj,da,hr,fij(1,2),nijlt,
     *         spval,rdiff0,ier)
              if (ier.eq.2) stop ' Cannot read data file.'
              if (ier.eq.1) go to 390
              if (hemisx.ne.'S') then
                write (6,'(a)') ' Expecting S hemis. data: trying again'
                call psrd(ioda,quant,level,lunit,source,dmode,unit,
     *           grid,ni,nj,hemisx,xcen,ycen,rproj,da,hr,fij(1,2),nijlt,
     *           spval,rdiff0,ier)
                if (hemisx.ne.'S') 
     *           stop ' Expecting S hemis. data.'
              endif
              if ((nhem.and.shem).and.((da.ne.idan).or.(hr.ne.ihrn)))
     *         stop ' N and S hemisphere data dates do not match.'
            endif
            if ((ni.ne.niexp.or.nj.ne.njexp.or.rproj.ne.rprojexp.or.
     *       xcen.ne.xcenexp.or.ycen.ne.ycenexp).and.
     *       (zdata.or.zpdata.or.zpwrit)) then
              write (6,*) ' ni,nj,xcen,ycen,rproj expected: ',
     *         niexp,njexp,xcenexp,ycenexp,rprojexp
              write (6,*) ' ni,nj,xcen,ycen,rproj read    : ',
     *         ni,nj,xcen,ycen,rproj
              stop
            endif
            if (diag) write (6,*) da,hr,hemisx,quant,level,source

            cunit = unit(1:lnblnk(unit)) // '/DL**2'

            if (rdiff0.eq.0.) then
              rdiff = rdiff1
            else if (rdiff1.eq.0.) then
              rdiff = rdiff0
            else
              rdiff = sqrt(rdiff0*rdiff0 + rdiff1*rdiff1)
            endif

c           Checking of date and time
c           -------------------------

c             This routine checks that time and date fall within specified 
c           time limits and that if follow sequentially from previous time
c           to within specified time interval limits.

            if ((dastrt.ne.0).or.(dastop.ne.0).or.(ddhmmn.ne.0).or.
     *       (ddhmmx.ne.0)) call checkdate(istrt,da,hr,dalast,hrlast,
     *       dmode,ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,ie2)
            if (ie2.eq.1) go to 370

c           Trangp and cycloc instruction parameters (as adjusted)
c           ------------------------------------------------------

            ientry = ientry + 1

            if (ientry.eq.1) then
              write (6,'(/a/)') ' Trangp parameters:'
              write (6,nmltrangp3)
              write (6,'(/a/)') ' Cycloc parameters:'
              write (6,nmlcycloc2)
              write (6,'(/a/)') ' Date and time limits:'
              write (6,nmldates)
              write (6,'()')
            endif

            if (imap.eq.1) write (6,'('' Reading PS data from file: '',
     *       a40)') fpfile(1:40)

            if (diag) write (6,'(/a,2x,i6,x,i4,t60,2f10.2/a,t50,f10.2)') 
     *       ' Start reading data: ',da,hr,eetimex-eetimey,eetimex,
     *       ' PS data read: ',ddtime()


c           Checking of information in data file
c           ------------------------------------

            nij = ni*nj
            if (nij.gt.nijlt) stop ' nij > nijlt.'

          endif

c         CYCLONE FINDING
c         ---------------

          if (cyccalc.or.sfcalc) then

            klast = 0
            do 300 ihem = 1,2

              if (ihem.eq.1) then
                 if (.not.nhem) go to 290
                 fhem = 1.
                 hemisx = 'N'
              else
                 if (.not.shem) go to 310
                 fhem = -1.
                 hemisx = 'S'
              endif

c             Smooth data if required
c             -----------------------

              if (sfcalc) then
                call pssmth(ndiff,ak0f,clatmx,fij(1,ihem),f,fxx,fyy,
     *           idiagx)
                if (diag) write (6,'(a,a1,a,t50,f10.2)') 
     *           ' Smoothing done, ',hemisx,' hemisphere'
     *,          ddtime()

                sourcey = source(1:lnblnk(source)) // '_S'
                if (fswrit) then
                  call pswr(iosp,quant,level,lunit,sourcey,
     *             dmode,unit,grid,ni,nj,hemisx,xcen,ycen,rproj,
     *             da,hr,fij(1,ihem),spval,rdiff)
                  if (diag) write (6,'(a,a1,a,t50,f10.2)') 
     *             ' Smoothed PS data written, ',hemisx,' hemisphere.'
     *,            ddtime()
                endif
              endif

c             Copy PS data for hemisphere to f array
c             --------------------------------------

              do 190 ij = 1,nij
                f(ij) = fij(ij,ihem)
  190         continue

c             Bicubic spline coefficients for PS array (cyclone finding)
c             ----------------------------------------------------------

c               Bicubic spline coefficients are evaluated for the entire array,
c             with an integral grid spacing and zero second derivatives at the
c             boundaries.  The field is checked for special values.  

              idiagy1 = idiagy * lint(idiagx.eq.5)
              call brsplsv(ni,nj,f,fxx,fyy,fxxyy,1,ni,1,nj,spval,
     *         idiagy1,ier)
              if (ier.gt.0) stop ' Error in brsplsv.'
              if (diag) write (6,'(a,a1,a,t50,f10.2)') 
     *         ' Bicubic splines done, ',hemisx,' hemisphere.'
     *,        ddtime()

c             Reinterpolate smoothed data
c             ---------------------------

              if (flwrit) then
                if (.not.llconv) then
                  nlons = nlon
                  nlats = nlat
                  do 210 i = 1,nlons
                    lons(i) = lon(i)
  210             continue
                  do 220 j = 1,nlats
                    lats(j) = lat(j)
  220             continue
                endif
                klo = 1
                khi = nlats
                if (nhem.and.shem) then
                  call srchls(0.,lats,nlat,keqs,keqn)
                  if (ihem.eq.2) khi = keqs
                  if (ihem.eq.1) klo = keqn
                endif
c               write (6,*) 'ni,nj,xcen,ycen,rproj,fhem,',
c    *           'intopt,nlons,nlats,klo,khi,spval,ierr'
c               write (6,*) ni,nj,xcen,ycen,rproj,fhem,
c    *           intopt,nlons,nlats,klo,khi,spval,ierr
                call intpgr(ni,nj,xcen,ycen,rproj,fhem,f,fxx,fyy,fxxyy,
     *           intopt,nlons,nlats,lons,lats,fhk,klo,khi,spval,ierr)
                if (diag) write (6,'(a,a1,a,t50,f10.2)') 
     *           ' Smoothed data reinterpolated, ',hemisx,
     *           ' hemisphere.'
     *,          ddtime()
              endif

c             Location of cyclones 
c             --------------------

              if (cyccalc) then
                idiagy1 = idiagy * lint(idiagx.eq.6) +
     *                    idiagx * lint(idiagx.le.3)
                if (idiagy1.ge.1) write (6,*) ' '
                if (idiagy1.ge.1) write (6,*) ' Diagnostic output ',
     *           da,hr,': ',hemisx,'H ',quant,level,feat
  
                call cycp(f,fxx,fyy,fxxyy,q,zij(1,ihem),
c#ifdef delsqzs
     *           zxx(1,ihem),zyy(1,ihem),zxxyy(1,ihem),
c#endif
     *           idiagy1,ier)

c               call cycpnw(f,fxx,fyy,fxxyy,q,zij(1,ihem),idiagy1,ier)
                if (ier.gt.0) stop ' Error in cycp.'
  
                if (diag) write (6,'(a,a1,a,t50,f10.2)') 
     *           ' Cyclones found, ',hemisx,' hemisphere.'
     *,          ddtime()
                if (.not.(cycwrit.or.flwrit))
     *           stop ' End of diagnostic finding run.'
              endif

  290         continue
  300       continue
  310       continue

c           Write reinterpolated smoothed data, if required
c           -----------------------------------------------

            if (flwrit) then
	        write(*,*)'nlats,nlons:',nlats,nlons
              write (iosl) nlats
              write (iosl) (lats(k),k=1,nlats)
              write (iosl) nlons
	      if(offlon.gt.0.)then  !KK Temporary fix 29/5/2007
                write (iosl) (lons(n)-offlon,n=1,nlons)
	      else
                write (iosl) (lons(n),n=1,nlons)
	      endif
              llhead(31:42) = sourcey
              write (iosl) llhead
              write (iosl) (fhk(nk),nk=1,nlons*nlats)
              if (diag) write (6,160) 
     *         ' Reinterpolated smoothed data written.'
     *,        ddtime()
            endif

            if (cyccalc) then

c             Cyclone prediction velocities from external fields
c             --------------------------------------------------
  
              if (upredu.or.upredg) then
                idiagy1 = idiagy * lint(idiagx.eq.7)
                call steer2(ioup,iovp,ufile,vfile,spval,idiagy1)
              if (diag) write (6,160) 
     *         ' Steering velocities from (u,v) calculated.'
     *,        ddtime()
              endif

c             Interpolation of supplementary variables
c             ----------------------------------------
  
              if (itabc5.ne.0) then
                idiagy1 = idiagy * lint(idiagx.eq.8)
                call supvar(iosv,spval,idiagy1)
                if (diag) write (6,'(a)') 
     *           ' Supplementary variables calculated.'
              endif

              if (.not.(cycwrit.or.flwrit))
     *         stop ' End of diagnostic finding run.'

c             Writing to cyclone position file
c             --------------------------------
crmw
crmw
crmw
c               An input-output subroutine writes details of the high/low
c             centres and information relating to data.

              if (cycwrit) then
                if (ientry.eq.1) call cychdwr(4,svname)
crmw

c
c * For regional model grids where -ve longitudes are present KK 21/2/2007 
c   Note: This requires namelist nmlcycloc parameter offlon e.g. offlon=90.,
c
                if(offlon.gt.0.)then
                  do k=1,nklt
                    xc(k)= xc(k) -offlon  ! Lon of cyclone pressure min.
                    xv(k)= xv(k) -offlon  ! Lon of cyclone vorticity max.
                  enddo
                endif
                call cycwr(iocy,nklt,iopc,xc,yc,fc,cc,dpc,rdc,
     *           zsc,upc,vpc,itabc5,sc)
crmw
                if (ier.gt.0) stop ' Error in cycwr.'

                if (diag) write (6,160) ' Cyclone record written'
     *,          ddtime()
                write (6,'(i3,'': '',a75)') ientry,chead(1:75)
              endif
            endif
          endif

          dalast = da
          hrlast = hr

          eetimey = eetime()
          if (diag) write (6,60) 
     *     ' End of processing for analysis time.'
     *,    eetimey-eetimex,eetimey

  370     continue
          if ((dastop.eq.0).or.(da.gt.dastop).or.((da.eq.dastop)
     *     .and.(hr.ge.hrstop))) go to 410
  380   continue
  390   continue
        close (ioda)
  400 continue
  410 continue
      if (fpwrit)  close (iops)
      if (cycwrit) close (iocy)

      stop

c-------------------------------------------------------------------------------
c     Error conditions
c-------------------------------------------------------------------------------

  910 write (6,*) ' Cannot find input file: ',
     * infile(1:lnblnk(infile))
      usage = .true.
      go to 30
  940 stop ' Error in input file.'
  950 stop ' End of input file.'

      end
