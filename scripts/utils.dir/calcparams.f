      program calcparams
c
c * Purpose: Interpolates entries in Eun-Pa Lim's PhD thesis
c   Table 31. p 38. The alpha value for 1000 hPa is 0,09 as per
c   her results - see below. Also the fsteer value for 700 hPa
c   is 1.5 as per results - see below.
c
c * Note: For pressure levels p <= 1000 hPa.
c
c * Modifications:
c   (1) 24/8/2009: Added -n nmlfile option; see: calparams.nml for
c       equivalent setup to data statements i.e. defaults
c
c * Author: Kevin Keay  Date: 18/8/2009
c
      parameter (xmiss=99999.9) 
      character*80 optarg
      character*80 nmlfile
      character*200 ch200
      logical lexist
      parameter (maxlev=20)
      real p(maxlev)
      real a(maxlev)
      real z(maxlev)
      real rd(maxlev)
      real fs(maxlev)
      real r(maxlev)
      real c1(maxlev)
      real c2(maxlev)
      real cw(maxlev)

c * nlev: number of levels of parameters
      data nlev /10/
c * p: pressure level in hPa
      data p /1000., 925., 850., 700., 600., 500., 400., 300.,
     *      200., 100., 10*0./
c * a: alpha - Eunpa''s NCEP2 1000 hPa results use alpha=0.09
c   but the formula (equation 3.2, p37 PhD thesis) would
c   give alpha= 0.157; I am using 0.09 @ 1000 hPa
      data a /0.09, 0.147, 0.126, 0.078, 0.042, 0., 0., 0.,
     *      0., 0., 10*0./
c * zsmax= z + 1000. zscr= z +200 where z is mean z
c   See: http://hurri.kean.edu/~yoh/calculations/standatm/StdAtm.html
c   - Pressure-Height Relationship in Standard Atmosphere
c   Formula is:
c
c   z = 44330.77 * (1 - (p/p0) ** (0.19025) )
c
c   where: p0 = 1013.25 hPa 
c
c   Strictly this applues to within the troposphere (up to 11 km)
c   so the value at 100 hPa is approximate
c
      data z /110.5, 768.75, 1457., 3012., 4206., 5574.,
     * 7185., 9163., 11774., 15796., 10*0./ 
c * r: rdpgrd
      data rd /5., 5., 5.5, 5.5, 6., 6., 6., 6., 6., 6., 10*0./
c * c1: cmnc1; c2: cmnc2
      data c1 /1.66, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7,
     * 1.7, 1.7, 10*0./
      data c2 /1.66, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7,
     * 1.7, 1.7, 10*0./
c * cw: cmncw
      data cw /5.60, 9 * 5.95, 10*0./
c * r: rhoa - last two values are an extrapolation
      data r /1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.58, 0.46,
     * 0.34, 0.22, 10*0./
c * fs: fsteer
c   Eun-pa''s table has 1.7 @ 700 hPa but her template
c   has 1.5 - I am using the latter
      data fs /2., 2., 1.9, 1.5, 1.2, 1., 1., 1., 1., 1., 10*0./
c
c * Namelist parameters
c
      character*10 nmlist
c
      namelist /nmcalcp/ nlev,p,a,z,rd,c1,c2,cw,r,fs
c
      if(iargc().eq.0)then
        write(*,*)'Usage: calcparams [-n nmlfile] plevel'
        write(*,*)'  plevel: Pressure level in hPa [100,1000]'
	write(*,*)'  nmlfile: Namelist file (namelist: nmcalcp)'
	write(*,*)'  See: calcparams.nml for defaults'
	write(*,*)'Examples:'
	write(*,*)'  (1) calcparams 750.'
	write(*,*)'  (2) calcparams -n params.nml 500.'
	write(*,*)' '
        stop
      else
        inml= 0
        i= 1
        do while (i.le.iargc()-1)
	  call getarg (i,optarg)
	  if(optarg.eq.'-n')then
	    call getarg (i+1,optarg)
	    nmlfile= optarg
	    inquire (file=nmlfile,exist=lexist)
	    if(.not.lexist)then
	      write(*,*)'ERROR: Namelist file: ',
     *	      nmlfile(:lnblnk(nmlfile)),' not found'
              stop 'ABORT: Due to error in calcparams'
	    else
	      inml= 1
	    endif
	  endif
	  i= i +1
	enddo
        call getarg (i,optarg)
        read(optarg,*)p0
        if(p0.gt.1000.or.p0.lt.100)then
          write(*,*)'ERROR: plevel must be in the range [100,1000]'
          stop 'ABORT due to error in calcparams'
        endif
      endif
c
c * Read namelist file
c
      if(inml.eq.1)then
	open (21,file=nmlfile)
	nmlist= 'nmcalcp'
	write(*,*)'Reading namelist: ',nmlist
	read(21,nmcalcp,end=1009,err=1008)
1009    continue
	close (21)
      endif
c
      do i=1,nlev
        if(p(i).eq.p0)then
          if (i.le.nlev)then  ! p > 500 hPa ; was i <= 6
            z1= z(i)+1000.
            z2= z(i)+200.
          else
            z1= xmiss
            z2= xmiss
          endif
          write(ch200,200)p(i),a(i),z1,z2,
     * rd(i),r(i),cw(i),c1(i),c2(i),fs(i)
c         write(ch200,*)'p: ',p(i),' :alpha: ',a(i),
c     *    ' :zsmax: ',z(i)+1000.,
c     *    ' :zscr: ',z(i)+200., ' :rdpgrd: ',r(i),
c     *    ' :fsteer: ',f(i)
          write(*,'(A)')ch200(:lnblnk(ch200))
          goto 1000
        endif
      enddo
200   format('p: ',F6.0,' alpha: ',F5.3,' :zsmax: ',F7.1,
     * ' :zscr: ',F7.1,' :rdpgrd: ',F3.1,
     * ' rhoa: ',F4.2,' :cmncw: ',F4.2,' :cmnc1: ',F4.2,
     * ' :cmnc2: ',F4.2,
     * ' :fsteer: ',F3.1)
c
      do i=2,nlev
        if(p0.lt.p(i-1).and.p0.gt.p(i))then
          p1= p(i-1)
          p2= p(i)
          call linterp (p1,p2,a(i-1),a(i),p0,a0)
          if (i.le.nlev) then ! p >= 500 hPa; was i <= 6
            call linterp (p1,p2,z(i-1),z(i),p0,z0)
            z1= z0+1000.
            z2= z0+200.
          else
            z1= xmiss
            z2= xmiss
          endif
          call linterp (p1,p2,rd(i-1),rd(i),p0,rd0)

          call linterp (p1,p2,r(i-1),r(i),p0,r0)
          call linterp (p1,p2,cw(i-1),cw(i),p0,cw0)
          call linterp (p1,p2,c1(i-1),c1(i),p0,c10)
          call linterp (p1,p2,c2(i-1),c2(i),p0,c20)

          call linterp (p1,p2,fs(i-1),fs(i),p0,fs0)

         write(ch200,200)p0,a0,z1,z2,
     * rd0,r0,cw0,c10,c20,fs0
c         write(ch200,*)'p: ',p0,' :alpha: ',a0,
c     *    ' :zsmax: ',z0+1000,
c     *    ' :zscr: ',z0+200, ' :rdpgrd: ',r0,
c     *    ' :fsteer: ',f0
          write(*,'(A)')ch200(:lnblnk(ch200))
          goto 1000
        endif
      enddo
c
c      if(p0.lt.p(maxlev))then
c         a0= 0.
c         z0= xmiss
c         write(ch200,*)'p: ',p0,' :alpha: ',a(maxlev),
c     *    ' :zsmax: ',z0,
c     *    ' :zscr: ',z0, ' :rdpgrd: ',r(maxlev),
c     *    ' :fsteer: ',f(maxlev)
c          write(*,'(A)')ch200(:lnblnk(ch200))
c        goto 1000
c      endif

c
1000  continue
      stop
c
c * Error messages for namelist file
c
1008  write(*,*)'ERROR: Problem reading file: ',
     * nmlfile(:lnblnk(nmlfile)),
     * ' for namelist: ',nmlist
      write(*,*)'       Possibly due to an invalid namelist variable'
      stop
      end

      subroutine linterp (p1,p2,f1,f2,p0,f0)
      b= (f2-f1)/(p2-p1)
      f0= f1 + b*(p0-p1)
      return
      end
