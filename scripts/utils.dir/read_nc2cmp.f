      program read_nc2cmp
c 
c * Purpose: To provide decoding of NetCDF data to 'conmap' format
c   mainly for subsequent processing by the cyclone tracking software.
c   Some ideas for the program were found at:
c
c     http://www.unidata.ucar.edu/software/netcdf/Contrib.html
c
c   The program is largely based on an example NOAA program:
c
c     http://www.cdc.noaa.gov/PublicData/readgeneral.f
c
c   plus some (suggested) guidance from:
c
c     ftp://ftp.unidata.ucar.edu/pub/netcdf/contrib/gennet.f
c
c * Notes
c
c  (1) 12/2/2007: Changed logic for adjusting dimension order.
c      Need to base IDs on *variable* index rather than *dimension* index.
c      The latter was okay if the variable list == dimension list.
c      However there is an error when the lists don''t match.
c      Hence base the IDs on the variable index i.e. index i in: 
c        call ncvinq (ncid,i, ...)
c  (2) 12/2/2007: Added a help option: --help; name as version 2.0
c  (3) 15/5/2007: Name as version 2.1; increased mlon to 601 and mlat to 480
c      This is done in two places i.e. parameter (mlon=...
c  (4) 7/6/2007: Increased mlon to 721
c  (5) 8/2/2008: Added -T option to handle CDO time definition:
c      -T "ttype" where presently only ttype = 'CDO' is allowed
c  (6) 4/6/2008: Added ttype = 'fcst' to handle time adjustment for NCEP/NCEP2
c      surface variables such as 10m winds - see parameter xfcst
c  (7) 12/4/2009: Added -t option; specify date-time pair
c  (8) 13/4/2009: Expanded functionality to allow for >1 user variable and >1 level
c      i.e. may output several varaibles and levels
c  (9) 14/4/2009: Added -S option; a separate output file is created for each 
c      time (contains specified variables and levels)
c      Added -G option to duplicate data at lon 0 to 360
c
c * Author: Kevin Keay  Date: 14/8/2006
c

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	This is a sample program that will read netCDF data files
C	from the NOAA-CIRES Climate Diagnostics Center.
C
C	In order to run the code you must have the netCDF and UDUNITS
C	libraries installed from UniData.
C
C		netCDF can be found at
C		http://www.unidata.ucar.edu/packages/netcdf/index.html
C
C		UDUNITS can be found at
C		http://www.unidata.ucar.edu/packages/udunits/index.html
C
C	Don''t forget to add them -lnetcdf and -ludunits options to the compile
C	or load command line.
C
C	There are four things that you must change to use this code
C	at your site.  Each place where you must make a change is
C	indicated with a comment of the form like the line below.
C
C STEP 1.
C	Change the location of the netcdf.inc and udunits.inc include files
C	to match your installation. (Occurs in 4 places.)
C
C STEP 2.
C	Change the location of the udunits.dat file to match your system
C	installation.
C
C STEP 3.
C	Change PARAMETER values for ilon and jlat to match your file.
C	You can find out the sizes of the lon and lat dimensions by looking
C	at the output of ncdump -h <your_file>.
C
C STEP 4.
C   Change the name of the input file to match the data file you want to read.
C   This is in the ncopn subroutine call.  Also, change the variable name in
C   the ncvid subroutine call right beneath the ncopn subroutine call.
C
C STEP 5.
C   Choose among the three different ways to read data and remove the comments
C	to get the code you need to do your job.
C
C NOTES:
C	Data is read in one 2D grid at a time in the same lat/lon order
C	it was stored.
C   The gridread subroutine returns the date of the grid read.
C
C     Code written 10/9/96 by Cathy Smith
C
C     NOAA-CIRES Climate Diagnostics Center, Boulder, CO.
C     based on code writen by Tom Baltzer, Roland Schweitzer and
C     Cathy Smith
C     For help, contact cdcdata@cdc.noaa.gov
C     :)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    MAIN CODE

c
c This is the latitude, longitude dimension of the grid to be read.
c This corresponds to the lat and lon dimension variables in the netCDF file.
c
C STEP 3.
cc      parameter (ilon =144)
cc      parameter (jlat=73)

c
c The data are packed into short integers (INTEGER*2).  The array work
c will be used to hold the packed integers.  The array 'x' will contain
c the unpacked data.
c
C    DATA ARRAY AND WORK ARRAY
c
c * Changed to COMMON KK 18/5/2006
c
c * Note: mlon=721,nlat=361 corresponds to a global 0.5 x 0.5 grid
c
      parameter (mlon=721)
      parameter (mlat=361)
      real x(mlon,mlat)
      integer*2 work(mlon,mlat)
      real*8 work2(mlon,mlat)
      common /blk1/x,work,work2
c
c * Extra real work array for flipping x for conmap form
c   or duplicating lon 360 == lon 0
c
      real rwork(mlon,mlat)
c
c * NetCDF include file
c
      include 'netcdf.inc'
c
c * Extra variables for handling dimension arrays (latitudes, longitudes)
c
      integer ncid
      integer rcode
      integer ndims, nvars, ngatts, recdim
      character*20 dim_name(maxncdim)
      integer size(maxncdim)
      integer rhtype,rhn,rhdims(maxncdim),rhnatt
      character*20 rhname
      integer one_index(1), one_length(1)
c
      integer nlats
      integer rhtype_lat ! Type for lat var
      real lats(mlat)
      real*8 dlats(mlat) ! For double precision " "
      integer nlons
      integer rhtype_lon ! Type for lon var
      real lons(mlon)
      real*8 dlons(mlon) ! For double precision " "
c
ckk      parameter (maxtimes=1464) ! Max. no. of times
      parameter (maxtimes=14000) ! Max. no. of times
      integer rhtype_time ! Type for time var (4= integer 6= real*8)
      integer itimes(maxtimes)  ! For integer time variable
      real rtimes(maxtimes)  ! For real time variable
      real*8  times(maxtimes)   ! For double precision " "
c
      parameter (maxlev=50) ! Max. no. of levels
      integer rhtype_lev ! Type for lev var (4= integer 5= real)
      integer ilevels(maxlev)  ! For integer levels
      real levels(maxlev)     ! For real levels
      real*8 dlevels(maxlev)     ! For double precision " "
c
      integer levind(maxlev)  ! List of level index numbers e.g. 1,2,4,8,10
c
      parameter (maxvar=10)  ! Max. no. of variables to process
      character*10 uuvar(maxvar) ! List of user variables
      character*8 vvunits(maxvar)  ! Units of user variables
      character*8 vvtype(maxvar)  ! User names for user variables
c
      character*80 head
      character*80 ncfile,cmpfile,optarg
      character*1 yn
      character vtype*8, rtype*5, vunits*8, uvar*10, lvar*10
      character*10 gtype  ! e.g. '2.5x2.5DEG'
      character cvtype*8,vtype0*8
      character*20 dvar(3)
      character*35 fmt
      integer int_att
      real real_att
      real*8 dble_att
      logical lexist
      character*80 udpath ! Path to udunits.dat (see: -p)
      real*8 ddate(2)
      real*8 cddate
      logical ldate
      character*20 fpx ! File prefix for separate time files
c
c * Assume max. of 10 variables each with a max. of 40 attributes
c   KK 17/5/2006
      character*31 attnam (10,40) ! Attribute name
      integer attlen (10,40) ! Attribute length
      integer attype (10,40) ! Attribute type e.g. 4= integer
c
c * Type of variable e.g real
c
      CHARACTER*11 AVARTYP(6)
      DATA AVARTYP /'LOGICAL*1','CHARACTER*1','INTEGER*2',
     +'INTEGER*4','REAL*4','REAL*8'/
c
c * For -T option
c
      character*4 ttype
      real*8 xtime
      real*8 xfcst
      parameter(xfcst= 6.)  ! Add 6hr to date-time specification for
                            ! this time step (-T option ttype = 'fcst')
c
c * This step added by Kevin Keay 15/5/2006
c   Process command-line arguments
c
      if(iargc().eq.0)then
        write(*,*)' '
        write(*,*)'read_nc2cmp: Version 2.4 (Apr 14 2009)'
	write(*,*)' '
        write(*,*)'Usage: read_nc2cmp [--help][-D idbg]',
     * '[-i ncfile][-o cmpfile][-d "lon,lat,time"]',
     * '[-u uservars][-U vunits][-v vtypes]',
     * '[-l levelvars][-L ilev]',
     * '[-g gridtype][-r rtype][-s uscal][-p udunits]',
     * '[-m no_maps][-M "map1,map2"][-t "date1,date2"]',
     * '[-T "ttype"][-G][-S]'
        write(*,*)' '
        write(*,*)'Options: '
        write(*,*)'D: 0= None 1= Basic 2= Verbose',
     *	' 3= Print dimension arrays to file fort.10'
        write(*,*)'G: Duplicate data at lon to 360 (default: no)'
        write(*,*)'S: Output one file per time (default: all in one)'
	write(*,*)'T: ttype: CDO, fcst'
	write(*,*)'   fcst: Adds 6 hr to date-time; use with',
     * 	' NCEP/NCEP2 10m winds'
        write(*,*)'Note: Max. sizes (characters) of text variables'
        write(*,*)'gridtype: 10 rtype: 5 vtypes: 8 units: 8' 
	write(*,*)' '
	write(*,*)'--help: Gives some examples '
        write(*,*)' '
        stop
      else
        do k=1,maxvar
	  uuvar(k)= ''
	  vvunits(k)= ''
	  vvtype(k)= ''
	enddo
	rtype= 'NCEP2'
	vtype= 'Z500'
	gtype= '2.5x2.5DEG'
	uvar= 'hgt'
	vunits= 'M'
        dvar(1)= 'lon'
        dvar(2)= 'lat'
        dvar(3)= 'time'
	idbg= 0
	ilev= 0
	nlev= 0
	lvar= 'levelist'
	iscal= 0
	uscal= 1.0
        udpath= 'udunits.dat'
        nmaps= 0
	imapset= 0
	ttype= ''
	idt= 0
	ddate(1)= 0000010100.
	ddate(2)= 9999123118.
	idrange= 0
	fpx= ''
	isepf= 0
	nuvar= 1
	nunvar= 0
	nutvar= 0
	iadd360= 0
	cmpfile=''
	narg= iargc()
	i= 0
	do while (i.le.narg)
	  call getarg(i,optarg)
	  if(optarg.eq.'--help')then
	    call help
	    stop
	  elseif(optarg.eq.'-d')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)(dvar(j),j=1,3) ! e.g. "lon,lat,time"
            write(*,*)'-d: dimensions are: '
            write(*,*)' - lon  variable: ',dvar(1)(1:ilen(dvar(1)))
            write(*,*)' - lat  variable: ',dvar(2)(1:ilen(dvar(2)))
            write(*,*)' - time variable: ',dvar(3)(1:ilen(dvar(3)))
	  elseif(optarg.eq.'-i')then
	    i= i +1
	    call getarg(i,ncfile)
	    inquire (file=ncfile,exist=lexist)
	    if(.not.lexist)then
	      write(*,*)'ERROR: NetCDF file: ',ncfile(1:ilen(ncfile)),
     * ' not found'
	      stop
	    endif
	  elseif(optarg.eq.'-g')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,'(A)')gtype
	  elseif(optarg.eq.'-l')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)lvar   ! e.g. levelist
            write(*,*)' - level variable: ',lvar(:ilen(lvar))
	  elseif(optarg.eq.'-m')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)nmaps
	  elseif(optarg.eq.'-p')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,'(A)')udpath
            write(*,*)'NOTE: Path to udunits.dat:',
     * udpath(1:ilen(udpath))
	  elseif(optarg.eq.'-r')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)rtype
	  elseif(optarg.eq.'-s')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)uscal
	    write(*,*)'NOTE: User scaler: ',uscal
	    iscal= 1
	  elseif(optarg.eq.'-o')then
	    i= i +1
	    call getarg(i,cmpfile)
	  elseif(optarg.eq.'-u')then
            i= i +1
            call getarg(i,optarg)
            do k=1,maxvar
              uuvar(k)= ''
            enddo
	    nuvar= maxvar
            read(optarg,*,end=197)uuvar
197         continue
            do k=1,maxvar
              if(uuvar(k).eq.'')then
	        nuvar= k -1
	        goto 203
	      endif
            enddo
203         continue
	    write(*,*)'No. of user variables: ',nuvar
	    write(*,*)'User variables: ',(uuvar(k),k=1,nuvar)
c
	  elseif(optarg.eq.'-t')then
	    i= i +1
	    call getarg(i,optarg)
            do k=1,2
              ddate(k)= 0.
            enddo
	    ndate= 2
            read(optarg,*,end=198)ddate
198         continue
            do k=1,2
              if(ddate(k).eq.0)then
	        ndate= k -1
	        goto 202
	      endif
            enddo
202         continue
	    idrange= 1
	    write(*,*)'No. of date arguments: ',ndate
	    write(*,'(''Date range: '',2F12.0)')(ddate(k),k=1,ndate)
	  elseif(optarg.eq.'-v')then
            i= i +1
            call getarg(i,optarg)
            do k=1,maxvar
              vvtype(k)= ''
            enddo
	    nutvar= maxvar
            read(optarg,*,end=195)vvtype
195         continue
            do k=1,maxvar
              if(vvtype(k).eq.'')then
	        nutvar= k -1
	        goto 205
	      endif
            enddo
205         continue
	    write(*,*)'No. of user variables: ',nutvar
	    write(*,*)'User names of user variables: ',
     *	    (vvtype(k),k=1,nutvar)
c
	  elseif(optarg.eq.'-D')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)idbg
	  elseif(optarg.eq.'-G')then
	    iadd360= 1 
	  elseif(optarg.eq.'-L')then
c
            i= i +1
            call getarg(i,optarg)
            do k=1,maxlev
              levind(k)= 0
            enddo
	    nlev= maxlev
            read(optarg,*,end=199)levind
199         continue
            do k=1,maxlev
              if(levind(k).eq.0)then
	        nlev= k -1
	        goto 201
	      endif
            enddo
201         continue
	    write(*,*)'No. of levels: ',nlev
	    write(*,*)'Level indices: ',(levind(k),k=1,nlev)
c
	  elseif(optarg.eq.'-M')then
	    imapset= 1
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)imap1,imap2   ! Map range to output
	    write(*,*)'Output map range: ',imap1,' - ',imap2
	  elseif(optarg.eq.'-P')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)fpx   ! Map range to output
	    write(*,*)'Output file prefix: ',fpx(:ilen(fpx))
	  elseif(optarg.eq.'-S')then
	    isepf= 1  ! Separate file output
	  elseif(optarg.eq.'-T')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)ttype
            if(ttype.eq.'CDO')then
              idt= 1
	      write(*,*)'-T option: ttype= ',ttype,' idt=',idt
            elseif(ttype.eq.'fcst')then
              idt= 2
	      write(*,*)'-T option: ttype= ',ttype,' idt=',idt
	    else
              write(*,*)'ERROR: ttype is invalid'
              write(*,*)'Choose: CDO only at this stage'
              stop
            endif
	  elseif(optarg.eq.'-U')then
            i= i +1
            call getarg(i,optarg)
            do k=1,maxvar
              vvunits(k)= ''
            enddo
	    nunvar= maxvar
            read(optarg,*,end=196)vvunits
196         continue
            do k=1,maxvar
              if(vvunits(k).eq.'')then
	        nunvar= k -1
	        goto 204
	      endif
            enddo
204         continue
	    write(*,*)'No. of user variables: ',nunvar
	    write(*,*)'Units of user variables: ',(vvunits(k),k=1,nunvar)
c
	  endif
	  i= i +1
        enddo
      endif
 
c--------------------------------------------------------------------
c
c * Some checks on input parameters
c
      ierr= 0
c
      if(nunvar.eq.0)nunvar= nuvar
      if(nutvar.eq.0)nutvar= nuvar
c
      if(isepf.eq.0.and.cmpfile.eq.'')then
        write(*,*)'ERROR: Specify -o option'
	ierr= ierr +1
      endif
      if(isepf.eq.1.and.cmpfile.ne.'')then
        write(*,*)'ERROR: Omit -o option with -S'
	ierr= ierr +1
      endif
c
      if(nmaps.gt.0.and.imapset.eq.1)then
        write(*,*)'ERROR: Specify -m or -M, not both'
	ierr= ierr +1
      endif
c
      if(idrange.eq.1.and.(nmaps.gt.0.or.imapset.eq.1))then
        write(*,*)'ERROR: Specify one of -t, -m or -M'
	ierr= ierr +1
      endif
c
      if(nuvar.eq.0)then
        write(*,*)'ERROR: Specify at least one variable on -u option'
	ierr= ierr +1
      endif
c
      if(nuvar.ne.nunvar.or.nuvar.ne.nutvar)then
        write(*,*)'ERROR: Specify same number of items',
     *	' on -u, -v and -U options (nuvar,nutvar,nunvar)'
        write(*,*)'nuvar,nutvar,nunvar: ',nuvar,nutvar,nunvar
	ierr= ierr +1
      endif
c
c * Stop here if ierr >0 
c
      if(ierr.gt.0)then
        write(*,*)'ABORT: Due to error(s)'
        stop
      endif

c--------------------------------------------------------------------
c
c Initialize the UDUNITS package.  You may need to change the location
c of the udunits.dat file to match your system.
c
C STEP 2.

ckk       retcode=utopen('/usr/local/etc/udunits.dat')
       retcode=utopen(udpath(1:ilen(udpath)))  !KK For Courtenay
c
c * This 'step' introduced by Kevin Keay 12/5/2006
c   The NetCDF file will be opened here
c
      ncid = ncopn(ncfile,ncnowrit,rcode)

      if (rcode.eq.0) then   
        write(6,*)'NetCDF file opened successfully (ncid=', ncid, ')'
      else
        write(6,*)'ERROR in opening NetCDF file: ',
     * ncfile(1:ilen(ncfile))
        stop
      endif
      
      write(6,*) 
      
c--------------------------------------------------------------------
c * Inquire about the file
      
      call ncinq(ncid,ndims,nvars,ngatts,recdim,rcode)

      if(idbg.eq.1)then
	write(6,*)'ndims= ',ndims
	write(6,*)'nvars= ',nvars
	write(6,*)'ngatts= ',ngatts
      endif
            
      if (rcode.ne.0) then
         write(6,*)'ERROR in inquiring about NetCDF file'
         stop
      endif 

c--------------------------------------------------------------------
c * Check user variables
c
      do kv=1,nuvar
c
        uvar= uuvar(kv)

c--------------------------------------------------------------------
c * Try inquiring about variables - update initial ids for dimensions
c   in case they differ from listing order (see above call to ncdinq)

        write(*,*)'Inquiring about variables ...'
        i_uvar= 0
	do i=1,nvars  !this corresponds to the variable i.d. (0,1,2.. in C)
	   call ncvinq(ncid,i,rhname,rhtype,rhn,rhdims,rhnatt,rcode)
	   if(idbg.eq.1)write(6,*)'For variable ',i,' name= ',rhname,
     *	' type= ',avartyp(rhtype),' (',rhtype,') no of dims ',rhn,
     * '  rhdims= ',(rhdims(j),j=1,rhn),' rhnatt= ',rhnatt
          if (rhname.eq.dvar(1)) then
            rhtype_lon= rhtype  ! Type for lon var 
	    k_lon= i            ! ID for lon var
          endif
          if (rhname.eq.dvar(2)) then
            rhtype_lat= rhtype  ! Type for lat var 
	    k_lat= i            ! ID for lat var
          endif
          if (rhname.eq.dvar(3)) then
            rhtype_time= rhtype ! Type for time var 
	    k_time= i           ! ID for time var
          endif
          if (rhname.eq.lvar) then
            rhtype_lev= rhtype  ! Type for level var 
	    k_lev= i            ! ID for level var
          endif
          if (rhname.eq.uvar)then
            rhtype_uvar= rhtype ! Type for user variable (not used in
                                ! this version)
            i_uvar= i           ! ID for user variable
            natt_uvar= rhnatt   ! No. of attributes for user variable
          endif
	enddo
c        
        if(i_uvar.eq.0) then
          write(*,*)'ERROR: User var (',uvar(:ilen(uvar)),
     *	  ' (variable ',kv,') not found'
          stop
        endif
c
      enddo ! End of kv loop

c--------------------------------------------------------------------
c * Inquire about the dimensions
      
      nlons= 0
      nlats= 0
      ntimes= 0
      do i=1,ndims
         call ncdinq(ncid,i,dim_name(i),size(i),rcode)
	 if(idbg.eq.1)write(6,*)'i=',i,' dim_name= ',dim_name(i),
     * ' size=',size(i)
         if(dim_name(i).eq.dvar(1))then ! Lon variable
           nlons= size(i)
           i_lon= i
         endif
         if(dim_name(i).eq.dvar(2))then ! Lat variable
           nlats= size(i)
           i_lat= i
         endif
         if(dim_name(i).eq.dvar(3))then ! Time variable
           ntimes= size(i)
           i_time= i
         endif
         if(nlev.gt.0) then  ! There is a level variable - was ilev KK 10/4/2009
           if(dim_name(i).eq.lvar)then
             nlevs= size(i)
             i_lev = i
           endif
         endif
         if (rcode.ne.0) then
            write(6,*)'ERROR in inquiring about dimensions'
            stop
         endif
      enddo
      ierr= 0
      if(nlons.eq.0)then
        write(*,*)'ERROR: Cannot find lon variable called ',
     * dvar(1)(:ilen(dvar(1)))
      endif
      if(nlats.eq.0)then
        write(*,*)'ERROR: Cannot find lat variable called ',
     * dvar(2)(:ilen(dvar(2)))
      endif
      if(ntimes.eq.0)then
        write(*,*)'ERROR: Cannot find time variable called ',
     * dvar(3)(:ilen(dvar(3)))
      endif
      if(ierr.gt.0)stop
c
      if(idbg.gt.0)then
        write(*,*)'nlons,nlats,ntimes,nlevs,ilev: ',
     * nlons,nlats,ntimes,nlevs,ilev
        write(*,*)'i_lon,i_lat,i_lev,i_time:'
        write(*,*)i_lon,i_lat,i_lev,i_time
      endif
c
c--------------------------------------------------------------------
c * Ensure correct order of dimensions 

        i_lon= k_lon
        i_lat= k_lat
        i_lev= k_lev
        i_time= k_time
c
        if(idbg.gt.0)then
	  write(*,*)'*** Adjusted order of dimensions'
          write(*,*)'i_lon,i_lat,i_lev,i_time:'
          write(*,*)i_lon,i_lat,i_lev,i_time
	endif

c--------------------------------------------------------------------
c * Read longitudes - usually real values but might be double precision
C
      write(*,*)'Reading longitudes ...'
      one_index(1) = 1
      one_length(1) = nlons
C      
      if(rhtype_lon.eq.5)then   ! Real lons
        call ncvgt(ncid,i_lon,one_index,one_length,lons,rcode)
      elseif(rhtype_lon.eq.6)then   ! Double precision lons
        call ncvgt(ncid,i_lon,one_index,one_length,dlons,rcode)
      else
        write(*,*)'Error: Need to introduce a suitable array',
     * ' of type ',avartyp(rhtype_lon),' for lon variable'
        stop
      endif
C      
      if (rcode.eq.0) then
ckk         write(6,*) "show longitudes?"
ckk         read(5,*) yn
         if(yn.eq.'y'.or.yn.eq.'Y'.or.idbg.eq.3) then
            write(10,*)
            write(10,*) "----- Longitudes -----"
            do i=1,nlons
               if(rhtype_lon.eq.5)then
                 write(10,600) i, lons(i)
               elseif(rhtype_lon.eq.6)then
                 write(10,605) i, dlons(i)
	       endif
600   format("lon(",i3,") = ",f6.1)
605   format("lon(",i3,") = ",f12.6)
            enddo
         endif
c * Ensure that real array lons is set
	 if(rhtype_lon.eq.6)then
	   write(*,*)'Converting double lons to real ...'
           do i=1,nlons
	     lons(i)= real(dlons(i))
	   enddo
	 endif
      else
         write(6,*) "Error in reading longitudes: stopped"
         stop
      endif
c
c * Check for -G option
c
      if(iadd360.eq.1)then
        dlon= lons(2) -lons(1)
 	xlon360= lons(nlons) +dlon
 	if(lons(1).eq.0.and.xlon360.eq.360)then
	  write(*,*)'NOTE: Grid consistent with -G option'
	else
	  write(*,*)'ERROR: First or last longitude is not'
	  write(*,*)'consistent with -G option'
	  write(*,*)'lons (1)=',lons(1),' lons (',nlons,')=',lons(nlons)
	  write(*,*)'ABORT: Due to error'
	  stop
        endif
      endif

c--------------------------------------------------------------------
c * Read latitudes - usually real values but might be double precision
C
      write(*,*)'Reading latitudes ...'
      one_index(1) = 1
      one_length(1) = nlats
C      
      if(rhtype_lat.eq.5)then   ! Real lats
        call ncvgt(ncid,i_lat,one_index,one_length,lats,rcode)
      elseif(rhtype_lat.eq.6)then   ! Double precision lats
        call ncvgt(ncid,i_lat,one_index,one_length,dlats,rcode)
      else
        write(*,*)'Error: Need to introduce a suitable array',
     * ' of type ',avartyp(rhtype_lat),' for lat variable'
        stop
      endif
C      
      if (rcode.eq.0) then
ckk         write(6,*) "show latitudes?"
ckk         read(5,*) yn
         if(yn.eq.'y'.or.yn.eq.'Y'.or.idbg.eq.3) then
            write(10,*)
            write(10,*) "----- Latitudes -----"
            do i=1,nlats
               if(rhtype_lat.eq.5)then
                 write(10,510) i, lats(i)
               elseif(rhtype_lat.eq.6)then
                 write(10,515) i, dlats(i)
	       endif
510   format("lat(",i2,") = ",f6.1)
515   format("lat(",i2,") = ",f12.6)
            enddo
         endif
c * Ensure that real array lats is set
	 if(rhtype_lat.eq.6)then
	   write(*,*)'Converting double lats to real ...'
           do i=1,nlats
	     lats(i)= real(dlats(i))
	   enddo
	 endif
      else
         write(6,*) "Error in reading latitudes: stopped"
         stop
      endif

c--------------------------------------------------------------------
c * Read levels - could be integer, real or double precision
C
      if(nlev.gt.0)then  ! There is a level variable - was ilev KK 10/4/2009
      write(*,*)'Reading levels ...'
      one_index(1) = 1
      one_length(1) = nlevs
C      
      write(*,*)rhtype_lev
      if(rhtype_lev.eq.4) then ! Integer levels
        call ncvgt(ncid,i_lev,one_index,one_length,ilevels,rcode)
      elseif(rhtype_lev.eq.5)then   ! Real levels
        call ncvgt(ncid,i_lev,one_index,one_length,levels,rcode)
      elseif(rhtype_lev.eq.6)then   ! Double precision levels
        call ncvgt(ncid,i_lev,one_index,one_length,dlevels,rcode)
      else
        write(*,*)'Error: Need to introduce a suitable array',
     * ' of type ',avartyp(rhtype_lev),' for level variable'
        stop
      endif
C      
      if (rcode.eq.0) then
         if(idbg.eq.3) then
	    write(10,*)
            write(10,*) "----- Levels -----"
            do i=1,nlevs
               if(rhtype_lev.eq.4)then
                 write(10,630) i, ilevels(i)
630   format("level(",i2,") = ",I8)
               elseif(rhtype_lev.eq.5)then
                 write(10,640) i, levels(i)
640   format("level(",i2,") = ",F12.4)
               elseif(rhtype_lev.eq.6)then
                 write(10,640) i, dlevels(i)
               endif
            enddo
         endif
      else
         write(6,*) "Error in reading levels: stopped"
         stop
      endif
c      
      endif

c--------------------------------------------------------------------
c * Read times - could be integer, real or double precision
C
      write(*,*)'Reading times ...'
      one_index(1) = 1
      one_length(1) = ntimes
C      
      if(rhtype_time.eq.4) then ! Integer times
        call ncvgt(ncid,i_time,one_index,one_length,itimes,rcode)
      elseif(rhtype_time.eq.5)then   ! Real times
        call ncvgt(ncid,i_time,one_index,one_length,rtimes,rcode)
      elseif(rhtype_time.eq.6)then   ! Double precision (real*8) times
        call ncvgt(ncid,i_time,one_index,one_length,times,rcode)
      else
        write(*,*)'Error: Need to introduce a suitable array',
     * ' of type ',avartyp(rhtype_time),' for time variable'
        stop
      endif
C      
      if (rcode.eq.0) then
         if(idbg.eq.3) then
            write(10,*)
            write(10,*) "----- Times -----"
            do i=1,ntimes
               if(rhtype_time.eq.4)then
                 write(10,610) i, itimes(i)
610   format("time(",i4,") = ",I8)
               elseif(rhtype_time.eq.5)then
                 write(10,620) i, rtimes(i)
               elseif(rhtype_time.eq.6)then
                 write(10,620) i, times(i)
620   format("time(",i4,") = ",F16.4)
               endif
            enddo
         endif
      else
         write(6,*) "Error in reading times: stopped"
         stop
      endif

c--------------------------------------------------------------------
C STEP 4.
c
c Below in the ncopn call is the file name of the netCDF file.
c You may want to add code to read in the file name and the variable name.
c
c The variable name is the netCDF variable name.  At CDC we name our files
c after the variable name.  If you follow that convention the variable name
c is the same as the first few letters of the file name (the letters up to
c but not including the first '.'.
C
C    OPEN FILE AND GET FILES ID AND VARIABLE ID(S)

c      inet=ncopn('hgt.58.0500.nc',0,icode)
c      ivar=ncvid(inet,'hgt',icode)
ckk      inet=ncopn(ncfile,0,icode)
ckk      ivar=ncvid(inet,uvar,icode)


C STEP 5.
C Pick one of the following scenarios to match your needs.
C
C Comment out this section and remove the comment from one of
C the sections below.

ckk        print*, 'Pick one of the three scenarios for your data.'
ckk        print*, 'See STEP 5 in the comments.'
ckk        stop 'Pick a scenario'
c
C Scenario 1.
C USE THIS LOOP TO READ THE FIRST 'NTIME' TIME STEPS FROM A FILE
C WITH ONLY ONE LEVEL.  IF THE FILE ONLY HAS ONE LEVEL SET ILEV TO 0.
C
ckk       ilev = 0  ! IF THE FILE ONLY HAS ONE LEVEL SET ILEV TO 0.


c ---------------------------------------------------------------------

c
c * Open concatenated conmap file (if -S option not specified)
c
        if(isepf.eq.0)then
          open (2,file=cmpfile,form='unformatted')
	endif
c
        idbg2= 1 ! gridread will return this as zero; used to print
                ! some info on first pass
c
c * Override number of maps with -m option
c   Also note effect of -M option
c
        if(nmaps.gt.0) then
          ntime =  nmaps
	  imap1= 1
	  imap2= ntime
        else
          ntime= ntimes
	  if(imapset.eq.0)then
	    imap1= 1
	    imap2= ntime
	  endif
        endif
ckk        if(imapset.eq.0)then
ckk          write(*,*)'No. of maps (times) to be extracted: ',ntime
ckk        else
ckk          write(*,*)'No. of maps (times) to be extracted: ',
ckk     *	  imap2-imap1+1
ckk        endif

c
c * Set up limits for level loop
c
       if(nlev.eq.0)then
         ilev1= 1
	 ilev2= 1
       else
         ilev1= 1
	 ilev2= nlev
       endif
 
c--------------------------------------------------------------------
c * Main processing stage
c
c   Process data by time, then user variable, then level, then lat and lon
c   e.g. float U(t, p, latitude, longitude) ;
c
       write(*,*)'Processing user variable(s) ...'
c
       kmap= 0  ! Map counter
c
       do it=1,ntime
c
c * End processing if it exceeds time or date range
c
         if(it.gt.imap2.or.cddate.gt.ddate(2))then
	   goto 5100
	 endif
c
         do kv=1,nuvar
c
           uvar= uuvar(kv)   ! User variable from -u option
	   vunits= vvunits(kv) ! Units of user variable from -U option
	   vtype= vvtype(kv) ! User names of user variable from -v option
           vtype0= vtype  ! Save vtype
c
c--------------------------------------------------------------------
c * Inquire about variable uvar 

        if(idbg.ge.2)write(*,*)'Inquiring about user variable: ',uvar
        i_uvar= 0
	do i=1,nvars  !this corresponds to the variable i.d. (0,1,2.. in C)
	   call ncvinq(ncid,i,rhname,rhtype,rhn,rhdims,rhnatt,rcode)
	   if(idbg.eq.1)write(6,*)'For variable ',i,' name= ',rhname,
     *	' type= ',avartyp(rhtype),' (',rhtype,') no of dims ',rhn,
     * '  rhdims= ',(rhdims(j),j=1,rhn),' rhnatt= ',rhnatt
          if (rhname.eq.uvar)then
            rhtype_uvar= rhtype ! Type for user variable (not used in
                                ! this version)
            i_uvar= i           ! ID for user variable
            natt_uvar= rhnatt   ! No. of attributes for user variable
          endif
	enddo

c--------------------------------------------------------------------
c * Get the scale_factor and add_offset attributes for 
c   the user variable uvar

      if(idbg.ge.2)write(*,*)'Reading attributes ...'
      j_scale_factor= 0
      j_add_offset= 0
      do j=1,natt_uvar
c * Get attribute names
        call ncanam(ncid,i_uvar,j,attnam(i,j),rcode)
        if(idbg.ge.2)write(*,*)'i,j,attnam:',i,j,attnam(i,j)
        if (attnam(i,j).eq.'scale_factor')then
          j_scale_factor= j
        endif
        if (attnam(i,j).eq.'add_offset')then
          j_add_offset= j
        endif
        if (attnam(i,j).eq.'missing_value')then
          j_miss= j
        endif
c ---
ckk          call ncainq(ncid,i_uvar,attnam(i,j),attype(i,j),attlen(i,j),
ckk     * rcode)
ckk          if(attype(i,j).ne.2) then
ckk            if(attype(i,j).eq.4)
ckk     *        call ncagt(ncid,i_uvar,attnam(i,j),int_att,rcode)
ckk            if(attype(i,j).eq.5)
ckk     *        call ncagt(ncid,i_uvar,attnam(i,j),real_att,rcode)
ckk            if(attype(i,j).eq.6)
ckk     *        call ncagt(ncid,i_uvar,attnam(i,j),dble_att,rcode)
ckk          typ_scale_factor= attype(i,j)
c ---
      enddo
      if(idbg.ge.2)then
        if(j_scale_factor.eq.0)write(*,*)'Note: No scale_factor',
     *	' attribute'
        if(j_add_offset.eq.0)write(*,*)'Note: No add_offset attribute'
        if(j_miss.eq.0)write(*,*)'Note: No missing_value attribute'
      endif
 
c--------------------------------------------------------------------
         do l=ilev1,ilev2
           if(nlev.gt.0)then
	     ilev= levind(l)
	   endif
	   if(idbg.ge.2)write(10,*)'ilev,it: ',ilev,it
c
c * User selected level - make appropriate label
c
        if(ilev.gt.0)then
          if(rhtype_lev.eq.4)then
	    levu= int(ilevels(ilev))
          elseif(rhtype_lev.eq.5)then
	    levu= levels(ilev)
          elseif(rhtype_lev.eq.6)then
	    levu= real(dlevels(ilev))
          endif
	  if(idbg.ge.2)then
	    write(10,*)'User selected level: Index ',
     * ilev,' gives level: ',levu,' )'
	  endif
	  if(uvar.eq.'Z'.or.uvar.eq.'z'.or.
     * uvar.eq.'HGT'.or.uvar.eq.'hgt')then	       ! Geopotential
	    if(levu.lt.10)then
	      fmt= '(''Z'',I1,6x)'
	    elseif(levu.ge.10.and.levu.lt.100)then
	      fmt= '(''Z'',I2,5x)'
	    elseif(levu.ge.100.and.levu.lt.1000)then
	      fmt= '(''Z'',I3,4x)'
	    elseif(levu.ge.1000)then
	      fmt= '(''Z'',I4,3x)'
	    endif
	    write(vtype,fmt)levu
ckk	    write(*,*)'New vtype label: ',vtype(1:ilen(vtype))
          else  ! Other variables
	    iv= ilen(vtype0)
	    ivx= len(vtype0) -ilen(vtype0)
	    cvtype= vtype0
            if(levu.eq.0)then
	      write(fmt,*)'(A',iv,',',ivx,'X)'
	    elseif(levu.gt.0.and.levu.lt.10)then
	      write(fmt,*)'(A',iv,',I1,',ivx,'X)'
	    elseif(levu.ge.10.and.levu.lt.100)then
	      write(fmt,*)'(A',iv,',I2,',ivx,'X)'
	    elseif(levu.ge.100.and.levu.lt.1000)then
	      write(fmt,*)'(A',iv,',I3,',ivx,'X)'
	    elseif(levu.ge.1000)then
	      write(fmt,*)'(A',iv,',I4,',ivx,'X)'
	    endif
	    if(levu.gt.0)then
	      write(vtype,fmt)cvtype(1:iv),levu
	    else
	      write(vtype,fmt)cvtype(1:iv)
	    endif
ckk	    write(*,*)'New vtype label: ',vtype(1:ilen(vtype))
	  endif
	else  ! ilev is 0
	  iv= ilen(vtype0)
	  ivx= len(vtype0) -ilen(vtype0)
	  cvtype= vtype0
	  write(fmt,*)'(A',iv,',',ivx,'X)'
	  write(vtype,fmt)cvtype(1:iv)
ckk	  write(*,*)'New vtype label: ',vtype(1:ilen(vtype))
	endif

c--------------------------------------------------------------------
c
c * Get the map for this time and level (if ilev >0)
c * Note: dvar(3) is the time variable and rhtype_time is the type
c * This version should allow adjustable dimensions nlons,nlats
c   rather than the fixed values ilon,jlat

         call gridread(ncid,i_uvar,dvar(3),rhtype_time,
     * j_scale_factor,j_add_offset,
     * it,nlats,nlons,ilev,ny,nm,nd,nh,ttype,idt,xfcst,idbg,idbg2)
c
c * Now have the map for this time
ckk	 write(31,*)'ny,nm,nd,nh: ',ny,nm,nd,nh
c--------------------------------------------------------------------

c
         if(idrange.eq.0)then
c
c * If map is within specified limits append to output conmap file
c   - see -M option
c   Otherwise imap1=i 1 and imap2= ntime
c
         if (it.ge.imap1.and.it.le.imap2) then
c
c * Put array x into conmap format and append to concatenated conmap file
c
           if(isepf.eq.1)then
	     if(kv.eq.1)then
	     if(l.eq.ilev1)then
               cddate= 1000000*ny +10000*nm +100*nd +nh
	       if(fpx.eq.'')then
	         write(cmpfile ,'(F11.0,''cmp'')')cddate
	       else
               write(cmpfile ,'(A,''.'',F11.0,''cmp'')')fpx(:ilen(fpx)),
     * cddate
	       endif
	       open (2,file=cmpfile,form='unformatted')
ckk	       write(*,*)cmpfile(:ilen(cmpfile))
	       write(*,*)'Output file: ',cmpfile(:ilen(cmpfile))
	     endif
	     endif
	   endif
c
c * Set conmap header
c
           call sethead (vtype,vunits,ny,nm,nd,nh,rtype,gtype,head)
	   write(*,700)it,head ! Write on screen just prior to output
700        format(I6,':',A)
           kmap= kmap +1
           call x2cmp (vtype,rtype,nlons,nlats,lons,lats,head,x,rwork,
     * iscal,uscal,iadd360,2)
           if(isepf.eq.1)then
	     if(kv.eq.nuvar.and.l.eq.ilev2)close(2)
	   endif
         endif
       else
c
c * Use -t option (based on date range)
c
         cddate= 1000000*ny +10000*nm +100*nd +nh
	 if (ndate.eq.1)then
	   ldate= (cddate.eq.ddate(1))
	 elseif (ndate.eq.2) then
	   ldate= (cddate.ge.ddate(1).and.cddate.le.ddate(2))
	 endif
	 if(idbg.gt.0)then
           write(10,'(''cddate: '',F11.0,'' ldate: '',L1)')cddate,ldate
	 endif
	 if(ldate)then
           if(isepf.eq.1)then
	     if(kv.eq.1)then
	     if(l.eq.ilev1)then
               cddate= 1000000*ny +10000*nm +100*nd +nh
	       if(fpx.eq.'')then
	         write(cmpfile ,'(F11.0,''cmp'')')cddate
	       else
	       write(cmpfile ,'(A,''.'',F11.0,''cmp'')')fpx(:ilen(fpx)),
     * cddate
	       endif
	       write(*,*)'Output file: ',cmpfile(:ilen(cmpfile))
	       open (2,file=cmpfile,form='unformatted')
	     endif
	     endif
	   endif
c
           call sethead (vtype,vunits,ny,nm,nd,nh,rtype,gtype,head)
	   write(*,700)it,head ! Write on screen just prior to output
           kmap= kmap +1
           call x2cmp (vtype,rtype,nlons,nlats,lons,lats,head,x,rwork,
     * iscal,uscal,iadd360,2)
           if(isepf.eq.1)then
	     if(kv.eq.nuvar.and.l.eq.ilev2)close(2)
	   endif
         endif
       endif
c
       if(idbg.ge.2)write(*,*)'End l loop:  l= ',l
       enddo   ! End of l loop
c
       if(idbg.ge.2)write(*,*)'End kv loop: kv= ',kv
       enddo   ! End of kv loop
c
       if(idbg.ge.2)write(*,*)'End it loop: it= ',it
       enddo   ! End of it loop
c

5100   continue
       write(*,*)'Processing completed'

c ---------------------------------------------------------------------

       if(nlev.eq.0)then
         ktmap= kmap/(nuvar)
       else
         ktmap= kmap/(nlev*nuvar)
       endif
       write(*,*)'No. of time steps extracted: ',ktmap
       write(*,*)'No. of maps extracted: ',kmap
       write(*,*)'No. of user variables: ',nuvar
       write(*,*)'No. of levels: ',nlev
c
c * Close NetCDF file
c
      call ncclos(ncid, rcode)
c
      if (rcode.eq.0) then
        write(6,*)'NetCDF file closed successfully (ncid=', ncid, ')'
      else
        write(6,*)'ERROR in closing NetCDF file'
        stop
      endif
c
c * Close concatenated conmap file
c
       if(isepf.eq.0)then
         close(2)
         write(*,*)'Output conmap file: ',cmpfile(1:ilen(cmpfile))
       endif
c
       write(*,*)'Finished!'
c
       end

c ---------------------------------------------------------------------
C
C Scenario 2.
C USE THIS LOOP TO READ THE FIRST 'NTIME' TIME STEPS FROM A FILE
C WITH 'NLEV' LEVELS IN IT.  EACH PASS THROUGH THE LOOP WILL RESULT
C IN A GRID AT LEVEL ILEV, AND TIME STEP NTIME.
C

c     ntime = 5
c     nlev = 17
c
c     do it=1,ntime
c       do ilev = 1, nlev
c          call gridread(inet,ivar,it,x,jlat,ilon,ilev,ny,nm,nd,nh,work,
c     *	       idbg)
c          print*,x(1,1),ny,nm,nd,nh
c       enddo
c     enddo

c
C Scenario 3.
c THIS CODE BLOCK WILL READ A SPECIFIC TIME AND DATE FROM THE FILE.
c

c
c Set the time of the grid to be read.
c

c      ny = 1995
c      nm = 3
c      nd = 6
c      nh = 12

c
c Set this to 0 for a file with one level, loop over it to get all levels,
c or set it to the index of a specific level.
c
c     ilev = 5

c     call timeindex(inet,ny,nm,nd,nh,it)
c     call gridread(inet,ivar,it,x,jlat,ilon,ilev,ny,nm,nd,nh,work,idbg)

c
c The value of ny, nm, nd, and nh have been overwritten with the
c values for the grid just read.
c
c     print*,x(1,1),ny,nm,nd,nh

c      stop
c      end

********************************************************************
********************************************************************
C     MAIN SUBROUTINE TO READ GRID

      subroutine gridread(inet,ivar,time_var,itype_time,
     * jscale_factor,jadd_offset,
     & nt,jlat,ilon,
     & ilev,iyear,imonth,
     & iday,ihour,ttype,idt,xfcst,idbg,idbg2)
c
c * Notes: 
c   (1) 15/5/2006: Debug variables idbg and idbg2 added by KK
c   (2) 17/5/2006: Added IDs for scale_factor and add_offset;
c       added time_var and itype_time (KK)
c   (3) 14/8/2006: Added extra processing for unpacked real
c       and real*8 user variables; these may have missing
c       values which need to be converted to the conmap code;
c       see also: dble2real and real2real (KK)
c              
C STEP 1.
ckk      include '/usr/local/include/udunits.inc'
ckk      include '/usr/local/include/netcdf.inc'
      include 'udunits.inc'
      include 'netcdf.inc'

********************************************************************
C     UDUNITS STUFF TO UNPACK TIME:
********************************************************************
C     declare udunits function calls

C      integer UTMAKE,UTDEC,uttime

C     declare everything else

c * Note: mlon=721,nlat=361 corresponds to a global 0.5 x 0.5 grid
      parameter (mlon=721)
      parameter (mlat=480)
      real x(mlon,mlat)
      integer*2 idata(mlon,mlat)
      real*8 ddata(mlon,mlat)
      common /blk1/x,idata,ddata
c
      integer*4 iyear,imonth,iday,ihour,itimeid
      integer*8 retcode_utdec
      integer retcode_uttime
      integer*8 unitptr
      character*1024 unitstrin
      character*1024 unitstr
c
ckk      integer*2 idata(ilon,jlat)
ckk      real*4 x(ilon,jlat)
c
      integer count(3),start(3)
      integer countl(4),startl(4)
      integer icount2(1),istart2(1)
      integer*2 miss  ! This only applies to packed data (ipack= 1) KK
      real rmiss  ! For unpacked real data (KK)
      real*8 dmiss! For unpacked double precision data (KK)
      integer vartype, rtncode, ndims, dims(100), natts
      integer isdfltao, isdfltsf
      character*128 varname
c
c * Added by KK 17/5/2006
c
      real*8 xtime  
      real*8 xfcst
      real rxtime
      integer*4 ixtime
      real xscale,xoff
      real*8 dxscale,dxoff 
c
      character*20 time_var ! Name of time variable
      integer idbg
      character*31 attnam(10,40) ! Attribute name
      integer attlen(10,40) ! Attribute length
      integer attype(10,40) ! Attribute type e.g. 4= integer
      integer int_att       ! Hold integer value
      real real_att         ! Hold real value
      real*8 dble_att       ! Hold double precision value
c
      integer idt           ! 0= normal use >0 -T option
      character*4 ttype     ! Currently 'CDO' or 'fcst' with -T option
c
c * Get time origin and units; in this routine time is in
c   double precision (xtime) so checks are made on the
c   input time variable (KK)
c
      if(idt.eq.0.or.idt.eq.2)then
      unitptr=utmake()
ckk      itimeid=NCVID(inet,'time',icode)
      itimeid=NCVID(inet,time_var,icode)
      call ncainq(inet,itimeid,'units',iNCCHAR,mlen,i)
      unitstrin= ' '
      unitstr= ' '
      call ncagtc(inet,itimeid,'units',unitstrin,mlen,icode)
      unitstr=unitstrin(1:nblen(unitstrin))
ckk      unitstr= 'hours since 1900-01-01 00:00:0.0'
      if(idbg.eq.2.and.idbg2.eq.1)
     *   write(*,*)'unitstr:',unitstr(1:nblen(unitstr))
      retcode_utdec=utdec(unitstr,unitptr)
      retcode_uttime=uttime(unitptr)
      isave=1
      elseif(idt.eq.1)then ! ttype = 'CDO'
      itimeid=NCVID(inet,time_var,icode)
      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     TEST AND SEE OF FILE HAS LEVELS
C     ALSO SEE IF PACKED

C     GET SCALE FACTOR AND MISSING VALUE IF NECESSARY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c * Next line is unchanged from example program (KK)      
      call NCPOPT(0)	!Turn off netCDF error reaction
c
c---
c      call ncagt(inet,ivar,'scale_factor',dxscale,icode1)
c      xscale= real(dxscale)
c      call ncagt(inet,ivar,'add_offset',dxoff,icode2)
c      xoff= real(dxoff)
c---

c
c * Get required attributes; convert to real values
c
          xscale= 1.
          xoff= 0.
c
          if(jscale_factor.ne.0)then
          i= ivar
          j= jscale_factor  ! scale_factor
          call ncanam(inet,i,j,attnam(i,j),rcode) ! Get attribute name
          if(idbg.eq.2.and.idbg2.eq.1)
     *       write(*,*)'i,j,attnam:',i,j,attnam(i,j)
          call ncainq(inet,i,attnam(i,j),attype(i,j),attlen(i,j),
     * rcode)
          if(attype(i,j).ne.2) then  ! Not a character attribute
            if(attype(i,j).eq.4)then
              call ncagt(inet,i,attnam(i,j),int_att,icode1)
              xscale= real(int_att)
            endif
            if(attype(i,j).eq.5)then
              call ncagt(inet,i,attnam(i,j),real_att,icode1)
              xscale= real_att
            endif
            if(attype(i,j).eq.6)then
              call ncagt(inet,i,attnam(i,j),dble_att,icode1)
              xscale= real(dble_att)
            endif
          endif
          endif
c
          if(jadd_offset.ne.0)then
          i= ivar
          j= jadd_offset  ! add_offset
          call ncanam(inet,i,j,attnam(i,j),rcode) ! Get attribute name
          if(idbg.eq.2.and.idbg2.eq.1)
     *       write(*,*)'i,j,attnam:',i,j,attnam(i,j)
          call ncainq(inet,i,attnam(i,j),attype(i,j),attlen(i,j),
     * rcode)
          if(attype(i,j).ne.2) then  ! Not a character attribute
            if(attype(i,j).eq.4)then
              call ncagt(inet,i,attnam(i,j),int_att,icode2)
              xoff= real(int_att)
            endif
            if(attype(i,j).eq.5)then
              call ncagt(inet,i,attnam(i,j),real_att,icode2)
              xoff= real_att
            endif
            if(attype(i,j).eq.6)then
              call ncagt(inet,i,attnam(i,j),dble_att,icode2)
              xoff= real(dble_att)
            endif
          endif
          endif
c
c * Assume missing_value has same type as user variable (usual case)
c
         call ncvinq(inet, ivar, varname, vartype, ndims, dims, natts,
     +               rtncode)
         if (rtncode .ne. ncnoerr) then
            print *, 'Error calling nf_inq_vartype.'
            stop
         endif
      if(vartype.eq.3)then  ! integer*2
              call ncagt(inet,ivar,'missing_value',miss,icode3)
      elseif(vartype.eq.5)then  ! real
              call ncagt(inet,ivar,'missing_value',rmiss,icode3)
      elseif(vartype.eq.6)then  ! real*8 
              call ncagt(inet,ivar,'missing_value',dmiss,icode3)
      endif
c
      if(idbg.eq.2.and.idbg2.eq.1)then
        write(*,*)'scale_factor: ',xscale
        write(*,*)'add_offset: ',xoff
      endif
c
C     you need to get level or else use integer level
C     SLAB INFO FOR DATA ARRAY

C     see if packed or not!!!!
      ipack=0
      if((icode1 .eq. 0) .and. (icode2 .eq. 0)) then
        isdfltao = 0
        isdfltsf = 0
        if (abs(xscale - 1.0) .lt. 1.0e-10) then
            isdfltsf = 1
        endif
        if (abs(xoff) .lt. 1.0e-10) then
            isdfltao = 1
        endif
        if ((isdfltsf .eq. 1).and.(isdfltao .eq. 1)) then
C        if((xscale.eq.1).and.(xoff.eq.0))then
          call ncvinq(inet, ivar, varname, vartype, ndims, dims, natts,
     +               rtncode)
          if (rtncode .ne. ncnoerr) then
              print *, 'Error calling nf_inq_vartype.'
              stop
          endif
          if (vartype .eq. ncshort) then
              ipack = 1
          else
              ipack = 0
          endif
        else
         ipack=1
        endif
      endif
c
        if(idbg.eq.2.and.idbg2.eq.1)then
          write(*,*)'pack: ',ipack
          if(jmissing.ne.0)write(*,*)'missing: ',miss
          idbg2= 0 ! This stops this printout
        endif
c
        start(3)=nt
        if(ilev.eq.0)then
          start(1)=1
          start(2)=1
          start(3)=nt

          count(1)=ilon
          count(2)=jlat
          count(3)=1

C     LOOP THROUGH 1 YEAR OF DATA, UNPACK ARRAY, UNPACK TIME
C     AND READ HEADER

         if(ipack.eq.1)then
           call ncvgt(inet,ivar,start,count,idata,icode)
           call unpack(idata,x,xscale,xoff,miss,ilon,jlat)
         else
	   if(vartype.eq.5)then
             call ncvgt(inet,ivar,start,count,x,icode)
             call real2real(x,rmiss,ilon,jlat)
	   elseif(vartype.eq.6)then
             call ncvgt(inet,ivar,start,count,ddata,icode)
             call dble2real(ddata,x,dmiss,ilon,jlat)
	   endif
         endif
c
         if(itype_time.eq.4) then ! Integer time
           call ncvgt(inet,itimeid,it,1,ixtime,icode)
	   xtime= dble(ixtime)
         elseif(itype_time.eq.5) then ! Real time
           call ncvgt(inet,itimeid,it,1,rxtime,icode)
	   xtime= dble(rxtime)
         elseif(itype_time.eq.6) then ! Double precision time
           call ncvgt(inet,itimeid,it,1,xtime,icode)
         else
           write(*,*)'ERROR: Need to add code for',
     * 'itype_time=',itype_time
           stop
         endif
ckk         if(idbg.eq.2)write(*,*)'xtime: ',xtime
c
         istart2(1)=nt
         icount2(1)=1
c
         if(itype_time.eq.4) then ! Integer time
           call ncvgt(inet,itimeid,istart2,icount2,ixtime,iercode)
	   xtime= dble(ixtime)
         elseif(itype_time.eq.5) then ! Real time
           call ncvgt(inet,itimeid,istart2,icount2,rxtime,iercode)
	   xtime= dble(rxtime)
         elseif(itype_time.eq.6) then ! Double precision time
           call ncvgt(inet,itimeid,istart2,icount2,xtime,iercode)
         else
           write(*,*)'ERROR: Need to add code for',
     * 'itype_time=',itype_time
           stop
         endif
         if(idbg.eq.2)write(*,*)'xtime: ',xtime
         if(idbg.eq.2)write(7,'(I2,1x,F11.2)')itype_time,xtime   !KK 8/2/2008
c
         if(idt.eq.0)then
         call udparse(unitstrin,xtime,iyear,imonth,iday,ihour)
	 elseif(idt.eq.1)then  ! ttype = 'CDO'
	 call ttime(ttype,xtime,iyear,imonth,iday,ihour)
	 elseif(idt.eq.2)then  ! ttype = 'fcst'
         call udparse(unitstrin,xtime+xfcst,iyear,imonth,iday,ihour)
	 endif
      else
          startl(1)=1
          startl(2)=1
          startl(3)=ilev
          startl(4)=nt

          countl(1)=ilon
          countl(2)=jlat
          countl(3)=1
          countl(4)=1

C     LOOP THROUGH 1 YEAR OF DATA, UNPACK ARRAY, UNPACK TIME
C     AND READ HEADER

          if(ipack.eq.1)then
            call ncvgt(inet,ivar,startl,countl,idata,icode)
            call unpack(idata,x,xscale,xoff,miss,ilon,jlat)
          else
ckk            call ncvgt(inet,ivar,startl,countl,x,icode)
	   if(vartype.eq.5)then
             call ncvgt(inet,ivar,startl,countl,x,icode)
             call real2real(x,rmiss,ilon,jlat)
	   elseif(vartype.eq.6)then
             call ncvgt(inet,ivar,startl,countl,ddata,icode)
             call dble2real(ddata,x,dmiss,ilon,jlat)
	   endif
          endif
c
          if(itype_time.eq.4) then ! Integer time
            call ncvgt(inet,itimeid,it,1,ixtime,icode)
	    xtime= dble(ixtime)
          elseif(itype_time.eq.5) then ! Real time
            call ncvgt(inet,itimeid,it,1,rxtime,icode)
	    xtime= dble(rxtime)
          elseif(itype_time.eq.6) then ! Double precision time
            call ncvgt(inet,itimeid,it,1,xtime,icode)
          else
            write(*,*)'ERROR: Need to add code for',
     * 'itype_time=',itype_time
            stop
          endif
ckk          if(idbg.eq.2)write(*,*)'xtime: ',xtime
c
          istart2(1)=nt
          icount2(1)=1
c
          if(itype_time.eq.4) then ! Integer time
            call ncvgt(inet,itimeid,istart2,icount2,ixtime,iercode)
	    xtime= dble(ixtime)
          elseif(itype_time.eq.5) then ! Integer time
            call ncvgt(inet,itimeid,istart2,icount2,rxtime,iercode)
	    xtime= dble(rxtime)
          elseif(itype_time.eq.6) then ! Double precision time
            call ncvgt(inet,itimeid,istart2,icount2,xtime,iercode)
          else
            write(*,*)'ERROR: Need to add code for',
     * 'itype_time=',itype_time
            stop
          endif
          if(idbg.eq.2)write(*,*)'xtime: ',xtime
          if(idbg.eq.2)write(7,'(I2,1x,F11.2)')itype_time,xtime   !KK 8/2/2008

c
          if(idt.eq.0)then
          call udparse(unitstrin,xtime,iyear,imonth,iday,ihour)
	  elseif(idt.eq.1)then  ! ttype = 'CDO'
	  call ttime(ttype,xtime,iyear,imonth,iday,ihour)
	  elseif(idt.eq.2)then  ! ttype = 'fcst'
          call udparse(unitstrin,xtime+xfcst,iyear,imonth,iday,ihour)
	  endif
      endif
c
      return
      end

********************************************************************
      subroutine unpack(x,y,xscale,xadd,miss,ilon,jlat)
ckk      parameter (zmiss=1e30)
      parameter (zmiss=99999.9) ! Conmap missing value code

      integer*2 x(ilon,jlat),miss
      real y(ilon,jlat)

      do i=1,ilon
         do j=1,jlat
            if(x(i,j).eq.miss)then
               y(i,j)=zmiss
            else
              y(i,j)=(x(i,j)*xscale)+xadd
           endif
         enddo
      enddo

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
C
C     This routine (part of the netopen package for CDC netCDF file 
C     handling) takes a netCDF file id, a year, month, day and hour
C     and converts it to the time index for that time in the file
C     and returns that index
C
C     NOTE: WILL NOT HANDLE LESS THAN HOURLY DATA!  Prints error message
C 		and terminates.
C
C     Written by Cathy Smith of CDC on ???
C     Modified by Tom Baltzer of CDC on 2/14/95
C		- Broke out into own file and added comments
C     Modified by Tom Baltzer of CDC on 2/27/95
CC		  added declarations comments and made to work with 
C		  reanalysis.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine timeindex(netid,nyr,nmn,ndy,nhr,itime)
ckk      include '/usr/local/include/netcdf.inc'
ckk      include '/usr/local/include/udunits.inc'
      include 'udunits.inc'
      include 'netcdf.inc'

      integer netid			! NetCDF file id
      integer nyr,nmn,ndy,nhr		! Input year,month,day & hour
      integer itime			! Return time index

      integer inyr2d			! 2 digit in year (used throughout)
      integer inyr4d			! 4 digit input year
      real*8  xtime(10)			! Holds 10 time values from file
      real*8  inxtime			! Input time in netCDF file form
      character*25 cdeltat		! delta_t attribute from file
      character*25 cstat		! Statistic attribute from file
      integer timevid			! Time variable id
      integer timedid			! Time dimension id
      character*100 timeunit 		! unit attribute for time variable
      integer*8 unitptr			! pointer to udunits unit
C      integer*4 UTMAKE			! Declare the function.  It's in the
					! uduints.inc at our site, but may 
                                        ! not be at others.
      integer yr1,mn1,dy1,hr1		! First time value in file
      integer yr2,mn2,dy2,hr2		! Second time value in file
      integer*4 xhour1,xhour2,xhourin   ! results of xhour calls 
      integer*4 hourinc			! delta time in hours
      integer monthly			! 1 if data is monthly
      integer seasonal			! 3 if data is monthly
      integer daily			! 1 if data is daily
      integer found			! 0 if not found 1 if found

      integer ltm			! Used to identify a LTM file
      integer equalinc			! Identify non-equal time increments
      
      integer ercode			! To get netCDF error codes
C	  integer UTDEC				! Declare the functions.
C	  integer UTICALTIME
      integer istart(1),icount(1)	! Used in netCDF calls
      character*15 cname		! Place holder

C     Initialize
c
c I added the condition to find values less that 100.  We assume that such
c a year is a 2 digit year in the 20th century.
c
c The other else if block gets set up for other years, like 1851 the beginning
c of the DOE data set.
c
c     if (nyr.gt.1900.and.nyr.lt.2000) then
c        inyr2d = nyr - 1900
c        inyr4d = nyr
c     else if ( nyr.lt.100 ) then       ! Between 0-99, it's a 2 digit year.
c        inyr2d = nyr
c        inyr4d = nyr + 1900
c     else if ( nyr .ge. 100 .and. nyr .lt. 2000) then  ! Some other year
c        inyr2d = nyr   !Not really, but it's non-zero which is inportant.
c        inyr4d = nyr   !What ever they gave us was the year we want.
c     endif

c
c Do a quick check for ltm, so we can avoid messing with the year value
c if it''s a ltm file.
c

      inyr2d = 0
      inyr4d = 0

      timevid=NCVID(netid,'time',ercode)      
      ltm=0
      call NCPOPT(0)
      call NCAGT(netid,timevid,'ltm_range',ltm_range,ercode)
      if (ercode .eq. 0) then
         ltm = 1
      endif
      call NCPOPT(NCVERBOS+NCFATAL)

      if (nyr.ge.1.and.nyr.le.99.and.ltm.eq.0) then
        inyr2d = nyr
        inyr4d = nyr + 1900
        write(0,*) '!*!*!*!*!*!*!*!*!* WARNING *!*!*!*!*!*!*'
        write(0,*) '! Your input of ',nyr,' was converted     !'
        write(0,*) '! to ', inyr4d, '.                           !'
        write(0,*) '! A 4-digit year must be used to open  !'
        write(0,*) '! files for the year 2000 and beyond.  !'
        write(0,*) '! We recommend using 4-digit years     !'
        write(0,*) '! in all cases.                        !'
        write(0,*) '!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*'
      elseif (ltm.eq.0) then
        inyr2d = 99   ! It just needs to be some non-zero value.
        inyr4d = nyr
      endif

      ltm = 0
      equalinc = 0
      monthly = 0
      daily = 0

C     Get all the time variable information including the first 10 or
C     (if < 10) total available values and convert 1st 3 to yr,mn,dy, & hr

      timedid=NCDID(netid,'time',ercode)
      call NCDINQ(netid,timedid,cname,idimt,ercode)
      istart(1)=1
      if (idimt.ge.10) then
         icount(1)=10
      else
         icount(1)=idimt
      endif

      call NCVGT(netid,timevid,istart,icount,xtime,ercode)      
      call NCAGTC (netid,timevid, 'units', timeunit, 100, ercode)

      if (idimt.ge.3) then
         call udparse(timeunit, xtime(1), yr1,mn1,dy1,hr1)
         call udparse(timeunit, xtime(2), yr2,mn2,dy2,hr2)
      elseif (idimt.eq.1) then
         write(0,*) "Only one time increment in the file - returning it"
         itime = 1
         found = 1
         return
      else
         equalinc = 1
         call udparse(timeunit, xtime(1), yr1,mn1,dy1,hr1)
         call udparse(timeunit, xtime(2), yr2,mn2,dy2,hr2)
      endif


C     Turn off error checking and see if this is a CDC data file, and 
C     if so, check time increment < hourly and check for ltm file.
C     Also do first check for equal increment.

      call NCPOPT(0)
      call NCAGTC(netid,timevid,'delta_t',cdeltat,25,ercode)
      if (ercode.eq.0) then
         equalinc = 1
c
c Previously, this was called with variable id hard coded to 1.
c This won''t work for multiple variables in the same run.
c
c        call NCAGTC(netid,1,'statistic',cstat,25,ercode)
c
c We will detect long term means from the ltm_range attribute.
c That way we don''t need the variable id at all.
c
	call NCAGT(netid,timevid,'ltm_range',ltm_range,ercode)

c
c Ugly kludge to handle different spellings of LTM.  Probably
c should do this as a caseless comparison.  rhs 1/8/96
c Using ltm range gets rid of this ugly kludge.
c
c        if(cstat(1:14).eq.'Long Term Mean' .or.
c    &      cstat(1:14).eq.'Long term mean' .or.
c    &      cstat(1:14).eq.'long term mean') then
c
c If the return code from the above is zero then we have
c and LTM file.
	if (ercode .eq. 0) then
c
c Looking for daily long term means as well as monthly.
c
c           if(inyr2d.ne.0.or.ndy.ne.0.or.nhr.ne.0) then
c              write(0,*) 'timeindex: Warning - non-monthly value ',
c    &                  'requested for monthly LTM file'
c              write(0,*) 'Using month value ',nmn,' to get index'
c           endif
c
c Detect both monthly and daily long term mean.
c
            if(inyr2d.ne.0.or.nhr.ne.0) then
               write(0,*) 'timeindex: Warning - non-zero hourly value ',
     &                  'requested for a LTM file.',
     &                  'Do not know how do deal with hourly LTM.  ',
     &                  'Quiting...'
                stop 'LTM'
            endif
            ltm = 1
         endif
         if (cdeltat(15:16).ne.'00'.or.cdeltat(18:19).ne.'00') then
            write(0,*) 'timeindex: Error: Time increment is < hourly -'
            write(0,*) ' cannot be handled. -Terminating.'
            stop
         endif
         if (cdeltat(7:7).eq.'1') then 
            monthly = 1
         else if (cdeltat(7:7).eq.'3') then 
            seasonal = 1
         else if (cdeltat(10:10).eq.'1') then
            daily = 1
         else if (cdeltat(7:7).ne.'1'.and.cdeltat(7:7).ne.'3'.and.
     $            cdeltat(7:7).ne.'0') then
            write(0,*) 'timeindex: Error:',
     $                 'Cannot handle a delta_t with months increment',
     $                 'other than 0000-01-00 and 0000-03-00.',
     $                 'Your delta_t = ', cdeltat
            stop 'Monthly Delta T'
         endif
      endif
      call NCPOPT(NCVERBOS+NCFATAL)

C     Get the time index.

      if (ltm.eq.1 .and. monthly.eq.1) then
         itime=(nmn-mn1)+1
         found = 1
      elseif ( ltm.eq.1 .and. daily.eq.1) then
         call xhour(yr1,mn1,dy1,hr1,xhour1)
         call xhour(yr2,mn2,dy2,hr2,xhour2)
         hourinc = xhour2 - xhour1
c
c Well, this looks ugly.
c Previously, these xhour values were computed from the year 0.
c xhour turns the year 0 into 1900 and all the calculations are
c done relative to 1900.  1900 is a leap year, which pushes the
c ltm values off by a day.  Instead, since we know it''s a ltm
c already from the statistic attribute, we use year 1 for
c the xhour calculations.  rhs 1/8/95
c
         if (daily.eq.1.or.hourinc.eq.24) then
c           call xhour(yr1,mn1,dy1,0,xhour1)
c           call xhour(yr2,mn2,dy2,0,xhour2)
c           call xhour(inyr4d,nmn,ndy,0,xhourin)
            call xhour(1,mn1,dy1,0,xhour1)
            call xhour(1,mn2,dy2,0,xhour2)
            call xhour(1,nmn,ndy,0,xhourin)
         else
            call xhour(inyr4d,nmn,ndy,nhr,xhourin)
         endif
         itime = INT(((xhourin - xhour1)/hourinc) + 0.5)
         itime = itime + 1
         found = 1
      elseif (monthly.eq.1) then
         itime=(((inyr4d*12)+nmn)-((yr1*12)+mn1))
         itime=itime+1
         found = 1
      elseif (seasonal.eq.1) then
         itime=(((inyr4d*12)+nmn)-((yr1*12)+mn1))/3
         itime=itime+1
         found = 1
      elseif (equalinc.eq.1) then
         call xhour(yr1,mn1,dy1,hr1,xhour1)
         call xhour(yr2,mn2,dy2,hr2,xhour2)
         hourinc = xhour2 - xhour1
         if (daily.eq.1.or.hourinc.eq.24) then
            call xhour(yr1,mn1,dy1,0,xhour1)
            call xhour(yr2,mn2,dy2,0,xhour2)
            call xhour(inyr4d,nmn,ndy,0,xhourin)
         else
            call xhour(inyr4d,nmn,ndy,nhr,xhourin)
         endif
         itime = INT(((xhourin - xhour1)/hourinc) + 0.5)
         itime = itime + 1
         found = 1
      else
         write(0,*) 'Cannot assume consistant delta time for ',
     &			'finding index (no delta_t attribute)'
         write(0,*) 'This may take a while ...'
         found = 0

C        Inverse parse the date for comparisons - and move through the 
C        time values searching for the one we want.

         if (timeunit(1:1).eq.'y'.or.timeunit(1:1).eq.'Y') then
            inxtime = inyr4d*10000000000.d0 + nmn*100000000.d0 +
     &                ndy*1000000. + nhr*10000.
         else
            unitptr = UTMAKE()
            ercode = UTDEC(timeunit(1:lnblnk(timeunit)),unitptr)
            if (ercode.ne.0) then
               call uduerr(ercode,'UTDEC','')
               write(0,*) ''
               write(0,*) 'NOTE: You must call netop_init to use 
     &				gridread,'
               write(0,*) '      gridreadx, dayread, and dayreadx'
               stop
            endif
            ercode = UTICALTIME(inyr4d,nmn,ndy,nhr,0,0.,unitptr,inxtime)
            if (ercode.ne.0) then
               call uduerr(ercode,'UTICALTIME','')
               write(0,*) ' '
               write(0,*) 'Something wrong with finding time increment'
               write(0,*) 'Terminating'
               stop
            endif
            call UTFREE(unitptr)
         endif

C        See if time is in first group of time values gotten from file

         do i = 1,icount(1)
            if (xtime(i).eq.inxtime) then
               itime = i
               found = 1
            endif
         enddo
         if (icount(1).eq.idimt) then
            write(0,*) 'Could not find desired time increment'
            write(0,*) ' - Terminating'
            stop
         endif

C        Step through file getting groups of values and seeing if it''s in there

         istart(1) = istart(1) + icount(1)
         do while (found.eq.0.and.istart(1)+icount(1).le.idimt)
            call NCVGT(netid,timevid,istart,icount,xtime,ercode)      
            do i = 1,icount(1)
               if (xtime(i).eq.inxtime) then
                  itime = (istart(1) + i) - 1
                  found = 1
               endif
            enddo
         enddo

C        Check the last group of time values in the file 
         if (found.eq.0) then
            icount(1) = (idimt - istart(1)) + 1
            call NCVGT(netid,timevid,istart,icount,xtime,ercode)
            do i = 1,icount(1)
               if (xtime(i).eq.inxtime) then
                  itime = (istart(1) + i) - 1
                  found = 1
               endif
            enddo
         endif

         if (found.eq.0) then
            write(0,*) 'Could not find desired time increment'
            write(0,*) ' - Terminating'
            stop
         endif

      endif

      if(itime.gt.idimt)then
         write(0,*)'You are requesting a time past the end of the file'
         istart(1)=idimt
         icount(1)=1
         call NCVGT(netid,timevid,istart,icount,xtime,ercode)
         call udparse(timeunit,xtime(1),iyear,imonth,iday,ihour)         
         write(0,*)' last day of file is: ',iyear,imonth,iday,ihour
      endif
      if(itime.le.0)then
        print*,'you asked for a time before first day of the dataset'
        print*,iyear,imonth,iday,ihour,' is first day of data'         
        itime=0
      endif

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  This subroutine is part of the netopen/read set of Fortran callable
C  routines for reading CDC netCDF files and returning grids.
C
C  This routine takes in a year, month, day and hour and calculates
C  the number of hours since a date before all our data (year 1 month 1
C  day 1) - this gives a referece value from which differences between
C  dates can be derived.
C
C  Note that this routine will take a 4 or a 2 digit date, if it
C  receives a 2 digit date it assumes that it is in the range 1900 -
C  1999.
C
C  Written by Cathy Smith of CDC on ???
C  Modified by Tom Baltzer of CDC on Feb 28, 1995
C 	- converted from days to hours so that hourly data can be
C 	  accounted for, and made starting date 1-1-1 from 1-1-1920.
C
C  File: xhour.f (to be included in netopen.subr.f)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine xhour(iy,im,id,ih,xhours)

      integer iy,im,id,ih	! The input year, month, day and hour
      integer*4 xhours		! OUT # of hours between in and 1-1-1

      integer*4 xdays		! Used in calculating hours
      integer   inyear		! Used to work with input year

      integer imonth(12)	! Used in calculating # days in this year
      integer imonthl(12)	! "					"

      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
      data imonthl/31,29,31,30,31,30,31,31,30,31,30,31/

C     See if date is in 1900s but given as 2 digit.

      if (iy.lt.100) then
         inyear = iy + 1900
      else
         inyear = iy
      endif

C     CALCULATE DAYS FROM JAN 1 Year 1

      xdays=0
      xhours=0

      xdays = INT((inyear-1)*365.25)

      if(im.gt.1)then
        do imm=1,im-1
           if((mod(inyear,4).eq.0).and.(inyear.ne.0))then
             xdays=xdays+imonthl(imm)
           else
             xdays=xdays+imonth(imm)
           endif
        enddo
      endif

      xdays=xdays+id

      xhours = xdays*24

      xhours = xhours + ih

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This subroutine is part of the netopen Fortran callable CDC
C     group of routines for reading generic netCDF files in the new
C     cooperative standard as presented in:
C	http://ferret.wrc.noaa.gov/noaa_coop/coop_cdf_profile.html
C
C     This routine takes a time value pulled from the netCDF file,
C     determines if it is in the new or old time format (by using the
C     udunits function) and then returns the representative year,
C     month, day and hour represented by the time value.
C
C     Written by Cathy Smith of CDC on ???
C     Modified by Tom Baltzer of CDC on Feb. 8, 1995
C	- To determine if time is old or new format and parse
C		accordingly
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine udparse(timeunit,intime,oyear,omonth,oday,ohour)

C STEP 1.
ckk      include '/usr/local/include/udunits.inc'
      include 'udunits.inc'

C     Note: POINTER type is integer*4 on the CDC SparcCenter 2000
C     system running SunOS 5.3
C      integer UTMAKE,UTDEC,utcaltime

      integer*8 unitptr ! Pointer to udunits "unit" type

      character*100 timeunit
			! The unit attribute for time in the netCDF file
      real*8 intime,xx	! The input time value and var to work with it
      integer oyear,omonth,oday,ohour
			! The output year,month,day and hour
      integer tmin	! Temporary storage for minutes value
      real    tsec	! Temporary storage for seconds value
      integer ercode    ! For determining udunits result

      unitptr = UTMAKE()
      ercode = UTDEC(timeunit(1:nblen(timeunit)),unitptr)
      if (ercode.ne.0) then

C        Assume old CDC standard if time unit is unknown

         if (ercode.eq.UT_EUNKNOWN.and.(timeunit(1:1).eq.'y'.or.
     &	     	timeunit(1:1).eq.'Y')) then
            xx=0.
            oyear=int(intime/10000000000.d0)
            xx=oyear*10000000000.d0
            omonth=int((intime-xx)/100000000.d0)
            xx=xx+omonth*100000000.
            oday=int((intime-xx)/1000000.)
            xx=xx+oday*1000000.
            ohour=int((intime-xx)/10000.)
         else
            call uduerr(ercode,'UTDEC','')
            write(0,*) ''
            write(0,*) 'NOTE: You must call netop_init to use gridread,'
            write(0,*) '      gridreadx, dayread, and dayreadx'
            stop
         endif
      else
         ercode = UTCALTIME(intime,unitptr,oyear,omonth,oday,ohour,
     &                      tmin,tsec)
         if (ercode.ne.0) then
            call uduerr(ercode,'UTCALTIME','')
            stop
         endif
      endif

C     Free up the pointer
      call UTFREE(unitptr)

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  This function is intended to handle all possible error conditions
C  for the Udunits package  (Version 1.7.1) from Unidata, it takes the
C  error code returned from a udunits function, the function name and
C  the filename the function was accessing (applicable only for utopen
C  function).  It prints an indication to the user of the error and
C  function in which it occurred.
C
C  Written by: Tom Baltzer of CDC  February 10, 1995
C
C  File:  uduerr.f  (To be added to netopen.subr.f)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine uduerr(ercode, funcname, filename)

C STEP 1.
ckk      include '/usr/local/include/udunits.inc'
      include 'udunits.inc'

      integer ercode 		! The Udunits Error number
      character* (*) funcname 	! Function name which returned error
      character* (*) filename	! Name of file on which funct operates
      character*1024 path	! Path to file on which funct really works

      if (filename.eq.'') then
         call getenv('UDUNITS_PATH',path)
         if (path.eq.'') then
            path = '/usr/local/src/udunits'
         endif
      else
         path = filename
      endif

      write(0,*) 'Error in udunits function: ',funcname
      if (ercode.eq.UT_ENOFILE) then
         write(0,*) 'File: ',path(1:nblen(path))
         write(0,*) 'Does not exist!'
      elseif (ercode.eq.UT_ESYNTAX) then
         write(0,*) 'A Udunits syntax error was detected'
         if (path.ne.'') then
            write(0,*) 'Possibly in the file: ',
     &			path(1:nblen(path))
         endif
      elseif (ercode.eq.UT_EUNKNOWN) then
         write(0,*) 'Udunits unknown specification found'
         if (path.ne.'') then
            write(0,*) 'Possibly in the file: ',
     &			path(1:nblen(path))
         endif
      elseif (ercode.eq.UT_EIO) then
         write(0,*) 'I/O Error detected'
         if (path.ne.'') then
            write(0,*) 'Possibly in the file: ',
     &			path(1:nblen(path))
         endif
      elseif (ercode.eq.UT_EINVALID) then
         write(0,*) 'An invalid udunits structure was found'
         write(0,*) ' Note: probably a temporal struct'
      elseif (ercode.eq.UT_ENOINIT) then
         write(0,*) 'Udunits package has not been initialized'
      elseif (ercode.eq.UT_ECONVERT) then
         write(0,*) 'No conversion between the two units is possible'
      elseif (ercode.eq.UT_ENOROOM) then
         write(0,*) 'Not enough room in character string for conversion'
      elseif (ercode.eq.UT_EALLOC) then
         write(0,*) 'Memory allocation falure'
      else
         write(0,*) 'UTOPEN: Unknown error: ',ercode
      endif

      return
      end
      integer function nblen (string)
c
c     given a character string, nblen returns the length of the string
c     to the last non-blank character, presuming the string is left-
c     justified, i.e. if string = '   xs  j   ', nblen = 8.
c
c     called non-library routines: none
c     language: standard fortran 77
c
      integer ls,i
      character*(*) string, blank*1, null*1
      data blank /' '/
c
      null = char(0)
      nblen = 0
      ls = len(string)
      if (ls .eq. 0) return
      do 1 i = ls, 1, -1
         if (string(i:i) .ne. blank .and. string(i:i) .ne. null) go to 2
    1    continue
      return
    2 nblen = i
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      integer function ilen(string)
c
      character*1 c
      character*(*) string
c
      do i=len(string),1,-1
        c= string(i:i)
        if (c.ne.' ') goto 10
      enddo
c * String is wholly blank
      ilen= -1
      return
c
   10 ilen= i
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine x2cmp(vtype,rtype,nlons,nlats,lons,lats,head,x,y,
     * iscal,uscal,iadd360,lunit)
c
c * Modified: Kevin Keay  Date: Apr 14 2009
c
c * Notes: If iadd360 is 1 then the data at lon 0 is duplicated to 360
c   See -G option in main program
c
c * Note: Array y has an extra column to hold lon 360 data
c   This is a work array from the calling program of size (mlon,mlat)
c
      real x(nlons,nlats),y(nlons+1,nlats)
      real lons(nlons),lats(nlats)
      integer iscal
      real uscal
      character*80 head
      integer lunit,iadd360
c
      character vtype*5, rtype*5
c
      real xmiss
      parameter (xmiss=99999.9) ! conmap missing value code (KK)
c
c * For ERA40 geopotential: convert to metres
c
      parameter (g=9.807)  ! Value of gravity  (m s**-2)
c
c * Copy x to y in case it is not processed by the options like iflip
c
      do i=1,nlons
        do j=1,nlats
	  y(i,j)= x(i,j)
	enddo
      enddo
c
c * Usually need to flip x into y so that latitudes are from S to N
c   Check first!
c
      iflip= 0
      xlat1= lats(1)
      xlat2= lats(2)
      if(xlat1.gt.xlat2)then
        iflip= 1 ! Need to flip
      endif
c
      if(iflip.eq.1)then
        do i=1,nlons
          do j=1,nlats
	    y(i,j)= x(i,nlats-j+1) ! Flip
    	  enddo
        enddo
      endif
c
c * Duplicate data at lon 0 to 360
c
c * Note: nlonsx is the final number of longitudes
c   This value is only used in this routine i.e. the
c   original array size is not changed
c
      nlonsx= nlons
c
      if(iadd360.eq.1)then
        dlon= lons(2) -lons(1)
	xlon360= lons(nlons) +dlon
	if(lons(1).eq.0.and.xlon360.eq.360)then
	  do j=1,nlats
	    y(nlons+1,j)= y(1,j)
	  enddo
	  lons(nlons+1)= xlon360
	  nlonsx= nlons +1
	else
	  write(*,*)'ERROR: First or last longitude is not'
	  write(*,*)'consistent with -G option'
	  write(*,*)'lons (1)=',lons(1),' lons (',nlons,')=',lons(nlons)
	  write(*,*)'ABORT: Due to error'
	  stop
	endif
      endif
c
c * Append y to concatenated conmap file
c
        write(lunit)nlats
        if(iflip.eq.1)then
          write(lunit)(lats(j),j=nlats,1,-1) ! Order is N to S; require S to N
        else
          write(lunit)(lats(j),j=1,nlats)    ! Order already correct
        endif
        write(lunit)nlonsx
        write(lunit)(lons(i),i=1,nlonsx)
        write(lunit)head
c
c * For ERA40 geopotential need to divide by g - see parameter statement
c
	if(rtype.eq.'ERA40'.and.vtype(1:1).eq.'Z')then
          do j=1,nlats
	    do i=1,nlonsx
                y(i,j)= y(i,j)/g  ! Convert to gepotential height (metres)
	    enddo
          enddo
	endif
c
c * Additional user scaling e.g. uscal= 0.01 for Pa -> hPa (KK)
c
        if(iscal.eq.1)then
          do j=1,nlats
            do i=1,nlonsx
              if(y(i,j).ne.xmiss)then  ! Was x(i,j) 
                y(i,j)=y(i,j)*uscal
              endif
            enddo
          enddo
        endif
c
c * Write out array y
c
        write(lunit)((y(i,j),i=1,nlonsx),j=1,nlats)
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine sethead (vtype,vunits,iy,im,id,ih,rtype,gtype,head)
c 
      character vtype*8, rtype*5, vunits*8, gtype*10
      integer iy,im,id,ih
      character*80 head
c
      if(vtype.eq.'PMSL')then
        vunits= 'MB'
      elseif(vtype(1:1).eq.'Z')then
        vunits= 'M '
      endif
      write(head,100)vtype,rtype,iy,im,id,100*ih,vunits,gtype
ckk100   format('PMSL',26x,'NCEP',
ckk100   format(A5,25x,A5,
ckk     * 5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A2,11x,'2.5x2.5DEG')
100   format(A8,22x,A5,
     * 5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine dble2real(x,y,miss,ilon,jlat)
ckk      parameter (zmiss=1e30)
      parameter (zmiss=99999.9) ! Conmap missing value code

      real*8 x(ilon,jlat),miss
      real y(ilon,jlat)

      do i=1,ilon
         do j=1,jlat
            if(x(i,j).eq.miss)then
               y(i,j)=zmiss
            else
              y(i,j)=real(x(i,j))
           endif
         enddo
      enddo

      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine real2real(x,miss,ilon,jlat)
ckk      parameter (zmiss=1e30)
      parameter (zmiss=99999.9) ! Conmap missing value code

      real x(ilon,jlat),miss

      do i=1,ilon
         do j=1,jlat
            if(x(i,j).eq.miss)then
               x(i,j)=zmiss
           endif
         enddo
      enddo

      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine ttime (ttype,xtime,iy,im,id,ih)
c
      character*4 ttype
      double precision xtime
      integer iy,im,id,ih
c
      character*11 ch11
c
      idbg= 0 ! Debug only
c
      write(ch11,'(F11.2)')xtime
      if(idbg.eq.1)then
        write(*,'(F11.2)')xtime
        write(*,'(A)')ch11
      endif
      read(ch11,'(I4,2I2,1x,I2)')iy,im,id,ih
      if(idbg.eq.1)write(*,*)iy,im,id,ih
      ih= nint(real(ih)*0.01*24)
      if(idbg.eq.1)write(*,*)iy,im,id,ih
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine help
      write(*,*)' '
      write(*,*)'Examples:'
      write(*,*)' '
      write(*,*)' (1) NCEP Mean sea level pressure'
      write(*,*)' There is no level variable'
      write(*,*)' We need to rescale the pressure in hPa to Pa'
      write(*,*)' i.e. apply a scale of 0.01 (-s option)'
      write(*,*)' Decode maps 5-8'
      write(*,*)' read_nc2cmp -i slp.2004.nc -o jj.cmp -u slp -r NCEP',
     * ' -v PMSL -s 0.01 -M "5,8"'
      write(*,*)' '
      write(*,*)' (2) NCEP 500 hPa specific humidity'
      write(*,*)' There is one level'
      write(*,*)' read_nc2cmp -i shum.2005.500.nc -o j.cmp',
     * ' -d "lon,lat,time" -u shum -D 2 -r NCEP -m 2 -l level -L 1',
     * ' -v SHUM500 -U "''kg/kg''" '
      write(*,*)' '
      write(*,*)' (3) ERA40 500 hPa geopotential; for ERA40 the'
      write(*,*)' data are automatically rescaled to height (metres)'
      write(*,*)' There are 23 levels: 17 == 500 hPa'
      write(*,*)' read_nc2cmp -i ncdata/hgt.200208.nc -o j.cmp',
     * ' -d "longitude,latitude,time" -u z -D 3 -r ERA40 -l levelist',
     * ' -L 17 -m 2'
      write(*,*)' '
      write(*,*)' (4) Regional GCM 500 hPa heights'
      write(*,*)' There is one level'
      write(*,*)' Decode maps 1-5'
      write(*,*)' read_nc2cmp -i z500.nc -o z500.cmp -u HGT_P -v Z500',
     *' -r RGCM  -U m -d "LON,LAT,TIME" -l PLEV10_10 -L 1 -M "1,5" '
      write(*,*)' '
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
