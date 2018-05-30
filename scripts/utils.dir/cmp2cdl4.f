      program cmp2cdl4
c
c * Purpose: Produce a NetCDF file from a conmap file
c   See: http://www.unidata.ucar.edu/software/netcdf/guidef/guidef-15.html
c   for a good guide to CDL and ncgen.
c   Use: ncecat to concatenate NetCDF files i.e. to form multi-map files
c   Note: Needs a little more work to improve generality!
c
c * Notes:
c   (1) Append an 'f' to the entry for missing_value otherwise this 
c       attribute is not recognised by GrADS.
c   (2) 12/6/2006: Changed time definition to hours; includes
c       routine xhour from UCAR.
c   (3) 22/4/2007: Fixed bug with nlons and length of output variable name
c   (4) 30/7/2007: Added -Y option (Year 19 -> 1999, Year 20 -> 2000)
c   (5) 8/7/2009:  -Y option now allows for Year 18 -> 1998
c       Added a check for grid size compared with mlons and mlats
c   (6) 19/8/2009: Added time option 'YD' == Year, Day of Year (1-366)
c
c * Author: Kevin Keay  Date: 1/4/2006
c
      parameter (xmiss= 99999.9)  ! NCAR missing value code
      parameter (gmiss= -999.0)   ! GrADS missing value code
      parameter(maxk = 1500)       ! Max. no. of maps in concatenated conmap file
      integer tk(maxk)            ! Time index for NetCDF (hourly)
      integer year(maxk)          ! Year value for each map
      parameter(mlons= 360, mlats= 180) ! Grid size for HadISST
      real xlats(mlats),xlons(mlons)
      real dat1(maxk,mlons,mlats)
      real dat2(mlons,mlats)
      character*80 infile,outfile,nmlfile
      character*80 head
      character*80 headk(maxk)    ! Map headers
      character*80 optarg
      character*80 fmt, cline
      character*400 cline2,cline3
      character*10 cdt
      character*80 nc_creation
      logical lexist
c
c * Namelist
c
      character*10 nmlist
      character*20 var
      character*80 attr_var_long_name, attr_var_units
      character*400 gattr_desc
      character*400 gattr_hist
      character*40 date_time_fmt
      character*4 date_time_type
      character*80 nc_name
      logical time_unlimited
      integer map1, map2
      namelist /nmcdl/ var,
     * attr_var_long_name, attr_var_units,
     * gattr_desc, gattr_hist,
     * date_time_fmt, date_time_type,
     * nc_name, time_unlimited,
     * map1, map2
c
      narg= iargc()
      if(narg.eq.0)then
	write(*,*)'Usage: cmp2cdl4 [-h][-Y] -n nmlist -i cmpfile -o cdlfile'
	write(*,*)' '
	write(*,*)'Aug 19 2009 '
	write(*,*)' '
	write(*,*)'Options:'
	write(*,*)' h: Help screen'
	write(*,*)' Y: Interpret year 18 as 1998, 19 as 1999, 20 as 2000'
	write(*,*)'Example: cmp2cdl4 -n nmlist.txt -i cstatdat.cmp -o test.cdl'
	write(*,*)'         ncgen -b test.cdl (uses name in cdl file)'
	write(*,*)'         ncgen -o test.nc test.cdl '
	write(*,*)' '
	stop
      else
        nmlfile= ''
	infile= ''
	iy2k= 0
        iarg= 0
        i= 1
	do while (i.le.narg)
	  call getarg (i,optarg)
	  if(optarg.eq.'-1')then
	    iarg= iarg +1
            iarg1= 1
	  elseif(optarg.eq.'-2')then
	    iarg= iarg +2
            iarg2= 1
	    i= i +1
	    call getarg(i,optarg)
	  elseif(optarg.eq.'-n')then
	    iarg= iarg +2
	    i= i +1
	    call getarg(i,optarg)
	    nmlfile= optarg
	  elseif(optarg.eq.'-i')then
	    iarg= iarg +2
	    i= i +1
	    call getarg(i,optarg)
	    infile= optarg
	  elseif(optarg.eq.'-o')then
	    iarg= iarg +2
	    i= i +1
	    call getarg(i,optarg)
	    outfile= optarg
	  elseif(optarg.eq.'-Y')then
	    iarg= iarg +1
	    iy2k= 1
	  elseif(optarg.eq.'-h')then
	    iarg= iarg +1
      write(*,*)' '
      write(*,*)'Namelist: '
      write(*,*)' namelist /nmcdl/ var,'
      write(*,*)'* attr_var_long_name, attr_var_units,'
      write(*,*)'* gattr_desc, gattr_hist,'
      write(*,*)'* date_time_fmt, date_time_type,'
      write(*,*)'* nc_name, time_unlimited,'
      write(*,*)'* map1, map2'
      write(*,*)' '
      write(*,*)'Example namelist:'
      write(*,*)' &nmcdl'
      write(*,*)'   var= ''H'','
      write(*,*)'   date_time_type= ''YM'','
      write(*,*)'   date_time_fmt= ''(18x,I4,I2)'','
      write(*,*)'   attr_var_long_name= ''H'','
      write(*,*)'   attr_var_units= ''per Kelvin'','
      write(*,*)'   gattr_desc= ''Monthly H (year-month) based'
      write(*,*)' on monthly HadISST T and NCEP'
      write(*,*)' Reanalysis E; Period Jan 1979 - Dec 2005'','
      write(*,*)'   nc_name= ''my_H'','
      write(*,*)'   map1= 5,'
      write(*,*)'   map2= 10,'
      write(*,*)'   time_unlimited=F,'
      write(*,*)'   gattr_hist= ''Created by Kevin Keay'','
      write(*,*)' &end'
      write(*,*)' '
            stop
	  endif
	  i= i +1
	enddo
      endif
c
c * Parameter checks
c
      ierr= 0
      inquire (file=nmlfile,exist=lexist)
      if(.not.lexist)then
        write(*,*)'ERROR: Namelist file (-n) not found (',
     *nmlfile(:ilen(nmlfile)),')'
        ierr= ierr +1
      endif
      if(.not.lexist)then
        write(*,*)'ERROR: Conmap file (-i) not found (',
     *infile(:ilen(infile)),')'
        ierr= ierr +1
      endif
c
      if(ierr.gt.0)then
        stop 'ABORT: Due to error(s)'
      endif
c
c * Default values for namelist parameters
c
      var= ''
      attr_var_long_name= ''
      attr_var_units= ''
      gattr_desc= ''
      gattr_hist= 'Conmap to CDL conversion by cmp2cdl4'
      date_time_fmt= ''
      date_time_type= ''
      nc_name= ''
      time_unlimited= .true.
      map1= 1
      map2= maxk
c
c * Read namelist file
c
      open (21,file=nmlfile)
      nmlist= 'nmcdl'
      write(*,*)'Reading namelist: ',nmlist
      read(21,nmcdl,end=1009,err=1008)
1009  continue
      close (21)
      write(*,*)'*** Namelist parameters: ',nmlist
      write(*,*)'var: ',var(:ilen(var))
      write(*,*)'attr_var_long_name: ',
     *  attr_var_long_name(:ilen(attr_var_long_name))
      write(*,*)'attr_var_units: ',
     *  attr_var_units(:ilen(attr_var_units))
      write(*,*)'gattr_desc: ',
     * gattr_desc(:ilen(gattr_desc))
      write(*,*)'gattr_hist: ',
     * gattr_hist(:ilen(gattr_hist))
      write(*,*)'date_time_fmt: ',
     * date_time_fmt(:ilen(date_time_fmt))
      write(*,*)'date_time_type: ',
     * date_time_type(:ilen(date_time_type))
      write(*,*)'nc_name: ',nc_name(:ilen(nc_name))
      write(*,*)'time_unlimited=',time_unlimited
      write(*,*)'map1: ',map1
      write(*,*)'map2: ',map2
c
c * Process namelist values
c
      if(date_time_fmt.eq.'')then
        idt= 0
      else
        if(date_time_type.eq.'YMDH'.or.
     *     date_time_type.eq.'YMD'.or.
     *     date_time_type.eq.'YM'.or.
     *     date_time_type.eq.'YD'.or.
     *     date_time_type.eq.'Y')then
          idt= 1
        else
          write(*,*)'ERROR: date_time_type is invalid'
	  write(*,*)'Choose: YMDH, YMD, YM, YD or Y'
	  stop
	endif
      endif
      if(var.eq.'')then
        var= 'Var'
	write(*,*)' '
        write(*,*)'* Updated - var: ',var(:ilen(var))
      endif
      if(gattr_desc.eq.'')then
        gattr_desc= 'CDL file of '//var(:ilen(var))
	write(*,*)' '
        write(*,*)'* Updated - gattr_desc: ',
     * gattr_desc(:ilen(gattr_desc))
      endif
      ierr= 0
      if(map1.lt.1.or.map1.gt.maxk)then
        write(*,*)'ERROR: map1 should be in range [1,',maxk,']'
	ierr= ierr +1
      endif
      if(map2.lt.1.or.map2.gt.maxk)then
        write(*,*)'ERROR: map2 should be in range [1,',maxk,']'
	ierr= ierr +1
      endif
      if(map2.lt.map1)then
        write(*,*)'ERROR: map2 should be greater than map1'
	ierr= ierr +1
      endif
      if(ierr.gt.0)then
        stop 'ABORT: Due to error(s)'
      endif
c
c * Get current date for creation_date attribute
c   Note: Uses g77 function time8 and routine ctime
      itime8= time8()
      call ctime(itime8,cline2)
      cline3= ':creation_date = "'//cline2(:ilen(cline2))//'" ;'
      nc_creation= cline3(:ilen(cline3))
c
c * Set NetCDF file name
c
      if(nc_name.eq.'')then
        i1= index(outfile,'.cdl')
        if(i1.gt.0)then
          nc_name= outfile(:i1-1)
        else
          nc_name= outfile(:ilen(outfile))
        endif
      else
        i1= index(nc_name,'.nc')
        if(i1.gt.0)then
          nc_name= nc_name(:i1-1)
	  write(*,*)'* Updated - nc_name: ',nc_name(:ilen(nc_name))
        endif
      endif
c
c * Read in maps, their headers and determine number of maps (nk)
c
      open(1,file=infile,form='unformatted')
      write(*,*)'Reading ',infile(:ilen(infile)),' ...'
      k= 0
      do kk=1,min(maxk,map2)
        read(1,end=9)nlats
	if(nlats.gt.mlats)then
	  write(*,*)'ERROR: nlats > mlats (',nlats,mlats,')'
	  stop 'ABORT: Due to error'
	endif
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
	if(nlons.gt.mlons)then
	  write(*,*)'ERROR: nlons > mlons (',nlons,mlons,')'
	  stop 'ABORT: Due to error'
	endif
        read(1)(xlons(i),i=1,nlons)
        read(1)head
ckk        read(1)((dat1(k,i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
        read(1)((dat2(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
	if(kk.ge.map1.and.kk.le.map2)then
	  k= k +1
	  headk(k)= head
          write(*,*)'Map ',k,' : ',headk(k)(:ilen(headk(k)))
	  do j=1,nlats
	    do i=1,nlons
	      dat1(k,i,j)= dat2(i,j)
	    enddo
	  enddo
        endif
      enddo
9     continue
      close(1)
      nkk= kk -1
      write(*,*)'No. of maps read: ',nkk
      nk= k
      write(*,*)'No. of maps to be output: ',nk
      write(*,*)'First header  1: ',headk(1)(:ilen(headk(1)))
      write(*,*)'Last  header ',nk,': ',headk(nk)(:ilen(headk(nk)))
c
c * Set time variable
c
      if(idt.eq.0)then
        do k=1,nk
c * Dummy date: Jan 1 2000 00UTC + 6 hour increment
	  tk(k)= 867816 + 6*(k-1)
	enddo
      else
        do k=1,nk
	  if(date_time_type.eq.'YMDH')then
	    read(headk(k),date_time_fmt)iy,im,id,ih
	  elseif(date_time_type.eq.'YMD')then
	    read(headk(k),date_time_fmt)iy,im,id
	    ih= 0
	  elseif(date_time_type.eq.'YM')then
	    read(headk(k),date_time_fmt)iy,im
	    id= 1
	    ih= 0
	  elseif(date_time_type.eq.'YD')then
	    read(headk(k),date_time_fmt)iy,jday
	    ih= 0
	  elseif(date_time_type.eq.'Y')then
	    read(headk(k),date_time_fmt)iy
	    im= 1
	    id= 1
	    ih= 0
	  endif
	  if(iy2k.eq.1)then
	    if(iy.eq.18)then
	      iy= 1998 
	      write(*,*)'NOTE: Year 18 -> 1998'
	    elseif(iy.eq.19)then
	      iy= 1999 
	      write(*,*)'NOTE: Year 19 -> 1999'
	    else if (iy.eq.20) then
	      iy= 2000
	      write(*,*)'NOTE: Year 20 -> 2000'
	    endif
	  endif
	  if(iy.ge.50.and.iy.le.99)then
	    iy= 1900 +iy
	  elseif(iy.ge.0.and.iy.lt.50)then
	    iy= 2000 +iy
	  endif
	  if(date_time_type.eq.'YD')then
            call jday2date (iy,jday,im,id)
          endif
	  call getdhour (iy,im,id,ih,tk(k))
	enddo
      endif
c
c * If lon 0 and lon 360 are present then 
c   exclude the latter
c
      if(xlons(1).eq.0.and.xlons(nlons).eq.360.)then
        nlonx= nlons -1
	write(*,*)'NOTE: Lon 0 and 360 present - ',
     * 'latter will be excluded'
      else
        nlonx= nlons
      endif
c
c * Write out CDL file
c
      open(2,file=outfile)
      write(*,*)'Writing CDL file ...'
c
c * Header 
c
c * Note: First line *must* be:
c     netcdf CDL_file_name (all on *one* line) e.g. netcdf test.cdl
c
      write(2,80)nc_name(1:ilen(nc_name))
80    format('netcdf ',A,' {')
      write(2,*)' '
      write(2,*)'// NetCDF specification for CDL'
      write(2,*)'// Output NetCDF equivalent of a concatenated conmap',
     * ' file'
      write(2,*)'// Author: Kevin Keay  Date: March 19 2007'
      write(2,*)' '
      write(2,*)'dimensions:'
      write(2,*)'  lon = ',nlonx,' ;'
      write(2,*)'  lat = ',nlats,' ;'
      if(time_unlimited)then
        write(2,*)'  time = UNLIMITED ; // (',nk,' currently)'
      else
        write(2,*)'  time = ',nk,' ;'
      endif
      write(2,*)' '
      write(2,*)'variables:' 
      write(2,*)'  float lon(lon) ;'
      write(2,*)'  float lat(lat) ;'
      write(2,*)'  integer time(time) ;'
      write(2,75)var(:ilen(var))
75    format(1x,2x,'float ',A,'(time,lat,lon) ;')
      write(2,*)' '
      write(2,*)'  lat:units = "degrees_north" ;'
      write(2,*)'  lon:units = "degrees_east" ;'
      isyear0= 1901
      ismon0= 1
      isday0= 1
      write(2,85)isyear0,ismon0,isday0
85    format(1x,'  time:units = "hours since ',I4.4,'-',I2.2,'-',
     * I2.2,' 0" ;')
      write(2,*)' '
      cline= attr_var_long_name
      write(2,76)var(:ilen(var)),cline(:ilen(cline))
76    format(1x,2x,A,':long_name = "',A,'" ;')
      cline= attr_var_units
      write(2,78)var(:ilen(var)),cline(:ilen(cline))
78    format(1x,2x,A,':units = "',A,'" ;')
      write(2,90)var(:ilen(var)),xmiss
90    format(1x,2x,A,':missing_value = ',F7.1,'f ;') ! Note 'f' KK 
c
c * Global attributes
c
      write(2,*)'// global attributes:'
      write(2,*)' '
      write(2,77)gattr_hist(1:ilen(gattr_hist))
77    format(1x,2x,':history = "',A,'" ;') 
      write(2,*)' '
      write(2,81)gattr_desc(1:ilen(gattr_desc))
81    format(1x,2x,':desc = "',A,'" ;') 
      write(2,*)' '
      write(2,*)'  ',nc_creation(:ilen(nc_creation))
      write(2,*)' '
c
c * Variables
c
      write(2,*)'data:'
      write(2,*)' '
      write(2,*)'lon = '
      write(fmt,101)nlonx-1  ! Leave off 360 === 0 i.e. use 1 ... nlons-1
101   format('(',I3,'(F10.4,'',''),F10.4,'';'')')  !KK
      write(2,fmt)(xlons(i),i=1,nlonx)
      write(2,*)' '
      write(2,*)'lat = '
      write(fmt,102)nlats-1
102   format('(',I3,'(F10.4,'',''),F10.4,'';'')')  !KK
      write(2,fmt)(xlats(i),i=1,nlats)
      write(2,*)' '
      write(2,*)'time = '
      if(nk.eq.1)then  ! A single map only
        write(2,'(I8,'';'')')tk(1)
      else
        write(fmt,105)nk-1
ckk105     format('(',I3,'(F5.0,'',''),F5.0,'';'')')  !KK
105     format('(',I3,'(I8,'',''),I8,'';'')')  !KK
        write(2,fmt)(tk(i),i=1,nk)
      endif
      write(2,*)' '

      write(2,108)var(:ilen(var))
108   format(1x,A,' =')
      do k=1,nk-1
        do j=1,nlats-1
          write(fmt,103)nlonx
103       format('(',I3,'(E14.6,'',''))')  !KK
          write(2,fmt)(dat1(k,i,j),i=1,nlonx)
        enddo
        write(fmt,104)nlonx-1
104     format('(',I3,'(E14.6,'',''),E14.6,'','')')  !KK
        write(2,fmt)(dat1(k,i,j),i=1,nlonx)
      enddo
c * Last map - end with ;
      k= nk
      do j=1,nlats-1
        write(fmt,103)nlonx
        write(2,fmt)(dat1(k,i,j),i=1,nlonx)
      enddo
      write(fmt,106)nlonx-1
106   format('(',I3,'(E14.6,'',''),E14.6,'';'')')  !KK
      write(2,fmt)(dat1(k,i,j),i=1,nlonx)
c
      write(2,*)'}'
c
      close(2)
c
      write(*,*)'CDL file: ',outfile(:ilen(outfile))
      stop
c
c * Error messages
c
1008  write(*,*)'ERROR: Problem reading file: ',nmlfile(:ilen(nmlfile)),
     * ' for namelist: ',nmlist
      write(*,*)'       Possibly due to an invalid namelist variable'
c
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

      subroutine getdhour (iy,im,id,ih,dh)
c
c * Purpose: Computes no. of hours since Jan 1 1901 (dh) for a 
c     given date-time (iy,im,id,ih) where iy is a 4 digit year.
c
c * Note: Valid for Jan 1 1901 00UTC onwards.
c     Based on the routine xhour from UCAR
c
c * Author: Kevin Keay 12/6/2006
c
      integer*4 dh
c
      integer*4 xhour0,xhours
c
      iy0= 1901
      im0= 1
      id0= 1
      ih0= 0
      call xhour(iy0,im0,id0,ih0,xhour0)
      call xhour(iy,im,id,ih,xhours)
      dh= xhours -xhour0
ckk      write(*,*)'iy,im,id,xhours,dh: ',iy,im,id,ih,xhours,dh
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

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
c * Notes:
c   (1) Modified to assume 4 digit years; valid for 1901-1-1 0 onwards
c       KK 12/6/2006
c
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

ckk      if (iy.lt.100) then
ckk         inyear = iy + 1900
ckk      else
ckk         inyear = iy
ckk      endif
  
      inyear= iy !KK

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

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine jday2date (iyear,jday,imon,iday)
c
c * Purpose: Given year >= 1600 (iyear) and day of year (1-366) (jday)
c     computes month (imon) and day (iday) of the year
c
c * Author:  Kevin Keay  Date: 22/11/99
c
      integer dpm(12),cpm(12)
      data dpm /31,28,31,30,31,30,31,31,30,31,30,31/
c
cc      if(iargc().eq.0)then
cc	write(*,*)'Usage: jday2date year jday e.g 1996 61 => Mar 1 1996'
cc	write(*,*)'  jday: Julian day 1-366'
cc	write(*,*)'Note: Valid year >= 1600'
cc	stop
	if(iyear.lt.1600)then
	  write(*,*)'ERROR: Invalid year - should be >= 1600'
	  stop 'ABORT: Due to error in jday2date'
	endif
cc        call getarg(2,optarg)
cc	read(optarg,*)jday
	ileap= 0
        if(mod(iyear,400).eq.0.or.iyear.eq.1700)then
	  ileap= 1
  	  dpm(2)= 29
        elseif(mod(iyear,4).eq.0.and.mod(iyear,100).ne.0)then
	  ileap= 1
	  dpm(2)= 29
        endif
	if(ileap.eq.0)then
	  mjday= 365
	else
	  mjday= 366
	endif
	if(jday.lt.1.or.jday.gt.mjday)then
	  write(*,*)'ERROR: Julian day not matched'
	  stop 'ABORT: Due to error in jday2date'
	endif
c
      cpm(1)= dpm(1)
      do m=2,12
	cpm(m)= cpm(m-1) +dpm(m)
      enddo
      if(jday.le.cpm(1))then
	imon= 1
	iday= jday
      else
        do m=2,12
  	  if(jday.gt.cpm(m-1).and.jday.le.cpm(m))then
	    imon= m 
	    iday= jday -cpm(m-1)
	    goto 19
	  endif
        enddo
      endif
19    continue
ckk      write(*,'(I4,''.'',I2.2,''.'',I2.2)')iyear,imon,iday
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
