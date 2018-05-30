      program catcon2cdl
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
c
c * Author: Kevin Keay  Date: 1/4/2006
c
      parameter (xmiss= 99999.9)  ! NCAR missing value code
      parameter (gmiss= -999.0)   ! GrADS missing value code
      parameter(maxk = 100)       ! Max. no. of maps in concatenated conmap file
      real tk(maxk)               ! Time index for NetCDF (yearly)
      parameter(mlons= 361, mlats= 181) ! Grid size for 1 x 1 deg. data
      real xlats(mlats),xlons(mlons)
      real dat1(maxk,mlons,mlats)
      character*80 infile1,outfile
      character*80 head
      character*80 optarg
      character*80 fmt
      character*8 reanal
      character*2 var
      character*80 lvar
      character*80 var_units
      character*120 hist
      character*320 ylist
      character*4 cyear
      character*3 csea
c
      narg= iargc()
      if(narg.ne.2)then
	write(*,*)'Usage: catcon2cdl infile outfile'
	write(*,*)'Example: catcon2cdl cstatdat.cmp test.cdl'
	write(*,*)'         ncgen -o test.nc test.cdl '
	write(*,*)' '
	stop
      else
        call getarg(1,infile1)
        call getarg(2,outfile)
      endif
c
      open(1,file=infile1,form='unformatted')
      do k=1,maxk
        read(1,end=9)nlats
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
        read(1)(xlons(i),i=1,nlons)
        read(1)head
        read(1)((dat1(k,i,j),i=1,nlons),j=1,nlats)  ! See conamp.f
        write(*,'(A)')head
cSD  LO PMSL             Ave   ERA40       970101-971231  /DEG.LAT.SQ  2.5x2.5DEG
	read(head,'(A2,28x,A8,4x,3I2)')var,reanal,
     * iyear,imon,iday
	if(iyear.eq.19.or.iyear.eq.20)then
	  iyear= iyear + 1980
        elseif(iyear.ge.0.and.iyear.lt.20)then
	  iyear= iyear + 2000 
	elseif(iyear.ge.21.and.iyear.le.99)then
	  iyear= iyear +1900
	endif
	if(k.eq.1)then
	  isyear= iyear
	  ismon= imon
	  isday= iday
	  if(ismon.eq.1.and.isday.eq.1)then
	    csea= 'ANN'
	  elseif(ismon.eq.12.and.isday.eq.1)then
	    csea= 'DJF'
	  elseif(ismon.eq.3.and.isday.eq.1)then
	    csea= 'MAM'
	  elseif(ismon.eq.6.and.isday.eq.1)then
	    csea= 'JJA'
	  elseif(ismon.eq.9.and.isday.eq.1)then
	    csea= 'SON'
	  endif
	  tk(k) = 0.  ! Time origin
	  write(cyear,'(I4)')iyear
	  ylist= ':map_list = "'//csea//' '//cyear  ! Global attribute
	  hist= ':history = "University of Melbourne automatic'
     * // ' cyclone tracking statistics based on '
     * //reanal(1:ilen(reanal))//'" ;'
	  if (var.eq.'SD')then
	    lvar= ':long_name= "Cyclone system density"'
	    var_units= ':units = "1000/(deg.lat.^2)"'
	  elseif (var.eq.'CC')then
	    lvar= ':long_name= "Cyclone Laplacian of MSLP"'
	    var_units= ':units = "hPa/(deg.lat.)^2"'
	  elseif (var.eq.'DP')then
	    lvar= ':long_name= "Cyclone depth"'
	    var_units= ':units = "hPa"'
	  elseif (var.eq.'PC')then
	    lvar= ':long_name= "Cyclone central pressure"'
	    var_units= ':units = "hPa"'
	  elseif (var.eq.'R0')then
	    lvar= ':long_name= "Cyclone radius"'
	    var_units= ':units = "deg.lat."'
	  endif
        else
	  tk(k)= real(iyear-isyear) ! Years since origin
	  write(cyear,'(I4)')iyear
	  ylist= ylist(1:ilen(ylist))//','//cyear
	endif
        write(*,*)var,':',reanal,':',iyear,imon,iday,tk(k)
      enddo
9     continue
      close(1)
      nk= k -1
      write(*,*)'No. of maps: ',nk
      ylist= ylist(1:ilen(ylist))//'" ;'
c
      open(2,file=outfile)
c
c * Header 
c
c * Note: First line *must* be:
c     netcdf CDL_file_name (all on*one* line) e.g. netcdf test.cdl
c
      write(2,80)outfile(1:ilen(outfile))
80    format('netcdf ',A)
      write(2,*)'{'
      write(2,*)' '
      write(2,*)'// NetCDF specification for CDL'
      write(2,*)'// Output NetCDF equivalent of a concatenated conmap',
     * ' file'
      write(2,*)'// Author: Kevin Keay  Date: Apr 1 2006'
      write(2,*)' '
      write(2,*)'dimensions:'
      write(2,*)'  lat = ',nlats,', lon = ',nlons-1,' ;'
      write(2,*)'  time = ',nk,' ;'
      write(2,*)' '
      write(2,*)'variables:' 
      write(2,*)'  float lat(lat), lon(lon) ;'
      write(2,*)'  float time(time);'
      write(2,75)var
75    format(1x,2x,'float ',A2,'(time,lat,lon) ;')
      write(2,*)' '
      write(2,*)'  lat:units = "degrees_north" ;'
      write(2,*)'  lon:units = "degrees_east" ;'
      write(2,85)isyear,ismon,isday
85    format(1x,'  time:units = "years since ',I4.4,'-',I2.2,'-',
     * I2.2,' 0" ;')
      write(2,*)' '
      write(2,76)var,lvar(1:ilen(lvar))
      write(2,76)var,var_units(1:ilen(var_units))
76    format(1x,2x,A2,A,' ;')
      write(2,90)var,xmiss
90    format(1x,2x,A2,':missing_value = ',F7.1,'f ;') ! Note 'f' KK 
c
c * Global attributes
c
      write(2,*)' '
      write(2,77)hist(1:ilen(hist))
      write(2,*)' '
      write(2,77)ylist(1:ilen(ylist))
77    format(1x,2x,A) 
      write(2,*)' '

c
c * Variables
c
      write(2,*)'data:'
      write(2,*)' '
      write(2,*)'lat = '
      write(fmt,101)nlats-1
101   format('(',I3,'(F10.4,'',''),F10.4,'';'')')  !KK
      write(2,fmt)(xlats(i),i=1,nlats)
      write(2,*)' '
      write(2,*)'lon = '
      write(fmt,102)nlons-2  ! Leave off 360 === 0 i.e. use 1 ... nlons-1
102   format('(',I3,'(F10.4,'',''),F10.4,'';'')')  !KK
      write(2,fmt)(xlons(i),i=1,nlons-1)
      write(2,*)' '

      write(2,*)'time = '
      if(nk.eq.1)then  ! A single map only
        write(2,'(F5.0,'';'')')tk(1)
      else
        write(fmt,105)nk-1
105     format('(',I3,'(F5.0,'',''),F5.0,'';'')')  !KK
        write(2,fmt)(tk(i),i=1,nk)
      endif
      write(2,*)' '

      write(2,108)var
108   format(1x,A2,' =')
      do k=1,nk-1
        do j=1,nlats-1
          write(fmt,103)nlons-1
103       format('(',I3,'(E14.6,'',''))')  !KK
          write(2,fmt)(dat1(k,i,j),i=1,nlons-1)
        enddo
        write(fmt,104)nlons-2
104     format('(',I3,'(E14.6,'',''),E14.6,'','')')  !KK
        write(2,fmt)(dat1(k,i,j),i=1,nlons-1)
      enddo
c * Last map - end with ;
      k= nk
      do j=1,nlats-1
        write(fmt,103)nlons-1
        write(2,fmt)(dat1(k,i,j),i=1,nlons-1)
      enddo
      write(fmt,106)nlons-2
106   format('(',I3,'(E14.6,'',''),E14.6,'';'')')  !KK
      write(2,fmt)(dat1(k,i,j),i=1,nlons-1)
c
      write(2,*)'}'
c
      close(2)
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
