      program stat2cdl
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
c   (2) This version reads in a (concatenated) statdat file and extracts
c       the selected variable e.g. SD, and writes a CDL file.
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
      character*10 cvar
c
      narg= iargc()
      if(narg.ne.3)then
	write(*,*)'Usage: stat2cdl var infile outfile'
	write(*,*)'Example: stat2cdl SD statdat.test j.cdl'
	write(*,*)'         ncgen -o SD.nc j.cdl '
	write(*,*)' '
	stop
      else
        call getarg(1,cvar)
        call getarg(2,infile1)
        call getarg(3,outfile)
      endif
c
      mk= 0
      open(1,file=infile1,form='unformatted')
      do k=1,maxk
        read(1,end=9)nlats
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
        read(1)(xlons(i),i=1,nlons)
        read(1)head
        read(1)((dat1(k,i,j),i=1,nlons),j=1,nlats)  ! See conamp.f
	read(head,'(30x,A8)')reanal
	  hist= ':history = "University of Melbourne automatic'
     * // ' cyclone tracking statistics based on '
     * //reanal(1:ilen(reanal))//'" ;'
	read(head,'(A2)')var
        if(cvar(1:2).eq.var)then
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
	  else  ! No entry added yet to this list
	    lvar= ':long_name= "'//cvar(:ilen(cvar))//'"'
	    read(head,'(57x,A8)')var_units
	    var_units= ':units = "'//var_units(:ilen(var_units))//'"'
	  endif
          mk= k
          tk(k)= 0.
          goto 9
        endif
      enddo
9     continue
      close(1)
      if(mk.eq.0)then
        write(*,*)'ERROR: Map for variable ',cvar(:ilen(cvar)),
     * ' not in statdat file'
        stop
      else
        write(*,'(A)')head(:ilen(head))
        write(*,*)'Output map for variable ',cvar(:ilen(cvar)),': ',mk
      endif
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
      write(2,*)'  time = 1 ;'
      write(2,*)' '
      write(2,*)'variables:' 
      write(2,*)'  float lat(lat), lon(lon) ;'
      write(2,*)'  float time(time);'
      write(2,75)cvar(:ilen(cvar))
75    format(1x,2x,'float ',A,'(time,lat,lon) ;')
      write(2,*)' '
      write(2,*)'  lat:units = "degrees_north" ;'
      write(2,*)'  lon:units = "degrees_east" ;'
      ismon= 1
      isday= 1
      isyear= 1
      write(2,85)isyear,ismon,isday
85    format(1x,'  time:units = "years since ',I4.4,'-',I2.2,'-',
     * I2.2,' 0" ;')
      write(2,*)' time:note = "NOTE: time is a dummy variable',
     * ' i.e. 0., 1., 2., ..." ;'
      write(2,*)' '
      write(2,76)cvar(:ilen(cvar)),lvar(1:ilen(lvar))
      write(2,76)cvar(:ilen(cvar)),var_units(1:ilen(var_units))
76    format(1x,2x,A,A,' ;')
      write(2,90)cvar(:ilen(cvar)),xmiss
90    format(1x,2x,A,':missing_value = ',F7.1,'f ;') ! Note 'f' KK 
c
c * Global attributes
c
      write(2,*)' '
      write(2,77)hist(1:ilen(hist))
      write(2,*)' '
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
      write(2,'(F5.0,'';'')')tk(mk)
      write(2,*)' '

      write(2,108)cvar(:ilen(cvar))
108   format(1x,A,' =')
c
c * Single map - end with ;
      k= mk
      do j=1,nlats-1
        write(fmt,103)nlons-1
103     format('(',I3,'(E14.6,'',''))')  !KK
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
      write(*,*)'Input statdat file: ',infile1(:ilen(infile1))
      write(*,*)'Output CDL file:    ',outfile(:ilen(outfile))
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
