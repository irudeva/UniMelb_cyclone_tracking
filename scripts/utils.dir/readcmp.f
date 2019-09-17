      program readcmp
c
c * Notes
c
c     (1) 20/7/2006 Added -F and -o options
c
c * Author: Kevin Keay  Date: 23/12/98
c
      parameter(num= 800)  ! Allows for ABW's sea ice data files
      integer*4 nlats,nlons
      real xlats(num),xlons(num),dat(num,num)
      character*80 head,infile,outfile,optarg,ofmt
      character*10 fmt
      logical lexist
c
      if(iargc().eq.0)then
        write(*,*)'Usage: readcmp [-F fmt][-o outputfile] cmpfile'
      write(*,*)'Options: '
      write(*,*)' F: Fortran format for listing e.g. ''(F10.4)'''
      write(*,*)' o: Output listing file'
            write(*,*)'Example: readcmp -F ''(F10.4)'' pmsl.cmp'
            stop
          else
            ifmt= 0
      iout= 0
      i= 1
      do while (i.lt.iargc())
              call getarg (i,optarg)
        if(optarg.eq.'-F')then
          i= i +1
          call getarg (i,optarg) 
          read(optarg,'(A)')fmt
              ofmt= '(''('',I3,'' - '',I3,'') '',10'//
     +        fmt(1:ilen(fmt))//')'
        ifmt= 1
        elseif(optarg.eq.'-o')then
          i= i +1
          call getarg (i,optarg) 
          outfile= optarg
          iout= 1
        endif
        i= i +1
      enddo
      call getarg (iargc(),optarg) 
      infile= optarg
      inquire(file=infile,exist=lexist)
      if(.not.lexist)then
        write(*,*)'ERROR: Input file ( ',infile(:ilen(infile)),' )'//
     +  ' not found'
        stop 'ABORT: Due to error'
      endif
          endif
c
c * Read the input 'conmap' file
c
      open(1,file=infile,form='unformatted')
      read(1)nlats
      read(1)(xlats(i),i=1,nlats)
      read(1)nlons
      read(1)(xlons(i),i=1,nlons)
      read(1)head
      read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      close(1)
c
      if(iout.eq.0)then
        iu= 6
      else
        iu= 2
        open (iu,file=outfile)
      endif
c
      write(iu,'(A)')head
      write(iu,*)'Lats (',nlats,')'
      do i=1,nlats,10
        i1= i
        i2= i1 + 10 -1
        if(i2.gt.nlats) i2= nlats
        write(iu,'(''('',I3,'' - '',I3,'') '',10F8.3)')
     * i1,i2,(xlats(ii),ii=i1,i2)
      enddo
      write(iu,*)'Lons (',nlons,')'
      do i=1,nlons,10
        i1= i
        i2= i1 + 10 -1
        if(i2.gt.nlons) i2= nlons
        write(iu,'(''('',I3,'' - '',I3,'') '',10F8.3)')
     * i1,i2,(xlons(ii),ii=i1,i2)
      enddo
      do j=1,nlats
        write(iu,*)'Data for Lat= ',xlats(j),' (',j,')'
	do i=1,nlons,10
	  i1= i
	  i2= i1 + 10 -1
	  if(i2.gt.nlons) i2= nlons
	  if(ifmt.eq.0)then
            write(iu,'(''('',I3,'' - '',I3,'') '',10G12.4)')
     * i1,i2,(dat(ii,j),ii=i1,i2)
          else
            write(iu,ofmt)
     * i1,i2,(dat(ii,j),ii=i1,i2)
	  endif
	enddo
      enddo
c
      if(iout.eq.1)then
        close (iu)
        write(*,*)'Output file: ',outfile(:ilen(outfile))
      endif
c
      stop
      end
      
c))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      integer function ilen (ch)
      character*(*) ch
      do i=len(ch),1,-1
        if(ch(i:i).ne.' ')then
          ilen= i
          return
        endif
      enddo 
      ilen= -1 
      return
      end

c))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

