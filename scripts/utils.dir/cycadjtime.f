      program cycadjtime
c
c * Author: Kevin Keay  Date: June 14 2006
c
      parameter(num= 800)  ! Allows for ABW's sea ice data files
      integer*4 nlats,nlons
      real xlats(num),xlons(num),dat(num,num)
      character*80 head,head2,infile,outfile,optarg
      character*2 cy2
      character*4 cy4
      logical lexist
c
      if(iargc().eq.0)then
        write(*,*)'Usage: cycadjtime -y yoff -i icmpfile [-o ocmpfile]'
        write(*,*)'Options: '
        write(*,*)' i: input conmap file with proper header for cyclocx'
        write(*,*)'e.g. MSLP header: '
        write(*,*)'PMSL                          ERA40     ',
     * '20020703 0000    MB           2.5x2.5DEG'
        write(*,*)' o: output conmap file with adjusted date-time'
        write(*,*)'    If ocmpfile is not given then ocmpfile',
     * ' will be icmpfile with a modified date-time'
        write(*,*)' y: year offset (-ve or +ve)  - this is defined',
     * ' according to the 4 digit year'
        write(*,*)'Examples of -y option: '
        write(*,*)' (1) NCEP2: -y -1960 would allow: '
        write(*,*)'     1979 -> 0019, ..., 1999 -> 0039, 2000 -> 0040,',
     * ' 2001 -> 0041 (19, ..., 39,40,41 as used by cyclocx)'
        write(*,*)'   i.e. add 1960 to recover the real year'
        write(*,*)' (1) ERA40: -y -1920 would allow: '
        write(*,*)'     1957 -> 0037, ..., 1999 -> 0079, 2000 -> 0080,',
     * ' 2001 -> 0081 (37, ..., 79,80,81 as used by cyclocx)'
        write(*,*)'   i.e. add 1920 to recover the real year'
        write(*,*)'Note: Both of these choices preserve *leap* years'
        write(*,*)'Examples:'
        write(*,*)'(1) cycadjtime -y -1960',
     * ' -i pmsl.ncep2.2000013118.cmp -o j40011318.cmp'
        write(*,*)'(2) cycadjtime -y -1920 -i pmsl.era40.2000013118.cmp'
        write(*,*)'    Output file is: pmsl.era40.0080013118.cmp'
        stop
      else
         ioutf= 0
         iopt_i= 0
         iopt_y= 0
         iyoff= 0
         i= 1
         do while (i.le.iargc())
           call getarg (i,optarg)
           if(optarg.eq.'-i')then
             iopt_i= 1
             call getarg (i+1,optarg)
             infile= optarg
             i= i +1
           elseif(optarg.eq.'-o')then
             call getarg (i+1,optarg)
             outfile= optarg
             ioutf= 1
             i= i +1
           elseif(optarg.eq.'-y')then
             iopt_y= 1
             call getarg (i+1,optarg)
             read(optarg,*)iyoff
             i= i +1
           endif
           i= i +1
         enddo
      endif
c
      ierr= 0
      if(iopt_i.eq.0)then
        write(*,*)'ERROR: Need to specify -i option'
        ierr= ierr +1
      endif
      if(iopt_y.eq.0)then
        write(*,*)'ERROR: Need to specify -y option'
        ierr= ierr +1
      endif
      if(ierr.gt.0)then
        stop 'ABORT: Due to error(s) in cycadjtime'
      endif
c
      inquire(file=infile,exist=lexist)
      if(.not.lexist)then
        write(*,*)'ERROR: Input file ',infile(:ilen(infile)),'not found'
        stop 'ABORT: Due to error in cycadjtime'
      endif
c
c * Read input file
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
c * Assume input file name is: var.reanal.date-time.cmp
c   where date-time is: yyyymmddhh (or perhaps yymmddhh)
c     
      lf= ilen(infile)
      iy2= 0
      iy4= 0
      if(infile(lf-12:lf-12).eq.'.')iy2= 1 ! 2 digit year in name
      if(infile(lf-14:lf-14).eq.'.')iy4= 1 ! 4 digit year in name
      if(iy2.eq.1)then
        read(infile(lf-11:lf-10),'(I2)')iyear
      elseif(iy4.eq.1)then
        read(infile(lf-13:lf-10),'(I4)')iyear
      endif
c
      iyear2= iyear + iyoff  ! Adjusted year
c
c * Adjust header
c
cPMSL                          ERA40     20020703 0000    MB           2.5x2.5DEG
c
      if(iy2.eq.1)then
        write(cy2,'(I2.2)')iyear2
        head2= head(1:40)//'  '//cy2//head(45:)
      elseif(iy4.eq.1)then
        write(cy4,'(I4.4)')iyear2
        head2= head(1:40)//cy4//head(45:)
      endif
c
      write(*,*)' '
      write(*,*)'Year offset: ',iyoff
      write(*,*)'Input header:  ',head(:ilen(head))
      write(*,*)'Output header: ',head2(:ilen(head2))
c
c * If no -o option, create output file name based on input file name
c
      if(ioutf.eq.0)then
        write(cy4,'(I4.4)')iyear2
        outfile= infile(1:lf-14)//cy4//infile(lf-9:)
      endif
c
c * Write output file 
c
      open(2,file=outfile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head2
      write(2)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      close(2)
c
      write(*,*)' '
      write(*,*)'Input file:  ',infile(:ilen(infile))
      write(*,*)'Output file: ',outfile(:ilen(outfile))
c
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

