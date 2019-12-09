      program statcycmm
c
c * Author: Kevin Keay  Date: 9/1/02
c
      parameter (xmiss= 99999.9)   ! conmap missing value code
      parameter (nq=28)   ! No. of quantities in statdatfile
      parameter (num= 200) ! Max. size of conmap grid
      real xlats(num),xlons(num)
      real dat(num,num)
      character*80 infile
      character*80 outfile
      character*80 ch80,head
      character*2 quant
c
      narg= iargc()
c
      if(narg.lt.1)then
        write(*,*)'Usage: statcycmm statdatfile',
     * ' quant outfile'
        write(*,*)'Example: statcycmm statdat.1980.djf.S0510 SD',
     * ' statdat.1980.djf.SD'
        write(*,*)' '
        stop
      endif
c
      call getarg (1,infile)
      call getarg (2,ch80)
      read(ch80,'(A2)')quant
      call getarg (3,outfile)
c      
      open(1,file=infile,form='unformatted')
      do kk=1,nq
        read(1)nlats
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
        read(1)(xlons(i),i=1,nlons)
        read(1)head      ! Save header
	if(head(1:2).eq.quant)then
	  nquant= kk
          read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
	  goto 10
	endif
        read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      enddo
      write(*,'('' ERROR: Quantity '',A2,'' not found'')')quant
      stop 'ABORT due to error'
10    close (1)

c
c * Save data
c
      open(2,file=outfile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head    
      write(2)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      close(2)
      write(*,'('' Output file: '',A)')outfile
c
      stop
      end
