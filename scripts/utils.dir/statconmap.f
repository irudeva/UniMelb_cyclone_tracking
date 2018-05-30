      program statconmap7
c
c * Author: Kevin Keay  Date: 27/3/2000
c
c * Notes:
c
c     (1) 10/7/2006: Added -C option for statdat conmap files.
c     (2) 13/7/2006: Added -1 option for concatenated conmap file input.
c     (3) 27/9/2006: num increased to 400.
c
      parameter (xmiss= 99999.9)   ! NCAR missing value code
      parameter(num= 400)
      real xlats(num),xlons(num)
      real dat(num,num)
      real meanp(num,num), varp(num,num)
      real meanp2(num,num), varp2(num,num)
      real stdev(num,num)
      integer nm(num,num)
      parameter (maxfiles= 4000)
      character*80 infile(maxfiles)
      character*60 avefile,sdfile,varfile,cntfile
      character*80 head1,head2,headx,optarg
      character*5 statlab(4)
      data statlab /'Ave  ','StD  ','Cnt  ', 'Var  '/
c
c * Functions
c
      integer iyadj
c
      narg= iargc()
c
      if(narg.lt.1)then
        write(*,*)'Usage: statconmap [-1][-A][-C][-V] conmapfiles',
     * ' avefile sdfile varfile cntfile'
        write(*,*)' '
        write(*,*)'Version: 7 '
        write(*,*)' '
	write(*,*)'Options: '
	write(*,*)'  1: Concatenated input conmap file (single file)'
	write(*,*)'  A: Output sum(x)= ave*(n) instead of ave'
	write(*,*)'  C: Assumes header is from tstatx - adjust year value'
	write(*,*)'     and output dates as yyyymm (omit dd)' 
	write(*,*)'     Default: Date is unchanged i.e. yymmdd'
	write(*,*)'  V: Output sum((x-xbar)**2)= var*(n-1)',
     * ' instead of var'
        write(*,*)'Example: statconmap -C SD.*.cmp SD.{ave,sd,var,cnt}'
        write(*,*)' '
        stop
      endif
c
      iasum= 0
      ivsum= 0
      icyc= 0
      iopt= 0
      ising= 0
      do i=1,narg
        call getarg (i,optarg)
	if(optarg.eq.'-1')then
	  ising= 1
	  iopt= iopt +1
	elseif(optarg.eq.'-A')then
	  iasum= 1
	  iopt= iopt +1
	elseif(optarg.eq.'-C')then
	  icyc= 1
	  iopt= iopt +1
	elseif(optarg.eq.'-V')then
	  ivsum= 1
	  iopt= iopt +1
	endif
      enddo
c
      nf= narg -4 -iopt
      do i=1,nf
        call getarg (i+iopt,infile(i))
      enddo 
      call getarg (iopt+nf+1,avefile)
      call getarg (iopt+nf+2,sdfile)
      call getarg (iopt+nf+3,varfile)
      call getarg (iopt+nf+4,cntfile)
c
c * Command line parameters
c
cc      open(11,file='fort.11')
cc      do i=1,nf
cc        write(11,'(A)')infile(i)
cc      enddo
cc      write(11,'(A)')avefile
cc      write(11,'(A)')sdfile
cc      write(11,'(A)')varfile
cc      write(11,'(A)')cntfile
cc      close (11)

c
c * Initialise sum arrays
c
      do j=1,num
        do i=1,num
          meanp(i,j)= 0.
          meanp2(i,j)= 0.
          varp(i,j)= 0.
          varp2(i,j)= 0.
	  stdev(i,j)= 0.
          nm(i,j)= 0
        enddo
      enddo
c
c * For -1 option determine the no. of maps (nf)
c
      if(ising.eq.1)then
        open(1,file=infile(1),form='unformatted')
        do k=1,maxfiles
          read(1,end=19)nlats
          read(1)(xlats(i),i=1,nlats)
          read(1)nlons
          read(1)(xlons(i),i=1,nlons)
          read(1)head1
          read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
	enddo
19      continue
        close(1)
        nf= k -1
	write(*,*)'No. of maps in concatenated file: ',nf
      endif
c
c * Compute sample mean of the set of nf data sets
c
      if(ising.eq.1)then
        open(1,file=infile(1),form='unformatted')
      endif
      do k=1,nf
        if(ising.eq.0)then
          open(1,file=infile(k),form='unformatted')
	endif
        read(1)nlats
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
        read(1)(xlons(i),i=1,nlons)
        read(1)head1
        read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
        if(ising.eq.0)then
          close(1)
	endif
        write(*,'('' No. lats, no. lons: '',2I6)')nlats,nlons
        write(*,'(1X,A)')head1
c
        if(icyc.eq.1)then
	  if(nf.eq.1)then
	    read(head1,100)iy,im,id
100         format(42x,3I2)
	    iyr1= iyadj(iy)
	    im1= im
	    id1= id
	    read(head1,110)iy,im,id
110         format(49x,3I2)
	    iyr2= iyadj(iy)
	    im2= im
	    id2= id
	  else
	    if(k.eq.1)then
	      read(head1,100)iy,im,id
	    elseif(k.eq.nf)then
	      read(head1,110)iy,im,id
	    endif
	    iyr= iyadj(iy)
	    if(k.eq.1)then
	      iyr1= iyr
	      im1= im
	      id1= id
	    elseif(k.eq.nf)then
	      iyr2= iyr
	      im2= im
	      id2= id
	    endif
          endif
	else
	  if(nf.eq.1)then
	    read(head1,100)iy,im,id
	    iy1= iy
	    im1= im
	    id1= id
	    read(head1,110)iy,im,id
	    iy2= iy
	    im2= im
	    id2= id
	  else
	    if(k.eq.1)then
	      read(head1,100)iy,im,id
	    elseif(k.eq.nf)then
	      read(head1,110)iy,im,id
	    endif
	    if(k.eq.1)then
	      iy1= iy
	      im1= im
	      id1= id
	    elseif(k.eq.nf)then
	      iy2= iy
	      im2= im
	      id2= id
	    endif
          endif
	endif
c
        do j=1,nlats
          do i=1,nlons
	    if (dat(i,j).ne.xmiss)then
              meanp2(i,j)= meanp2(i,j) +dat(i,j)
	      nm(i,j)= nm(i,j) +1
            endif
          enddo
        enddo
      enddo
      do j=1,nlats
        do i=1,nlons
	  if(nm(i,j).ne.0)then
            meanp(i,j)= meanp2(i,j)/float(nm(i,j))
          else
	    meanp(i,j)= xmiss
	  endif
        enddo
      enddo
c
      if(icyc.eq.0)then
        if(iasum.eq.1)then
ckk          head2= head1(1:24) // 'S' // statlab(1)(2:4) // head1(30:48)
ckk     * // head1(49:)
          write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
          head2= head2(1:24) // 'S' // statlab(1)(2:4) // head2(30:80)
        else
ckk          head2= head1(1:24) // statlab(1) // head1(30:48)
ckk     *   // head1(49:)
          write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
          head2= head2(1:24) // statlab(1) // head2(30:80)
        endif
      else
        if(iasum.eq.1)then
          write(head2,'(A42,I4,I2.2,''-'',I4,I2.2,2x,A23)')head1(1:42),
     * iyr1,im1,iyr2,im2,head1(58:)
          head2= head2(1:24) // 'S' // statlab(1)(2:4) // head2(30:80)
	else
          write(head2,'(A42,I4,I2.2,''-'',I4,I2.2,2x,A23)')head1(1:42),
     * iyr1,im1,iyr2,im2,head1(58:)
          head2= head2(1:24) // statlab(1) // head2(30:80)
	endif
      endif
c
      if(ising.eq.1)then
        close (1)
      endif
c
      open(2,file=avefile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head2    
      if(iasum.eq.1)then
        write(2)((meanp2(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
        write(*,*)'NOTE: Sums output instead of average'
      else
        write(2)((meanp(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      endif
      close(2)
      write(*,'('' No. of files: '',I4)')nf
      write(*,'(1X,A)')head2
      write(*,*) 'Averages saved'
c
c
c * Compute sample variance of the set of nf data sets
c

c * Initialise count array
c
      do j=1,nlats
        do i=1,nlons
          nm(i,j)= 0
        enddo
      enddo
c
      if(ising.eq.1)then
        open(1,file=infile(1),form='unformatted')
      endif
c
      do k=1,nf
        if(ising.eq.0)then
          open(1,file=infile(k),form='unformatted')
	endif
        read(1)nlats
        read(1)(xlats(i),i=1,nlats)
        read(1)nlons
        read(1)(xlons(i),i=1,nlons)
        read(1)head1
        read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
        if(ising.eq.0)then
          close(1)
	endif
        do j=1,nlats
          do i=1,nlons
	    if(dat(i,j).ne.xmiss.and.meanp(i,j).ne.xmiss)then
              varp2(i,j)= varp2(i,j) +(dat(i,j) -meanp(i,j))**2.0
	      nm(i,j)= nm(i,j) +1
	    endif
          enddo
        enddo
      enddo
      do j=1,nlats
        do i=1,nlons
	  if(nm(i,j).gt.1)then
	    if(varp2(i,j).ne.xmiss)then
              varp(i,j)= varp2(i,j)/float(nm(i,j)-1)
	    endif
          else
	    varp(i,j)= xmiss
	  endif
        enddo
      enddo
c
c * Convert to standard deviation
c
      do j=1,nlats
        do i=1,nlons
	  if(varp(i,j).ne.xmiss)then
            stdev(i,j)= sqrt(varp(i,j))
          else
            stdev(i,j)= xmiss
	  endif
        enddo
      enddo
c
c * Save standard deviations
c
      if(icyc.eq.0)then
        write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
        head2= head2(1:24) // statlab(2) // head2(30:80)
      else
        write(head2,'(A42,I4,I2.2,''-'',I4,I2.2,2x,A23)')head1(1:42),
     * iyr1,im1,iyr2,im2,head1(58:)
        head2= head2(1:24) // statlab(2) // head2(30:80)
      endif
c
      open(2,file=sdfile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head2    
      write(2)((stdev(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      close(2)
      write(*,'('' No. of files: '',I4)')nf
      write(*,'(1X,A)')head2
      write(*,*) 'Standard deviations saved'
c
c * Save variance
c
      if(icyc.eq.0)then
        if(ivsum.eq.1)then
        write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
          head2= head2(1:24) // 'S' // statlab(4)(2:4) // head2(30:48)
     * // head2(49:)
        else
        write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
          head2= head2(1:24) // statlab(4) // head2(30:48)
     * // head2(49:)
        endif
      else
        write(head2,'(A42,I4,I2.2,''-'',I4,I2.2,2x,A23)')head1(1:42),
     * iyr1,im1,iyr2,im2,head1(58:)
        if(vsum.eq.1)then
          head2= head2(1:24) // 'S' // statlab(4)(2:4) // head2(30:80)
	else
          head2= head2(1:24) // statlab(4) // head2(30:80)
	endif
      endif
c
      open(2,file=varfile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head2    
      if(ivsum.eq.1)then
        write(2)((varp2(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
        write(*,*)'NOTE: Sums output instead of variance'
      else
        write(2)((varp(i,j),i=1,nlons),j=1,nlats)  ! See conmap.f
      endif
      close(2)
      write(*,'('' No. of files: '',I4)')nf
      write(*,'(1X,A)')head2
      write(*,*) 'Variance saved'
c
c * Save counts at each gridpoint
c
      if(icyc.eq.0)then
        head2= head1(1:24) // statlab(3) // head1(30:48)
     * // head1(49:)
        write(head2,'(A42,3I2.2,''-'',3I2.2,2x,A23)')head1(1:42),
     * iy1,im1,id1,iy2,im2,id2,head1(58:)
        head2= head2(1:24) // statlab(3) // head2(30:80)
      else
        write(head2,'(A42,I4,I2.2,''-'',I4,I2.2,2x,A23)')head1(1:42),
     * iyr1,im1,iyr2,im2,head1(58:)
        head2= head2(1:24) // statlab(3) // head2(30:80)
      endif
c
      open(2,file=cntfile,form='unformatted')
      write(2)nlats
      write(2)(xlats(i),i=1,nlats)
      write(2)nlons
      write(2)(xlons(i),i=1,nlons)
      write(2)head2    
      write(2)((float(nm(i,j)),i=1,nlons),j=1,nlats)  ! See conmap.f
      close(2)
      write(*,'('' No. of files: '',I4)')nf
      write(*,'(1X,A)')head2
      write(*,*) 'Counts saved'
c
      write(*,*)'Output files:'
      write(*,*)'Average:     ',avefile(:ilen(avefile))
      write(*,*)'Stand. dev.: ',sdfile(:ilen(sdfile))
      write(*,*)'Variance:    ',varfile(:ilen(varfile))
      write(*,*)'Count:       ',cntfile(:ilen(cntfile))
c
      stop
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      integer function iyadj (iy)
c
c * Purpose: Adjusts two-digit year in header from statdat file (tstatx).
c     Returns the correct four-digit year.
c
c * Author: Kevin Keay  Date: 11/7/2006
c
      if(iy.eq.19)then
	iyr= 1999
      elseif(iy.eq.20)then
        iyr= 2000
      elseif(iy.gt.20)then
	iyr= 1900 +iy
      elseif(iy.lt.20)then
	iyr= 2000 +iy
      endif
c
      iyadj= iyr
c
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
