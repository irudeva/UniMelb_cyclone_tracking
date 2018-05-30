      program splitcon
c
c * Purpose:  Reads in a concatenated conmap file (unformatted) and writes
c             them out to a set of individual conmap files i.e. splits up
c             the input file.
c
c * Notes:    (1) iargc() and getarg are Sun Fortran routines - they
c                 are also recognised by g77.
c             (2) In general one should check for missing values but
c                 most of the data sets we use do not have them.
c             (3) 11/5/2006: Added -d option
c             (4) 10/6/2006: Added -n, -4 and -l options; changed array definition
c                 to handle up to 1 degree resolution
c             (5) 27/7/2006: Fixed bug with -n (output name contained blanks
c                 if year of form yy rather than yyyy
c             (6) 2/9/2006: Increased max. no. of maps to 3000
c             (7) 13/9/2007: Added -M option (range of maps)
c             (8) 22/2/2008: Added -r option (remove lon 360)
c             (8) 22/2/2008: Added -w option (if lon 0 present, add lon 360)
c             (9) 25/8/2009: Fixed bug with default split - the CMP output
c                 header (head2) was not correctly set
c
c * Author: Kevin Keay  Date: 13/9/2001
c
      parameter (xmiss= 99999.9)  ! NCAR missing value code
c
      parameter (maxc= 3000)      ! Max. number of maps in input conmap file
      parameter (mlons= 721)      ! Max. no. of longitudes
      parameter (mlats= 361)      ! Max.  no. of latitudes
      real xlons(mlons)           ! Longitude array
      real xlats(mlats)           ! Latitude array
      real dat(mlons,mlats)       ! Data array - subscript 1 corresponds
				  ! to longitude and subscript 2 to latitude
      character*80 head           ! conmap text header (80 characters)
c
      real xlons2(mlons)           ! Longitude array (-r -w options)
      real dat2(mlons,mlats)       ! Data array (-r -w options)
c
      character*80 infile,outfile
      character*80 head2
      character*80 base
      character cvar*10, ctype*5, cdate*8, ctime*4
      character cvar0*10, ctype0*5
      character*14 cvar2
      logical lexist
      character*80 optarg
c
      if(iargc().eq.0)then
	write(*,*)'Usage: splitcon [-b base][-d][-h][-L][-l][-m mapno]',
     * ' [-n][-H][-M mapno1,mapno2] concat_conmapfile'
	write(*,*)'Options:'
	write(*,*)' b: base - basename for output maps i.e. base.0001 etc',
     * ' (default: split)'
	write(*,*)' 4: when used with -n use 4 digit time (default: 2 digits)'
	write(*,*)' d: when used with -m the output file is def.cmp'
	write(*,*)' h: when used with -m prints conmap to dump.lis'
	write(*,*)' l: when used with -n the output file is in lowercase',
     * ' (default: preserve case)' 
	write(*,*)' m: mapno - map number to output (default: all)'
	write(*,*)' n: Construct name from header (default: see -b)'
	write(*,*)' r: Remove lon 360'
	write(*,*)' w: If lon 0 present, add lon 360'
	write(*,*)' H: Remove blanks from variable in header',
     * ' e.g. ''T 1000'' -> T1000'
	write(*,*)' L: prints list of maps and conmap headers'
	write(*,*)' M: mapno1,mapno2 - range of map numbers to output',
     * ' (default: all)'
	stop
      else
	base= 'split'
	iall= 1
	imap= 0
	idef= 0
	iname= 0
	ilow= 0
	it4= 0
	ilist= 0
	idump= 0
	imapset= 0
	ihead= 0
	irm360= 0
	iadd360= 0
	narg= iargc()
	i= 0
	do while (i.le.narg)
	  call getarg(i,optarg)
	  if(optarg.eq.'-4')then
	    it4= 1
	  elseif(optarg.eq.'-b')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)base
	  elseif(optarg.eq.'-d')then
	    idef= 1
	  elseif(optarg.eq.'-h')then
	    idump= 1
	  elseif(optarg.eq.'-H')then
	    ihead= 1
	  elseif(optarg.eq.'-L')then
	    ilist= 1
	  elseif(optarg.eq.'-l')then
	    ilow= 1
          elseif(optarg.eq.'-M')then
	    imapset= 1
	    iall= 0
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)imap1,imap2   ! Map range to output
	    write(*,*)'Output map range: ',imap1,' - ',imap2
	  elseif(optarg.eq.'-m')then
	    i= i +1
	    call getarg(i,optarg)
	    read(optarg,*)imap
	    iall= 0
	  elseif(optarg.eq.'-n')then
	    iname= 1
	  elseif(optarg.eq.'-r')then
	    irm360= 1
	  elseif(optarg.eq.'-w')then
	    iadd360= 1
	  endif
	  i= i +1
        enddo
	if(irm360.eq.1.and.iadd360.eq.1)then
	  write(*,*)'ERROR: Specify -r or -w, not both'
	  stop 'ABORT: Due to error'
	endif
	call getarg (narg,optarg)
	infile= optarg
	inquire (file=infile,exist=lexist)
	if(.not.lexist)then
	  write(*,*)'ERROR: File: ',infile,' not found'
	  stop
	endif
      endif
c
c * -L option: Print headers only
c
      if(ilist.eq.1)then
        open(1,file=infile,form='unformatted')
        do k=1,maxc
          read(1,end=5)nlats           ! No. of latitudes
          read(1)(xlats(i),i=1,nlats)   ! Array of latitude values
          read(1)nlons                  ! No. of longitudes
          read(1)(xlons(i),i=1,nlons)   ! Array of longitude values
          read(1)head                   ! 80 character text header
          read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! 2D data array
	  write(*,'(I4,1x,A)')k,head(:ilen(head))
        enddo
5       continue
        close(1)
        write(*,*)'No. of conmap files: ',k-1
        stop
      endif
c
c * Read in concatenated conmap file
c
      write(*,*)'Searching ...'
      ifnd= 0
      ncs= 0
      open(1,file=infile,form='unformatted')
      do k=1,maxc
        read(1,end=10)nlats           ! No. of latitudes
        read(1)(xlats(i),i=1,nlats)   ! Array of latitude values
        read(1)nlons                  ! No. of longitudes
        read(1)(xlons(i),i=1,nlons)   ! Array of longitude values
        read(1)head                   ! 80 character text header
        read(1)((dat(i,j),i=1,nlons),j=1,nlats)  ! 2D data array
c
	head2= head
c
        if(k.eq.1)then
	  if(iadd360.eq.1)then  ! -w option
	    dx= (xlons(nlons) - xlons(1))/(nlons-1)
	    dx2= xlons(1) +nlons*dx
	    iy360= 0
	    if(dx2.eq.360..and.xlons(nlons).ne.360.)then 
	      iy360= 1 ! Add lon 360 (lon 360 is not present)
	    endif
	    if(iy360.eq.0)then
	      write(*,*)'ERROR: -w option: lon 360 is present'
	      stop 'ABORT: Due to error'
	    endif
	    do i=1,nlons
	      xlons2(i)= xlons(i)
	    enddo
	    nlons2= nlons +1
	    xlons2(nlons2)= 360.
	    write(*,*)'NOTE: lon 360 added; new nlons: ',nlons2
	  endif
	  if(irm360.eq.1)then  ! -r option
	    dx= (xlons(nlons) - xlons(1))/(nlons-1)
	    dx2= xlons(1) +(nlons-1)*dx
	    iyrm360= 0
	    if(dx2.eq.360..and.xlons(1).eq.0.and.xlons(nlons).eq.360.)then
	      iyrm360= 1  ! Remove lon 360 if present
	    endif
	    if(iyrm360.eq.0)then
	      write(*,*)'ERROR: -r option: lon 360 is not present'
	      stop 'ABORT: Due to error'
	    endif
	    nlons2= nlons -1
	    do i=1,nlons2
	      xlons2(i)= xlons(i)
	    enddo
	    write(*,*)'NOTE: lon 360 removed; new nlons: ',nlons2
	  endif
	endif
c
c * Write out this map as a separate conmap file
c
        if((iall.eq.1.or.imap.eq.k).or.
     *	(imapset.eq.1.and.(k.ge.imap1.and.k.le.imap2)))then
          ncs= ncs +1
          ifnd= 1
	  if(idef.eq.1)then
	    outfile= 'def.cmp'
	  elseif(iname.eq.1)then ! Construct name from header
cc	    write(head,100)vtype,rtype,iy,im,id,100*ih,vunits
cc100         format(A8,22x,A5,
cc     * 5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,'2.5x2.5DEG')
c
            read(head,110)cvar2,ctype,cdate,ctime
110         format(A14,16x,A5,5x,A8,1x,A4)
c * Remove embedded blanks e.g. 'T 1000' rather than 'T1000'            
            call rmblanks (cvar2)
            cvar= cvar2 
	    if(ihead.eq.1)then
	      write(head2,115)cvar,head(31:)
115           format(A8,22x,A50)
            else
	      head2= head
	    endif
ckk            read(head,110)cvar,ctype,cdate,ctime
ckk110         format(A8,22x,A5,5x,A8,1x,A4)
            cvar0= cvar ! Original text
	    ctype0= ctype 
            if(ilow.eq.1)then ! Use lowercase
              call tolower (cvar)
              call tolower (ctype)
            endif
	    if(it4.eq.1)then
	      lt= 4
	    else
	      lt= 2
	    endif
	    if(cdate(1:2).eq.'  ')then
              write(outfile,120)cvar(:ilen(cvar)),ctype(:ilen(ctype)),
     * cdate(3:),ctime(:lt)
120           format(A,'.',A,'.',A6,A,'.cmp')
	    else
              write(outfile,121)cvar(:ilen(cvar)),ctype(:ilen(ctype)),
     * cdate,ctime(:lt)
121           format(A,'.',A,'.',A8,A,'.cmp')
            endif
	  else
	    write(outfile,'(A,''.'',I4.4)')base(1:ilen(base)),k
	  endif
          write(*,*)'Output file: ',outfile(1:ilen(outfile))
	  open (2,file=outfile,form='unformatted')
	  if(iadd360.eq.1.or.irm360.eq.1)then
	    if(iadd360.eq.1)then  ! -w option: Add lon 360
	      do j=1,nlats
	        dat2(nlons2,j)= dat(1,j)
	      enddo
	      do j=1,nlats
	        do i=1,nlons
	          dat2(i,j)= dat(i,j)
		enddo
	      enddo
	    endif
	    if(irm360.eq.1)then  ! -r option: Remove lon 360
	      do j=1,nlats
	        do i=1,nlons2
	          dat2(i,j)= dat(i,j)
		enddo
	      enddo
	    endif
            write(2)nlats                  ! No. of latitudes
            write(2)(xlats(i),i=1,nlats)   ! Array of latitude values
            write(2)nlons2                 ! No. of longitudes
            write(2)(xlons2(i),i=1,nlons2) ! Array of longitude values
            write(2)head2                  ! 80 character text header
            write(2)((dat2(i,j),i=1,nlons2),j=1,nlats)  ! 2D data array
	  else
            write(2)nlats                  ! No. of latitudes
            write(2)(xlats(i),i=1,nlats)   ! Array of latitude values
            write(2)nlons                  ! No. of longitudes
            write(2)(xlons(i),i=1,nlons)   ! Array of longitude values
            write(2)head2                  ! 80 character text header
            write(2)((dat(i,j),i=1,nlons),j=1,nlats)  ! 2D data array
	  endif
	  close(2)
	  if(imap.eq.k)then
            write(*,*)'Specific map output: ',k
	    if(idump.eq.1)then
	      open (2,file='dump.lis')
              write(2,*)'Header:'
              write(2,*)head(:ilen(head))
              write(2,*)' '
	      write(2,*)'No. of longitudes:',nlons
	      do i=1,nlons
	        write(2,'(I4,1x,F6.2)')i,xlons(i)
	      enddo
              write(2,*)' '
	      write(2,*)'No. of latitudes:',nlats
	      do j=1,nlats
	        write(2,'(I4,1x,F6.2)')j,xlats(j)
	      enddo
              write(2,*)' '
       write(2,*)'Latitude    Longitude    Data at (Longitude,Latitude)'
       write(2,*)'====================================================='
              do j=1,nlats
                do i=1,nlons
                  write(2,*)xlats(j),xlons(i),dat(i,j)
                enddo
              enddo
              close(2)
	    endif
	    goto 10  ! Just this map
          endif
	  if(imapset.eq.1.and.k.eq.kmap2) goto 10 ! End of -M map range
	endif
      enddo
10    continue
      close(1)
      if(iall.eq.1)then
        nc= k -1
        write(*,*)'No. of conmap files: ',nc
      endif
      if(imapset.eq.1)then
        write(*,*)'Map range: ',imap1,' - ',imap2
        write(*,*)'No. of conmap files: ',ncs
      endif
      if(ifnd.eq.1)then
        if(idef.eq.1)then
          write(*,*)'Output file: ',outfile(1:ilen(outfile))
        else
	  if(iname.eq.0) then
            write(*,*)'Base name of output files: ',base(1:ilen(base))
	  else
            write(*,*)'Output name based on conmap header'
	  endif
        endif
	if(idump.eq.1)write(*,*)'Dump file: dump.lis (text)'
      else
        write(*,*)'ERROR: Selected map (',imap,' ) not found'
      endif
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

      subroutine tolower (cvar)
c
      character*(*) cvar
c
      character*1 c
      character*80 ctmp
c
      ctmp= ' '
      do i=1,len(cvar)
        c= cvar(i:i)
        if (c.ne.' ') then
	  ic= ichar(c)
	  if(ic.ge.65.and.ic.le.90)then  ! A-Z
	    ic= ic +32 ! A -> a etc
	    c= char(ic)
	  endif
	endif
	ctmp(i:i)= c
      enddo
c
      cvar= ctmp(1:len(cvar))
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine rmblanks (cvar)
c
      character*(*) cvar
c
      character*1 c
      character*80 ctmp
c
      ctmp= ' '
      j= 0
      do i=1,len(cvar)
        c= cvar(i:i)
        if (c.ne.' ') then
	  ic= ichar(c)
	  j= j +1
	  ctmp(j:j)= c
	  endif
      enddo
c
      cvar= ctmp(1:len(cvar))
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

