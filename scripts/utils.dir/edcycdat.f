      program edcycdat
c
c * Author: Kevin Keay  Date: Jul 27 2006
c
c * Notes:
c
c   (1) 18/8/2011: Added -a -d options
c   (2) 18/2/2014: Added -F option
c   (3) 25/03/2020: Increased g77/gfortran header size to 45 char
c
      parameter (mhdr=200) ! 15= SUN, 45= g77, gfortran
      character*120 chdr(mhdr)
c
      parameter (maxrec=50000) ! Max. no. of cycdat records
c
      character*120 ch120,chinfo,chinfo2
      character*80 ch80,infile,outfile,optarg
      character*10 cdate,cdate1,cdate2
      character*4 ch4
      character*2 ch2
      logical lexist
      double precision xdate,xdate1,xdate2

      narg= iargc()
      if(narg.eq.0)then
        write(*,*)'Usage: edcycdat [-z][-a iyadj]',
     * '[-d "date1 date2" -i incycdatfile', 
     * ' -o edcycdatfile'
        write(*,*)'Options: '
        write(*,*)' a: iyadj - year adjustment e.g. -80 => 99 -> 19'
        write(*,*)' d: date1,date2 - yyyymmddhh',
     *	' e.g. "2002010100 2002022818"'
        write(*,*)' F: ifort cycfile (default: gfortran/g77 cycfile)'
        write(*,*)' S: SUN f77 cycfile (default: gfortran/g77 cycfile)'
        write(*,*)' z: Remove records with zero number of cyclones'
        write(*,*)'Note: Max. no of cycdat records: ',maxrec
        write(*,*)' '
        write(*,*)'Version for gfortran'
        write(*,*)' '
        stop
      else
        infile= ' '
        outfile= ' '
        ioptz= 0
	      ioptd= 0
        cdate1= '1900010100'
        cdate2= '2999123123'
	      iopta= 0
	      iyadj= 0
        nhdr= 45   ! cycdat file header length (18 for SUN, 45 for g77/gfortran
                 ! 45 for ifort (default: gfortran/g77)
        i= 1
        do while (i.le.narg)
          call getarg (i,optarg)
          if(optarg.eq.'-i')then
            i= i +1
            call getarg (i,infile)
            inquire (file=infile,exist=lexist)
            if(.not.lexist)then
              write(*,*)'ERROR: Input file: ',infile(:ilen(infile)),
     *        ' not found'
                    stop 'ABORT: Due to error'
            endif
          elseif(optarg.eq.'-a')then
            i= i +1
            call getarg (i,optarg)
            read(optarg,*)iyadj
            write(*,*)'Adjustment to year: ',iyadj
            iopta= 1
          elseif(optarg.eq.'-d')then
            i= i +1
            call getarg (i,optarg)
            read(optarg,*)cdate1,cdate2
            write(*,*)'Date range for output: ',cdate1,' - ',cdate2
            ioptd= 1
          elseif(optarg.eq.'-F')then
            nhdr=45 ! ifort cycdatfile
            write(*,*)'NOTE: ifort cycdatfile'
          elseif(optarg.eq.'-S')then
            nhdr=18 ! SUN f77 cycdatfile
            write(*,*)'NOTE: SUN f77 cycdatfile'
          elseif(optarg.eq.'-o')then
            i= i +1
            call getarg (i,outfile)
          elseif(optarg.eq.'-z')then
            ioptz= 1
          endif
          i= i +1
        enddo
c
        ierr= 0
        if(ilen(infile).eq.-1)then
          write(*,*)'ERROR: No input file specified'
          ierr= ierr +1
        endif
        if(ilen(outfile).eq.-1)then
          write(*,*)'ERROR: No output file specified'
          ierr= ierr +1
        endif
    	  if(ierr.gt.0)then
          stop 'ABORT: Due to error'
	      endif
      endif
c
      open (1,file=infile)
      open (2,file=outfile)
c
      ni= 0
      no= 0
      nz= 0
      nh= 0
      do k=1,maxrec
        read(1,'(A)',end=19)ch120  ! Blank line
        read(1,'(A)')chinfo  ! CENTRES line
        read(chinfo,'(9x,I6,2I2,1x,I2)')iy4,im,id,ih
ckk        read(chinfo,'(32x,I2)')nhdr
!mm change from 51:54 to 53:56 since we have two digits more in the year
!format
        read(chinfo(53:56),*)ncyc
        do i=1,nhdr
	        read(1,'(A)',end=19)chdr(i)
ckk  	  write(*,*)i,':',chdr(i)
        enddo

c * Assume years 0-47 are 2000-2047 and > 47 are > 1947
!mm	if(iy.lt.48) then
!mm	  iy4= iy +2000
!mm	else
!mm	  iy4= iy +1900
!mm	endif
        write(cdate,'(I4,3I2.2)')iy4,im,id,ih
c
        if(ioptd.eq.1)then
          write(*,*)cdate
          read(cdate,*)xdate
          read(cdate1,*)xdate1
          read(cdate2,*)xdate2
          if(xdate.ge.xdate1.and.xdate.le.xdate2)then
            idate= 1
          else
            idate= 0
          endif
        else
          idate= 1
        endif
c
        if(iopta.eq.1)then
          iya= iy + iyadj
          write(ch2,'(I2)')iya
          chinfo2= chinfo(1:11)//ch2//chinfo(14:) ! New header
          chinfo= chinfo2  ! Overwrite header
          nh= nh +1
        else
          iya= iy
        endif

        if(ioptz.eq.1)then
          if(ncyc.eq.0)then
            izero= 1
            nz= nz + 1
          else
            izero= 0
          endif
              else
          izero= 0
        endif
       
        if(izero.eq.0.and.idate.eq.1)then
          no= no +1
          write(2,'(A)')''
          write(2,'(A)')chinfo(:80) ! To match input record length
          do i=1,nhdr
            write(2,'(A)')chdr(i)(:ilen(chdr(i)))
          enddo
          do i=1,ncyc
            read(1,'(A)')ch120
            write(2,'(A)')ch120(1:ilen(ch120))
          enddo
        else
          do i=1,ncyc
            read(1,'(A)')ch120
          enddo
        endif
      enddo
19    continue
      close (1)
      close (2)
      ni= k -1
ckk      no= ni -nz
      write(*,*)'No. of input records:  ',ni
      write(*,*)'No. of output records: ',no
      if(izero.eq.1)write(*,*)'No. of zero records:   ',nz
      if(nh.gt.0)write(*,*)'No. of header-adjusted records:  ',nh
      write(*,*)'Input file:  ',infile(:ilen(infile))
      write(*,*)'Output file: ',outfile(:ilen(outfile))
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
