      program excycdat
c
c * Author: Kevin Keay  Date: Dec 16 2005
c
c * Notes:
c
c     (1) 28/7/2006: Added single date-time argument
c
      parameter (nhdr=44) ! 15= SUN, 44= g77

      character*120 ch120
      character*80 ch80,infile
      character*9 cdt,ccdt

      if(iargc().ne.2)then
        write(*,*)'Usage: excycdat cycdatfile', 
     * ' "date-time" e.g. "981201 06"'
        stop
      else
        call getarg (1,ch80)
        read(ch80,'(A)')infile
        call getarg (2,ch80)
        read(ch80,'(A)')cdt
	write(*,*)'Requested date-time: ',cdt
      endif
c
      ifound= 0
      open (1,file=infile)
      do while (ifound.eq.0)
        read(1,'(A)',end=19)ch120
        if(ch120(2:7).eq.'CENTRE')then
ckk          read(ch120,'(9x,I4,2I2,1x,I2)')iy,im,id,ih
          read(ch120,'(9x,2x,A9)')ccdt
ckk	  write(*,*)'ccdt: ',ccdt
	  if(cdt.eq.ccdt)then
            ifound= 1
	    write(*,*)'Record matched: ',ccdt
            read(ch120(51:54),*)ncyc
            open (2,file='excyc.dat')
            write(2,'(A)')' '
            write(2,'(A)')ch120(1:ilen(ch120))
            do i=1,nhdr
              read(1,'(A)',end=19)ch120
              write(2,'(A)')ch120(1:ilen(ch120))
            enddo
            do i=1,ncyc
              read(1,'(A)',end=19)ch120
              write(2,'(A)')ch120(1:ilen(ch120))
            enddo
            close (2)
	    write(*,*)'Output file: excyc.dat'
          endif
        endif
      enddo
19    continue
      close (1)
      if(ifound.eq.0)then
        write(*,*)'WARNING: Record not matched'
      endif
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
