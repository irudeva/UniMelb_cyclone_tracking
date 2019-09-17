      program trk2map
c
c * Purpose: Reads in a track file and passes selected tracks to a new track file
c   (trkdat format) and a mapline file (mapline format).
c   
c * Notes:
c   (1) 7/8/2006: Added a number of new options: -n, -r
c   (2) 16/8/2006: Added -d option: min. track duration in days; computed
c       from date-hour information
c   (3) 17/9/2006: A comma-delimited (comma separated value or CSV) version
c       of the track data is written to: trk.csv
c       These options apply to CSV file only:
c       Added -T option: include track header line with a T in column 1
c       Added -L option: include label row
c   (4) 18/9/2006: Added -Y option - years 18-21 -> 1998-2001
c   (5) 25/4/2007: Added -R option - alters -r option such that
c       *any* track point in the region is passed
c       Not: With -r a track is passed if the endpoints are within the region
c       Give -r and -R together in this case
c   (6) 28/4/2007: Added -D option - limited debug - see: fort.20
c       Also changed header format of mapline file to be consistent with 
c       kmapline
c   (7) 29/9/2007: Added -N option; specified track numbers are read
c       from a file
c   (8) 14/1/2009: Added -y option; specify selected year for tabulation of track counts
c       by month
c   (9) 18/1/2009: Added -S option; sets nhdr=18 for SUN f77 otherwise nhdr=66 for g77
c       Added -A option; works with -R only; effectively if a track passes 
c       through the region then the whole track is passed to the mapline and CSV files
c  (10) 4/8/2009: Removed -b and -d options
c
c * Author: Kevin Keay
c
c * Date: 25/5/2001
c
c
cc      implicit none
c
      integer maxinkeep, maxnp, nhdr, maxtrk
      parameter (maxinkeep= 60000) ! Max. no. of tracks (see inkeep)
      parameter (maxnp= 200)      ! Max. no. of track points per track
c * See: -S option
ckk      parameter (nhdr=66)         ! Trackfile header - 18 for SUN, 66 for g77
      parameter (maxtrk=500000)   ! Max. no. of tracks to read
c
      real tlats(maxinkeep,maxnp),tlons(maxinkeep,maxnp) ! For mapline
c
      character*120 ctrkdat(maxnp)! Holds track data in text form
c
      character*120 cline        ! To hold a text line
      character*120 cline2       ! To hold a text line
      character*120 ctrk         ! To hold a track header line
      character*120 clabel       ! To hold a track label line
      character*80 infile        ! Input track file
      character*80 outfile       ! Output edited track file
      character*80 mapfile       ! Output mapline file
      character*80 csvfile       ! Output CSV file
      parameter (csvfile='trk.csv')
      character*80 trknfile     ! Track number file (-N option)
      character*80 head 
      character*120 hdrn         ! To hold a track header line (-n option)
c
      integer idbg
      parameter (idbg= 0)          ! 1= Turn on debug in routine genlst
      integer iy2k
      parameter (iy2k= 0)          ! yy format (not yyyy)
      integer date1,hr1,date2,hr2

      integer it,track,itrk,utrk,itrkf,ntrkl,imatch
c
      integer maxntrkl
      parameter (maxntrkl=200)
      integer trklist(maxntrkl) ! For -N option
c
      integer np,i,j,k
c
      character*80 optarg
      character*20 chemi         ! Hemisphere and latitude band e.g 'N30.5-78.3'
      character*1 ch1            ! Holds hemisphere i.e. N,S (or G)
      real xlat1,xlat2           ! Latitude band from chemi *and* latitude limits for -r option
      real xlon1,xlon2           ! Longititude limits for -r option
      integer trkinc             ! Keep every trkinc-th track
      integer ivalid,ikeep,inkeep
      real xlona,xlonb,xlata,xlatb
      integer ireg
      real reg(4)                ! Region array (-r option)
      real dmin
      integer idmin
      integer irany, irpt, kr
      integer idebug
c
      character*12 cdatehr0,cdatehr1,cdatehr2
      integer i1,i2, idash, narg
      integer iy,im,id,ih,idval1,idval2
      real dval1,dval2,dur
      real temp
c
      real t,x,y,p,c,dp,rd,up,vp
      integer da,hr,iop
      integer itcsv,ilcsv,iyfix
c
      real xmiss ! conmap missing value code
      parameter (xmiss=99999.9)
c
      logical lreg
      logical lysel
 
c * Functions

      integer iargc
      integer ilen
c
      if(iargc().eq.0)then
    	write(*,*)'Usage: trk2map [-b band][-I trkinc][-n trackno]',
     * '[-N tracknfile][-r "lon1,lon2,lat1,lat2"][-D][-L]',
     * '[-R][-T][-Y]',
     * ' trkfile trkfile_ed maplinefile'
        write(*,*)'(Ver. 1.5)'
        write(*,*)'See also: trk.csv (comma-delimited output file)'
	write(*,*)'Options:'
        write(*,*)'  A: Only valid with combination -r -R; passes',
     * ' whole track to mapline file and CSV file'
        write(*,*)'  D: Limited debug - see: fort.20 (and .8,.10)'
	write(*,*)'  I: track increment e.g. 2 (keep every 2nd track)',
     * ' (default: 1)'
        write(*,*)'  L: For CSV file include a label line as'
        write(*,*)'     the first line (default: no label line)'
        write(*,*)'  N: tracknfile: A list of track numbers'
        write(*,*)'  R: The mapline and CSV files contain track points'
	write(*,*)'     which lie within the region given by the'//
     +' -r option'
        write(*,*)'  S: SUN f77 trkfile (default: g77 trkfile)'
        write(*,*)'  T: For CSV file include a track header line for'
        write(*,*)'     each track (default: no headers)'
        write(*,*)'  Y: If yy = 18-21 -> 1998-2001 otherwise'
        write(*,*)'     yy 50-99 -> 1950-1999, yy 01-49 -> 2001-2049'
        write(*,*)'  n: trackno: Track number as given in trkfile',
     * ' e.g. -n 346'
        write(*,*)'  r: Region for track endpoints'
        write(*,*)'  y: Year for count tabulation e.g. 2008'
	write(*,*)'Examples: '
        write(*,*)'(1) trk2map -r "0,90,10,60" trkdat.90.jja',
     * ' 90.jja.ed 90.jja.reg.map'
        write(*,*)'Retain tracks whose endpoints lie in 0-90 E',
     * ' and 10-60 N'
        write(*,*)'(2) trk2map -R -r "0,90,10,60" trkdat.90.jja',
     * ' 90.jja.ed 90.jja.reg.map'
        write(*,*)'Retain only those track points which lie in 0-90 E',
     * ' and 10-60 N'
        write(*,*)'(3) trk2map -n 346 trkdat.90.jja',
     * ' 90.jja.ed 90.jja.track_346.map'
        write(*,*)'Retain track with ID number 346 from trkdat.90.jja'
        write(*,*)'(4) trk2map -R -r "0,90,10,60" -y 2008 trkdat.2008',
     * ' 2008.ed 2008.map'
        write(*,*)'Pass track points to tmap and trk.csv',
     * ' that lie in the regiona and start in 2008'
        write(*,*)'(5) trk2map -A -R -r "120,240,20,80" -y 2008 tj',
     * ' ted tmap'
        write(*,*)'All tracks that pass through the region are output',
     * ' to tmap and trk.csv'
	stop
      endif

c * Defaults

      nhdr= 66   ! trkfile header length (66 ro SUN
      trkinc= 1  ! Track increment of 1
      chemi= 'G' ! Global band
      itrk= 0    ! Flag for -n option (1 if used)
      itrkf= 0   ! Flag for -N option (1 if used)
      utrk= 0    ! This will be >0 with -n option
      ireg= 0    ! Flag for -r option (1 if used)
      dmin= 0.   ! Min. track duration
      itcsv= 0
      ilcsv= 0
      iyfix= 0
      irany= 0   ! -R option (requires -r too)
      do i=1,4
        reg(i)= xmiss
      enddo
      iysel= 0
      lysel= .true.
      iopty= 0
      iallp= 0
      idebug= 0

c * Process arguments

      narg= iargc()
      i= 1
      do while (i.le.narg)
	call getarg(i,optarg)
	if(optarg.eq.'-I')then
	  i= i +1
	  call getarg(i,optarg)
	  read(optarg,*)trkinc
	elseif(optarg.eq.'-n')then
	  i= i +1
	  call getarg(i,optarg)
	  read(optarg,*)utrk
	  itrk= 1
	elseif(optarg.eq.'-r')then
	  i= i +1
	  call getarg(i,optarg)
	  read(optarg,*,end=29)(reg(j),j=1,4)
29        continue
          do j=1,4
	    if(reg(j).eq.xmiss)then
	      write(*,*)'ERROR: -r option requires 4 values'
	      write(*,*)j-1,' values (',(reg(k),k=1,j-1),') given'
	      stop 'ABORT: Due to error'
	    endif
	  enddo
	  xlon1= reg(1)
	  xlon2= reg(2)
	  xlat1= reg(3)
	  xlat2= reg(4)
	  write(*,*)'-r option: Region is: '
	  write(*,*)'  Lons: ',xlon1,' - ',xlon2
	  write(*,*)'  Lats: ',xlat1,' - ',xlat2
	  ireg= 1
	  chemi= 'R'
	  iwrap= 0
c * This allows crossing lon 0 e.g. xlon1= 260, xlon2= 90 
	  if(xlon1.gt.xlon2)then
	    iwrap= 1
	  endif
	elseif(optarg.eq.'-y')then
	  i= i +1
	  call getarg(i,optarg)
	  read(optarg,*)iysel
	  iopty= 1
	elseif(optarg.eq.'-A')then
	  iallp= 1
	elseif(optarg.eq.'-D')then
	  idebug= 1
	elseif(optarg.eq.'-L')then
	  ilcsv= 1
	elseif(optarg.eq.'-N')then
	  i= i +1
	  call getarg(i,optarg)
	  read(optarg,*)trknfile
	  open (1,file=trknfile)
	  do i=1,maxntrkl
	    read(1,*,end=399)trklist(i)
	  enddo
399       continue
	  close (1)
	  ntrkl= i -1
	  write(*,*)'No. of tracks to match: ',ntrkl
	  itrkf= 1
	elseif(optarg.eq.'-R')then
	  irany= 1
	elseif(optarg.eq.'-S')then
	  nhdr= 18  ! SUN f77 trackfile
	  write(*,*)'NOTE: SUN f77 trackfile'
	elseif(optarg.eq.'-T')then
	  itcsv= 1
	elseif(optarg.eq.'-Y')then
	  iyfix= 1
	endif
	i= i +1
      enddo

c * File arguments (last 3 on command line)

      call getarg(narg-2,infile)
      call getarg(narg-1,outfile)
      call getarg(narg,mapfile)
c
c
      write(*,*)'Reading tracks ...'
c
c * Open files

      open(2,file=infile)
      open(3,file=outfile)
      open(4,file=mapfile,form='unformatted')
      open(7,file=csvfile)
      if(ilcsv.eq.1)then
        write(7,280)
cc280      format('Track,k,t,da,hr,iop,x,y,p,c,dp,rd,up,vp')
280     format('Track,Track-point,Time,Date,Hour,IO,Lon,Lat,',
     * 'PC,CC,DP,R0,UE,UN')
      endif

c * 'Keep' counters

      ikeep= 0
      inkeep= 0

c * Information lines (header)      
      do j=1,nhdr
         read(2,'(A)')cline 
         write(3,'(A)')cline(1:ilen(cline))
      enddo

c * Start to read in a set of tracks
      do it=1,maxtrk
c
        ivalid= 0  ! Flag for 'valid' track
c
c * Read track line
        read(2,'(A)',end=9)ctrk
        read(ctrk,100)track,np
100     format(6x,I5,44x,I4)
cc        print*,'Track: ',track,'   No. of points: ',np	 

c * Skip blank and header lines  
        read(2,'(A)')cline 
        read(2,'(A)')clabel

c * Read in data for this track

        do k=1,np
 	  read(2,'(A)')ctrkdat(k)
        enddo

c * Skip blank line
        read(2,'(A)')cline     

c * Track has been read in

c   (1) Extract date-hour information and
      
        read(ctrk,'(A,1x,I6,I4,3x,I6,I4,A)')
     * ctrk(1:59),date1,hr1,date2,hr2,ctrk(84:)
        write(cdatehr1,'(I6,I4)')date1,hr1
        write(cdatehr2,'(I6,I4)')date2,hr2

        call extrdate (iy2k,cdatehr1,iy,im,id,ih)
	if(idebug.eq.1)then
	write(10,*)'it,cdatehr1,iy:',it,cdatehr1,iy
	write(10,*)'iyfix:',iyfix
	endif
        if(iyfix.eq.1)then
	  if(iy.ge.18.and.iy.le.21)then
	    iy= iy +1980
	  elseif(iy.ge.0.and.iy.le.49) then
	    iy= iy +2000
	  else
	    iy= iy +1900
	  endif
	else
	  if(iy.ge.0.and.iy.le.49) then
	    iy= iy +2000
	  else
	    iy= iy +1900
	  endif
	endif
c
c * Keep track if it starts in this year (-y option)
	if(iopty.eq.1)then
	  lysel= .false.
	  if(iy.eq.iysel)then
	    lysel= .true.
	  endif
	endif

c *  -r with -R option: at least one point lies in region
          if(irany.eq.1.and.lysel)then
	    irpt= 0
	    do k=1,np
              read(ctrkdat(k),110)xlonk,xlatk
110           format(44x,2F9.0)
              if(iwrap.eq.0)then
	        lreg= ((xlatk.ge.xlat1.and.xlatk.le.xlat2)
     * .and.(xlonk.ge.xlon1.and.xlonk.le.xlon2))
              else
	        lreg= ((xlatk.ge.xlat1.and.xlatk.le.xlat2)
     * .and.(xlonk.ge.xlon1.or.xlonk.le.xlon2))
	      endif
              if(lreg)then
	        ivalid= 1
	        ikeep= ikeep +1
	        irpt= 1  ! at least one point in region
		goto 500 
	      endif
	    enddo
500         continue
	  endif

c * Debug
        if(idebug.eq.1)then
          write(8,*)'track: ',track,' np: ',np,
     *' ivalid=',ivalid,' chemi=',chemi(:ilen(chemi)),' trkinc=',trkinc,
     *   ' irpt: ',irpt,' iallp: ',iallp,' lysel: ',lysel
        endif

c * Keep every *valid* trkinc-th track so long as it is >= dmin (idmin=1)

        if(ivalid.eq.1.and.ikeep.eq.trkinc.and.lysel)then
	  ikeep= 0 ! Reset 'keep' flag
	  inkeep= inkeep +1
	  if(inkeep.gt.maxinkeep)then
	    write(*,*)'WARNING: Max. no. of output tracks exceeded'
	    write(*,*)'See: maxinkeep (',maxinkeep,')'
	    goto 9  ! Exit 
	  endif

c   (3) Write out track data

          write(3,'(A)')ctrk(1:ilen(ctrk))
          write(3,'(A)')cline(1:ilen(cline))
          write(3,'(A)')clabel(1:ilen(clabel))
          do k=1,np
            write(3,'(A)')ctrkdat(k)(1:ilen(ctrkdat(k)))
          enddo
          write(3,'(A)')cline(1:ilen(cline))

c   (4) Write out track in mapline format
c       Need (lon,lat) pairs

          if(irpt.eq.1)then 
	        ! -R option : Output all points that lie in region
	        ! If -A option given then output whole track
		! Note: -R and -A don't require endpoints in region
	    kr= 0
            do i=1,np
              read(ctrkdat(i)(1:ilen(ctrkdat(i))),300)
     * t,da,hr,iop,x,y,p,c,dp,rd,up,vp
	      xlona= x
	      xlata= y
	      if(iallp.eq.1)then ! If -A option given then output whole track
	        lreg=.true.
	      else ! If -R option given then output trackpoints in region
                if(iwrap.eq.0)then
	          lreg= ((xlata.ge.xlat1.and.xlata.le.xlat2)
     * .and.(xlona.ge.xlon1.and.xlona.le.xlon2))
                else
	          lreg= ((xlata.ge.xlat1.and.xlata.le.xlat2)
     * .and.(xlona.ge.xlon1.or.xlona.le.xlon2))
	        endif
	      endif
cc	    if((xlata.ge.xlat1.and.xlata.le.xlat2)
cc     * .and.(xlona.ge.xlon1.and.xlona.le.xlon2))then
              if(lreg)then
	        kr= kr +1
                tlats(inkeep,kr)= xlata
                tlons(inkeep,kr)= xlona
	      endif
	    enddo
ckk	  write(head,'(''Track: '',I4,'' ('',I4,'')'')')track,inkeep
	    write(head,'(''Track: '',I5,'' ('',I5,'')'')')track,inkeep
            if(itrk.eq.1)then
              write(4)hdrn
	    else
              write(4)head
	    endif
            write(4)kr  ! No. points in region (-r with -R)
	                ! or all points (-r with -R and -A)
            write(4)(tlats(inkeep,i),i=1,kr)
            write(4)(tlons(inkeep,i),i=1,kr)
	  endif
 
c   (5) Comma-delimited (test version)
c
          if(itcsv.eq.1)then
c * Convert to 4 digit year 
            if(iyfix.eq.1)then
	      call extrdate (iy2k,cdatehr1,iy,im,id,ih)
	      if(iy.ge.18.and.iy.le.21)then
	        iy= iy +1980
	      elseif(iy.ge.50.and.iy.le.99)then
	        iy= iy +1900
	      elseif(iy.ge.0.and.iy.le.49)then
	        iy= iy +2000
	      endif
	      date1= iy*10000+im*100+id
	      call extrdate (iy2k,cdatehr2,iy,im,id,ih)
	      if(iy.ge.18.and.iy.le.21)then
	        iy= iy +1980
	      elseif(iy.ge.50.and.iy.le.99)then
	        iy= iy +1900
	      elseif(iy.ge.0.and.iy.le.49)then
	        iy= iy +2000
	      endif
	      date2= iy*10000+im*100+id
	    else
	      call extrdate (iy2k,cdatehr1,iy,im,id,ih)
	      if(iy.ge.0.and.iy.le.49) then
	        iy= iy +2000
	      else
	        iy= iy +1900
	      endif
	      date1= iy*10000+im*100+id
	      call extrdate (iy2k,cdatehr2,iy,im,id,ih)
	      if(iy.ge.0.and.iy.le.49) then
	        iy= iy +2000
	      else
	        iy= iy +1900
	      endif
	      date2= iy*10000+im*100+id
	    endif

            write(7,290)track,np,date1,hr1,date2,hr2
290         format('T',I5,',',I3,',',I8,',',I4,',',I8,',',I4)
          endif
	  kr= 0
          do k=1,np
            read(ctrkdat(k)(1:ilen(ctrkdat(k))),300)
     * t,da,hr,iop,x,y,p,c,dp,rd,up,vp
c300         format(1x,F9.4,2x,I6,1x,I4,12x,I2,7x,8(1x,F8.3))
300         format(1x,F9.4,2x,I6,1x,I4,12x,I2,7x,9(F9.3))
            call convdate (da,id,im,iy) 
            if(iyfix.eq.1)then
	      if(iy.ge.18.and.iy.le.21)then
	        iy= iy +1980
	      elseif(iy.ge.50.and.iy.le.99)then
	        iy= iy +1900
	      elseif(iy.ge.0.and.iy.le.49)then
	        iy= iy +2000
	      endif
	      da= iy*10000+im*100+id
	    else
	      if(iy.ge.0.and.iy.le.49) then
	        iy= iy +2000
	      else
	        iy= iy +1900
	      endif
	      da= iy*10000+im*100+id
	    endif
	    if(irpt.eq.0)then
              write(7,310)track,k,t,da,hr,iop,x,y,p,c,dp,rd,up,vp
	    else ! -R : write out point if in region (see -r)
	      xlona= x
	      xlata= y
cc	      if((xlata.ge.xlat1.and.xlata.le.xlat2)
cc     * .and.(xlona.ge.xlon1.and.xlona.le.xlon2))then
	      if(iallp.eq.1)then  ! -A with -R
                lreg=.true.  ! Check this - KK
	      else
                if(iwrap.eq.0)then
	          lreg= ((xlata.ge.xlat1.and.xlata.le.xlat2)
     * .and.(xlona.ge.xlon1.and.xlona.le.xlon2))
                else
	          lreg= ((xlata.ge.xlat1.and.xlata.le.xlat2)
     * .and.(xlona.ge.xlon1.or.xlona.le.xlon2))
	        endif
	      endif
	      if(lreg)then
	        kr= kr +1
                write(7,310)track,kr,t,da,hr,iop,x,y,p,c,dp,rd,up,vp
	      endif
	    endif
cc310         format(I5,1x,F9.4,1x,I6,1x,I4,1x,I2,8(1x,F8.3))
310       format(1x,I5,',',I3,',',F9.4,',',I8,',',I4,',',I2,8(',',F9.3))
          enddo
c
	endif
c
      enddo      !End of it loop-read a set of tracks
      
9     continue
c
      write(*,*)'No. of tracks read in: ',it-1
      write(*,*)'No. of tracks written: ',inkeep
c
      close(2)
      close(3)
c      
      end
  
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine genlst (cdatehr0,cdatehr1,cdatehr2,i1,i2,iy2k,idbg)
c
c * Author: Kevin Keay  Date: 1/7/2000
c
      character*12 cdatehr0,cdatehr1,cdatehr2
      integer i1,i2,iy2k,idbg
c
      double precision xjday0,xjday1,xjday2,xjdiff1,xjdiff2
      parameter (itinc= 6)
      double precision tinc
      parameter (tinc= itinc/24.d0)
c
      call extrdate (iy2k,cdatehr0,iy0,im0,id0,ihr0)
      call datevalu (id0,im0,iy0,jday0)
      xhr0= 0.
      if(ihr0.gt.0)xhr0= ihr0/float(24)
      xjday0= dble(jday0) +dble(xhr0)	
c
      call extrdate (iy2k,cdatehr1,iy1,im1,id1,ihr1)
      call extrdate (iy2k,cdatehr2,iy2,im2,id2,ihr2)
c
      call datevalu (id1,im1,iy1,jday1)
      call datevalu (id2,im2,iy2,jday2)
      xhr1= 0.
      if(ihr1.gt.0)xhr1= ihr1/float(24)
      xhr2= 0.
      if(ihr2.gt.0)xhr2= ihr2/float(24)
      xjday1= dble(jday1) +dble(xhr1)	
      xjday2= dble(jday2) +dble(xhr2)	
c
      xjdiff1= xjday1 -xjday0
      jdiff1= int(xjdiff1/tinc)
c
      i1= 1 +jdiff1
c
      xjdiff2= xjday2 -xjday0
      jdiff2= int(xjdiff2/tinc)
c
      i2= 1 +jdiff2
c
      if(idbg.eq.1)then
        write(10,*)'Origin:  ',iy0,im0,id0,ihr0,' Julian: ',xjday0
        write(10,*)'-Datehr1: ',iy1,im1,id1,ihr1,' Julian: ',xjday1
        write(10,*)'-Datehr2: ',iy2,im2,id2,ihr2,' Julian: ',xjday2
        write(10,*)'-Index numbers: ',i1,i2
      endif
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
      subroutine convdate (date,day,month,year)
c
      integer date,day,month,year
c
      year = date/10000
      month = date/100 - year*100
      day = date - year*10000 - month*100
c
      return
      end
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
      subroutine valudate (dval,d,m,y)
c
c     gives day, month, and year for given Julian day
c     written by David Hooke
c     Rural Water Corporation (1994)
c
      integer  dval, d, m, y
      integer  dv, t0
c
      t0 = 4
      y = t0*400 + 1
      dv = dval
      y = y + (dv/146097)*400
      dv = mod (dv,146097)
      y = y + (dv/36524)*100
      if (dv .eq. 146096) go to 10
      dv = mod (dv,36524)
      y = y + (dv/1461)*4
      dv = mod (dv,1461)
      y = y + dv/365
      if (dv .eq. 1460) go to 10
      dv = mod (dv,365)
      m    = 1
cc---------
cc      do while (dv .ge. monthlen(m,y))
cc           dv = dv - monthlen (m,y)
cc           m = m + 1
cc      enddo
cc---------
c==========
      iwhile= 0
1000  if(dv.ge.monthlen(m,y))then
        iwhile= 1
        dv= dv - monthlen(m,y)
        m= m+1
      else
        iwhile= 0
      endif
      if(iwhile.eq.1)goto 1000
c==========
      d = dv + 1
      go to 90
  10  continue
      y = y - 1
      m = 12
      d = 31
  90  continue
      return
      end
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
      subroutine datevalu (d,m,y,dval)
c
c     gives julian day for day, month, and year
c     written by David Hooke
c     Rural Water Corporation (1994)
c
      integer  d, m, y, dval, dy, t0
      t0 = 4
      dy = 0
c
c     Compute day of year
c
      do 1000 i = 1,m-1
           dy = dy + monthlen(i,y)     ! days at end of month (m-1)
1000     continue
cc      enddo
      dy = dy + d - 1                  ! days at date (with 1st Jan zero).
c
c     Compute days since start of accumulation period .....................
c       (Day zero is 01/01/1601, negative before then).
c
     0 dval = ((y-1)/100-(t0*4))*36524
     1        + (y-(t0*400+1))/400
     2        + mod ((y-1),100)*365
     3        + mod ((y-1),100)/4
     4        + dy
c
      if (y.le.t0*400) dval = dval - 1
c
      return
      end
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
      integer function monthlen(imon,iyr)
c
      dimension mday(12)
      data mday/31,28,31,30,31,30,31,31,30,31,30,31/
      monthlen = mday(imon)
      if(imon.eq.2) then
         if(mod(iyr,4).eq.0) then
            monthlen = 29
            if(mod(iyr,100).eq.0) then
               if(mod(iyr,400).ne.0) then 
                  monthlen = 28
               endif
            endif
         endif
      endif
      return
      end
c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 
      integer function ilen(string)
c
      character*1 c
      character*(*) string
c
      l= len(string)
      do i = l,1,-1
        c = string(i:i)
        if (c.ne.' ') goto 10
      end do
c * String is wholly blank
      ilen= -1
      return
c
   10 ilen = i
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine extrdate (iy2k,cdatehr,iy,im,id,ihr)
c
c * Author: Kevin keay  Date: 29/6/2000
c
      integer iy2k
      character*12 cdatehr
      integer iy,im,id,ihr
c
      character*4 chr
c
      if(iy2k.eq.0)then  ! Year is yy
	read(cdatehr,'(I2,I2,I2)')iy,im,id
	write(chr,'(A)')cdatehr(7:10)
	read(chr,'(I4)')ihr
	if(ihr.gt.24)ihr= ihr/100
      else               ! Year is yyyy
	read(cdatehr,'(I4,I2,I2)')iy,im,id
	write(chr,'(A)')cdatehr(9:12)
	read(chr,*)ihr
	if(ihr.gt.24)ihr= ihr/100
      endif
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

      subroutine matchtrk (n,trklist,itrack,imatch)
c
      integer trklist(n)
c
      imatch= 0
      do i=1,n
        if(trklist(i).eq.itrack)then
	  imatch= i
	  return
	endif
	
      enddo
c
      return
      end

c ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
c ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
