      Program map

      parameter (nilt=362,njlt=217,nbuf=nilt*njlt)
      parameter (nmaplt=1000)
      parameter (spval=99999.9,rad=57.2957)

      real lat(njlt),lon(nilt),a(nbuf)
      real lat2(njlt),lon2(nilt),dlong(nilt),b(nbuf)
      integer mext(nmaplt),tlev(nmaplt),where(nmaplt)
      dimension sumfk(52),sumak(52),isumk(52)
      character*1 ak(nilt),aksel,achconv

      integer map,map2
      real lonW,lonE,latS,latN
      character*80 head,head1,head2,head3,head4
      character*80 file1,file2,file3,file4,mfile
      character*80 fields,optarg
      integer optind,fseek,ftell
      character ans*1,unit*13,string*20

      integer file1val,in2val,nmapval,globval,ascval,jobval,headval
      logical compdet,diffdet,formdet,head1det,head2det,namedet
      logical headdet,polardet,ascdet,nmapdet,nfiledet,file1det
      logical mxmndet,extdet1,extdet2,cleavdet,limdet,odet,globdet
      logical extdet3,winddet,zonavdet,unfmdet,jobdet,scaledet
      logical thindet,lintdet,bispldet,serdet1,serdet2,man1det,man2det
      logical xsecdet,rectdet,spvdet,outdet,out2det,quotdet,nocheck
      logical angledet,raddet,lim2det,checkdet,newlldet
      logical msectdet,zsectdet,efmtdet,avedet,sumdet,proddet
      logical vectsp,helpdet,extdet0,unitdet,iregser,mxmn2det
      logical mxmn1det,grendet,subsdet,grphzdet,readdet,forcedet
      logical regavdet,regavdet2,maskdet,extlon,lldet
      logical mxflddet,mnflddet
      logical alldet(100)
      logical found1,found2

      equivalence (dlong(1),lon2(1))
      equivalence (file1det,file1val)
      equivalence (in2det,in2val)
      equivalence (nmapdet,nmapval)
      equivalence (globdet,globval)
      equivalence (ascdet,ascval)
      equivalence (jobdet,jobval)
      equivalence (headdet,headval)
      equivalence (alldet(1),angledet)

      common /blmapl/ angledet,extdet3,bispldet,
     *   compdet,extdet1,extdet2,
     *   head1det,head2det,lintdet,cleavdet,limdet,
     *   namedet,odet,polardet,quotdet,nocheck,rectdet,
     *   raddet,scaledet,serdet1,serdet2,unfmdet,spvdet,
     *   formdet,mxmndet,thindet,winddet,zonavdet,
     *   diffdet,xsecdet,lim2det,checkdet,newlldet,
     *   msectdet,zsectdet,efmtdet,avedet,sumdet,
     *   vectsp,helpdet,extdet0,unitdet,iregser
     *   mxmn2det,proddet,grendet,subsdet,grphzdet,
     *   mxmn1det,readdet,forcedet,regavdet,regavdet2,
     *   maskdet,extlon,lldet,mxflddet,mnflddet
      common /blmapi/map,map2
      common /blmapc/file3,file4

      do 05 idet = 1,60
          alldet(idet) = .false.
   05 continue
      map  = 0
      map2 = 0
      icstrt = 0
      icstop = 80
      file3 = 'tape11'
      file4 = 'tape12'

c-----------------------------------------------------------------------
c       (1)  Option assignments.
c-----------------------------------------------------------------------

 10   continue
      nopt = ngtopt(
     * "a:Ab:BcCdDe:EfF:gG:hHiI:jJ:kKlLm:M:nNoOpPqQrRsStT" //
     * ":uUvVwWxXyYzZ1:2:9"
     *, optind,optarg)
          if (nopt.eq.-1) go to 20
          if (char(nopt).eq.'A') angledet = .true.
          if (char(nopt).eq.'a') then
            read (optarg,'(i10)',err=11) iavtp
            write (6,*) optarg,iavtp
            if (iavtp.eq.1) mxmn2det  = .true.
            if (iavtp.eq.2) mxmn1det  = .true.
            if (iavtp.eq.3) mxmndet   = .true.
            go to 12
   11       continue
            regavdet = .true.
            mfile = optarg
   12       continue
          endif
          if (char(nopt).eq.'b') then
            regavdet2 = .true.
            aksel = optarg
          endif
          if (char(nopt).eq.'B') bispldet = .true.
          if (char(nopt).eq.'c') compdet  = .true.
          if (char(nopt).eq.'C') checkdet = .true.
          if (char(nopt).eq.'d') diffdet  = .true.
          if (char(nopt).eq.'D') forcedet  = .true.
          if (char(nopt).eq.'e') then
              read (optarg(1:1),'(i1)',err=971) iextract
              if (iextract.eq.1) extdet1  = .true.
              if (iextract.eq.2) extdet2  = .true.
              if (iextract.eq.3) extdet3  = .true.
          endif
          if (char(nopt).eq.'E') efmtdet  = .true.
          if (char(nopt).eq.'f') formdet  = .true.
          if (char(nopt).eq.'F') read (optarg(1:3),'(i3)',err=970) map
          if (char(nopt).eq.'g') grendet  = .true.
          if (char(nopt).eq.'G') read (optarg(1:3),'(i3)',err=970) map2
          if (char(nopt).eq.'h') helpdet  = .true.
          if (char(nopt).eq.'H') head1det = .true.
          if (char(nopt).eq.'i') lintdet  = .true.
          if (char(nopt).eq.'I') read (optarg(1:3),'(i3)',err=970)icstrt
          if (char(nopt).eq.'J') read (optarg(1:3),'(i3)',err=970)icstop
          if (char(nopt).eq.'j') thindet  = .true.
          if (char(nopt).eq.'k') cleavdet = .true.
          if (char(nopt).eq.'K') grphzdet = .true.
          if (char(nopt).eq.'l') limdet   = .true.
          if (char(nopt).eq.'L') lldet    = .true.
          if (char(nopt).eq.'m') then
            maskdet = .true.
            mfile    = optarg
          endif
c         if (char(nopt).eq.'m') mxmndet  = .true.
          if (char(nopt).eq.'y') mxmn1det = .true.
          if (char(nopt).eq.'O') mxmn2det = .true.
c         if (char(nopt).eq.'M') msectdet = .true.
          if (char(nopt).eq.'M') then
            if (optarg(1:3).eq.'max') mxflddet = .true.
            if (optarg(1:3).eq.'min') mnflddet = .true.
          endif
          if (char(nopt).eq.'n') namedet  = .true.
          if (char(nopt).eq.'N') newlldet = .true.
          if (char(nopt).eq.'o') odet     = .true.
          if (char(nopt).eq.'p') proddet  = .true.
          if (char(nopt).eq.'P') polardet = .true.
          if (char(nopt).eq.'q') quotdet  = .true.
          if (char(nopt).eq.'Q') nocheck  = .true.
          if (char(nopt).eq.'r') readdet  = .true.
          if (char(nopt).eq.'R') rectdet  = .true.
          if (char(nopt).eq.'Y') raddet   = .true.
          if (char(nopt).eq.'s') sumdet   = .true.
          if (char(nopt).eq.'S') scaledet = .true.
          if (char(nopt).eq.'t') serdet1  = .true.
          if (char(nopt).eq.'T') then
              iregser  = .true.
              open (9,file=optarg)
          endif
          if (char(nopt).eq.'u') unfmdet  = .true.
          if (char(nopt).eq.'U') unitdet  = .true.
          if (char(nopt).eq.'v') spvdet   = .true.
          if (char(nopt).eq.'V') vectsp   = .true.
          if (char(nopt).eq.'w') winddet  = .true.
          if (char(nopt).eq.'W') subsdet  = .true.
          if (char(nopt).eq.'x') xsecdet  = .true.
          if (char(nopt).eq.'z') zonavdet = .true.
          if (char(nopt).eq.'Z') zsectdet = .true.
          if (char(nopt).eq.'1') read (optarg,'(a80)') file3
          if (char(nopt).eq.'2') read (optarg,'(a80)') file4
          if (char(nopt).eq.'9') extlon   = .true.
          go to 10
 20   continue

          if (limdet.and.zonavdet) then
            lim2det = .true.
            limdet = .false.
          endif
          unfmdet = unfmdet.or.grendet
          extdet0 = (unfmdet.and.(map.gt.0))
          if (iregser) serdet1 = .true.
          if (serdet1) then
              if (map.eq.0) serdet1 = .false.
              serdet2 = .not.serdet1
          endif      
          nmapdet = extdet0+extdet1+extdet2+extdet3+xsecdet+serdet2
c         if (nmapval.gt.1) stop ' Too many options.'
          if (regavdet2.and.(.not.regavdet)) stop
     *     ' -b (aksel) also requires -a (maskfile)'
          mxmndet = mxmndet.or.mxmn2det.or.mxmn1det.or.regavdet
          globdet = mxmndet+zonavdet+thindet+lintdet+bispldet
     *     .or.extlon
c         if (globval.gt.1) stop ' Too many options.'
          if (newlldet.and.(.not.(lintdet.or.bispldet))) stop
     *     ' Option i or b must accompany N.'
          man1det = scaledet.or.limdet.or.globdet.or.subsdet.or.
     *     readdet
          headdet = head1det + head2det
c         if (headval.gt.1) stop ' Only one heading option allowed.'
          globdet = globdet.or.odet
          man1det = man1det.or.odet
          man2det = diffdet+quotdet+rectdet+polardet+vectsp
     *               +proddet+sumdet+mxflddet+mnflddet
          headdet = headdet.or.diffdet.or.rectdet.or.polardet
     *               .or.proddet.or.sumdet.or.mxflddet.or.mnflddet
          unitdet = unitdet+scaledet+quotdet+proddet
          in2det  = compdet+(man2det .and. iargc().ge.optind+1)
C         if (map2.gt.0) in2det = .false.
c         if (in2val.gt.1) stop ' Too many options.'
          file1det = nmapdet+man1det+in2det
c         if (file1val.gt.1) stop 'Too many options.'
          formdet = formdet.or.compdet
          ascdet = formdet + winddet + lldet
c         if (ascval.gt.1) stop ' Only one formatted output option.'
          file1det = file1det.or.ascdet.or.headdet.or.spvdet.or.
     *     grendet
          nfiledet = cleavdet+namedet+serdet1
          jobdet = nfiledet+file1det
c         if (jobval.gt.1) stop ' Too many options.'
          nfiledet = .not.file1det
          out2det = polardet.or.rectdet
          if ((formdet.and.(.not.unfmdet)).or.winddet.or.mxmndet
     *     .or.lldet) then
              headdet = .false.
              unitdet = .false.
          endif
          outdet = headdet.or.unitdet.or.odet.or.spvdet.or.limdet.
     *     or.vectsp.or.unfmdet.or.subsdet.or.extlon
          if (formdet.and.(.not.unfmdet)) outdet = .false.
          if (lldet) outdet = .false.
          if (msectdet.and.zsectdet) stop 
     *     ' Only one of msectdet and zsectdet allowed.'

c-----------------------------------------------------------------------
c       (2)  Listing of map headers or separation of maps.
c-----------------------------------------------------------------------

      if (nfiledet) then

      if (cleavdet) then
          write (0,'('' Select fields to be extracted (or not)'',
     *     '' by 1 (0), eg. 0 0 1 0 1.'')')
          read (5,30) fields
 30       format (a80)
          call getnos(fields,0,1,nmaplt,mext,nextend,ie)
          if ((nextend.eq.0).or.(ie.gt.1)) stop
      endif

      if (namedet) stop ' Automatic naming of files not available.'

          ifileno = 0
      do 60 iargno = optind,iargc()
          ifileno = ifileno + 1
          call getarg(iargno,file1)
          open (1,file=file1,status='old',form='unformatted',
     *     err=980)
          if (.not.serdet1) write (6,'(a/a)') 'File: ', file1(1:74)
      do 40 imap = 1,nmaplt
          call qmapread(1,file1,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *     head1,a,ie)
          if (grendet.and.(lon(nlon).eq.360.)) lon(nlon) = 359.99
          if (grendet.and.(lon(1).eq.0.)) lon(nlon) = 0.01
          if (ie.eq.1) go to 50
          if (ie.gt.1) stop
          if (checkdet) call checknos(nilt,njlt,nbuf,nlat,
     *     lat,nlon,lon,a,ierno)
          if (cleavdet.and.(mext(imap).eq.1)) then
              write (6,'(a)') head1
c             call mapfilename(head1,file3)
c             write (6,'(a)') file3
              stop ' Separation of files not yet available.'
c             open (3,file=file3,status='new',form='unformatted',
c    *         err=990)
c             if (checkdet) call checknos(nilt,njlt,nbuf,nlat,
c    *         lat,nlon,lon,a,ierno)
c             call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
c    *         head3,a,ie)
c             close (3,status='keep')
          else if ((serdet1).and.(imap.eq.map)) then
          if (ifileno.eq.1) then
              open (3,file=file3,status='unknown')
              nfileno = iargc() - optind + 1
              if (grphzdet) 
     *         write (0,*)' Constructing a series from ',nfileno,
     *         ' files.'
              if (grphzdet) write (0,'(a)') head1
              if (grphzdet) write (0,'(a)') ' Enter heading:'
              if (grphzdet) read (5,30) head3
              lnb=lnblnk(head3)
              if (head3(lnb:lnb).eq.'$') head3(lnb:80) = head1(lnb:80)
              write (0,'(
     *         '' Enter gridpoint                   (i,j) : ''$)')
              read (5,*) isel,jsel
              ijsel = (jsel-1)*nlon + isel
              if (grphzdet) write (6,'(a)') head3
              if (grphzdet) write (6,*) nfileno
              if (iregser) then
                  write (0,'('' Enter '',i3,'' time values:'')') nfileno
                  read (9,*) (tlev(itlev),itlev=1,nfileno)
              endif
          endif
              if (iregser) then
                  fmap = tlev(imap)
              else
                  fmap = ifileno
              endif
              write (6,35) fmap,a(ijsel)
 35           format (f8.3,x,10g16.8)
              go to 39
          else
              if (checkdet) call checknos(nilt,njlt,nbuf,nlat,
     *         lat,nlon,lon,a,ierno)
              if (.not.serdet1) write (6,'(a)') head1
          endif
 39   continue
 40   continue
 50   continue
 60   continue

          if (serdet1) close (3)
          stop
      endif

c-----------------------------------------------------------------------
c       (3) Opening of input file(s) (for all specified options).
c-----------------------------------------------------------------------

      if (iargc().ge.optind) then
          call getarg(optind,file1)
      else
          stop ' No file specified.'
      endif
      if (readdet) then
        open (unit=1,file=file1,status='old',err=980)
      else
        open (unit=1,file=file1,status='old',form='unformatted',
     *   err=980)
      endif

      if (in2det.or.newlldet) then
        if (newlldet .and. map2.ne.0) stop 
     *   ' Cannot have 2 files if -G option used.'
        if (iargc().ge.optind+1) then
          call getarg(optind+1,file2)
        else
          if (newlldet) stop ' No second file specified.'
        endif
        open (unit=2,file=file2,status='old',form='unformatted',
     *   err=990)
      endif

c-----------------------------------------------------------------------
c       (4)  Extraction of wanted files.
c-----------------------------------------------------------------------

      if (nmapdet) then

          ie = 0
          nextend = 0
      if (extdet1) then
          write (6,'('' Select fields (y or .):'')')
      else if (extdet2) then
          write (6,'('' Select fields to be extracted (or not)'',
     *     '' by 1 (0), eg. 0 0 1 0 1.'')')
          read (5,30) fields
          call getnos(fields,0,1,nmaplt,mext,nextend,ie)
          if ((nextend.eq.0).or.(ie.gt.1)) stop
      else if (extdet3) then
          write (6,'('' Select input map nos. to be extracted in'',
     *     '' order of output; eg. 3 5 1 4 2.'')')
          read (5,30) fields
          call getnos(fields,1,nmaplt,nmaplt,mext,nextend,ie)
          if ((nextend.eq.0).or.(ie.gt.1)) stop
      else if (xsecdet) then
          stop ' Cross sections not available.'
      endif

      if (.not.serdet2) open (3,file=file3,form='unformatted')

      do 80 imap = 1,nmaplt
          if (extdet3) where(imap) = ftell(1)
          call qmapread(1,file1,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *     head1,a,ie)
          if (grendet.and.(lon(nlon).eq.360.)) lon(nlon) = 359.99
          if (grendet.and.(lon(1).eq.0.)) lon(nlon) = 0.01
          if (checkdet) call checknos(nilt,njlt,nbuf,nlat,
     *     lat,nlon,lon,a,ierno)
          if (ie.eq.1) go to 90
          if (ie.gt.1) stop
          nmap = imap
          if (extdet0.and.(imap.eq.map)) then
              write (6,'(a80)') head1
              call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *         head1,a,ie)
          else if (extdet1) then
              write (6,'(a77,'': ''$)') head1(1:77)
              read (5,'(a1)') ans
              if ((ans.eq.'y').or.(ans.eq.'.')) then
                  call qmapwrite(3,
     *             nilt,njlt,nbuf,1,nlat,lat,nlon,lon,head1,a,ie)
              endif
          else if (extdet2.and.(mext(imap).eq.1)) then
              write (6,'(a)') head1
              call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *         head1,a,ie)
          else if (extdet3) then
              write (6,75) imap,head1(1:76)
 75           format (i2,': ',a76)
              if(where(imap).lt.0) stop 'Improper map location'
          endif
 80   continue
 90   continue

      if (extdet3) then
          write (6,'('' Fields extracted. '')')
          do 110 iext = 1,nextend
              imap = mext(iext)
              if ((imap.gt.nmap).or.(imap.lt.1)) then
              write (6,*) ' Map ',imap,' does not exist.'
              else
              ifs = fseek(1,where(mext(iext)),0)
              if (ifs.ne.0) stop ' fseek unsuccessful.'
              call qmapread(1,file1,nilt,njlt,nbuf,1,nlat,lat,nlon,
     *         lon,head1,a,ie)
              if (grendet.and.(lon(nlon).eq.360.)) lon(nlon) = 
     *         359.99
              if (grendet.and.(lon(1).eq.0.)) lon(nlon) = 0.01
              call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *         head1,a,ie)
              write (6,75) iext,head1(1:76)
              endif
 110      continue
      else if (serdet2) then
          if (grphzdet)
     *     write (0,*)' Constructing a series from ',nmap,' maps.'
          if (grphzdet) write (0,'(a)') head1
          if (grphzdet) write (0,'(a)') ' Enter heading:'
          if (grphzdet) read (5,30) head3
          lnb=lnblnk(head3)
          if (head3(lnb:lnb).eq.'$') head3(lnb:80) = 
     *     head1(lnb:80)
          if (grphzdet) write (6,'(a)') head3
          if (grphzdet) write (6,*) nmap
          write (0,'('' Enter gridpoint'',
     *     ''                   (i,j) : ''$)')
          read (5,*) isel,jsel
          ijsel = (jsel-1)*nlon + isel
          if (iregser) then
              write (0,'('' Enter '',i3,'' time values:'')') nmap
              read (9,*) (tlev(itlev),itlev=1,nmap)
          endif

          rewind (1)
          do 120 imap = 1,nmap
              call qmapread(1,file1,nilt,njlt,nbuf,1,nlat,lat,nlon,
     *         lon,head1,a,ie)
              if (grendet.and.(lon(nlon).eq.360.)) lon(nlon) = 359.99
              if (grendet.and.(lon(1).eq.0.)) lon(nlon) = 0.01
              if (iregser) then
                  fmap = tlev(imap)
              else
                  fmap = imap
              endif
              write (6,35) fmap,a(ijsel)
 120      continue
      endif

          close (1,status='keep')
          if (.not.serdet2) close (3,status='keep')
          stop

      endif

c-----------------------------------------------------------------------
c       (5) Reading of input file(s).
c-----------------------------------------------------------------------

c           (a)  Reading of file(s).
c                -------------------

      if (map.le.0) map = 1
      if (map.gt.nmaplt) stop ' Selected map > nmaplt.'
      if (map2.gt.nmaplt) stop ' Selected map2 > nmaplt.'
      if (map2.eq.map .and. .not.in2det) stop ' map2 = map.'

      found1 = .false.
      found2 = .false.
      map0 = 0
      if (.not.in2det) map0 = map2
      write (0,*)
      do 130 imap = 1,max(map,map0)
          if (imap.eq.map) then
c             if (readdet) then
c               call fmread(1,file1,nilt,njlt,nbuf,1,nlat,lat
              call qmapread(1,file1,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,
     *         head1,a,ie)
              if (ie.ne.0) stop
              if (checkdet) call checknos(nilt,njlt,nbuf,nlat,
     *         lat,nlon,lon,a,ierno)
              if (ierno.eq.1) stop ' Invalid numbers in data field.'
              if (.not.(msectdet.or.zsectdet.or.grendet)) then
                  if ((lon(1).le.0.01).and.(lon(1).ge.0.)) lon(1) = 0.
                  if ((lon(nlon).le.360.).and.(lon(nlon).ge.359.99)) 
     *             lon(nlon) = 360.
              endif
              found1 = .true.
          else if (imap.eq.map2 .and. .not.in2det) then
              call qmapread(1,file1,nilt,njlt,nbuf,1,nlat2,lat2,nlon2,
     *         lon2,head2,b,ie)
              if (ie.ne.0) stop
              if (checkdet) call checknos(nilt,njlt,nbuf,nlat2,
     *         lat2,nlon2,lon2,b,ierno)
              if (ierno.eq.1) stop ' Invalid numbers in data field.'
              if (.not.(msectdet.or.zsectdet.or.grendet)) then
                  if ((lon2(1).le.0.01).and.(lon2(1).ge.0.)) 
     *             lon2(1) = 0.
                  if ((lon2(nlon2).le.360.).and.(lon2(nlon2).
     *             ge.359.99)) 
     *             lon(nlon) = 360.
              endif
              found2 = .true.
          else
              read (1,err=135,end=135) dummy
              read (1,err=135,end=135) dummy
              read (1,err=135,end=135) dummy
              read (1,err=135,end=135) dummy
              read (1,err=135,end=135) head4
              read (1,err=135,end=135) dummy
          endif
 130  continue
      go to 136
 135  continue
      stop ' Unexpected end of file/error'
 136  continue
      head4 = ' '
      if (.not.(mxmn1det.or.regavdet2)) then
        write (0,'(a,a)') ' File: ',file1(1:lnblnk(file1))
        if (.not.found1) stop 'First map not found'
        write (0,30) head1
        if (map2.ne.0 .and. .not.in2det) then
          if (.not.found2) stop 'Second map not found'
          write (0,30) head2
        endif
      endif

      if (in2det) then
          map0 = map
          if (map2.ne.0) map0 = map2
          write (0,'(a,a)') ' File: ',file2(1:lnblnk(file2))
          do 140 imap = 1,map0
            call qmapread(2,file2,nilt,njlt,nbuf,1,
     *       nlat2,lat2,nlon2,lon2,head2,b,ie)
            if (ie.ne.0) stop ' Error/end of file 2.'
            if (imap.eq.map0) then
              if ((lon2(1).le.0.01).and.(lon2(1).ge.0.)) lon2(1) = 0.
              if ((lon2(nlon2).le.360.).and.(lon2(nlon2).ge.359.99)
     *         .and.(.not.grendet)) lon2(nlon2) = 360.
              go to 141
            endif
 140      continue
          stop 'Map not found in file2.'
 141      continue
          write (0,30) head2

          if (checkdet) call checknos(nilt,njlt,nbuf,nlat2,
     *     lat2,nlon2,lon2,b,ierno)
          if (ierno.eq.1) stop ' Invalid numbers in data field.'
          if (nlat.ne.nlat2) stop 'nlat1.ne.nlat2'
          if (nlon.ne.nlon2) stop 'nlat1.ne.nlat2'
          if (.not.nocheck) then
          do 160 j = 1,nlat
              adif = abs(lat(j) - lat2(j))
c             if (adif.gt.0.001. and .(.not.forcedet))  stop
c    *         ' Latitudes not equal.'
 160      continue
          do 170 i = 1,nlon
              adif = abs(lat(j) - lat2(j))
c             if (adif.gt.0.001. and .(.not.forcedet))  stop
c    *         ' Longitudes not equal.'
 170      continue
          endif
          if (grendet.and.(lon2(nlon2).eq.360.)) lon2(nlon2) = 
     *     359.99
      endif
      if (grendet.and.(lon(nlon).eq.360.)) lon(nlon) = 359.99
      if (grendet.and.(lon(1).eq.0.)) lon(nlon) = 0.01

c      if (maskdet) then
c            open (9,file=mfile,form='formatted',status='old')
cc           write (0,*) ' Reading regional mask from: ',
cc    *       mfile(1:lnblnk(mfile))
c            read (9,'(a80)') head
cc           write (0,'(a)') head
c            read (9,*) nlon2,nlat2
cc           write (0,'(2i4)') nlon2,nlat2
c            if ((nlon2.gt.nilt).or.(nlat2.gt.njlt)) stop 
c     *       ' Mask array too large.'
c            if ((nlon2.ne.nlon).or.(nlat2.ne.nlat)) stop 
c     *       ' Mask array size differs from that of data array.'
c
ccXX
c              ijadd = (j-1)*nlon
c              do 219 i=1,nlon
c                areaij = clatj*dlong(i)
c                ij = ijadd + i
cXX
c            do 178 j=nlat,1,-1
c              read (9,'(i3,x,400a1)') jj, (mask(i,j),i=1,nlon)
c              do 176 i=1,nlon
c                if (mask(i,j).eq.0) a(i,j) = spval
c  176         continue
c              if (in2det) then
c                do 177 i=1,nlon
c                  if (mask(i,j).eq.0) b(i,j) = spval
c  177           continue
c              endif
c  178       continue
c            close(9)
c      endif

      if (newlldet) then
          read (2,err=175,end=175) nlat2
          read (2,err=175,end=175) (lat2(j),j=1,nlat2)
          read (2,err=175,end=175) nlon2
c          read (2,err=175,end=175) (lon2(i),i=1,nlon2)
          go to 176
 175      stop ' Error/end of file in supplied lats. and lons.'
 176      continue
          if (ie.ne.0) stop
          if (imap.eq.map) then
          if ((lon2(1).le.0.01).and.(lon2(1).ge.0.)) lon2(1) = 0.
          if ((lon2(nlon2).le.360.).and.(lon2(nlon2).ge.359.99).
     *     and.(.not.grendet)) lon2(nlon2) = 360.
          endif
      endif

      narray = nlat*nlon

      if (diffdet)  write (0,*)'Finding difference.'
      if (angledet) write (0,*)'Differences interpreted as angles.'
      if (proddet)  write (0,*)'Finding product.'
      if (sumdet)   write (0,*)'Finding sum.'
      if (mxflddet) write (0,*)'Finding maximum field.'
      if (mnflddet) write (0,*)'Finding minimum field.'
      if (quotdet)  write (0,*)'Finding quotient.'
      if (polardet) write (0,*)'Finding polar coords (r,th).'
      if (vectsp)   write (0,*)'Setting special values to zero'
      if (rectdet)  write (0,*)'Finding rect. coords (x,y).'
      if (raddet)   write (0,*)'Angles interpreted in radians.'
      if (scaledet) write (0,*)'Scaling values.'
      if (grendet)  write (0,*)'360 degE > 359.99 degE.'
      if (zonavdet) write (0,*)'Finding zonal averages.'
      if (limdet)   write (0,*)'Extracting limited area map.'
      if (lintdet.or.bispldet.or.thindet) write (0,*)
     * 'Interpolating array.'
      if (serdet1)   write (0,*)'Constructing a series from n files.'
      if (tsdet2)   write (0,*)'Constructing a series from n maps.'
      if (lintdet)  write (0,*)'Remapping by linear interpolation.'
      if (bispldet) write (0,*)'Remapping by bicubic spine interp''n.'
c     if (mxmn2det) write (0,*)'Finding average information (1)'
c     if (mxmn1det) write (0,*)'Finding average information (2)'
c     if (mxmndet) write (0,*)'Finding average information (3)'
c     if (regavdet) write (0,*)'Finding regional averages'

c           (b)  New header(s).
c                --------------

      if (headdet) then
          if (head2det)
     *     stop ' Automatic changes to heading not available.'

          if (out2det.or.vectsp) then
            write (0,'(a)') ' Headings adapted from:'
            write (0,'(a80)') head1
            write (0,'(a80)') head2
            write (0,'(a,a,a,a)') ' Enter headings (output to files ',
     *       file3(1:lnblnk(file3)),' & ',file4(1:lnblnk(file4)),'): '
          else
            write (0,'(a)') ' Heading adapted from:'
            write (0,'(a80)') head1
            write (0,'(a,a,a)') ' Enter heading (output to file ',
     *       file3(1:lnblnk(file3)),'): '
          endif
          if (icstrt.ne.0. or. icstop.ne.0) then
              if (icstrt.eq.0) icstrt = 1
              if (icstrt.eq.0) icstrt = 80
              head3 = head1
              head4 = head2
              head = ' '
              read (5,30) head
              head3(icstrt:icstop) = head(1:lnblnk(head))
              head4(icstrt:icstop) = head(1:lnblnk(head))
          else
              read (5,30) head3
              lnb = lnblnk(head3)
              if (head3(lnb:lnb).eq.'$') then
                  head3(lnb:80) = head1(lnb:80)
              else
                  unitdet = .false.
              endif
              if (out2det.or.vectsp) then
              read (5,30) head4
              lnb = lnblnk(head4)
              if (head4(lnb:lnb).eq.'$') head4(lnb:80) = head2(lnb:80)
              endif
          endif
      else
          head3 = head1
          head4 = head2
      endif

      if (unitdet) then
          write (0,'('' Enter new unit (1): ''$)')
          read (5,'(a13)') unit
          head3(58:70) = unit
          if (out2det.or.vectsp) then
          write (0,'(''       new unit (2): ''$)')
          read (5,'(a13)') unit
          head4(58:70) = unit
          endif
      else if (polardet) then
          head3(58:70) = head1(58:70)
          head4(58:70) = 'DEG'
      else if (rectdet) then
          head3(58:70) = head1(58:70)
          head4(58:70) = head1(58:70)
      endif

c           (c)  Change of special values.
c                -------------------------

          spv = spval
      if (spvdet) then
          write (0,'( '' Enter old special value: ''$)')
          read (5,*) spv1
          spv = spv1
          write (0,'( '' Enter new special value: ''$)')
          read (5,*) spv2
          do 180 ij = 1,narray
              if (a(ij).eq.spv) a(ij) = spv2
180       continue
          if (in2det) then
          do 185 ij = 1,narray
              if (b(ij).eq.spv) b(ij) = spv2
185       continue
          endif
          spv1 = spv
          spv = spv2
          write (0,*) ' Special value changed from ',spv1,' to ',spv
      endif

c-----------------------------------------------------------------------
c       (6)  Manipulation of single arrays.
c-----------------------------------------------------------------------

      if (man2det) go to 810
      if (.not.man1det) go to 830

c           (a)  Scaling.
c                --------

      if (scaledet) then
          write (0,'(/'' Enter multiplicand     : ''$)')
          read (5,*) fmulend
          write (0,'( '' Enter addend           : ''$)')
          read (5,*) faddend
          if ((fmulend.eq.1.).and.(faddend.eq.0.))
     *     stop ' Scale unchanged.'
          do 190 ij = 1,narray
              if (a(ij).ne.spv) a(ij) = a(ij)*fmulend + faddend
 190      continue
          write (0,*) 
     *     ' Data scaled: a(ij) = ',fmulend,'*a(ij) + ',faddend,'.'
          go to 830
      endif

c           (a)  Substitution.
c                -------------
      
      if (subsdet) then
      
          write (0,'(/'' Size of border (i3)    : ''$)')
          read (5,'(i3)') ijfr
          if (ijfr.gt.0) then
          write (0,'('' Inc.(0)/Dec.(1) down   : ''$)')
          read (5,'(i3)') iincg
          endif
 200      continue
          write (0,'(/'' Enter i,j (<ret> exits): ''$)')
          read (5,'(a20)') string
          if (string(1:5).eq.'     ') go to 205
          read (string,*) i,j
          if (i.eq.0) go to 205
          ij = i + (j-1)*nlon
          if (ijfr.gt.0) then
              iL = amax0(1,i-ijfr)
              iR = amin0(nlon,i+ijfr)
              jD = amax0(1,j-ijfr)
              jU = amin0(nlat,j+ijfr)
              if (iincg.gt.0) then
                  call matrix(0,a,nlon,iL,iR,-jD,-jU,0,spval)
              else
                  call matrix(0,a,nlon,iL,iR,jD,jU,0,spval)
              endif
              write (0,*) ' '
          endif
          write (0,*) ' Present value is       : ',a(ij)
          write (0,'( '' Enter a(i,j)           : ''$)')
          read (5,'(a20)') string
          if (string(1:5).eq.'     ') go to 200
          read (string,*) a(ij)
          go to 200
 205      continue
      endif

      
c           (b)  Extraction of limited area map (part 1).
c                ----------------------------------------

      if (limdet.or.odet) then
          write (0,'(/'' Longitude range    (i=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlon,lon(1),lon(nlon)
          write (0,'( '' Latitude  range    (j=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlat,lat(1),lat(nlat)
      endif

      if (limdet) then
          write (0,*) ' '
          write (0,'(
     *     '' Using grid points (1) or lons. and lats. (2)?    : ''$)')
          read (5,*) ill
          write (0,'(
     *     '' Enter W,E,S, & N limits of area.                 : ''$)')
          if (ill.eq.1) then
              read (5,*) iW,iE,jS,jN
              if ((iW.lt.1).or.(iE.gt.nlon).or.(jS.lt.1).or.
     *         (jN.gt.nlat)) stop ' Limits lie outside array.'
              if (jS.gt.jN) stop ' Latitudes out of order.'
          else
 310          read (5,*) lonW,lonE,latS,latN
              if ((lonW.gt.lon(nlon)).or.(lonE.lt.lon(1)).or.
     *         (latS.lt.-90.).or.(latN.gt.90.))
     *         stop ' Limits lie outside array.'
              if (latS.gt.latN) stop ' Latitudes out of order.'
              do 320 iW = 1,nlon
                  if (lon(iW).ge.lonW) go to 330
 320          continue
 330          continue
              do 340 iE = nlon,1,-1
                  if (lon(iE).le.lonE) go to 350
 340          continue
 350          continue
              do 360 jS = 1,nlat
                  if (lat(jS).ge.latS) go to 370
 360          continue
 370          continue
              do 380 jN = nlat,1,-1
                  if (lat(jN).le.latN) go to 390
 380          continue
 390          continue
          endif
          if (iW.gt.iE) then
              write (0,*) ' '
              write (0,'(a)') ' Westerly limit E of easterly limit.'
              write (0,'(a)') 
     *         ' Array to be recast if longitudes cyclic.'
              odet = .true.
              globdet = .true.
          endif 
      endif

c           (c)  Determination whether map is a world map.
c                -----------------------------------------

      if (globdet) call globtest(nlat,nlon,nlonx,iworldx,iworldS,
     * iworldN,lat,lon,njlt,nilt)


c           (ab) Addition of an extra longitude
c                ------------------------------

      if (extlon .and. iworldx.eq.2) then
          nlonlst = nlon
          nlon = nlon + 1
          lon(nlon) = lon(1) + 360.

          do 440 j = 1,nlat
            do 430 i = 1,nlon
              ij1 = i + nlonlst*(j-1)
              ij2 = i + nlon   *(j-1)
              b(ij2) = a(ij1)
  430       continue
            ijW = 1 + nlon*(j-1)
            ijE = ijW + nlonlst
            b(ijE) = b(ijW)
  440     continue

          do 450 ij = 1,nlon*nlat
            a(ij) = b(ij)
  450     continue
      endif

c           (d)  Reversal of hemispheres.
c                ------------------------

      if (odet) then

          write (0,*) ' '
          if (iworldx.eq.4) write (0,'(a)') 
     *     ' Longitudes cyclic with 2 repeated end longs.'
          if (iworldx.eq.3) write (0,'(a)') 
     *     ' Longitudes cyclic with repeated end longs.'
          if (iworldx.eq.2) write (0,'(a)') 
     *     ' Longitudes cyclic without repeated end longs.'
          if (iworldx.le.1) stop
     *     ' Longitudes not cyclic: end meridians cannot be altered.'

          if (limdet) then
              alonturn = lon(iW)
              iworldx2 = 3
          else
              write (0,'(
     *         '' Enter new longitude, lon(1)                  : ''$)')
              read (5,*) alonturn
              write (0,'(
     *         '' Shall end meridian be repeated (2) or not (1)? ''$)')
              read (5,*) iworldx2
          endif

          nlon2 = nlonx
          if (iworldx2.eq.3) nlon2 = nlonx + 1

          do 470 iL2 = 1,nlon
              if (lon(iL2).ge.alonturn) go to 480
 470      continue
 480      continue

          iR2 = iL2 - 1

              i2 = 0
              addlon = 0.
              if (lon(iR2).gt.0.) addlon = -360.
          do 490 i1 = iL2,nlonx
              i2 = i2 + 1
              lon2(i2) = lon(i1) + addlon
 490      continue
              addlon = addlon + 360.
          do 500 i1 = 1,iR2
              i2 = i2 + 1
              lon2(i2) = lon(i1) + addlon
              if (i1.eq.iE) iE2 = i2
 500      continue
              if (iworldx2.eq.3) lon2(nlon2) = lon2(1) + 360.

          do 530 j = 1,nlat
              lat2(j) = lat(j)
              ijstart1 = (j-1)*nlon
              ijstart2 = (j-1)*nlon2
              i2 = 0
          do 510 i1 = iL2,nlonx
              i2 = i2 + 1
              ij1 = ijstart1 + i1
              ij2 = ijstart2 + i2
              b(ij2) = a(ij1)
 510      continue
          do 520 i1 = 1,iR2
              i2 = i2 + 1
              ij1 = ijstart1 + i1
              ij2 = ijstart2 + i2
              b(ij2) = a(ij1)
 520      continue
              if (iworldx2.eq.3) b(ijstart2+nlon2) = b(ijstart2+1) 
 530      continue

          iW = 1
          iE = iE2
          nlon = nlon2
          do 540 i = 1,nlon
              lon(i) = lon2(i)
 540      continue
          narray = nlat*nlon
          do 560 ij = 1,narray
              a(ij) = b(ij)
 560      continue
          
          write (0,'(/'' Longitude range    (i=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlon,lon(1),lon(nlon)
          write (0,'( '' Latitude  range    (j=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlat,lat(1),lat(nlat)

          call globtest(nlat,nlon,nlonx,iworldx,iworldS,
     *     iworldN,lat,lon,njlt,nilt)
      endif

c           (e)  Extraction of limited area map (part2).
c                ---------------------------------------

      if (limdet) then

          nlon2 = iE - iW + 1
          nlat2 = jN - jS + 1
          narray = nlon2*nlat2

          do 610 ij2 = 1,narray
              b(ij2) = 0.
 610      continue
          do 620 i2 = 1,nlon2
              lon2(i2) = 0.
 620      continue
          do 630 j2 = 1,nlat2
              lat2(j2) = 0.
 630      continue

              i2 = 0
          do 640 i1 = iW,iE
              i2 = i2 + 1
              lon2(i2) = lon(i1)
 640      continue

              j2  = 0
          do 650 j1 = jS,jN
              j2  = j2 + 1
              lat2(j2) = lat(j1)
              ij1start = (j1-1)*nlon
              ij2start = (j2-1)*nlon2
              i2 = 0
          do 650 i1 = iW,iE
              i2 = i2 + 1
              ij1 = ij1start + i1
              ij2 = ij2start + i2
              b(ij2) = a(ij1)
 650      continue

          nlat = nlat2
          nlon = nlon2
          do 660 j = 1,nlat
              lat(j) = lat2(j)
 660      continue
          do 670 i = 1,nlon
              lon(i) = lon2(i)
 670      continue
          do 680 ij = 1,narray
              a(ij) = b(ij)
 680      continue

          write (0,'(/'' Longitude range    (i=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlon,lon(1),lon(nlon)
          write (0,'( '' Latitude  range    (j=1,'',i3,'') = '',
     *     f10.1,f8.1)') nlat,lat(1),lat(nlat)

      call globtest(nlat,nlon,nlonx,iworldx,iworldS,
     * iworldN,lat,lon,njlt,nilt)

      endif

c           (f)  Zonal averages.
c                ---------------

      if (zonavdet) then

          if (lim2det) then
          write (0,'(
     *     '' Enter W and E averaging limits.                  : ''$)')
          read (5,*) lonW,lonE
          endif

          write (6,'(a)') head3
          write (6,*) nlat
          do 420 j = 1,nlat
              jstart = (j-1)*nlon
              sum1 = 0.
              sum2 = 0.
              isum = 0
          do 410 i = 1,nlonx
              ij = jstart + i
              if (a(ij).ne.spv) then
                  sum1 = sum1 + a(ij)
                  sum2 = sum2 + a(ij)*a(ij)
                  isum = isum + 1
              endif
 410      continue
              if (isum.eq.0) then
                  sum1 = spv
                  sum2 = spv
              else
                  abar = sum1/float(isum)
                  avar = sum2/float(isum) - abar*abar
                  asd  = sqrt(avar)
              endif
              write (6,35) lat(j),abar,asd
 420      continue
          close (3)
          stop

      endif

c           (g)  Range, mean, and standard deviation of field.
c                ---------------------------------------------

      if (mxmndet) then

          latS = -90.
          latN = 90.
          if (nlat.gt.1) then
              if (iworldS.eq.1) latS = 1.5*lat(1) - 0.5*lat(2)
              if (iworldN.eq.1) latN = 1.5*lat(nlat)-0.5*lat(nlat-1)
          else 
              latS = lat(1) - 0.5
              latN = lat(1) + 0.5
          endif

          if (iworldx.eq.0) then
              lonW = lon(1) - 0.5
              lonE = lon(1) + 0.5
          else if (iworldx.eq.1) then
              lonW = 2.*lon(1) - lon(2)
              lonE = 2.*lon(nlon) - lon(nlon-1)
          else if (iworldx.eq.2) then
              lonW = lon(nlon) - 360.
              lonE = lon(1) + 360.
          else
              lonW = lon(nlon-1) - 360.
              lonE = lon(nlon)
          endif

          if (iworldx.ge.2) then
              alonra1 = 360.
          else if (iworldx.eq.0) then
              alonra1 = 1.
          else
              alonra1 = (-lonW - lon(1) + lon(nlon) + lonE)*0.5
          endif

          if (iworldx.eq.0) then
              dlong(1) = 1.
          else
          do 210 i = 2,nlon-1
              dlong(i) = (lon(i+1) - lon(i-1))*0.5
 210      continue
              dlong(1) = (lon(2) - lonW)*0.5
              dlong(nlon) = (lonE - lon(nlon-1))*0.5
          endif
              
          if (regavdet) then
            open (9,file=mfile,form='formatted',status='old')
c           write (0,*) ' Reading regional mask from: ',
c    *       mfile(1:lnblnk(mfile))
            read (9,'(a80)') head
c           write (0,'(a)') head
            read (9,*) nlon2,nlat2
c           write (0,'(2i4)') nlon2,nlat2
            if ((nlon2.gt.nilt).or.(nlat2.gt.njlt)) stop 
     *       ' Mask array too large.'
            if ((nlon2.ne.nlon).or.(nlat2.ne.nlat)) stop 
     *       ' Mask array size differs from that of data array.'

            if (regavdet2) then
              sumf = 0.
              suma = 0.
              isum = 0
            else
              do 215 kreg = 1,52
                sumfk(kreg) = 0.
                sumak(kreg) = 0.
                isumk(kreg) = 0.
  215         continue
            endif

            sinlatlo = sin(latS/rad)
            do 220 j=nlat,1,-1
              read (9,'(i3,x,400a1)',err=216,end=216) 
     *         jj, (ak(i),i=1,nlon)
              go to 217
  216         continue
              write (6,*) ' j = ',j
              write (6,*) jj, (ak(i),i=1,nlon)
              stop
  217         continue
              if (j.eq.nlat) then
                sinlathi = sin(latN/rad)
              else
                sinlathi = sin((lat(j+1) + lat(j))*0.5/rad)
              endif
              clatj = (sinlathi - sinlatlo)*rad
              sinlatlo = sinlathi
              ijadd = (j-1)*nlon
              do 219 i=1,nlon
                areaij = clatj*dlong(i)
                ij = ijadd + i
                if (regavdet2) then
                  if (ak(i).eq.aksel .and. a(ij).ne.spval) then
                    sumf = sumf + a(ij)*areaij
                    suma = suma + areaij
                    isum = isum + 1
                  endif
                else
                  kreg = kchconv(ak(i))
                  if (kreg.ne.0 .and. a(ij).ne.spval) then
                    sumfk(kreg) = sumfk(kreg) + a(ij)*areaij
                    sumak(kreg) = sumak(kreg) + areaij
                    isumk(kreg) = isumk(kreg) + 1
                  endif
                endif
  219         continue
  220       continue
            close(9)

            if (regavdet2) then
              if (isum.eq.0) then
                write (6,'(1pe13.5)') spval
              else
                write (6,'(1pe13.5)') sumf/suma
              endif
            else
              write (6,*) ' '
              do 225 kreg = 1,52
                if (isumk(kreg).ne.0) 
     *           write (6,'(x,a1,x,i4,2x,e12.5)') achconv(kreg),
     *           isumk(kreg),sumfk(kreg)/sumak(kreg)
  225         continue
            endif
            stop
          endif

          amax = -1.e+10
          amin =  1.e+10
          sum1 = 0.
          sum2 = 0.
          suma = 0.
          sumb = 0.
          Na   = 0.
          Nb   = 0.

              sinlatlo = sin(latS/rad)
          do 230 j = 1,nlat
              if (j.eq.nlat) then
                  sinlathi = sin(latN/rad)
              else
                  sinlathi = sin((lat(j+1) + lat(j))*0.5/rad)
              endif
              clatj = (sinlathi - sinlatlo)*rad
              sinlatlo = sinlathi
              ijstart = (j-1)*nlon
          do 230 i = 1,nlonx
              ij = ijstart + i
              Nb = Nb + 1
              areaij = clatj*dlong(i)
              sumb = sumb + areaij
          if (a(ij).ne.spv) then
              if (a(ij).gt.amax) then
                  imax = i
                  jmax = j
                  amax = a(ij)
              endif
              if (a(ij).lt.amin) then
                  imin = i
                  jmin = j
                  amin = a(ij)
              endif
              Na   = Na + 1
              suma = suma + areaij
              sum1 = sum1 + a(ij)*areaij
              sum2 = sum2 + a(ij)*a(ij)*areaij
          endif
 230      continue

          abar = sum1/suma
          avar = sum2/suma - abar*abar
          asd  = sqrt(avar)
          if (mxmn1det) then
          write (6,'(1pe13.5)') abar
          else if (mxmn2det) then
          write (6,'(4(1pe13.5))') amin,amax,abar,asd
          else
          write (6,'( '' Minimum value:    a('',i3,'','',i3,'') = '',
     *     f26.9)') imin,jmin,amin
          write (6,'( '' Maximum value:    a('',i3,'','',i3,'') = '',
     *     f26.9)') imax,jmax,amax
          write (6,'( '' Points (unrepeated)('',i3,''x'',i3,
     *     '') = '',i16)') nlonx,nlat,Nb
          write (6,'( '' Points counted               = '',i16)') Na
          write (6,'( '' Array area  (globe=41252.96) = '',f18.1)') sumb
          write (6,'( '' Area counted   (deg.lat.**2) = '',f18.1)') suma
          write (6,'( '' Mean         (area weighted) = '',f26.9)') abar
          write (6,'( '' Variance                (do) = '',f26.9)') avar
          write (6,'( '' Standard deviation      (do) = '',f26.9)') asd
          endif
      endif

c           (h)  Thinning of array.
c                ------------------

      if (thindet) then
          write (0,'(a)') ' Thinning not available.'
          stop
c         go to 830
      endif

c           (i)  Interpolation to a new array.
c                -----------------------------

      if ((lintdet).or.(bispldet)) then

          if (newlldet) then
          lonW = lon2(1)
          lonE = lon2(nlon)
          else
          write (0,'('' Enter starting longitude (deg.)         : ''$)')
          read (5,*) lonW
          write (0,'('' Enter finishing longitude               : ''$)')
          read (5,*) lonE
          write (0,'('' Enter longitude spacing                 : ''$)')
          read (5,*) dlon
          endif

              iworldx2 = iworldx
              alondif = lonE - lonW
              if (lonE.lt.lonW) stop ' Finishing lon. < starting lon.'
          if (iworldx.ge.2) then
              if (alondif.ge.360.) then
                  iworldx2 = 3
              else
                  alondif = alondif + dlon
                  if (alondif.ge.360.) iworldx2 = 2
              endif
              alondif = 360.
          else
              alon1c = 1.5*lon(1) - 0.5*lon(2)
              if (lonE.lt.alon1c) stop ' Starting longitude too small.'
              alonnc = 1.5*lon(nlon) - 0.5*lon(nlon-1)
              if (lonE.gt.alonnc) stop ' Finishing longitude too large.'
              nlon2 = (alonnc - alon1c)/dlon
              alondif = alonnc - alon1c
          endif
          nlon2x = nint(alondif/dlon)
          dlon = alondif/float(nlon2x)
          nlon2 = nlon2x + 1
          if (iworldx2.eq.2) nlon2 = nlon2x
          if (nlon.eq.1) then
              nlon2 = 1
              lon2(1) = lon(1)
              write (0,*) ' Single longitude available, ',lon(1),'.'
          else
          if (.not.newlldet) then
          write (0,*) ' Lon. spacing set to ',dlon,' (=',alondif,'/',
     *     nlon2x,') nlon2 = ',nlon2
              alon = lonW
          do 790 i = 1,nlon2
              lon2(i) = alon
              alon = alon + dlon
 790      continue
          endif
              if (iworldx2.eq.3) lon2(nlon2) = lon2(1) + 360.
          endif

          if (newlldet) then
          latS = lat2(1)
          latN = lat2(nlat)
          else
          write (0,'('' Enter starting latitude                 : ''$)')
          read (5,*) latS
          write (0,'('' Enter finishing latitude                : ''$)')
          read (5,*) latN
          write (0,'('' Enter latitude spacing                  : ''$)')
          read (5,*) dlat
          endif 

          if (latN.lt.latS) stop
     *     ' Finishing latitude < starting latitude.'
          if (iworldS.ge.2) then
              if (latS.lt.-90.) latS = -90.
          else
              alon1c = 1.5*lon(1) - 0.5*lon(2)
              if (latS.lt.alat1c) stop ' Starting latitude too small.'
          endif
          if (iworldN.ge.2) then
              if (latN.lt.90.) latN = 90.
          else
              alonnc = 1.5*lon(nlon) - 0.5*lon(nlon-1)
              if (latN.lt.alatnc) stop ' Finishing latitude too small.'
          endif
          alatdif = latN - latS
          nlat2x = nint(alatdif/dlat)
          dlat = alatdif/float(nlat2x)
          nlat2 = nlat2x + 1
          if (nlat.eq.1) then
              nlat2 = 1
              lat2(1) = lat(1)
              write (0,*) ' Single latitude available, ',lat(1),'.'
          else
          if (.not.newlldet) then
          write (0,*) ' Lat. spacing set to ',dlat,' (=',alatdif,'/',
     *     nlat2x,') nlat2 = ',nlat2
              alat = latS
          do 800 i = 1,nlat2
              lat2(i) = alat
              alat = alat + dlat
 800      continue
          endif
          endif

          write (0,'('' Is variable to be interpreted as a''/
     *               ''  scalar interpolated across poles (1)?    ''/
     *               ''  vector comp. intpd. across poles (-1)?   ''/
     *               ''  scalar extrapolated (to poles)   ( )?  : ''$)')
          read (5,'(i1)') ivect

      if (lintdet) call lintxy(nlat,lat,nlon,lon,nlat2,lat2,
     *     nlon2,lon2,iworldS,iworldN,iworldx,spv,ivect,a,b,ie)
c     if (bispldet) call bisplxy(nlat,lat,nlon,lon,nlat2,lat2,
c    *     nlon2,lon2,iworldS,iworldN,iworldx,spv,ivect,a,b,ie)

          nlat = nlat2
          nlon = nlon2
          do 803 j = 1,nlat
              lat(j) = lat2(j)
 803      continue
          do 804 i = 1,nlon
              lon(i) = lon2(i)
 804      continue
          do 805 ij = 1,narray
              a(ij) = b(ij)
 805      continue
      
          go to 830

      endif

      go to 830
   
c-----------------------------------------------------------------------
c       (7)  Manipulation of double arrays.
c-----------------------------------------------------------------------

 810  continue

          if (raddet) then
              anglefact = 1.
              circle = 360./rad
              semicl = 180./rad
          else
              anglefact = rad
              circle    = 360.
              semicl    = 180.
          endif

          ij = 0
          do 820 j = 1,nlat
          do 820 i = 1,nlon
          ij = ij + 1
          if ((a(ij).eq.spv).or.(b(ij).eq.spv)) then
              if (vectsp) then
                  a(ij) = 0.
                  b(ij) = 0.
              else
                  a(ij) = spv
                  b(ij) = spv
              endif
          else if (diffdet) then
              a(ij) = a(ij) - b(ij)
              if (angledet) then
                  if (a(ij).gt.circle)  a(ij) = mod(a(ij),circle)
                  if (a(ij).lt.-circle) a(ij) = mod(a(ij),circle)
                  if (a(ij).gt.semicl)  a(ij) = a(ij) - circle
                  if (a(ij).le.-semicl) a(ij) = a(ij) + circle
              endif
          else if (quotdet) then
              if (b(ij).eq.0.) then
                  a(ij) = spv
              else
                  a(ij) = a(ij)/b(ij)
              endif
          else if (proddet) then
              a(ij) = a(ij)*b(ij)
          else if (sumdet) then
              a(ij) = a(ij)+b(ij)
          else if (mxflddet) then
              a(ij) = amax1(a(ij),b(ij))
          else if (mnflddet) then
              a(ij) = amin1(a(ij),b(ij))
          else if (polardet) then
              radius = sqrt(a(ij)*a(ij) + b(ij)*b(ij))
              if ((a(ij).eq.0.).and.(b(ij).eq.0.)) then
                  theta = spv
              else if ((a(ij).eq.spv).or.(b(ij).eq.spv)) then
                  theta = spv
              else
                  theta = atan2(b(ij),a(ij))*anglefact
              endif
              a(ij) = radius
              b(ij) = theta
          else if (rectdet) then
              theta = b(ij)/rad
              if (a(ij).eq.0.) then
                  x = 0.
                  y = 0.
              else if ((a(ij).eq.spv).or.(b(ij).eq.spv)) then
                  x = spv
                  y = spv
              else
                  x = a(ij)*cos(theta)
                  y = a(ij)*sin(theta)
              endif
              a(ij) = x
              b(ij) = y
          endif
 820  continue

c-----------------------------------------------------------------------
c       (8)  Writing of output unformatted output file(s).
c-----------------------------------------------------------------------

 830  continue

      if (outdet) then

      open (unit=3,file=file3,status='unknown',form='unformatted')
      write (6,'(a)') head3
      call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,head3,a,ie)
      if (vectsp) 
     * call qmapwrite(3,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,head4,b,ie)

      close (3)

      if (out2det) then
      write (6,'(a)') head4
      open (unit=4,file=file4,status='unknown',form='unformatted')
      call qmapwrite(4,nilt,njlt,nbuf,1,nlat,lat,nlon,lon,head4,b,ie)
      close (4)
      endif

      endif

c-----------------------------------------------------------------------
c       (9)  Formatted output
c-----------------------------------------------------------------------

      if (formdet .or. lldet) then
      if (unfmdet) then
          if (out2det) then
          write (0,*) ' Unformatted output has been written to ',
     *     file3(1:lnblnk(file3)),' and ',file3(1:lnblnk(file3)),'.'
          else
          write (0,*) ' Unformatted output has been written to ',
     *     file3(1:lnblnk(file3)),'.'
          endif
      endif
          if (msectdet.or.zsectdet) then
              write (6,'(/'' Levels  ('',i3,''):'')') nlat
          else
              write (6,'(/'' Latitudes  ('',i3,''):'')') nlat
          endif
          write (6,840) (lat(j),j=1,nlat)
 840      format (8f10.3)
 860      format (8e10.3)
          if (msectdet) then
              write (6,'(/'' Latitudes ('',i3,''):'')') nlon
          else
              write (6,'(/'' Longitudes ('',i3,''):'')') nlon
          endif
          write (6,840) (lon(i),i=1,nlon)
          if (lldet) stop
              ij1 = 1
              ij2 = nlon
          do 850 j = 1,nlat
              if (msectdet.or.zsectdet) then
              write (6,'(/'' Lev. '',f9.2,'' (j = '',i3,'')'')')
     *         lat(j),j
              else
              write (6,'(/'' Lat. '',f9.2,'' (j = '',i3,'')'')')
     *         lat(j),j
              endif
              if (efmtdet) then
                  write (6,860) (a(ij),ij=ij1,ij2)
              else
                  write (6,840) (a(ij),ij=ij1,ij2)
              endif
              if (compdet.or.polardet.or.rectdet) then
              write (6,'()')
              if (efmtdet) then
                  write (6,860) (b(ij),ij=ij1,ij2)
              else
                  write (6,840) (b(ij),ij=ij1,ij2)
              endif
              endif
              ij1 = ij1 + nlon
              ij2 = ij2 + nlon
 850      continue

          write (6,*) ' '
          stop
      endif

c-----------------------------------------------------------------------
c       (10)  Window of array.
c-----------------------------------------------------------------------

      if (winddet) then
          write (0,'('' Limited part of array (y or n)?         : ''$)')
          read (5,'(a1)') ans
          if (ans.eq.'y') then
              write (0,
     *         '('' Enter min and max i coordinates         : ''$)')
              read (5,*) iL,iR
              write (0,
     *         '('' Enter min and max j coordinates         : ''$)')
              read (5,*) jD,jU
              if (iL.lt.1) iL = 1
              if (iR.gt.nlon) iR = nlon
              if (jD.gt.jU) then
                  if (jU.lt.1) jU = 1
                  if (jD.gt.nlat) jD = nlat
                  jx = jU
                  jU = -jD
                  jD = -jx
              else
                  if (jD.lt.1) jD = 1
                  if (jU.gt.nlat) jU = nlat
              endif
          else
              iL = 1
              iR = nlon
              write (0,
     *         '('' Should j values be descending (y or n)? : ''$)')
              read (5,'(a1)') ans
              if (ans.eq.'y') then
                  jD = -1
                  jU = -nlat
              else
                  jD = 1
                  jU = nlat
              endif
          endif
          write (0,'('' Factor to divide values by (f6.2 format)''/
     *               ''  (format is 1pe12.4) if zero)           : ''$)')
          read (5,*) scale

          call matrix(6,a,nlon,iL,iR,jD,jU,scale,spval)
          write (6,*) ' '
      endif

      stop

c-----------------------------------------------------------------------
c       (11)  Errors.
c-----------------------------------------------------------------------

 960  stop ' Incorrect usage.  Type "mapx -h" for help.'
 970  stop ' F takes integer argument (map no. in file).'
 971  stop ' e takes integer argument (map no. in file).'
 980  write (6,*) ' Cannot find file ',file1
      stop
 990  write (6,*) ' Cannot find file ',file2
      stop

c------------------------------------------------------------------------------
c       (12)  Usage
c------------------------------------------------------------------------------

      end

c==============================================================================

      Function achconv(k)

      character achconv*1
      if (k.le.0 .or. k.ge.57) then
        achconv = ' '
      else if (k.le.26) then
        achconv = char(k+96)
      else
        achconv = char(k+38)
      endif

      return
      end

c==============================================================================

      Function kchconv(a)

      character a*1

      k = ichar(a)
      if (k.ge.65 .and. k.le.90) then
        kchconv = k - 38 ! = 64 + 26
      else if (k.ge.97 .and. k.le.122) then
        kchconv = k - 96
      else
        kchconv = 0
      endif

      return
      end
