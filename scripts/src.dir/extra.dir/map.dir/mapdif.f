      Program mapdif

      parameter (nilt=121,njlt=91,nbuf=10000)
      parameter (spval=99999.9)
      real lat(njlt),lon(nilt),a1(nbuf),a2(nbuf)
      character head1*80,fname1*80
      character head2*80,fname2*80
      character head3*80,fname3*80
      character run*27

      call getarg(1,fname1)
      call getarg(2,fname2)
      call getarg(3,fname3)

      open (1,file=fname1,form='unformatted',status='old')
      open (2,file=fname2,form='unformatted',status='old')
      open (3,file=fname3,form='unformatted',status='unknown')

      write (6,*) ' Reading from file: ',fname1(1:lnblnk(fname1))
      write (6,*) ' Subtracting file : ',fname2(1:lnblnk(fname2))
      write (6,*) ' Writing to file  : ',fname3(1:lnblnk(fname3))
      write (6,*) ' '

      do 200 imap = 1,100
        call qmapread(1,fname1,nilt,njlt,nbuf,1,
     *   nlat,lat,nlon,lon,head1,a1,ie)
        if (ie.ge.1) go to 210
        if (ie.ge.2) go to 220
        write (6,'(a80)') head1
        call qmapread(2,fname2,nilt,njlt,nbuf,1,
     *   nlat,lat,nlon,lon,head2,a2,ie)
        if (ie.ge.1) go to 210
        if (ie.ge.2) go to 220
        write (6,'(a80)') head2

        if (imap.eq.1) then
          write (6,*) ' nlat = ',nlat
          write (6,*) ' nlon = ',nlon
          write (6,'(a27,''####'')') head1(31:57)
          write (6,'(a27,''####'')') head2(31:57)
          write (6,*) ' Enter new run name:'
          read (5,'(a27)') run
        endif
        head3 = head1
        head3(31:57) = run
        write (6,'(a80/)') head3

        do 100 ij = 1,nlat*nlon
          if ((a1(ij).eq.spval).or.(a2(ij).eq.spval)) then
            a1(ij) = spval
          else
            if ((abs(a1(ij)).gt.1000.).or.
     *          (abs(a2(ij)).gt.1000.)) 
     *       write (6,*) i,j,a1(ij),a2(ij)
            a1(ij) = a1(ij) - a2(ij)
          endif
  100   continue

        call qmapwrite(3,nilt,njlt,nbuf,1,
     *   nlat,lat,nlon,lon,head3,a1,ie)

  200 continue
  210 close (1)
      close (2)
      close (3)
      stop
  220 stop ' Error in reading file.'
      end
