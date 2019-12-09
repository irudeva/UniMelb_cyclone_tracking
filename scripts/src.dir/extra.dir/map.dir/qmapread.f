      Subroutine qmapread(nunit,filename,nilt,njlt,nbuf,icompact,
     * nlat,lat,nlon,lon,head,a,ie)

      real lat(njlt),lon(nilt),a(nbuf)
      character head*80,filename*80

      ie = 0

      irec = 1
      read (nunit,err=70,end=50) nlat
          if(nlat.lt.1.or.nlat.gt.njlt) then
              write (6,30) 'NLAT',nlat,njlt
 30           format(' ',a4,'=',i6,'.  Should be between 1 and',i4)
              go to 100
          endif
      irec = 2
      call qread(nunit,lat,nlat,ie)
          if (ie.eq.2) go to 70
          if (ie.eq.1) go to 80
      irec = 3
      read (nunit,err=70,end=80) nlon
          if(nlon.lt.1.or.nlon.gt.nilt) then
              write (6,30) 'NLON',nlon,nilt
              go to 100
          endif
      irec = 4
      call qread(nunit,lon,nlon,ie)
          if (ie.eq.2) go to 70
          if (ie.eq.1) go to 80
      head = ' '
      irec = 5
      read (nunit,err=70,end=80) head
      irec = 6
      if ((icompact.ne.1).and.(nlon.ne.nilt)) then
          narray = nilt*nlat
          nini = nilt
          if (narray.gt.nbuf) go to 60
          call sread(nunit,a,nilt,nlon,nlat,ie)
      else
          narray = nlon*nlat
          nini = nlon
          if (narray.gt.nbuf) go to 60
          call qread(nunit,a,narray,ie)
      endif
          if (ie.eq.2) go to 70
          if (ie.eq.1) go to 90
      return

c       Errors
c       ------

 50   ie = 1
      return
 60   write (6,*) ' Limits ',nini,'*',nlat,' > buffer size ',nbuf,'.'
      ie = 3
      return
 70   write (6,'('' Error during read from '',a)') filename
      write (6,'('' Record number '',i1)') irec
      ie = 3
      return
 80   write (6,'('' Unexpected end of file '',a)') filename
      ie = 3
      return
 90   write (6,*) head
      write (6,'('' Unexpected end of main data in file '',a)') filename
      ie = 2
      return
 100  ie = 3
      return

      end

      Subroutine qread(nunit,buf,nbuf,ie)

      real buf(nbuf)

      ie = 0

      read (nunit,err=10,end=20) buf
      return

 10   ie = 2
      return
 20   ie = 1
      return

      end

      Subroutine sread(nunit,buf,nilt,nlon,nlat,ie)

      real buf(nilt,nlat)

      ie = 0

      read (nunit,err=10,end=20) ((buf(i,j),i=1,nlon),j=1,nlat)
      return

 10   ie = 2
      return
 20   ie = 1
      return

      end
