      Subroutine qmapwrite(nunit,nilt,njlt,nbuf,icompact,
     * nlat,lat,nlon,lon,head,a,ie)

      real lat(njlt),lon(nilt),a(nbuf)
      character head*80

      write (nunit) nlat
          if(nlat.lt.1.or.nlat.gt.njlt) then
            write (6,40) 'NLAT',nlat,njlt
 40         format(' ',a4,'=',i6,'.  Should be ',
     *       'between 1 and',i4)
            go to 100
          endif
      call qwrite(nunit,lat,nlat)
      write (nunit) nlon
          if(nlon.lt.1.or.nlon.gt.nilt) then
            write (6,40) 'NLON',nlon,nilt
            go to 100
          endif
      call qwrite(nunit,lon,nlon)
      write (nunit) head
      if ((icompact.ne.1).and.(nlon.ne.nilt)) then
          narray = nilt*nlat
          nini = nilt
          if (narray.gt.nbuf) go to 60
          call swrite(nunit,a,nilt,nlon,nlat)
      else
          narray = nlon*nlat
          nini = nlon
          if (narray.gt.nbuf) go to 60
          call qwrite(nunit,a,narray)
      endif
      return

 60   write (6,*) ' Limits ',nini,'*',nlat,' > buffer size ',nbuf,'.'
 100  ie = 3
      return

      end

      Subroutine qwrite(nunit,buf,nbuf)
      real buf(nbuf)
      write (nunit) buf
      return
      end

      Subroutine swrite(nunit,buf,nilt,nlon,nlat)
      real buf(nilt,nlat)
      write (nunit) ((buf(i,j),i=1,nlon),j=1,nlat)
      return
      end
