      Subroutine checknos(nilt,njlt,nbuf,nlat,lat,nlon,lon,a,ierno)

      real lat(njlt),lon(nilt),a(nbuf)

          ierno = 0
      do 10 i = 1,nlon
          if (.not.((lon(i).ge.0.).or.(lon(i).le.0.))) then
              write (6,*) ' lat(',i,') = ',lat(i)
              ierno = 1
          endif
 10   continue
      do 20 j = 1,nlat
          if (.not.((lat(j).ge.0.).or.(lat(j).le.0.))) then
              write (6,*) ' lat(',j,') = ',lat(j)
              ierno = 1
          endif
 20   continue
          ij = 0
      do 30 j = 1,nlat
      do 30 i = 1,nlon
          ij = ij + 1
          if (.not.((a(ij).ge.0.).or.(a(ij).le.0.))) then
              write (6,*) ' a(',i,',',j,') = ',a(ij)
              ierno = 1
          endif
 30   continue

      return
      end
