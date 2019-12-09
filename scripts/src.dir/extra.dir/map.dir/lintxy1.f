      Subroutine lintxy(nlat,lat,nlon,lon,nlat2,lat2,nlon2,
     * lon2,iworldS,iworldN,iworldx,spv,ivect,a,b,ie)

      parameter (nilt=361)

      real lat(nlat),lon(nlon),latpole,loni2,latj2
      real lat2(nlat2),lon2(nlon2)
      dimension a(nlon,nlat),b(nlon2,nlat2)
      dimension Ax(nilt),Bx(nilt),i1l(nilt)

      write (6,*) nlat2,lat2,nlon2,lon2

          alon1p360 = lon(1) + 360.
          alonnm360 = lon(nlon) - 360.
      do 40 i2 = 1,nlon2
          loni2 = lon2(i2)
          if (loni2.gt.alon1p360) then
              diff = mod((loni2-lon(1)),360.)
              loni2 = lon(1) - diff
          endif
          if (loni2.lt.alonnm360) then
              diff = mod((lon(nlon)-loni2),360.)
              loni2 = lon(nlon) + diff
          endif
          if ((iworldx.eq.1).or.(iworldx.eq.3)) then
              call findi1(loni2,nlon,lon,i1l(i2),i1hi,ie)
          else
              if ((loni2.lt.lon(1)).or.(loni2.gt.lon(nlon))) then
                  i1l(i2) = nlon
                  i1hi = 1
              else
                  call findi1(loni2,nlon,lon,i1l(i2),i1hi,ie)
              endif
          endif
          difflon  = lon(i1hi) - lon(i1l(i2))
          difflon1 = lon(i1hi) - loni2
          if (difflon.lt.0.) difflon = difflon + 360.
          if (difflon1.lt.0.) difflon = difflon + 360.
          Ax(i2) = difflon1 /difflon
          Bx(i2) = 1. - Ax(i2)
          i1lo = i1l(i2)
          write (6,*) 'i2=',i2,loni2,i1lo,i1hi,lon(i1lo),lon(i1hi),
     *     Ax(i2),Bx(i2)
 40   continue

          nlonx = nlon
          if (iworldx.eq.3) nlonx = nlonx - 1
          nlonxh = nlonx/2
          rv = ivect
       
      do 70 j2 = 1,nlat2
          latj2 = lat2(j2)

          if ((ivect.ne.0).and.((iworldS.eq.2).and.(latj2.lt.lat(1))).
     *     or.((iworldN.eq.2).and.(latj2.gt.lat(nlat)))) then

          if (latj2.lt.lat(1)) then
              j1lo = 1
              latpole = -90.
          else
              j1lo = nlat
              latpole = 90.
          endif
          By = 0.5*(latj2-lat(j1lo))/(latpole-lat(j1lo))
          Ay = 1. - By
          write (6,*) 'Outside'
          write (6,*) 'j2=',j2,latj2,j1lo,j1lo,lat(j1lo),lat(j1lo),
     *     Ay,By
      do 50 i2 = 1,nlon2
          i1lo = i1l(i2)
          i1hi = i1l(i2) + 1
          if (i1hi.gt.nlon) i1hi = 1
          iopplo = i1lo + nlonxh
          if (iopplo.gt.nlon) iopplo = i1lo - nlonxh
          iopphi = i1hi + nlonxh
          if (iopphi.gt.nlon) iopphi = i1hi - nlonxh
          write (6,*) 'i2=',i2,lati2,i1lo,i1hi,lon(i1lo),lon(i1lo),
     *     Ay,By,iopplo,iopphi
          if ((a(i1lo,j1lo).eq.spv).or.(a(i1hi,j1lo).eq.spv).or.
     *        (a(iopplo,j1lo).eq.spv).or.(a(iopphi,j1lo).eq.spv)) then
          b(i2,j2) = spv
          else
          b(i2,j2) = Ay*(Ax(i2)*a(i1lo,j1lo) + Bx(i2)*a(i1hi,j1lo)) +
     *            rv*By*(Ax(i2)*a(iopplo,j1lo) + Bx(i2)*a(iopphi,j1lo))
          write (6,*) '     ',a(i1lo,j1lo),a(i1hi,j1lo),a(iopplo,j1lo),
     *       a(iopphi,j1lo),b(i2,j2)
          endif
 50   continue

          else

          call findi1(latj2,nlat,lat,j1lo,j1hi,ie)
          Ay = (lat(j1hi)-latj2)/(lat(j1hi) -lat(j1lo))
          By = 1. - Ay
          write (6,*) 'j2=',j2,latj2,j1lo,j1hi,lat(j1lo),lat(j1hi),
     *     Ay,By
      do 60 i2 = 1,nlon2
          i1lo = i1l(i2)
          i1hi = i1l(i2) + 1
          if (i1hi.gt.nlon) i1hi = nlon
          if ((a(i1lo,j1lo).eq.spv).or.(a(i1hi,j1lo).eq.spv).or.
     *        (a(i1lo,j1hi).eq.spv).or.(a(i1hi,j1hi).eq.spv)) then
          b(i2,j2) = spv
          else
          b(i2,j2) = Ay*(Ax(i2)*a(i1lo,j1lo) + Bx(i2)*a(i1hi,j1lo)) +
     *               By*(Ax(i2)*a(i1lo,j1hi) + Bx(i2)*a(i1hi,j1hi))
          endif
 60   continue
      endif

 70   continue

      return
      end
   

      Subroutine bisplxy(nlat,lat,nlon,lon,nlat2,lat2,nlon2,
     *     lon2,iworldS,iworldN,iworldx,spv,ivect,a,b,ie)

      return
      end
