c     File "psio.f"

c       This file contains subroutines for reading unformatted polar
c     stereographic array data associated with programme "cycloc",
c     where variables are defined.

c     Written by R.J. Murray
c     Last revised 17th Dec., 1995.

c-----------------------------------------------------------------------

      Subroutine pswr(iunit,quant,level,lunit,source,dmode,unit,grid,
     * ni,nj,hemis,xcen,ycen,rproj,da,hr,fij,spval,rdiff)

      integer da,hr
      character quant*8,level*9,lunit*10,source*10,grid*17,
     * unit*12,dmode*6
      character hemis*1
      dimension fij(ni*nj)
      character*10 fileid
      data fileid/'psdata'/

      write (iunit) fileid,quant,level,lunit,source,dmode,unit,grid,
     * ni,nj,hemis,xcen,ycen,rproj,da,hr,spval,rdiff
      write (iunit) fij

      return
      end

c-------------------------------------------------------------------------------

      Subroutine psrd(iunit,quant,level,lunit,source,dmode,unit,grid,
     * ni,nj,hemis,xcen,ycen,rproj,da,hr,fij,nijlt,spval,rdiff,ier)

      integer da,hr
      character quant*8,level*9,lunit*10,source*10,grid*17,unit*12,
     * dmode*6
      character hemis*1
      dimension fij(nijlt)
      character*10 fileid

      ier = 0
      read (iunit,err=20,end=10) fileid,quant,level,lunit,source,
     * dmode,unit,grid,ni,nj,hemis,xcen,ycen,rproj,da,hr,spval,
     * rdiff

      if (fileid(1:6).ne.'psdata') stop
     * ' Not a polar stereographic data file.'
      if (hemis.eq.'n') hemis = 'N'
      if (hemis.eq.'s') hemis = 'S'
      if (ni*nj.gt.nijlt) go to 30
      read (iunit,err=20,end=20) (fij(ij),ij=1,ni*nj)
      return

   10 ier = 1
      return
   20 ier = 2
      return
   30 ier = 3
      return

      end
