      Subroutine predvl

c       This routine contains entry points associated with the
c     prediction of cyclone displacements from a cyclone 
c     velocity statistics file.

c     Entry point advgrd reads the prediction velocity file.
c     Entry point intdx interpolates the velocities to the cyclone
c       position and converts to grid displacement/day.

c     Each entry point employs one or other of two codes depending on 
c     the value of iadvtp.

c     iadvtp = 1  Interpolates a 1-D latitude dependent array
c              2  Interpolates a 2-D Polar Stereographic array

c     Written by Ross Murray.
c     Last revised 6th Jan. 1996.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations and common blocks
c     ------------------------------------

c     character hilox*20
      character card*80
      real lat,latad
crmw
      real lon,lonad
crmw
      parameter (rad=57.2957,c2rad=2.*rad)
      parameter (nialt=121,njalt=91,nijalt=7000)
      dimension lonad(nialt),latad(njalt)
      dimension uad(nijalt),vad(nijalt)
      save nia,nja,lonad,latad,uad,vad,rprojsq,rscfac

      parameter (spval=99999.9)

c-----------------------------------------------------------------------
c     Advgrd
c-----------------------------------------------------------------------

      Entry advgrd(iunit)

c       This entry point reads cyclone prediction velocities 
c     from data given in an "advdat" file.

      rprojsq = rproj*rproj

      rscfac = 1./1.286      ! 1 deg.lat./day = 1/1.286 m./s.
      if (pstrak) rscfac = rscfac/(2.*rproj*rad)

      if (iadvtp.eq.2) go to 200

c     Zonally averaged (1-D) prediction velocity array
c     ------------------------------------------------

      if (idiagt.ge.1) write (6,*) ' '

      read (iunit,'(a80)',err=230) card
      if (idiagt.ge.1) write (6,*) card
c     read (card(xx:xx) hilox
c     if (hilox.ne.hilo) stop ' predvl: Wrong u/v prediction file.'
      read (iunit,*,err=240,end=250) nja
      do 120 ja = 1,nja
        read (iunit,'(a80)',end=250) card
        read (card,*,err=240) latad(ja),uad(ja),vad(ja)
        if (idiagt.ge.3) write (6,'(a)') card
  120 continue

      return

c     Grid point averaged (2-D) prediction velocity array
c     ---------------------------------------------------

  200 continue

      call qmapread(iunit,afile,nialt,njalt,nijalt,1,
     * nja,latad,nia,lonad,card,uad,ie)
      if (idiag.ge.1) write (6,'(a80)') card
      call qmapread(iunit,afile,nialt,njalt,nijalt,1,
     * nja,latad,nia,lonad,card,vad,ie)
      if (idiag.ge.1) write (6,'(a80)') card
      return

  230 stop ' predvl: End of average prediction velocities.'
  240 stop ' predvl: Error in read of average prediction velocities.'
  250 stop ' predvl: Premature end of average prediction velocities.'

c-----------------------------------------------------------------------

      Entry intdx(x,y,u,v,iadvuv)

c       This routine interpolates cyclone prediction velocity data
c     to a particular location, by one of two methods according to
c     the value of iadvtp

c-----------------------------------------------------------------------

c     Find latitude (and longitude) of point
c     --------------------------------------

      if (pstrak) then
        xs = x - xcen
        ys = y - ycen
        rssq = xs*xs + ys*ys
        rs = sqrt(rssq)
        lat = fhem*(90. - c2rad*atan(rs/rproj))
      else
        lat = y
      endif
      call srchls(lat,latad,nja,jlo,jhi)

c     Interpolate prediction velocities
c     ---------------------------------

      if (jlo.lt.1) go to 400
      if (jhi.gt.njalt) go to 400
      B2 = (lat - latad(jlo))/(latad(jhi)-latad(jlo))
      B1 = 1. - B2
      if (iadvtp.eq.1) then
        if (uad(jlo).eq.spval) go to 400
        if (uad(jhi).eq.spval) go to 400

        uex = B1*uad(jlo) + B2*uad(jhi)
        vnx = B1*vad(jlo) + B2*vad(jhi)
        if (idiagt.ge.4) write (6,'(/a/3f7.2,2i3,20f7.2)') 
     *   ' Prediction velocity diagnostics',
     *   x,y,lat,jlo,jhi,latad(jlo),latad(jhi),
     *   uad(jlo),uad(jhi),uex,
     *   vad(jlo),vad(jhi),vnx
      else
        if (pstrak) then
          lon = rad*atan2(xs,-ys*fhem)
        else
          lon = x
        endif
        if (lon.lt.c0)   lon = lon + c360
        if (lon.gt.c360) lon = lon - c360

        call srchls(lon,lonad,nia,ilo,ihi)
        if (ilo.lt.1) go to 400
        if (ilo.lt.1) go to 400
        if (ihi.gt.nialt) go to 400

        ilojlo = ilo + (jlo-1)*nia
        ihijlo = ilojlo + 1
        ilojhi = ilojhi + nia
        ihijhi = ilojhi + 1

        if (uad(ilojlo).eq.spval) go to 400
        if (uad(ihijlo).eq.spval) go to 400
        if (uad(ilojhi).eq.spval) go to 400
        if (uad(ihijhi).eq.spval) go to 400

        A2 = (lon - lonad(ilo))/(lonad(ihi)-lonad(ilo))
        A1 = 1. - A2
        uex = B1*(A1*uad(ilojlo)+A2*uad(ihijlo))+
     *        B2*(A1*uad(ilojhi)+A2*uad(ihijhi))
        vnx = B1*(A1*vad(ilojlo)+A2*vad(ihijlo))+
     *        B2*(A1*vad(ilojhi)+A2*vad(ihijhi))

        if (idiagt.ge.3) then
          write (6,'(/a)') ' Prediction velocity diagnostics'
          write (6,*) ' intdx (2) diagnostics'
          write (6,*) 'x,y,ilo,ihi,jlo,jhi',x,y,ilo,ihi,jlo,jhi
          write (6,*) 'A1,A2,B1,B2',A1,A2,B1,B2
          write (6,*)uad(ilojlo),uad(ihijlo),uad(ilojhi),uad(ihijhi)
          write (6,*)vad(ilojlo),vad(ihijlo),vad(ilojhi),vad(ihijhi)
          write (6,*) 'uex,vnx',uex,vnx
          write (6,*) ' '
        endif
      endif

c     Scale and rotate velocities
c     ---------------------------

      if (pstrak) then
        velfac = (rprojsq + rssq)*rscfac
        ue = uex*velfac
        vn = vnx*velfac
        costheta = -fhem*ys/rs
        sintheta = -fhem*xs/rs
        u =  ue*costheta + vn*sintheta
        v = -ue*sintheta + vn*costheta

        if (idiagt.ge.4) write (6,'(20f7.2)') 
     *   x,y,xs,ys,uex,vnx,ue,vn,u,v
      else
        u = uex*rscfac
        v = vnx*rscfac
      endif

      iadvuv = 1
      return

  400 continue
      u = spval
      v = spval
      iadvuv = 0
      return

      end
