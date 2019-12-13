c-------------------------------------------------------------------------------
c     Modified 09/12/2019, Ahmad Galea (Science IT University of Melbourne)
c     
c     Date format changed from yymmdd to yyyymmdd.
c-------------------------------------------------------------------------------
      
      Subroutine tstatzone(irun,dystrts,dystops,hrstrts,hrstops,
     * hemis,level,feat,dts,dlat,lslt,rproj,projn,diss2,djss1,dmodet,
     * zdet,adet,exfmt)

      parameter (lsmax=45)
      parameter (rad=57.2957)

      integer dystrts,dystops,hrstrts,hrstops
      character hemis*4,level*6,feat*12,projn*1,qfmt*45,rfmt*25
      character dmodet*6,exfmt*40
      logical zdet,adet

      common /blztstat/ifglz(lsmax),ifllz(lsmax),flz(lsmax),
     * ulz(lsmax),vlz(lsmax),plz(lsmax),ptlz(lsmax),wlz(lsmax),
     * clz(lsmax),ctlz(lsmax)
      dimension alat(lsmax)

      if (zdet) then
      if ((dmodet(1:3).eq.'ddd').or.(dmodet(1:3).eq.'DDD')) then
        write (rfmt,'('' RUN '',i4,'' DAYS '',i3,'' - '',i3,'' '')')
     *   irun,dystrts,dystops
      else
        write (rfmt,'(i8,i4,'' - '',i8,i4,'' '')')
     *   dystrts,hrstrts,dystops,hrstops
      endif
      write (qfmt,'(a4,x,a6,x,a12,'' (LASTING '',f5.3,'' DAYS)'')')
     * hemis,level,feat,dts

      write (32,10) qfmt,rfmt,exfmt
  10  format (/'ZONAL STATISTICS OF ',a45,1x,a25,1x,a40/)
      write (32,'(
     *  ''Lat.      Mid.   Cyclogen.  Cyclolys.   System      '',
     *  ''Scalar E.wd   N.ward Vector  E.wd  N.wd   '',
     *  ''Cent.  Cent. Dsq.p Dsq.p  Mean   Cent.''/

     *  ''zone      Lat.   No. No/d/  No. No/d/   Density     '',
     *  ''Flux   Flux   Flux   Flux    vel.  vel.   '',
     *  ''Press. Press mb./  tend.  Press. Press''/

     *  ''                     1000       1000    No. No/1000 '',
     *  ''No./d/ No./d/ No./d/ No./d/  m/sec m/sec  '',
     *  ''mb.    tend. dlsq. mb./   mb.    Diff.''/

     *  ''                     dl.sq      dl.sq       dlsq    '',
     *  ''1000dl 1000dl 1000dl 1000dl               '',
     *  ''       mb./d       dlsq/d        mb.''/)')
      endif

          polgrdsz = 2.*rad/rproj
          afact = 360.*rad
      do 30 ls = lslt,1,-1
          alathi  = 90. + (1 - ls)*dlat
          alatlo  = alathi - dlat
          alatav  = (alathi + alatlo)*0.5
          alat(ls) = alatav
          if ((projn.ne.'n').or.(projn.ne.'N')) alat(ls) = -alatav
          phihi = alathi/rad
          philo = alatlo/rad
          zarea = afact*(sin(phihi) - sin(philo))
          coshaphi = cos((90. - alatav)*0.5/rad)
          scallat  = coshaphi*coshaphi*polgrdsz

              uu = ulz(ls)*scallat
              vv = vlz(ls)*scallat
          if (flz(ls).le.0.) then
              ulz(ls) = 0.
              vlz(ls) = 0.
              plz(ls) = 0.
              ptlz(ls) = 0.
              clz(ls) = 0.
              ctlz(ls) = 0.
          else
c             if ((projn.eq.'n').or.(projn.eq.'N')) then
c                 uu = -uu
c                 vv = -vv
c             endif
              ulz(ls) = uu/flz(ls)*1.286
              vlz(ls) = vv/flz(ls)*1.286
              plz(ls) = plz(ls)/flz(ls)
              ptlz(ls) = ptlz(ls)/flz(ls)
              clz(ls) = clz(ls)/flz(ls)
              ctlz(ls) = ctlz(ls)/flz(ls)
          endif

          if (zdet) then
              fglz = ifglz(ls)*1000./zarea/diss2
              fllz = ifllz(ls)*1000./zarea/diss2
              sdlz = flz(ls)*1000./zarea/djss1
              fslz = wlz(ls)*scallat*1000./zarea/djss1
              felz = uu*1000./zarea/djss1
              fnlz = vv*1000./zarea/djss1
              fvlz = sqrt(felz*felz + fnlz*fnlz)

              iflz = flz(ls)
              write (32,20) alatlo,alathi,alatav,ifglz(ls),fglz,
     *         ifllz(ls),fllz,iflz,sdlz,fslz,felz,fnlz,fvlz,
     *         ulz(ls),vlz(ls),plz(ls),ptlz(ls),clz(ls),ctlz(ls)
  20          format (f4.1,'-',f4.1,f5.1,1x,2(i5,f6.2),i7,f7.2,
     *         4f7.1,1x,2f6.1,f8.1,f6.1,f6.2,f7.2,f7.1,f6.1)
          endif
  30  continue

      if (adet) then
          if ((projn.eq.'n').or.(projn.eq.'N')) then
              ls2 = 1
              ls1 = lslt
              incr = -1
          else
              ls1 = 1
              ls2 = lslt
              incr = 1
          endif

              write (33,*) 'PREDICTION VELOCITIES U COMPONENT'
              write (33,*) lslt
          do 40 ls = ls1,ls2,incr
              write (33,'(f6.2,3x,f6.2)') alat(ls),ulz(ls)
  40      continue

              write (33,*) 'PREDICTION VELOCITIES V COMPONENT'
              write (33,*) lslt
          do 50 ls = ls1,ls2,incr
              write (33,'(f6.2,3x,f6.2)') alat(ls),vlz(ls)
  50      continue
      endif

      return
      end
