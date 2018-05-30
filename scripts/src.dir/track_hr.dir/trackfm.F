      Program trackfm

c       This programme reads unformatted output from "trackx" and
c     writes it in formatted form.

c     Written by Ross Murray.
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      character tfile*80
      integer optind
      character*80 optarg
      logical idet,vdet,fdet,tdet
c     integer dastrt,dastop
      integer trk,da,hr
      character*10 fileid
      logical ltrue,lfals
      data ltrue,lfals/.true.,.false./

c-----------------------------------------------------------------------
c     File checking
c-----------------------------------------------------------------------

      trfm = lfals
      vdet = lfals
      idet = lfals
      fdet = lfals
      tdet = lfals
      fhem = 0.
c     dastrt = 0
c     dastop = 999999
   10 continue
      nopt = ngtopt("Fvitfa:b:NS",optind,optarg)
      if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'i') idet = ltrue
        if (char(nopt).eq.'v') vdet = ltrue
        if (char(nopt).eq.'t') tdet = ltrue
        if (char(nopt).eq.'f') fdet = ltrue
        if (char(nopt).eq.'F') trfm = ltrue
        if (char(nopt).eq.'N') fhem = 1.
        if (char(nopt).eq.'S') fhem = -1.
c       if (char(nopt).eq.'a') read (optarg,'(i6)',err=910) dastrt
c       if (char(nopt).eq.'b') read (optarg,'(i6)',err=910) dastop
      go to 10
c  15 write (0,*) ' Usage: trackfmx [-ivtfF] [-a dastrt -b dastop]',
   15 write (0,*) ' Usage: trackfmx [-ivtfF] thist or track filename'
      write (0,*) ' '
      write (0,*) '   -F  formatted input file to be used'
      write (0,*) '   -i  full list of instruction parameters'
      write (0,*) '   -v  track numbers at start of each day'
      write (0,*) '   -t  track summaries'
      write (0,*) '   -f  full track histories'
      stop
   20 continue
      if (iargc().lt.optind) go to 15

      if (iargc().lt.optind) stop 
     * ' trackfmx requires filename as argument.'
      call getarg(optind,tfile)

      if (trfm) then
        open (unit=1,file=tfile,status='old',form='formatted',
     *   err=920)
        write (0,*) ' Reading formatted file: ',
     *   tfile(1:lnblnk(tfile))
        read (1,'(a10)',err=940,end=930) fileid
        rewind (1)
        if (fileid.eq.' TRACK HIS') go to 200
        if (fileid(1:6).eq.' Track') go to 400
        stop ' trackfm: Not a track history file.'
      else
        open (unit=1,file=tfile,status='old',form='unformatted',
     *   err=920)
        write (0,*) ' Reading unformatted file: ',
     *   tfile(1:lnblnk(tfile))
        read (1,err=940,end=930) fileid
        rewind (1)
        if (fileid(1:6).eq.'thist1') go to 200
        if (fileid(1:5).eq.'thnml') go to 200
        if (fileid(1:6).eq.'thist2') go to 400
        write (6,*) ' Fileid: ',fileid
        stop ' trackfm: Not a track history file.'
      endif

c-----------------------------------------------------------------------
c     Instruction parameters
c-----------------------------------------------------------------------

  200 continue

      trnml = fileid(1:5).eq.'thnml'
      call tr1rd(1,2,trfm,trnml,ierd)
      if (ierd.eq.1) stop ' trackfm: End of track history content file.'
      if (ierd.eq.2) stop
     * ' trackfm: Error in reading track history content file.'
      if (ierd.eq.3) stop ' trackfm: Not a track history content file.'

      if (idet) then
        call tr1wr(6,2,ltrue,lfals)
      else
        write (6,260) da1,hr1,dac,hrc,t1,tc,ic,
     *   lstrk1,lstrk2,ndt,nkc
  260   format (/' HISTORY FILE : ',i6,1x,i4,' to ',i6,1x,i4,
     *   ' (t = ',f7.2,' - ',f7.2,'; i = 1,',i3,')'/
     *   '     (lstrk1 = ',i5,', lstrk2 = ',i5,
     *   ', ndt = ',f7.3,', nkc = ',i3,').')
      endif

c-----------------------------------------------------------------------
c     Track histories.
c-----------------------------------------------------------------------

  400 continue

      ifstlast = 0
      do 480 trkdash = 1,99999

c       Read track header
c       -----------------

        call tr2rd(1,trfm,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3,itabt4,ierd)
        if (ierd.eq.1) go to 490
        if (ierd.eq.2)stop ' trackfm: Error in reading track data file.'
        if (ierd.eq.3)stop ' trackfm: Not a track data file.'

        if (trkdash.eq.1) call trtabhd(itabt3,itabt4,nvarlt)

c       Read point data
c       ---------------

        if (fdet) then
          call tr3rd(1,trfm,lfals,itabt3,itabt4,nvarlt,nitlt,
     *     nit,vart,ierd)
          if (ierd.eq.1) stop ' trackfm: End of track data points.'
          if (ierd.eq.2) stop 
     *     ' trackfm: Error in reading track data points.'
          call tr2wr(6,ltrue,trk,statf,statl,ifst,ilst,nit,
     *     idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          if (fhem.ne.0.) then
            do 460 k = 1,nk
              call lltops(xcen,ycen,rproj,fhem,alon,alat,xc(k),yc(k))
              xc(k) = alon
              yc(k) = alat
  460       continue
          endif
          call tr3wr(6,ltrue,itabt3,itabt4,nvarlt,nitlt,nit,vart)
        else
          call tr3rd(1,trfm,ltrue,itabt3,itabt4,nvarlt,nitlt,
     *     nit,vart,ierd)
          if (ierd.eq.1) stop ' trackfm: End of track data points.'
          if (ierd.eq.2) stop 
     *     ' trackfm: Error in reading track data points.'
          if (tdet) then
            call tr2wr(6,ltrue,trk,statf,statl,ifst,ilst,nit,
     *       idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          else if (vdet) then
            if (ifst.gt.ifstlast) write (6,470) ifst,idafst,ihrfst,trk
  470       format (/i4,':  ',i6,1x,i4,'  Tracks ',i5,' ...',$)
            ifstlast = ifst
            itrklast = trk
          endif
        endif
  480 continue
  490 continue

      if (vdet) then
        write (6,'(x,i4)') itrklast
        write (6,'()')
      endif

      stop

c-----------------------------------------------------------------------
c     Error conditions
c-----------------------------------------------------------------------

  910 stop ' trackfm: Error in reading optional argument (format i6).'
  920 stop ' trackfm: Error in opening file.'
  930 stop ' trackfm: End of track history content file .'
  940 stop ' trackfm: Error in reading track history content file.'
      end
