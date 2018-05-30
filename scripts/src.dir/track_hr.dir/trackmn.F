      Program trackmn

c       This programme reads unformatted or formatted output from 
c     "trackx" and performs various manipulations of the data, viz.,

c     1. Producing day or track summaries of a track file;
c     2. Converting the data between formatted and unformatted
c          forms;
c     3. Culling weak or open systems or certain data periods from
c          the record;
c     4. Producing an interpolated track data file (not implemented).

c     Written by Ross Murray.
c     Modified from "trackfm.F"; Last revised 11th Jul., 1996.

c
c * Important Notes - Kevin Keay 13/05/03
c
c      (1) Initialised interp to .false. - this is the default for f77
c          on the Sun but the default with g77 (Sun or Linux) is .true. !!
c      (2) Made some minor adjustments to formats and intrinsic function calls
c          - see: diff.lis.
c

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      character*80 tfile,infile,ofile
      character card*80,mark*3
      integer optind,statf,statl
      character*80 optarg
      integer trk,trknew
      character*10 fileid
      logical skip,charttrk
      logical idet,vdet,ddet,tdet,diag
      logical man,odet,renumb,clipdh,cliptk,clipgl,clipas
     * ,interp,wrgl
      logical ltrue,lfals
      data ltrue,lfals/.true.,.false./

      parameter (r6=1./6.)
      parameter (njtlt=5*nitlt)
      dimension x1(nitlt),y1(nitlt),z1(nitlt),gam(nitlt)
      dimension xtt1(nitlt),ytt1(nitlt),ztt1(nitlt)
      dimension xt2(500),yt2(500),t2(500)

      namelist /nmltrkmn/dastrt,hrstrt,dastop,hrstop,dttrk,
     * ioptk,cctk,dptk,fmxtk,iopgl,ccgl,dpgl,fmxgl,delt,clipas

c-----------------------------------------------------------------------
c     File checking
c-----------------------------------------------------------------------

      interp   = lfals
      trfmin   = lfals
      trnmlin  = lfals
      trfm  = ltrue
      trnml = lfals
      vdet = lfals
      idet = lfals
      ddet = lfals
      tdet = lfals
      diag = lfals
      renumb = lfals
      odet = lfals
      iomn = 6
      charttrk = lfals
      wrgl = lfals
   10 continue
      nopt = ngtopt("i:o:FulIvtdDfRcCg",optind,optarg)
      if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'i') then
          infile = optarg
          man = .true.
        endif
        if (char(nopt).eq.'o') then
          ofile = optarg
          odet = .true.
          iomn = 3
        endif
        if (char(nopt).eq.'F') trfmin   = ltrue
        if (char(nopt).eq.'u') trfm  = lfals
        if (char(nopt).eq.'l') trnml = ltrue
        if (char(nopt).eq.'I') idet = ltrue
        if (char(nopt).eq.'v') vdet = ltrue
        if (char(nopt).eq.'t') tdet = ltrue
        if (char(nopt).eq.'D') diag = ltrue
        if (char(nopt).eq.'d') ddet = ltrue
        if (char(nopt).eq.'f') ddet = ltrue
        if (char(nopt).eq.'R') renumb = ltrue
        if (char(nopt).eq.'c') interp = ltrue
        if (char(nopt).eq.'g') wrgl   = ltrue
        if (char(nopt).eq.'C') charttrk = ltrue
      go to 10
   15 write (0,'(a,a)') 
     * ' Usage: trackfmx [-FulIvtdD] [-i infile [-R]]',
     * ' [-o outfile] thist or trackfile'
      write (0,*) ' '
      write (0,*) '   -i: input file specifying data manipulations'
      write (0,*) '   -o: output data file (otherwise std output)'
      write (0,*) '   -F  formatted input file expected'
      write (0,*) '   -u  unformatted output file to be used'
      write (0,*) '   -l  unformatted output file with namelist'
      write (0,*) '   -I  full list of instruction parameters'
      write (0,*) '   -v  track numbers at start of each day'
      write (0,*) '   -t  track summaries'
      write (0,*) '   -f  full track histories'
      write (0,*) '   -R  renumber tracks'
      write (0,*) '   -C  assume chart track format'
      write (0,*) '   -D  write diagnostics'
      stop
   20 continue
      if (iargc().lt.optind) go to 15

      if (charttrk) trfmin = .true.

      if (iargc().lt.optind) stop 
     * ' trackfmx requires filename as argument.'
      call getarg(optind,tfile)

      iddet= 0
      itdet= 0
      ivdet= 0
      if (ddet) iddet= 1
      if (tdet) itdet= 1
      if (vdet) ivdet= 1
      if ((iddet+itdet+ivdet).gt.1) stop 
     * ' trackmn: Cannot have more than one of options d,t,v.'

      ddet = ddet.or.man
      if (.not.ddet) then
        trfm  = ltrue
        trnml = lfals
      endif
      trfm = tdet.or.vdet.or.(trfm.and.(.not.trnml))

      if (diag) idiagt = 1
      if (man) then
        if (diag) write (6,*) ' man = T'
        dastrt = 0
        dastop = 999999
        hrstrt = 0
        hrstop = 0
        ddet   = ltrue
        ioptk  = 999
        cctk   = 0.
        dptk   = -999.
        fmxtk  = 1.e6
        iopgl  = 999
        ccgl   = 0.
        dpgl   = -999.
        fmxgl  = 1.e6
        dttrk  = 0.
        delt   = 0.
        clipas = .false.

        open (2,file=infile,form='formatted')
        read (2,nmltrkmn)
        close (2)

        ioptk = min(ioptk,iopgl)
        cctk  = max(cctk,ccgl)
        dptk  = max(dptk,dpgl)
        fmxtk = min(fmxtk,fmxgl)

        write (6,nmltrkmn)

c       clipdh = (dastrt.gt.0 .or. dastop.ne.0)
        clipdh = (dastrt.gt.0 .or. dastop.ne.0 .or. dttrk.gt.0.)
        cliptk = (ioptk.ne.999 .or. (cctk.gt.0.) .or. 
c    *   (dptk.gt.0.) .or. (fmxtk.ne.1.e6) .or. dttrk.gt.0.)
     *   (dptk.gt.0.) .or. (fmxtk.ne.1.e6))
        clipgl = (iopgl.ne.999 .or. (ccgl.gt.0.) .or. 
     *   dpgl.gt.0. .or. (fmxgl.ne.1.e6))
        clipas = clipas .and. (clipdh.or.cliptk.or.clipgl)
        if (diag) write (6,*) ' clipdh = ',clipdh,' cliptk = ',
     *   cliptk,' clipgl = ',clipgl,' clipas = ',clipas
      endif

      skip = .not.ddet

      if (iomn.eq.3) then
        if (trfm) then
          open (3,file=ofile,form='formatted')
        else
          open (3,file=ofile,form='unformatted')
        endif
      endif

      if (trfmin) then
        open (unit=1,file=tfile,status='old',form='formatted',
     *   err=920)
        write (0,*) ' Reading formatted file: ',
     *   tfile(1:lnblnk(tfile))
        if (charttrk) go to 400
        read (1,'(a10)',err=940,end=930) fileid
        rewind (1)
        if (fileid.eq.' TRACK HIS') go to 200
        if (fileid(1:6).eq.' Track') go to 400
        stop ' trackmn: Not a track history file.'
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
        stop ' trackmn: Not a track history file.'
      endif

c-----------------------------------------------------------------------
c     Instruction parameters
c-----------------------------------------------------------------------

  200 continue

      trnmlin = fileid(1:5).eq.'thnml'
      call tr1rd(1,2,trfmin,trnmlin,ierd)
      if (ierd.eq.1) stop ' trackmn: End of track history content file.'
      if (ierd.eq.2) stop 
     * ' trackmn: Error in reading track history content file.'
      if (ierd.eq.3) stop ' trackmn: Not a track history content file.'

      if (idet.or.man) then
        call tr1wr(iomn,2,trfm,trnml)
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
      trknew   = 0
      do 600 trkdash = 1,99999

        if (charttrk) then
  270     read (1,'(a80)',err=610,end=610) card
          if (lnblnk(card).eq.0) go to 270
          read (card,*) trk
          do 280 it = 1,10000
            read (1,'(a80)',err=610,end=610) card
            if (lnblnk(card).eq.0) go to 290
            read (card,'(a3,1x,i2,f6.1,f8.1)') mark,iopt(it),
     *       xt(it),yt(it)
            nlb = lnblnk(mark)
            if (mark(nlb:nlb).eq.'''') then
              nlb = nlb - 1
              dt = 0.5
              hrt(it) = 1200
            else if (mark(nlb:nlb).eq.',') then
              nlb = nlb - 1
              dt = 0.25
              hrt(it) = 600
            else if (mark(nlb:nlb).eq.'"') then
              nlb = nlb - 1
              dt = 0.75
              hrt(it) = 1800
            else
              dt = 0.
              hrt(it) = 0
            endif
            read (mark(1:nlb),*) day
            tt(it) = day + dt
            dat(it) = nint(day)
            nit = it
  280     continue
          go to 290
        endif
 
c       Read track header
c       -----------------

        call tr2rd(1,trfmin,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3,itabt4,ierd)
        if (ierd.eq.1) go to 610
        if (ierd.eq.2) stop 
     *   ' trackmn: Error in reading track data file.'
        if (ierd.eq.3) stop ' trackmn: Not a track data file.'
        if (nit.eq.0 .or. ifst.eq.0 .or. ilst.lt.ifst .or.
     *   nit.ne.(ilst-ifst+1)) then
          write (6,*) 'trk,nit,ifst,ilst ',trk,nit,ifst,ilst
          call tr2wr(iomn,ltrue,trk,statf,statl,ifst,ilst,nit,
     *     idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          stop
        endif

        if (trkdash.eq.1) call trtabhd(itabt3,itabt4,nvarlt,
     *   idiagt)

c       Read point data
c       ---------------

        call tr3rd(1,trfmin,skip,itabt3,itabt4,nvarlt,nitlt,
     *   nit,vart,ierd)
        if (ierd.eq.1) stop ' trackmn: End of track data points.'
        if (ierd.eq.2) stop 
     *   ' trackmn: Error in reading track data points.'

  290   continue

c       Manipulate point data
c       ---------------------

        if (man) then

c         1.  Remove any system which falls entirely outside the required 
c             data period or persists for less than the required lifetime
c             -----------------------------------------------------------

          if (clipdh) then
            if (dat(nit).lt.dastrt .or. (dat(nit).eq.dastrt .and. 
     *       hrt(nit).lt.hrstrt)) go to 590
            if (dat(1  ).gt.dastop .or. (dat(1  ).eq.dastop .and. 
     *       hrt(1  ).gt.hrstop)) go to 590
            if (tt(nit)-tt(1).lt.dttrk) go to 590
          endif
          if (diag) write (6,*) 'not clipped dh (1st cull)'

c         2.  Remove any tracks which do not fulfil requirements
c             --------------------------------------------------

          if (cliptk) then
            do 300 it = 1,nit
              if (iopt(it).le.ioptk .and. ct(it).ge.cctk .and. 
     *         dpt(it).ge.dptk .and. pt(it).le.fmxtk) go to 310
  300       continue
            go to 590
  310       continue
          endif
          if (diag) write (6,*) 'not clipped tk'

c         3.  Determine those parts of each track which lie within the times 
c             that the system first and last met certain requirements.
c             --------------------------------------------------------------

          if (clipgl) then
            do 410 it = 1,nit
              if (iopt(it).le.iopgl .and. ct(it).ge.ccgl 
     *         .and. dpt(it).ge.dpgl .and. pt(it).le.fmxgl) go to 420
  410       continue
            go to 590
  420       continue
            itg = it

            do 430 it = nit,itg,-1
              if (iopt(it).le.iopgl .and. ct(it).ge.ccgl 
     *         .and. dpt(it).ge.dpgl .and. pt(it).le.fmxgl) go to 440
  430       continue
            go to 590
  440       continue
            itl = it
          else
            itg = 1
            itl = nit
          endif
          if (diag) write (6,*) 'not clipped gl'

c         4.  Remove the parts of tracks which fall outside the
c               required data period.
c             -------------------------------------------------

          if (clipdh) then
            if (dat(1  ).lt.dastrt .or. (dat(1  ).eq.dastrt .and. 
     *       hrt(1  ).lt.hrstrt)) then
              do 450 it = itg,itl
                if (dat(it).gt.dastrt .or. (dat(it).eq.dastrt .and. 
     *           hrt(it).ge.hrstrt)) go to 460
  450         continue
              go to 590
  460         continue
              its = it
            else
              its = itg
            endif

            if (dat(nit).gt.dastop .or. (dat(nit).eq.dastop .and. 
     *       hrt(nit).gt.hrstop)) then
              do 470 it = itl,its,-1
                if (dat(it).lt.dastop .or. (dat(it).eq.dastop .and. 
     *           hrt(it).le.hrstop)) go to 480
  470         continue
              go to 590
  480         continue
              ite = it
            else
              ite = itl
            endif
          else
            its = itg
            ite = itl
          endif
          if (diag) write (6,*) 'not clipped dh'

c         5.  Remove tracks which persist for less than the 
c             required lifetime after stripping (if clipas)
c             ---------------------------------------------

          if (clipas) then
            if (tt(ite)-tt(its).lt.dttrk) go to 590
          endif

c         6.  Apply a spline to the track record
c             ----------------------------------

          if (interp) then
            do 510 it = 1,nit
              call lltorl(xt(it),yt(it),x1(it),y1(it),z1(it))
  510       continue

            call isplcf(tt,x1,xtt1,gam,nit,1,nit,0.,0.)
            call isplcf(tt,y1,ytt1,gam,nit,1,nit,0.,0.)
            call isplcf(tt,z1,ztt1,gam,nit,1,nit,0.,0.)

            call tr2wr(iomn,ltrue,trk,statf,statl,ifst,ilst,nit,
     *       idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
            write (iomn,'(/a9,a4,2a9)') 't','iop','x','y'

            it2 = 0
            do 530 it=its,ite-1
              itp = it+1
              it2 = it2 + 1
              dt = tt(itp) - tt(it)
              dtsqr6 = dt*dt*r6
              nsub = max(1,nint(dt/delt))
              if (nsub.gt.20) stop 'nsub.gt.20'
              rsub = 1./float(nsub)
c             write (iomn,'(20f8.3)') xt(it),yt(it),x1(it),xtt1(it),
c    *         y1(it),ytt1(it),z1(it),ztt1(it),xt(it),yt(it),tt(it)
              dist = sqrt((x1(itp)-x1(it))**2 + (y1(itp)-y1(it))**2
     *                  + (z1(itp)-z1(it))**2)
              write (iomn,'(f10.4,i3,2f9.2,2f9.4)') tt(it),iopt(it),
     *         xt(it),yt(it)
c    *         ,1.,dist
c             if (dist.gt.0.3) write (iomn,*) 'warn'
              do 520 itx = 1,nsub-1
                it2 = it2 + 1
                A2 = float(itx)*rsub
                A1 = 1. - A2
                A3 = (A1*(A1*A1 - 1.))*dtsqr6
                A4 = (A2*(A2*A2 - 1.))*dtsqr6
                x2 = A1*x1(it) + A2*x1(itp) + A3*xtt1(it) + A4*xtt1(itp)
                y2 = A1*y1(it) + A2*y1(itp) + A3*ytt1(it) + A4*ytt1(itp)
                z2 = A1*z1(it) + A2*z1(itp) + A3*ztt1(it) + A4*ztt1(itp)
                t2(it2) = tt(it) + A2*dt
                call rltoll(x2,y2,z2,xt2(it2),yt2(it2))
                rads = sqrt(x2**2 + y2**2 + z2**2)
c               write (iomn,'(16x,3(f8.3,8x),10f8.3)') x2,
c    *           y2,z2,xt2(it2),yt2(it2),t2(it2)
                write (iomn,'(9x,4x,2f9.2,f9.4)') xt2(it2),yt2(it2)
c    *           ,rads
                if (rads.gt.1.1 .or. rads.lt.0.9) write (iomn,*) 'warn'
  520         continue
  530       continue
            nit2 = it2 + 1
c           write (iomn,'(20f8.3)') xt(ite),yt(ite),x1(ite),xtt1(ite),
c    *       y1(ite),ytt1(ite),z1(ite),ztt1(ite),xt(ite),yt(ite),tt(ite)
            write (iomn,'(f10.4,i3,2f9.2,2f9.4)') tt(ite),iopt(ite),
     *       xt(ite),yt(ite)
            write (iomn,'()')
          endif

c         7.  Write positions of cyclogenesis and cyclolysis
c             ----------------------------------------------

          if (wrgl) then
            call tr2wr(iomn,ltrue,trk,statf,statl,ifst,ilst,2,
     *       idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
            write (iomn,'(/a9,a4,2a9)') 't','iop','x','y'

            if (ifst.gt.1 .and. its.eq.1) 
     *       write (iomn,'(f10.4,i3,2f9.2,2x,i1)') 
     *       tt(itg),iopt(itg),xt(itg),yt(itg),1
            if (ilst.lt.ic .and. ite.eq.nit)
     *       write (iomn,'(f10.4,i3,2f9.2,2x,i1)') 
     *       tt(itl),iopt(itl),xt(itl),yt(itl),2
            write (iomn,'()')
          endif

c         7.  Modify header information
c             -------------------------

          if (its.ne.1  ) then
            statf = 4
            ifst = ifst + (its-1)
            idafst = dat(its)
            ihrfst = hrt(its)
          endif
          if (ite.ne.nit) then
            statl = 4
            ilst = ilst - (nit-ite)
            idalst = dat(ite)
            ihrlst = hrt(ite)
          endif

          nit = ite - its + 1

c         8.  Renumber surviving systems
c             --------------------------

          if (renumb) then
            trknew = trknew + 1
          else
            trknew = trk
          endif

c         9.  Obtain the interpolated values of function values 
c             and selected differential quantities 
c             -------------------------------------------------

c         if (interp) then
c         endif

        endif

c       Write point data
c       ----------------

        if (.not.(interp.or.wrgl)) then
        if (ddet) then
          call tr2wr(iomn,trfm,trknew,statf,statl,ifst,ilst,nit,
     *     idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          call tr3wr(iomn,trfm,itabt3,itabt4,nvarlt,nitlt,nit,
     *     vart(its,1))
        else
          if (tdet) then
            call tr2wr(iomn,ltrue,trk,statf,statl,ifst,ilst,nit,
     *       idafst,ihrfst,idalst,ihrlst,itabt3,itabt4)
          else if (vdet) then
            if (ifst.gt.ifstlast) write (iomn,570) 
     *       ifst,idafst,ihrfst,trk
  570       format (/i4,':  ',i6,1x,i4,'  Tracks ',i5,' ...',$)
            ifstlast = ifst
            itrklast = trk
          endif
        endif
        endif

  590   continue
  600 continue
  610 continue

      if (vdet) then
        write (6,'(x,i4)') itrklast
        write (6,'()')
      endif

      if (iomn.eq.3) close (3)

      stop

c-----------------------------------------------------------------------
c     Error conditions
c-----------------------------------------------------------------------

  910 stop ' trackmn: Error in reading optional argument (format i6).'
  920 stop ' trackmn: Error in opening file.'
  930 stop ' trackmn: End of track history content file .'
  940 stop ' trackmn: Error in reading track history content file.'
      end
