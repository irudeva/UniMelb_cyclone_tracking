      Program psfm

c       This programme produces a formatted file from the unformatted
c     polar stereographic file produced by "cycloc" or "trangp".

c     Written by R.J. Murray
c     Last revised 5th Jan., 1996.

c     ------------------------------------------------------------------

      parameter (nilt=321,njlt=321,nijlt=nilt*njlt)
      integer da,hr,dasel,hrsel
      character quant*8,level*9,lunit*10,source*10,dmode*6,hemis*1
      character grid*17,unit*12,hemsel*1
      character*80 file1,optarg
      character*1 ans
      integer optind
      logical form,window

      dimension f(nijlt)

      namelist /nml/quant,level,lunit,source,dmode,unit,grid,
     * ni,nj,hemis,xcen,ycen,rproj,rdiff

      da = 0
      hr = 0
      hemsel = ' '
      form = .false.
      window  = .false.
   10 continue
      nopt = ngtopt("d:h:p:fw",optind,optarg)
        if (nopt.eq.-1) go to 20
        if (char(nopt).eq.'d') read (optarg,'(i6)') dasel
        if (char(nopt).eq.'h') read (optarg,'(i4)') hrsel
        if (char(nopt).eq.'p') read (optarg,'(a1)') hemsel
        if (char(nopt).eq.'f') form = .true.
        if (char(nopt).eq.'w') window  = .true.
      go to 10
   20 continue
      if (iargc().lt.optind) then
        write (6,*) ' Usage: psfmx [-d dasel -h hrsel',
     *   ' -p hemsel -f -w] psfile'
        write (6,*) '   -d  day number required'
        write (6,*) '         (first field if unspecified)'
        write (6,*) '   -h  hour required'
        write (6,*) '   -p  projection (N or S) required'
        write (6,*) '   -f  write formatted file'
        write (6,*) '   -w  write window formatted file'
        stop
      endif
      call getarg(optind,file1)

      open (1,file=file1,status='old',form='unformatted')
      do 40 k = 1,10000
        call psrd(1,quant,level,lunit,source,dmode,unit,grid,
     *   ni,nj,hemis,xcen,ycen,rproj,da,hr,f,nijlt,spval,
     *   rdiff,ier)
        if (ier.eq.1) stop
        if (ier.eq.2) stop ' Error in reading data file.'
        if (ier.eq.3) stop ' Data array exceeds bounds of psfm.'
        if (.not.(form.or.window)) then
          if (k.eq.1) write (6,nml)
          if (k.eq.1) write (6,*) ' '
          write (6,'(i6,x,i4,2x,a1,''H'')') da,hr,hemis
        else
          if (((dasel.eq.0).or.((da.eq.dasel).and.(hr.eq.hrsel)))
     *     .and.((hemsel.eq.' ').or.(hemsel.eq.hemis))) then
            write (6,nml)
            write (6,'(/'' Date = '',i6,x,i4,2x,a1,''H'')') da,hr,
     *       hemis
            if (form) then
              write (6,*) ' '
              ijstop = 0
              do 30 j = 1,nj
                ijstrt = ijstop + 1
                ijstop = ijstop + ni
                write (6,'('' j = '',i3)') j
                write (6,'(6f13.4)') (f(ij),ij=ijstrt,ijstop)
   30         continue
              write (6,*) ' '
            else if (window) then
              write (0,'('' Limited part of array (y or n)?         : ''
     *         $)')
              read (5,'(a1)') ans
              if (ans.eq.'y') then
                write (0,
     *           '('' Enter min and max i coordinates         : ''$)')
                read (5,*) iL,iR
                write (0,
     *           '('' Enter min and max j coordinates         : ''$)')
                read (5,*) jD,jU
                if (iL.lt.1) iL = 1
                if (iR.gt.ni) iR = ni
                if (jD.gt.jU) then
                  if (jU.lt.1) jU = 1
                  if (jD.gt.nj) jD = nj
                  jx = jU
                  jU = -jD
                  jD = -jx
                else
                  if (jD.lt.1) jD = 1
                  if (jU.gt.nj) jU = nj
                endif
              else
                iL = 1
                iR = ni
                write (0,
     *           '('' Should j values be descending (y or n)? : ''$)')
                read (5,'(a1)') ans
                if (ans.eq.'y') then
                  jD = -1
                  jU = -nj
                else
                  jD = 1
                  jU = nj
                endif
              endif
              write (0,'('' Factor to divide values by (f6.2 format)''/
     *               ''  (format is 1pe12.4) if zero)           : ''$)')
              read (5,*) scale
              call matrix(6,f,ni,iL,iR,jD,jU,scale,spval)
            endif
            stop
          endif
        endif
   40 continue
      close (1)

      stop
      end
