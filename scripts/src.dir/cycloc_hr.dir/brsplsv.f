      Subroutine brsplsv(nilt,njlt,f,fxx,fyy,fxxyy,iL,iR,jD,jU,
     * spval,idiag,ierr)

c         The programme calculates bicubic spline coefficients,
c     fxx,fyy,fxxyy for the function f, with grid spacings assumed
c     to be constant.  It is assumed that the
c     second derivatives are zero along the boundaries.

c         If missing values ("spval") be expected (ispv=1), the spline 
c     coefficients are calculated in segments whose starting and ending 
c     indices are determined by "splseg".

c     R.J. Murray, 9th December, 1992 (renamed from "bsplcf.f 1/3/93)
c                  Extended to deal with missing values (15/5/93)

      parameter (ijmax=1461)

      dimension f(nilt,njlt),fxx(nilt,njlt)
      dimension fyy(nilt,njlt),fxxyy(nilt,njlt)
      dimension a(ijmax),c(ijmax),gam(ijmax)

c     Dimension checks
c     ----------------

      if ((iL.lt.1).or.(iR.gt.nilt).or.(jD.lt.1).or.(jU.gt.njlt)) then
        write (6,*) ' brsplsv: Limits of 2D spline exceed array bounds.'
        write (6,*) ' iL=',iL,', iR=',iR,', jD=',jD,', jU=',jU,'.'
        stop
      endif
      if ((iL.gt.iR).or.(jD.gt.jU)) then
        write (6,*) 'brsplsv: iL=',iL,' or iR=',iR,
     *   ' dimension of spline < 1.'
        stop
      endif
      if ((nilt.gt.ijmax).or.(njlt.gt.ijmax)) then
        write (6,*) 'brsplsv: nilt=',nilt,' or njlt=',nj,
     *   ' dimension  > ',ijmax
        stop
      endif

      ierr = 0

c     Check whether there be special values
c     -------------------------------------

      ispv = 0
      do 05 j = 1,njlt
      do 05 i = 1,nilt
        if (f(i,j).eq.spval) ispv = 1
   05 continue
      if (idiag.ge.1) then
        if (ispv.eq.1) write (6,*) ' Special values found'
        if (ispv.eq.0) write (6,*) ' No special values found'
      endif

      if (ispv.eq.1) then

c       Missing values expected
c       -----------------------

        do 15 j = 1,njlt
        do 10 i = 1,nilt
          fxx(i,j) = spval
          fyy(i,j) = spval
          fxxyy(i,j) = spval
   10   continue
   15   continue
 
        do 30 j = jD,jU
          istrt = 0
          istop = 0
          do 20 iseg = iL,iR
            call splseg(f(1,j),iR,spval,istrt,istop)
            if (istrt.eq.0) go to 25
            call rsplcf(f(1,j),fxx(1,j),gam,iR,istrt,istop,0.,0.)
   20     continue
   25     continue
   30   continue

        do 70 i = iL,iR
          do 40 j = jD,jU
            a(j) = f(i,j)
            c(j) = spval
   40     continue
          jstrt = 0
          jstop = 0
          do 50 jseg = jD,jU
            call splseg(a,jU,spval,jstrt,jstop)
            if (jstrt.eq.0) go to 55
            call rsplcf(a,c,gam,jU,jstrt,jstop,0.,0.)
   50     continue
   55     continue
          do 65 j = jD,jU
            fyy(i,j) = c(j)
   65     continue
   70   continue

        do 90 j = jD,jU
          istrt = 0
          istop = 0
          do 80 iseg = iL,iR
            call splseg(f(1,j),iR,spval,istrt,istop)
            if (istrt.eq.0) go to 85
            call rsplcf(fyy(1,j),fxxyy(1,j),gam,iR,istrt,istop,0.,0.)
   80     continue
   85     continue
   90   continue
      else

c       Missing values not expected
c       ---------------------------

        do 130 j = jD,jU
          call rsplcf(f(1,j),fxx(1,j),gam,iR,iL,iR,0.,0.)
  130   continue

        do 160 i = iL,iR
          do 140 j = jD,jU
            a(j) = f(i,j)
  140     continue
          call rsplcf(a,c,gam,jU,jD,jU,0.,0.)
          do 150 j = jD,jU
            fyy(i,j) = c(j)
  150     continue
  160   continue

        do 190 j = jD,jU
          call rsplcf(fyy(1,j),fxxyy(1,j),gam,iR,iL,iR,0.,0.)
  190   continue
      endif

      return
      end
