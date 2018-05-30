      Subroutine bisplcf(nilt,njlt,x,y,f,fxx,fyy,fxxyy,iL,iR,jD,jU,
     * idiag,ierr)

c         The programme calculates bicubic spline coefficients,
c     fxx,fyy,fxxyy for the function f, with grid positions x and y
c     as given in the argument list.  It is assumed that the
c     second derivatives are zero along the boundaries.

c     R.J. Murray, 9th December, 1992 (renamed from "bsplcf.f 1/3/93)

      parameter (ijmax=100)

      dimension f(nilt,njlt),fxx(nilt,njlt)
      dimension fyy(nilt,njlt),fxxyy(nilt,njlt)
      dimension x(nilt),y(njlt)
      dimension a(ijmax),c(ijmax),gam(ijmax)

      ierr = 0
      if ((iL.lt.1).or.(iR.gt.nilt).or.(jD.lt.1).or.
     * (jU.gt.njlt)) stop ' Limits of 2D spline exceed array bounds.'
      if ((iL.gt.iR).or.(jD.gt.jU)) stop
     * ' i or j dimension of spline < 1.'
      if ((nilt.gt.ijmax).or.(njlt.gt.ijmax)) then
        write (6,*) ' i or j dimension  < ',ijmax
        stop
      endif

      do 30 j = jD,jU
        call isplcf(x,f(1,j),fxx(1,j),gam,iN,iL,iR,0.,0.)
   30 continue

      do 60 i = iL,iR
        do 40 j = jD,jU
          a(j) = f(i,j)
   40   continue
        call isplcf(y,a,c,gam,jN,jD,jU,0.,0.)
        do 50 j = jD,jU
          fyy(i,j) = c(j)
   50   continue
   60 continue

      do 90 j = jD,jU
        call isplcf(x,fyy(1,j),fxxyy(1,j),gam,iN,iL,iR,0.,0.)
   90 continue

      return
      end
