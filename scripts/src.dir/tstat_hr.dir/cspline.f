      Subroutine cspline(ireg,dxN,x,f,c,gamma,N,margin,cL,cR,idiag)

      dimension f(1-margin:N+margin),c(1-margin:N+margin)
      dimension gamma(1-margin:N+margin),x(1-margin:N+margin)
      integer trk
      common /blcs/trk

      if ((margin.ge.1).and.(f(1).ne.f(N))) 
     * stop 'f(1) should = f(N) for cyclic splines.'

c----------------------------------------------------------------------
c       (1)  Spline limits and type.
c----------------------------------------------------------------------

      iL = 1 - margin
      iR = N + margin
      N1 = N + 1
      Nm1 = N - 1
      iLR1 = iR - iL + 1
      iL1 = iL + 1
      iRm1 = iR - 1

      c(iL) = cL
      c(iR) = cR

      if (iLR1.le.2) return

      if (ireg.le.0) then
          if (x(N).le.x(1)) then
            write (6,*) trk, ' Circumference le 0.'
            write (6,*) x
            write (6,*) f
            stop
          endif
          circum = (x(N) - x(1))
          if (ireg.lt.0) go to 100
          dx = circum/float(Nm1)
          dcrit = dx*0.005
          do 10 i = 2,N
              difx = abs(x(i) - x(i-1) - dx)
              if (difx.gt.dcrit) go to 100
 10       continue
      else
          circum = dxN
          dx = dxN/float(Nm1)
      endif
              
c----------------------------------------------------------------------
c       (2)  Regular spline.
c----------------------------------------------------------------------

      if (margin.ge.1) then
          do 20 i = N1,iR
              f(i) = f(i - Nm1)
 20       continue
          do 30 i = 0,iL,-1
              f(i) = f(i + Nm1)
 30       continue
      endif

      dxsq = dx*dx

          ga = (f(iL1) - f(iL))/dx
          gamma(iL) = 0.


      do 80 i = iL1,iRm1

          gb = (f(i+1) - f(i))/dx
          r = 6.*(gb - ga)/dx
          beta = 4. - gamma(i-1)
          if (beta.eq.0.) stop
          c(i) = (r - c(i-1))/beta
          gamma(i) = 1./beta
          ga = gb

 80   continue

      do 90 i = iRm1,iL1,-1
          c(i) = c(i) - gamma(i)*c(i+1)
 90   continue

      if (idiag.eq.1) then
          write (6,*) 'circum,dx=',circum,dx
          write (6,*) (f(i),i=iL,iR)
          write (6,*) (c(i),i=iL,iR)
      endif

      return

c----------------------------------------------------------------------
c       (3)  Irregular spline.
c----------------------------------------------------------------------

 100  continue

      if (margin.ge.1) then
          do 120 i = N1,iR
              f(i) = f(i - Nm1)
              x(i) = x(i - Nm1) + circum
 120      continue
          do 130 i = 0,iL,-1
              f(i) = f(i + Nm1)
              x(i) = x(i + Nm1) - circum
 130      continue
      endif

          dxa = x(iL1) - x(iL)
          if (dxa.le.0.) stop ' x(i+1) le x(i)'
          ga = (f(iL1) - f(iL))/dxa
          gamma(iL) = 0.

      do 140 i = iL1,iRm1

          dxb = x(i+1) - x(i)
          if (dxb.le.0.) stop ' x(i+1) le x(i)'
          xr1 = dxb/dxa
          xr2 = 2.*(xr1 + 1.)
          gb = (f(i+1) - f(i))/dxb
          r = 6.*(gb - ga)/dxa
          beta = xr2 - gamma(i-1)
          if (beta.eq.0.) stop ' Beta = 0.'
          c(i) = (r - c(i-1))/beta
          gamma(i) = xr1/beta
          dxa = dxb
          ga = gb

 140  continue

      do 160 i = iRm1,iL1,-1
          c(i) = c(i) - gamma(i)*c(i+1)
 160  continue

      if (idiag.eq.1) then
          write (6,*) (x(i),i=iL,iR)
          write (6,*) (f(i),i=iL,iR)
          write (6,*) (c(i),i=iL,iR)
      endif

      return
      end
