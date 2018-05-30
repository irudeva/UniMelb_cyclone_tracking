      Subroutine isplcf(x,f,c,gam,N,iL,iR,cL,cR)

c         This subroutine calculates the values of the second derivative
c     coefficients, c(i), of a cubic spline fitted to the function values,
c     f(i) over the interval i = imin,imax, where the values of the
c     independent variable, x, are given.

c         The routine follows the algorithm suggested in 'Numerical Recipes,
c     The Art of Scientific Computing', Press W.H., Flannery B.P., Teukolsky 
c     S.A., and Vetterling W.T., Cambridge University Press, 1986. pp. 86-89.

c     2 Dec. 1992, R.J. Murray, 

c-------------------------------------------------------------------------------
c       (1)  Explanation.
c-------------------------------------------------------------------------------

c     N                dimensions of the function array
c     i = iL,iR        The range of array elements to be fitted
c     x(i)             Coordinate of the (i) grid point
c     f(i)             Function value at the (i) grid point
c     c(i)             2nd derivative calculated for grid point (i)
c     gam(i)           A working array used by the fitting routine
c     cL,cR            Prescribed boundary values of c at i = iL and iR

c-------------------------------------------------------------------------------
c       (1)  Spline dimensions, limits, and regularity.
c-------------------------------------------------------------------------------

      parameter (c1=1.,c6=6.)
      dimension f(N),c(N),gam(N),x(N)

      iLR1 = iR - iL + 1
      iL1 = iL + 1
      iRm1 = iR - 1
      if (iLR1.le.2) then
        c(iL) = cL
        c(iR) = cR
        return
      endif

      c(iL) = cL
      c(iR) = cR

      dxa = x(iL1) - x(iL)
      if (dxa.le.0.) stop ' x(i+1) le x(i)'

      ga = (f(iL1) - f(iL))/dxa
      gam(iL) = 0.

      do 140 i = iL1,iRm1
        dxb = x(i+1) - x(i)
        if (dxb.le.0.) stop ' x(i+1) le x(i)'
        xr1 = dxb/dxa
        xr2 = 2.*(xr1 + c1)
        gb = (f(i+1) - f(i))/dxb
        r = c6*(gb - ga)/dxa
        beta = xr2 - gam(i-1)
        if (beta.eq.0.) stop ' Beta = 0.'
        c(i) = (r - c(i-1))/beta
        gam(i) = xr1/beta
        dxa = dxb
        ga = gb
 140  continue

      do 160 i = iRm1,iL1,-1
        c(i) = c(i) - gam(i)*c(i+1)
 160  continue

      return
      end
