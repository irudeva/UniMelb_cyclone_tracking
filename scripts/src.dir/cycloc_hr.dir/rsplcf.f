      Subroutine rsplcf(f,c,gam,N,iL,iR,cL,cR)

c         This subroutine calculates the values of the second derivative
c     coefficients, c(i), of a cubic spline fitted to the function values,
c     f(i) of a linear array assumed of equally spaced data points over 
c     the interval i = iL,iR.  

c         The routine follows the algorithm suggested in 'Numerical Recipes,
c     The Art of Scientic Computing', Press W.H., Flannery B.P., Teukolsky S.A.,
c     and Vetterling W.T., Cambridge University Press, 1986. pp. 86-89.

c     18 Feb 1991, R.J. Murray, 

c-------------------------------------------------------------------------------
c     Explanation.
c-------------------------------------------------------------------------------

c     N                dimensions of the function array
c     i = iL,iR        The range of array elements to be fitted
c     f(i)             Function value at the (i) grid point
c     c(i)             2nd derivative calculated for grid point (i)
c     gam(i)           A working array used by the fitting routine
c     cL,cR            Prescribed boundary values of c at i = iL and iR

c-------------------------------------------------------------------------------
c     Declarations
c-------------------------------------------------------------------------------

      parameter (c0=0.,c1=1.,c2=2.,c4=4.,c6=6.)
      dimension f(N),c(N),gam(N)

c-------------------------------------------------------------------------------
c     Checks
c-------------------------------------------------------------------------------

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

c-------------------------------------------------------------------------------
c     Spline calculation
c-------------------------------------------------------------------------------

      ga = (f(iL1) - f(iL))
      gam(iL) = c0

      do 80 i = iL1,iRm1
        gb = (f(i+1) - f(i))
        r = c6*(gb - ga)
        beta = c4 - gam(i-1)
        if (beta.eq.c0) stop ' Beta = 0.'
        c(i) = (r - c(i-1))/beta
        gam(i) = c1/beta
        ga = gb
 80   continue

      do 90 i = iRm1,iL1,-1
          c(i) = c(i) - gam(i)*c(i+1)
 90   continue

      return
      end
