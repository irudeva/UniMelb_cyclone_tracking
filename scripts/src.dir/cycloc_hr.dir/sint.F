      Subroutine sint(x,y,m,n,fmn)

c     Routine for evaluating a bicubic function using a Taylor series:
c     f = Sigma_(j=0)^3 Sigma_(i=0)^3 fij x^m y^n /(m!n!)

c     Last revised 11th Jan., 1996

#include "cyc6.h"     

      dimension xx(0:3),yy(0:3)

      xfact = 1.
      den = 1.
      do 10 md = m,3
        xx(md) = xfact
        xfact = xfact*x/den
        den = den + 1.
 10   continue

      yfact = 1.
      den = 1.
      do 20 nd = n,3
        yy(nd) = yfact
        yfact = yfact*y/den
        den = den + 1.
 20   continue

      fmn = 0.
      do 40 nd = n,3
        do 30 md = m,3
          fmn = fmn + fmn0(md,nd)*xx(md)*yy(nd)
 30     continue
 40   continue

      return
      end
