      Subroutine sdiv(i1,j1,ierr)

c         A routine for calculating the derivatives 
c     fij = del^(i+j)/delx^i/dely^j at (i,j) for a function bicubic 
c     in the domain (i<x<i+1, j<y<j+1) from the values of
c     f,f20,f02,f22 at (i,j),(i+1,j),(i,j+1),(1+1,j+1).

c     Last revised 11th Jan., 1996.

c-------------------------------------------------------------------------------

      parameter (r3=1./3.,r6=1./6.,r9=1./9.,r18=1./18.,r36=1./36.)

#include "cyc3.h"   
#include "cyc5.h"   
#include "cyc6.h"   

c-------------------------------------------------------------------------------

      if (i1.eq.ni) stop ' sdiv: i1 = ni not permitted.'
      if (j1.eq.nj) stop ' sdiv: j1 = nj not permitted.'

      i1j1 = i1 + ni*(j1-1)
      i2j1 = i1j1 + 1
      i1j2 = i1j1 + ni
      i2j2 = i2j1 + ni

      f11 = f(i1j1)
      f12 = f(i1j2)
      f21 = f(i2j1)
      f22 = f(i2j2)

      ierr = 0
      if ((f11.eq.spval).or.(f12.eq.spval).or.
     *    (f21.eq.spval).or.(f22.eq.spval)) then
        ierr = 1
        return
      endif

      fyy11 = fyy(i1j1)
      fyy12 = fyy(i1j2)
      fyy21 = fyy(i2j1)
      fyy22 = fyy(i2j2)
      fxx11 = fxx(i1j1)
      fxx12 = fxx(i1j2)
      fxx21 = fxx(i2j1)
      fxx22 = fxx(i2j2)
      fxxyy11 = fxxyy(i1j1)
      fxxyy12 = fxxyy(i1j2)
      fxxyy21 = fxxyy(i2j1)
      fxxyy22 = fxxyy(i2j2)

      fmn0(0,0) = f11
      fmn0(1,0) = f21 - f11 - r6*fxx21 - r3*fxx11
      fmn0(2,0) = fxx11
      fmn0(3,0) = fxx21 - fxx11

      fmn0(0,1) = f12 - f11 - r6*fyy12 - r3*fyy11
      fmn0(1,1) = f22 - f12 - f21 + f11
     *         + r6*(fxx21 - fxx22  + fyy12 - fyy22)
     *         + r3*(fxx11 - fxx12  + fyy11 - fyy21)
     *         + r36*fxxyy22 + r18*(fxxyy12 + fxxyy21) + r9*fxxyy11
      fmn0(2,1) = fxx12 - fxx11 - r6*fxxyy12 - r3*fxxyy11
      fmn0(3,1) = fxx22 - fxx12 - fxx21 + fxx11 + r6*(fxxyy12 - fxxyy22)
     *           + r3*(fxxyy11 - fxxyy21)

      fmn0(0,2) = fyy11
      fmn0(1,2) = fyy21 - fyy11 - r6*fxxyy21 - r3*fxxyy11
      fmn0(2,2) = fxxyy11
      fmn0(3,2) = fxxyy21 - fxxyy11

      fmn0(0,3) = fyy12 - fyy11
      fmn0(1,3) = fyy22 - fyy21 - fyy12 + fyy11 + r6*(fxxyy21 - fxxyy22)
     *           + r3*(fxxyy11 - fxxyy12)
      fmn0(2,3) = fxxyy12 - fxxyy11
      fmn0(3,3) = fxxyy22 - fxxyy21 - fxxyy12 + fxxyy11

      return
      end
