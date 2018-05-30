      Subroutine rlconv

c       Subroutine for converting latitude and longitude to and from
c     rectilinear coordinates on the surface of a unit sphere.

c-------------------------------------------------------------------------------

c     implicit double precision (a-h,l,o-z)
c     parameter (c0=0.d0,c1=1.d0,c2=2.d0,r2=c1/c2,c90=90.d0)
c     parameter (c180=180.d0,c360=360.d0)
c     parameter (c1260=1260.d0,c1440=1440.d0)
c     parameter (pi=3.1415926535898d0)

      implicit real (a-h,l,o-z)
      parameter (c0=0.,c1=1.,c2=2.,r2=c1/c2,c90=90.)
      parameter (c180=180.,c360=360.)
      parameter (c1260=1260.,c1440=1440.)
      parameter (pi=3.1415926535898)

      parameter (r2pi=pi*r2,c2pi=c2*pi)
      parameter (rad=c180/pi,rrad=c1/rad,c2rad=c2*rad,r2rad=c1/c2rad)

c     angd(xx) = damod(xx+c1440,c360)
      angd(xx) = amod(xx+c1440,c360)

c-------------------------------------------------------------------------------

      Entry lltorl(lam,phi,x,y,z)

      lamr = lam*rrad
      phir = phi*rrad
c     x = dcos(lamr)*dcos(phir)
c     y = dsin(lamr)*dcos(phir)
c     z = dsin(phir)
      x = max(min(cos(lamr)*cos(phir),1.),-1.)
      y = max(min(sin(lamr)*cos(phir),1.),-1.)
      z = max(min(sin(phir),1.),-1.)

      return

c-------------------------------------------------------------------------------

      Entry rltoll(x,y,z,lam,phi)

c     lam = angd(rad*datan2(y,x))
c     phi = rad*datan(z/dsqrt(x*x + y*y))
      lam = angd(rad*atan2(y,x))
      phi = rad*atan(z/sqrt(x*x + y*y))
      if (abs(phi).gt.89.9999) then
        lam = 0.
        phi = sign(90.,phi)
      endif

      return
      end
