c Include file "cyc5.h"

c     Arrays used on the PS grid

      parameter (nlonlt=1441,nlatlt=721,marglt=10)
      parameter (nlon2lt=nlonlt+2*marglt,nlat2lt=nlatlt+2*marglt)
      real lon,lat,lon2,lat2
ckk      common /blltln/lon(nlonlt),lat(nlatlt),lon2(nlon2lt),lat2(nlon2lt)
      common /blltln/lon(nlonlt),lat(nlatlt),lon2(nlon2lt),lat2(nlat2lt)
      common /blltlnz/lonz(nlonlt),latz(nlatlt)

c     parameter (nlllt=10585,nll2lt=14329)     ! 10585 = 145*73, 14329 = 161*89
c     parameter (nlllt=16471,nll2lt=22311)     ! 16471 = 181*91, 22311 = 201*111
ckk   parameter (nlllt=65341,nll2lt=77081)     ! 65341 = 361*181,77081 = 381*201
      parameter (nlllt=1038961,nll2lt=1082601) !  = 1441*721, = 1461*741
c     parameter (nijlt=6561)                   ! 6561  = 81*81
c     parameter (nijlt=22801)                  ! 22801 = 151*151
c     parameter (nijlt=25921)                  ! 25921 = 161*161
ckk   parameter (nijlt=103041)                 ! 103041= 321*321
      parameter (nijlt=1640961)                ! 1640961= 1281*1281

crmw      parameter (nar1lt=max0(nlllt,2*nijlt))
crmw      parameter (nar2lt=max0(nll2lt,nijlt))

      parameter (nar1lt=3281922)
      parameter (nar2lt=1640961)

      common /blfhk/ fhk(nar1lt)
      common /blfij/ fij(nijlt,2)

      common /blfxy1/f(nar2lt)
      common /blfxy2/fxx(nar2lt)
      common /blfxy3/fyy(nar2lt)
      common /blfxy4/fxxyy(nar2lt)
      common /blfxy5/q(nar2lt)

      common /blfxy6/zij(nijlt,2)
