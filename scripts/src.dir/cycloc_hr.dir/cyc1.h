c Include file "cyc1.h"

c     Cyclone finding parameters

      parameter (nsupmx=6)
      character dmode*6,quant*8,level*9,lunit*10,source*10,unit*12
      character hilo*1,feat*4,cunit*17,area*10,qsteer*6,svname*10
      common /blcyc1/quant,level,lunit,source,unit,cunit,dmode,
     * hilo,feat,area,qsteer,svname(nsupmx)

      real latmnc,latmxc,lonmnc,lonmxc
      logical sphtrg
      common /blcyc2/iopmxc,istmxc,latmnc,latmxc,lonmnc,lonmxc,
     * nshell,mscrn,sdrmx,drmx1,drmx2,itmx1,itmx2,diflt1,diflt2,
     * iconcv,cmnh,cmnc0,cmnc1,cmnc2,swvmn,dpmn,fccmn,icendp,cvarad,
     * rdincr,nrddir,sphtrg,rdustr,npgdir,alatgv,rhoa,upfact,
     * fmxc,frmxc,frcmxc,iavsup(nsupmx),rdsupv,zsmax,zscr1,zscr2,
     * ftopeq,rdiff,cmnhw,cmncw,dpmnw,swvmnw,fmxcw,qminmm
