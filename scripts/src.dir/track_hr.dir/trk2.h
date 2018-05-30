c Include file 'trk2.h'
c     Date counters
      integer da1,hr1,dab,hrb,dac,hrc
      real mdt,ndt
      COMMON /bltrk02/t1,da1,hr1,ib,tb,dab,hrb,nkb,mdt
     *, ic,tc,dac,hrc,nkc,ndt

c     Track file counters
      COMMON /bltrk03/ihza,lstrk1,lstrk2

c     Prediction velocity file
      character*50 afile
      COMMON /bltrk04/afile

