c Include file 'trk1.h'
c     Tracking parameters
      integer da0,hr0,ddhmt
      logical rsttks
      COMMON /bltrk01/da0,hr0,ddhmt,iopmxt,cmnt1,cmnt2,refdt
     *, iadvtp,wsteer,fsteer,wmotn,wpten
     *, requiv,rcprob,rpbell,qmxopn,qmxnew
     *, nsort,irevmx,merget,qmerge,itabt1,itabt2,rsttks

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

c Include file 'trk3.h'
c     Track arrays
      parameter (nitlt=120,nvarlt=17)
      integer stat1t,stat2t,dat,hrt
      COMMON /bltrk05/tt(nitlt),dat(nitlt),hrt(nitlt)
     *, stat1t(nitlt),stat2t(nitlt),kt(nitlt),iopt(nitlt)
     *, qt(nitlt),xt(nitlt),yt(nitlt),pt(nitlt),ct(nitlt)
     *, cxt(nitlt),rdt(nitlt),dpt(nitlt),ugt(nitlt),vgt(nitlt)
      dimension vart(nitlt,nvarlt)
      equivalence (tt(1),vart(1,1))

c     Tracking dates
      integer ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop
      COMMON /bltrk06/ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop

c     Projection parameters and limits

      character hemis*1
      logical pstrak
      common /bltrk07/hemis
      common /bltrk08/pstrak,fhem,xcen,ycen,rproj

c     Current cyclone file data arrays
      parameter (nklt=100)
      common /bltrk09/iopc(nklt),xc(nklt),yc(nklt),pc(nklt),cc(nklt)
     *, cxc(nklt),rdc(nklt),dpc(nklt),upb(nklt),vpb(nklt)
     *, upc(nklt),vpc(nklt)

c     Recent cyclone file data arrays
      COMMON /bltrk10/
     *  xa(nklt),xb(nklt),xp(nklt), ya(nklt),yb(nklt),yp(nklt)
     *, pa(nklt),pb(nklt),pp(nklt), iopa(nklt),iopb(nklt)
     *, kab(nklt),kbc(nklt),kcb(nklt), qa(nklt),qf(nklt)

c     Cyclone centred geostrophic winds
      COMMON /bltrk11/ugc(nklt),vgc(nklt)

c     Association arrays
      parameter (nasslt=140)
      COMMON /bltrk12/nass,kpn(nasslt),kcn(nasslt),qn(nasslt)

c     Switches
      logical wdata,newtks,trfm,trfmin,trnml,trnmlin
      COMMON /bltrk13/idiagt,wdata,ierr,newtks,trfm,trfmin,trnml,trnmlin
