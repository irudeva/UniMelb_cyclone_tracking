c Include file 'trk3.h'
c     Track arrays
      parameter (nitlt=3400,nvarlt=17)  ! Was 120 then 400 KK 30/10/2005
                                        ! nitlt = 200 * 17 = 3400 KK 8/9/2006
      integer stat1t,stat2t,dat,hrt
      COMMON /bltrk05/tt(nitlt),dat(nitlt),hrt(nitlt)
     *, stat1t(nitlt),stat2t(nitlt),kt(nitlt),iopt(nitlt)
     *, qt(nitlt),xt(nitlt),yt(nitlt),pt(nitlt),ct(nitlt)
     *, dpt(nitlt),rdt(nitlt),upt(nitlt),vpt(nitlt)
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
      parameter (nklt=1000) ! Was 160 then 400 KK 30/10/2005
      common /bltrk09/iopc(nklt),xc(nklt),yc(nklt),pc(nklt),cc(nklt)
     *, dpc(nklt),rdc(nklt),upb(nklt),vpb(nklt)
     *, zsc(nklt),upc(nklt),vpc(nklt)

c     Recent cyclone file data arrays
      COMMON /bltrk10/
     *  xa(nklt),xb(nklt),xp(nklt), ya(nklt),yb(nklt),yp(nklt)
     *, pa(nklt),pb(nklt),pp(nklt), iopa(nklt),iopb(nklt)
     *, kab(nklt),kbc(nklt),kcb(nklt), qa(nklt),qf(nklt)

c     Association arrays
      parameter (nasslt=2000)  ! ckk Was 900
      COMMON /bltrk12/nass,kpn(nasslt),kcn(nasslt),qn(nasslt)

c     Switches
      logical wdata,newtks,trfm,trfmin,trnml,trnmlin
      COMMON /bltrk13/idiagt,wdata,ierr,newtks,trfm,trfmin,trnml,trnmlin
