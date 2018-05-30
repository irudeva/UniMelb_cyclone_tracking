c Include file "cyc4.h"

c     Cyclone arrays 

      parameter (nklt=1000,nsuplt=6) ! Was 200 KK 6/7/2004 then 320 
      parameter (spvc=-999.)
      common /blcyc7/iopc(nklt),xc(nklt),yc(nklt),fc(nklt),cc(nklt),
     * cxc(nklt),rdc(nklt),dpc(nklt),zsc(nklt),upc(nklt),vpc(nklt),
     * sc(nklt,nsuplt)
      common /blcyc8/klast

