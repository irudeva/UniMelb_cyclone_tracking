      Subroutine findi1(x,N,xd,ilast,inext,ie)

      dimension xd(N)

          ilast = 1
          inext = N
          ie = 0
      if (x.le.xd(1)) then
          inext = 2
          ie = 1
      else if (x.ge.xd(N)) then
          ilast = N - 1
          ie = 2
      else
 10       continue
              idif = inext - ilast
          if (idif.gt.1) then
              iav = (inext + ilast)/2
              if (xd(iav).gt.x) then
                  inext = iav
              else
                  ilast = iav
              endif
          go to 10
          endif
      endif
 
      return
      end
