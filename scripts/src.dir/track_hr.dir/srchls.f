      Subroutine srchls(x1,x2,N,klo,khi)

c     Routine for searching an ordered list x2(i), i =1,N to find
c     the indices k=klo,khi of x2 which bracket the value x1.

      dimension x2(N)

      klo = 1
      khi = N
      if (x2(1).gt.x2(N)) then
        if (x1.lt.x2(khi)) then
          klo = N
          khi = 0
        else if (x1.gt.x2(klo)) then
          khi = 1
          klo = 0
        else
 10       continue
          kdif = khi - klo
          if (kdif.gt.1) then
            kav = (khi + klo)/2
            if (x2(kav).lt.x1) then
              khi = kav
            else
              klo = kav
            endif
            go to 10
          endif
        endif
      else
        if (x1.gt.x2(khi)) then
          klo = N
          khi = 0
        else if (x1.lt.x2(klo)) then
          khi = 1
          klo = 0
        else
 20       continue
          kdif = khi - klo
          if (kdif.gt.1) then
            kav = (khi + klo)/2
            if (x2(kav).gt.x1) then
              khi = kav
            else
              klo = kav
            endif
            go to 20
          endif
        endif
      endif
 
      return
      end
