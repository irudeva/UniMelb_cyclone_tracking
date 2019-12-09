      Subroutine splseg(f,n,sp,is,ie)

      dimension f(n)

      if (ie+1.eq.n) then
        is = 0
        ie = 0
        return
      endif

      if (ie.le.0) ie = -1
      do 10 is= ie+2,n
        if (f(is).ne.sp) go to 20
   10 continue
      is = 0
      ie = 0
      return
   20 continue
      if (is.eq.n) then
        ie = n
        return
      endif

      do 30 ie = is+1,n
        if (f(ie).eq.sp) go to 40
c       write (6,*) '  ',is,ie,f(ie)
   30 continue
      ie = n
      return
   40 continue
      ie = ie - 1

      return
      end

